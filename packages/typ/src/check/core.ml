open Std
open Std.Collections
module TypAst = Ast
module SurfacePath = Model.Surface_path
module BindingId = Model.Binding_id
module EntityId = Model.Entity_id

type ty =
  | TInt
  | TBool
  | TChar
  | TString
  | TFloat
  | TUnit
  | TList of ty
  | TOption of ty
  | TTuple of ty list
  | TArrow of arg_label * ty * ty
  | TCon of SurfacePath.t * ty list
  | TPolyVariant of poly_variant_bound * poly_variant_tags
  | TPackage of package_ty
  | TVar of tyvar_cell

and arg_label =
  | Nolabel
  | Labelled of string
  | Optional of string

and poly_variant_bound =
  | Exact
  | Upper
  | Lower

and poly_variant_field = {
  tag: string;
  payload: ty option;
}

and poly_variant_tags = {
  mutable tags: poly_variant_field list;
}

and package_constraint = {
  type_name: SurfacePath.t;
  manifest: ty;
}

and package_ty = {
  binder: string option;
  module_type: SurfacePath.t;
  constraints: package_constraint list;
}

and tyvar_cell = {
  mutable var: tvar;
}

and tvar =
  | Unbound of int * int
  | Link of ty
  | Generic of int

type binding = {
  binding_id: BindingId.t;
  entity_id: EntityId.t;
  ty: ty;
}

type env = binding list

type record_label = {
  label: SurfacePath.t;
  owner_ty: ty;
  field_ty: ty;
}

type module_summary = {
  path: SurfacePath.t;
  items: TypAst.structure_item list;
  env_bindings: binding list;
  value_bindings: binding list;
}

type module_type_summary = {
  path: SurfacePath.t;
  items: TypAst.signature_item list;
}

type functor_summary = {
  path: SurfacePath.t;
  parameters: TypAst.functor_parameter list;
  items: TypAst.structure_item list;
  env_bindings: binding list;
  value_bindings: binding list;
}

type state = {
  mutable next_tyvar: int;
  mutable next_binding_stamp: int;
  mutable diagnostics: Diagnostics.Diagnostic.t list;
  mutable record_labels: record_label list;
  mutable module_value_bindings: binding list;
  mutable type_aliases: (SurfacePath.t * SurfacePath.t) list;
  mutable type_manifests: (SurfacePath.t * ty) list;
  mutable locally_abstract_types: (SurfacePath.t * ty) list;
  mutable module_summaries: module_summary list;
  mutable module_type_summaries: module_type_summary list;
  mutable functor_summaries: functor_summary list;
}

let unsupported_syntax = fun origin summary ->
  Diagnostics.Diagnostic.UnsupportedSyntax {
    span = origin.TypAst.span;
    kind = origin.TypAst.kind;
    summary
  }

let unsupported_type = fun origin summary ->
  Diagnostics.Diagnostic.UnsupportedType { span = origin.TypAst.span; summary }

let add_diagnostic = fun state diagnostic -> state.diagnostics <- diagnostic :: state.diagnostics

let make_state = fun ~next_binding_stamp ->
  {
    next_tyvar = 0;
    next_binding_stamp;
    diagnostics = [];
    record_labels = [];
    module_value_bindings = [];
    type_aliases = [];
    type_manifests = [];
    locally_abstract_types = [];
    module_summaries = [];
    module_type_summaries = [];
    functor_summaries = [];
  }

let fresh_tyvar = fun state ~level ->
  let id = state.next_tyvar in
  state.next_tyvar <- state.next_tyvar + 1;
  TVar { var = Unbound (id, level) }

let fresh_binding_id = fun state ~name ->
  let stamp = state.next_binding_stamp in
  state.next_binding_stamp <- stamp + 1;
  BindingId.local ~stamp ~name

let make_binding = fun state ~name ~ty ->
  let binding_id = fresh_binding_id state ~name in
  let entity_id = EntityId.resolved ~binding_id ~surface_path:name in
  { binding_id; entity_id; ty }

let rec prune = function
  | TVar ({ var=Link linked_ty } as cell) ->
      let linked_ty = prune linked_ty in
      cell.var <- Link linked_ty;
      linked_ty
  | ty -> ty

let normalized_poly_variant_tags = fun tags ->
  tags |> List.sort
    ~compare:(fun left right ->
      String.compare left.tag right.tag) |> List.fold_left ~init:[]
    ~fn:(fun fields field ->
      match fields with
      | previous :: rest when String.equal previous.tag field.tag -> previous :: rest
      | _ -> field :: fields) |> List.reverse

let find_poly_variant_field = fun tag fields ->
  List.find fields
    ~fn:(fun field ->
      String.equal field.tag tag)

let poly_variant_tag_names = fun tags -> tags |> List.map ~fn:(fun field -> field.tag)

let poly_variant_tags_subset = fun left right ->
  let right_names = poly_variant_tag_names right in
  List.all (poly_variant_tag_names left)
    ~fn:(fun tag ->
      List.exists
        (fun other ->
          String.equal tag other)
        right_names)

let same_poly_variant_tags = fun left right ->
  poly_variant_tags_subset left right && poly_variant_tags_subset right left

let copy_poly_variant_tags = fun copies ~map_payload tags ->
  match
    List.find !copies
      ~fn:(fun (source, _) ->
        Ptr.equal source tags)
  with
  | Some (_, copy) -> copy
  | None ->
      let copy = { tags = [] } in
      copies := (tags, copy) :: !copies;
      copy.tags <- tags.tags
      |> List.map
        ~fn:(fun field -> { field with payload = Option.map field.payload ~fn:map_payload })
      |> normalized_poly_variant_tags;
      copy

let rec string_of_ty = fun ty ->
  match prune ty with
  | TInt ->
      "int"
  | TBool ->
      "bool"
  | TChar ->
      "char"
  | TString ->
      "string"
  | TFloat ->
      "float"
  | TUnit ->
      "unit"
  | TList element ->
      string_of_ty element ^ " list"
  | TOption element ->
      string_of_ty element ^ " option"
  | TTuple elements ->
      elements |> List.map ~fn:string_of_ty |> String.concat " * "
  | TArrow (_, parameter, result) ->
      string_of_ty parameter ^ " -> " ^ string_of_ty result
  | TCon (path, []) ->
      SurfacePath.to_string path
  | TCon (path, [ argument ]) ->
      string_of_ty argument ^ " " ^ SurfacePath.to_string path
  | TCon (path, arguments) ->
      "("
      ^ (arguments |> List.map ~fn:string_of_ty |> String.concat ", ")
      ^ ") "
      ^ SurfacePath.to_string path
  | TPolyVariant (Exact, tags) ->
      "[ " ^ render_poly_variant_tags tags.tags ^ " ]"
  | TPolyVariant (Upper, tags) ->
      "[< " ^ render_poly_variant_tags tags.tags ^ " ]"
  | TPolyVariant (Lower, tags) ->
      "[> " ^ render_poly_variant_tags tags.tags ^ " ]"
  | TPackage package ->
      let constraints = package.constraints
      |> List.map
        ~fn:(fun constraint_ ->
          " with type "
          ^ SurfacePath.to_string constraint_.type_name
          ^ " = "
          ^ string_of_ty constraint_.manifest)
      |> String.concat "" in
      "(module " ^ SurfacePath.to_string package.module_type ^ constraints ^ ")"
  | TVar { var=Unbound (id, _) } ->
      "'_" ^ Int.to_string id
  | TVar { var=Generic id } ->
      "'a" ^ Int.to_string id
  | TVar { var=Link linked_ty } ->
      string_of_ty linked_ty

and render_poly_variant_payload = fun ty ->
  match prune ty with
  | TArrow _ -> "(" ^ string_of_ty ty ^ ")"
  | _ -> string_of_ty ty

and render_poly_variant_field = fun field ->
  match field.payload with
  | None -> "`" ^ field.tag
  | Some payload -> "`" ^ field.tag ^ " of " ^ render_poly_variant_payload payload

and render_poly_variant_tags = fun tags ->
  tags |> normalized_poly_variant_tags |> List.map ~fn:render_poly_variant_field |> String.concat " | "

exception Occurs

let arg_label_equal = fun left right ->
  match left, right with
  | Nolabel, Nolabel -> true
  | (Labelled left, Labelled right)
  | (Optional left, Optional right) -> String.equal left right
  | _ -> false

let row_tags_seen = fun seen tags ->
  List.exists
    (fun other_tags ->
      Ptr.equal other_tags tags)
    !seen

let rec occurs_adjust_levels = fun id level ty ->
  let seen_rows = ref [] in
  let rec loop ty =
    match prune ty with
    | TVar ({ var=Unbound (other_id, other_level) } as cell) ->
        if Int.equal id other_id then
          raise Occurs;
        if other_level > level then
          cell.var <- Unbound (other_id, level)
    | TVar { var=Generic _ } ->
        ()
    | TList element ->
        loop element
    | TOption element ->
        loop element
    | TTuple elements ->
        List.for_each elements ~fn:loop
    | TArrow (_, parameter, result) ->
        loop parameter;
        loop result
    | TCon (_, arguments) ->
        List.for_each arguments ~fn:loop
    | TPolyVariant (_, tags) ->
        if not (row_tags_seen seen_rows tags) then
          (
            seen_rows := tags :: !seen_rows;
            tags.tags |> List.for_each ~fn:(fun field -> Option.for_each field.payload ~fn:loop)
          )
    | TPackage package ->
        package.constraints |> List.for_each ~fn:(fun constraint_ -> loop constraint_.manifest)
    | TInt
    | TBool
    | TChar
    | TString
    | TFloat
    | TUnit ->
        ()
    | TVar { var=Link linked_ty } ->
        loop linked_ty
  in
  loop ty

let rec ty_mentions_unbound_var = fun id ty ->
  let seen_rows = ref [] in
  let rec loop ty =
    match prune ty with
    | TVar { var=Unbound (other_id, _) } ->
        Int.equal id other_id
    | TVar { var=Generic _ } ->
        false
    | TList element
    | TOption element ->
        loop element
    | TTuple elements ->
        List.exists loop elements
    | TArrow (_, parameter, result) ->
        loop parameter || loop result
    | TCon (_, arguments) ->
        List.exists loop arguments
    | TPackage package ->
        List.exists (fun constraint_ -> loop constraint_.manifest) package.constraints
    | TPolyVariant (_, tags) ->
        if row_tags_seen seen_rows tags then
          false
        else (
          seen_rows := tags :: !seen_rows;
          List.exists
            (fun field -> Option.map field.payload ~fn:loop |> Option.unwrap_or ~default:false)
            tags.tags
        )
    | TInt
    | TBool
    | TChar
    | TString
    | TFloat
    | TUnit ->
        false
    | TVar { var=Link linked_ty } ->
        loop linked_ty
  in
  loop ty

let supports_recursive_occurrence = fun ty ->
  match prune ty with
  | TPolyVariant _ -> true
  | _ -> false

let resolve_type_manifest = fun state path ->
  match
    List.find state.type_manifests
      ~fn:(fun (manifest_path, _) ->
        SurfacePath.equal manifest_path path)
  with
  | Some (_, manifest) -> Some manifest
  | None -> None

let has_type_manifest = fun state path -> Option.is_some (resolve_type_manifest state path)

let rec unify = fun state ~at left right ->
  match left, right with
  | TVar ({ var=Link linked_ty } as cell), TCon (path, []) when has_type_manifest state path -> (
      match resolve_type_manifest state path with
      | Some manifest ->
          unify state ~at linked_ty manifest;
          cell.var <- Link (TCon (path, []))
      | None -> ()
    )
  | TCon (path, []), TVar ({ var=Link linked_ty } as cell) when has_type_manifest state path -> (
      match resolve_type_manifest state path with
      | Some manifest ->
          unify state ~at linked_ty manifest;
          cell.var <- Link (TCon (path, []))
      | None -> ()
    )
  | _ ->
      unify_pruned state ~at left right

and unify_pruned = fun state ~at left right ->
  match prune left, prune right with
  | TVar left_cell, TVar right_cell when Ptr.equal left_cell right_cell ->
      ()
  | (TInt, TInt)
  | (TBool, TBool)
  | (TChar, TChar)
  | (TString, TString)
  | (TFloat, TFloat)
  | (TUnit, TUnit) ->
      ()
  | TList left, TList right ->
      unify state ~at left right
  | TOption left, TOption right ->
      unify state ~at left right
  | TTuple left, TTuple right ->
      if Int.equal (List.length left) (List.length right) then
        List.zip left right |> List.for_each ~fn:(fun (left, right) -> unify state ~at left right)
      else
        add_diagnostic
          state
          (unsupported_type
            at
            ("tuple arity mismatch: expected "
            ^ Int.to_string (List.length left)
            ^ " but got "
            ^ Int.to_string (List.length right)))
  | TArrow (left_label, left_parameter, left_result), TArrow (right_label, right_parameter, right_result) when arg_label_equal
    left_label
    right_label ->
      unify state ~at left_parameter right_parameter;
      unify state ~at left_result right_result
  | TCon (left_path, left_arguments), TCon (right_path, right_arguments) when SurfacePath.equal
    left_path
    right_path ->
      if Int.equal (List.length left_arguments) (List.length right_arguments) then
        List.zip left_arguments right_arguments
        |> List.for_each ~fn:(fun (left, right) -> unify state ~at left right)
      else
        add_diagnostic
          state
          (unsupported_type
            at
            ("type constructor arity mismatch: expected "
            ^ Int.to_string (List.length left_arguments)
            ^ " but got "
            ^ Int.to_string (List.length right_arguments)))
  | TPolyVariant (left_bound, left_tags), TPolyVariant (right_bound, right_tags) ->
      let left = normalized_poly_variant_tags left_tags.tags in
      let right = normalized_poly_variant_tags right_tags.tags in
      let unify_payloads left right =
        left
        |> List.for_each
          ~fn:(fun left_field ->
            match find_poly_variant_field left_field.tag right with
            | None -> ()
            | Some right_field -> (
                match left_field.payload, right_field.payload with
                | Some left_payload, Some right_payload -> unify state ~at left_payload right_payload
                | None, None -> ()
                | (Some _, None)
                | (None, Some _) -> add_diagnostic
                  state
                  (unsupported_type
                    at
                    ("polymorphic variant payload mismatch for `" ^ left_field.tag))
              ))
      in
      let merged =
        right
        |> List.fold_left ~init:left
          ~fn:(fun fields right_field ->
            match find_poly_variant_field right_field.tag fields with
            | None -> right_field :: fields
            | Some left_field ->
                unify_payloads [ left_field ] [ right_field ];
                fields)
        |> normalized_poly_variant_tags
      in
      let ok =
        match left_bound, right_bound with
        | (Upper, Upper)
        | (Lower, Lower) ->
            left_tags.tags <- merged;
            right_tags.tags <- merged;
            true
        | Exact, Exact ->
            same_poly_variant_tags left right
        | Upper, Lower ->
            poly_variant_tags_subset right left
        | Lower, Upper ->
            poly_variant_tags_subset left right
        | Exact, Upper ->
            poly_variant_tags_subset left right
        | Upper, Exact ->
            poly_variant_tags_subset right left
        | Exact, Lower ->
            poly_variant_tags_subset right left
        | Lower, Exact ->
            poly_variant_tags_subset left right
      in
      if ok then
        unify_payloads left right;
      if not ok then
        add_diagnostic
          state
          (unsupported_type
            at
            ("polymorphic variant mismatch: "
            ^ string_of_ty (TPolyVariant (left_bound, { tags = left }))
            ^ " vs "
            ^ string_of_ty (TPolyVariant (right_bound, { tags = right }))))
  | TPackage left, TPackage right ->
      unify_package state ~at left right
  | TVar ({ var=Unbound (id, _) } as cell), TCon (path, []) when has_type_manifest state path -> (
      match resolve_type_manifest state path with
      | Some manifest when ty_mentions_unbound_var id manifest -> ()
      | Some _
      | None -> cell.var <- Link (TCon (path, []))
    )
  | TCon (path, []), TVar ({ var=Unbound (id, _) } as cell) when has_type_manifest state path -> (
      match resolve_type_manifest state path with
      | Some manifest when ty_mentions_unbound_var id manifest -> ()
      | Some _
      | None -> cell.var <- Link (TCon (path, []))
    )
  | TCon (path, []), other when has_type_manifest state path -> (
      match resolve_type_manifest state path with
      | Some manifest -> unify state ~at manifest other
      | None -> ()
    )
  | other, TCon (path, []) when has_type_manifest state path -> (
      match resolve_type_manifest state path with
      | Some manifest -> unify state ~at other manifest
      | None -> ()
    )
  | (TVar ({ var=Unbound (id, level) } as cell), ty)
  | (ty, TVar ({ var=Unbound (id, level) } as cell)) -> (
      try
        occurs_adjust_levels id level ty;
        cell.var <- Link ty
      with
      | Occurs ->
          if supports_recursive_occurrence ty then
            cell.var <- Link ty
          else
            add_diagnostic state (unsupported_type at "occurs check failed")
    )
  | (TVar { var=Generic _ }, _)
  | (_, TVar { var=Generic _ }) ->
      add_diagnostic state (unsupported_type at "unexpected generic type variable")
  | left, right ->
      add_diagnostic
        state
        (unsupported_type at ("type mismatch: " ^ string_of_ty left ^ " vs " ^ string_of_ty right))

and unify_package = fun state ~at left right ->
  if not (SurfacePath.equal left.module_type right.module_type) then
    add_diagnostic
      state
      (unsupported_type
        at
        ("package module type mismatch: "
        ^ SurfacePath.to_string left.module_type
        ^ " vs "
        ^ SurfacePath.to_string right.module_type));
  left.constraints |> List.for_each
    ~fn:(fun left_constraint ->
      match
        List.find right.constraints
          ~fn:(fun right_constraint ->
            SurfacePath.equal left_constraint.type_name right_constraint.type_name)
      with
      | Some right_constraint -> unify state ~at left_constraint.manifest right_constraint.manifest
      | None -> ())

and coerce = fun state ~at source target ->
  match prune source, prune target with
  | TCon (source_path, []), TCon (target_path, []) when SurfacePath.equal source_path target_path ->
      ()
  | TCon (path, []), target when has_type_manifest state path -> (
      match resolve_type_manifest state path with
      | Some manifest -> coerce state ~at manifest target
      | None -> ()
    )
  | source, TCon (path, []) when has_type_manifest state path -> (
      match resolve_type_manifest state path with
      | Some manifest -> coerce state ~at source manifest
      | None -> ()
    )
  | TPolyVariant (_, source_tags), TPolyVariant (_, target_tags) ->
      coerce_poly_variant state ~at source_tags target_tags
  | source, target ->
      unify state ~at source target

and coerce_poly_variant = fun state ~at source_tags target_tags ->
  let source = normalized_poly_variant_tags source_tags.tags in
  let target = normalized_poly_variant_tags target_tags.tags in
  if poly_variant_tags_subset source target then
    source |> List.for_each
      ~fn:(fun source_field ->
        match find_poly_variant_field source_field.tag target with
        | None -> ()
        | Some target_field -> (
            match source_field.payload, target_field.payload with
            | Some source_payload, Some target_payload -> unify state ~at source_payload target_payload
            | None, None -> ()
            | (Some _, None)
            | (None, Some _) -> add_diagnostic
              state
              (unsupported_type at ("polymorphic variant payload mismatch for `" ^ source_field.tag))
          ))
  else
    add_diagnostic
      state
      (unsupported_type
        at
        ("polymorphic variant coercion mismatch: "
        ^ string_of_ty (TPolyVariant (Exact, { tags = source }))
        ^ " vs "
        ^ string_of_ty (TPolyVariant (Exact, { tags = target }))))

let generalize = fun level ty ->
  let poly_variant_copies = ref [] in
  let rec loop ty =
    match prune ty with
    | TVar ({ var=Unbound (id, other_level) } as cell) when other_level > level ->
        cell.var <- Generic id;
        TVar cell
    | TList element ->
        TList (loop element)
    | TOption element ->
        TOption (loop element)
    | TTuple elements ->
        TTuple (List.map elements ~fn:loop)
    | TArrow (label, parameter, result) ->
        TArrow (label, loop parameter, loop result)
    | TCon (path, arguments) ->
        TCon (path, List.map arguments ~fn:loop)
    | TPolyVariant (bound, tags) ->
        TPolyVariant (bound, copy_poly_variant_tags poly_variant_copies ~map_payload:loop tags)
    | TPackage package ->
        TPackage {
          package
          with constraints = package.constraints
          |> List.map
            ~fn:(fun constraint_ -> { constraint_ with manifest = loop constraint_.manifest })
        }
    | ty ->
        ty
  in
  loop ty

let instantiate = fun state ~level ty ->
  let subst = ref [] in
  let poly_variant_copies = ref [] in
  let rec loop ty =
    match prune ty with
    | TVar { var=Generic id } -> (
        match
          List.find !subst
            ~fn:(fun (other_id, _) ->
              Int.equal id other_id)
        with
        | Some (_, replacement) -> replacement
        | None ->
            let replacement = fresh_tyvar state ~level in
            subst := (id, replacement) :: !subst;
            replacement
      )
    | TList element ->
        TList (loop element)
    | TOption element ->
        TOption (loop element)
    | TTuple elements ->
        TTuple (List.map elements ~fn:loop)
    | TArrow (label, parameter, result) ->
        TArrow (label, loop parameter, loop result)
    | TCon (path, arguments) ->
        TCon (path, List.map arguments ~fn:loop)
    | TPolyVariant (bound, tags) ->
        TPolyVariant (bound, copy_poly_variant_tags poly_variant_copies ~map_payload:loop tags)
    | TPackage package ->
        TPackage {
          package
          with constraints = package.constraints
          |> List.map
            ~fn:(fun constraint_ -> { constraint_ with manifest = loop constraint_.manifest })
        }
    | ty ->
        ty
  in
  loop ty

let instantiate_pair = fun state ~level left right ->
  let subst = ref [] in
  let poly_variant_copies = ref [] in
  let rec loop ty =
    match prune ty with
    | TVar { var=Generic id } -> (
        match
          List.find !subst
            ~fn:(fun (other_id, _) ->
              Int.equal id other_id)
        with
        | Some (_, replacement) -> replacement
        | None ->
            let replacement = fresh_tyvar state ~level in
            subst := (id, replacement) :: !subst;
            replacement
      )
    | TList element ->
        TList (loop element)
    | TOption element ->
        TOption (loop element)
    | TTuple elements ->
        TTuple (List.map elements ~fn:loop)
    | TArrow (label, parameter, result) ->
        TArrow (label, loop parameter, loop result)
    | TCon (path, arguments) ->
        TCon (path, List.map arguments ~fn:loop)
    | TPolyVariant (bound, tags) ->
        TPolyVariant (bound, copy_poly_variant_tags poly_variant_copies ~map_payload:loop tags)
    | TPackage package ->
        TPackage {
          package
          with constraints = package.constraints
          |> List.map
            ~fn:(fun constraint_ -> { constraint_ with manifest = loop constraint_.manifest })
        }
    | ty ->
        ty
  in
  (loop left, loop right)

let path_int = SurfacePath.from_name "int"

let path_bool = SurfacePath.from_name "bool"

let path_char = SurfacePath.from_name "char"

let path_string = SurfacePath.from_name "string"

let path_float = SurfacePath.from_name "float"

let path_unit = SurfacePath.from_name "unit"

let path_list = SurfacePath.from_name "list"

let path_option = SurfacePath.from_name "option"

let path_none = SurfacePath.from_name "None"

let path_some = SurfacePath.from_name "Some"

let path_not = SurfacePath.from_name "not"

let path_plus = SurfacePath.from_name "+"

let path_minus = SurfacePath.from_name "-"

let path_star = SurfacePath.from_name "*"

let path_slash = SurfacePath.from_name "/"

let path_plus_dot = SurfacePath.from_name "+."

let path_minus_dot = SurfacePath.from_name "-."

let path_star_dot = SurfacePath.from_name "*."

let path_slash_dot = SurfacePath.from_name "/."

type builtin = {
  path: SurfacePath.t;
  ty: ty;
}

let generic_var = fun id -> TVar { var = Generic id }

let arrow = fun parameter result -> TArrow (Nolabel, parameter, result)

let builtin_bindings = [
  { path = path_none; ty = TOption (generic_var 0) };
  { path = path_some; ty = arrow (generic_var 0) (TOption (generic_var 0)) };
  { path = path_not; ty = arrow TBool TBool };
  { path = path_plus; ty = arrow TInt (arrow TInt TInt) };
  { path = path_minus; ty = arrow TInt (arrow TInt TInt) };
  { path = path_star; ty = arrow TInt (arrow TInt TInt) };
  { path = path_slash; ty = arrow TInt (arrow TInt TInt) };
  { path = path_plus_dot; ty = arrow TFloat (arrow TFloat TFloat) };
  { path = path_minus_dot; ty = arrow TFloat (arrow TFloat TFloat) };
  { path = path_star_dot; ty = arrow TFloat (arrow TFloat TFloat) };
  { path = path_slash_dot; ty = arrow TFloat (arrow TFloat TFloat) };
]

let rec lookup_builtin = fun path builtins ->
  match builtins with
  | [] -> None
  | builtin :: rest ->
      if SurfacePath.equal builtin.path path then
        Some builtin.ty
      else
        lookup_builtin path rest

type row_alias = {
  row_tags: poly_variant_tags;
  row_alias_id: int;
  mutable row_alias_emitted: bool;
}

let shared_poly_variant_row_aliases = fun ty ->
  let rows = ref [] in
  let visited_rows = ref [] in
  let remember tags =
    match
      List.find !rows
        ~fn:(fun (other_tags, _) ->
          Ptr.equal other_tags tags)
    with
    | Some (_, count) -> count := !count + 1
    | None -> rows := (tags, ref 1) :: !rows
  in
  let already_visited tags =
    List.exists
      (fun other_tags ->
        Ptr.equal other_tags tags)
      !visited_rows
  in
  let rec collect ty =
    match prune ty with
    | TList element
    | TOption element ->
        collect element
    | TTuple elements ->
        List.for_each elements ~fn:collect
    | TArrow (_, parameter, result) ->
        collect parameter;
        collect result
    | TCon (_, arguments) ->
        List.for_each arguments ~fn:collect
    | TPolyVariant (_, tags) ->
        remember tags;
        if not (already_visited tags) then
          (
            visited_rows := tags :: !visited_rows;
            tags.tags |> List.for_each ~fn:(fun field -> Option.for_each field.payload ~fn:collect)
          )
    | TPackage package ->
        package.constraints |> List.for_each ~fn:(fun constraint_ -> collect constraint_.manifest)
    | TVar { var=Link linked_ty } ->
        collect linked_ty
    | TInt
    | TBool
    | TChar
    | TString
    | TFloat
    | TUnit
    | TVar { var=Unbound _ }
    | TVar { var=Generic _ } ->
        ()
  in
  collect ty;
  let next_alias_id = ref (-1) in
  !rows |> List.filter_map
    ~fn:(fun (row_tags, count) ->
      if !count > 1 then
        (
          let row_alias_id = !next_alias_id in
          next_alias_id := row_alias_id - 1;
          Some { row_tags; row_alias_id; row_alias_emitted = false }
        )
      else
        None)

let find_row_alias = fun row_aliases tags ->
  List.find row_aliases
    ~fn:(fun alias ->
      Ptr.equal alias.row_tags tags)

let rec public_type_of_ty = fun vars row_aliases ty ->
  match prune ty with
  | TInt ->
      Typing_context.Int
  | TBool ->
      Typing_context.Bool
  | TChar ->
      Typing_context.Char
  | TString ->
      Typing_context.String
  | TFloat ->
      Typing_context.Float
  | TUnit ->
      Typing_context.Unit
  | TList element ->
      Typing_context.List (public_type_of_ty vars row_aliases element)
  | TOption element ->
      Typing_context.Option (public_type_of_ty vars row_aliases element)
  | TTuple elements ->
      Typing_context.Tuple (List.map elements ~fn:(public_type_of_ty vars row_aliases))
  | TArrow (label, parameter, result) ->
      let label = public_arg_label label in
      let parameter = public_type_of_ty vars row_aliases parameter in
      let result = public_type_of_ty vars row_aliases result in
      Typing_context.Arrow { label; parameter; result }
  | TCon (path, arguments) ->
      Typing_context.TypeConstructor {
        path;
        arguments = List.map arguments ~fn:(public_type_of_ty vars row_aliases)
      }
  | TPolyVariant (bound, tags) ->
      let public_poly_variant () = Typing_context.PolyVariant {
        bound = public_poly_variant_bound bound;
        fields = normalized_poly_variant_tags tags.tags
        |> List.map
          ~fn:(fun field ->
            {
              Typing_context.tag = field.tag;
              payload = Option.map field.payload ~fn:(public_type_of_ty vars row_aliases)
            })
      } in
      (
        match find_row_alias row_aliases tags with
        | Some alias ->
            let id = public_tyvar_id vars alias.row_alias_id in
            if alias.row_alias_emitted then
              Typing_context.Var id
            else (
              alias.row_alias_emitted <- true;
              Typing_context.Alias { type_ = public_poly_variant (); id }
            )
        | None -> public_poly_variant ()
      )
  | TPackage package ->
      Typing_context.Package {
        binder = package.binder;
        module_type = package.module_type;
        constraints = package.constraints
        |> List.map
          ~fn:(fun constraint_ ->
            {
              Typing_context.type_name = constraint_.type_name;
              manifest = public_type_of_ty vars row_aliases constraint_.manifest
            })
      }
  | TVar { var=Generic id } ->
      Typing_context.Var (public_tyvar_id vars id)
  | TVar { var=Unbound (id, _) } ->
      Typing_context.Var (public_tyvar_id vars id)
  | TVar { var=Link linked_ty } ->
      public_type_of_ty vars row_aliases linked_ty

and public_tyvar_id = fun vars id ->
  match
    List.find !vars
      ~fn:(fun (other_id, _) ->
        Int.equal id other_id)
  with
  | Some (_, public_id) -> public_id
  | None ->
      let public_id = List.length !vars in
      vars := (id, public_id) :: !vars;
      public_id

and public_poly_variant_bound = function
  | Exact -> Typing_context.Exact
  | Upper -> Typing_context.Upper
  | Lower -> Typing_context.Lower

and public_arg_label = function
  | Nolabel -> Typing_context.Nolabel
  | Labelled label -> Typing_context.Labelled label
  | Optional label -> Typing_context.Optional label

let public_scheme_of_ty = fun ty ->
  let vars = ref [] in
  let row_aliases = shared_poly_variant_row_aliases ty in
  let body = public_type_of_ty vars row_aliases ty in
  let forall = !vars |> List.map ~fn:(fun (_, public_id) -> public_id) |> List.reverse in
  { Typing_context.forall; body }

let public_binding_of_binding = fun binding ->
  {
    Typing_context.binding_id = binding.binding_id;
    entity_id = binding.entity_id;
    scheme = public_scheme_of_ty binding.ty
  }

let rec import_scheme = fun scheme ->
  let rec loop type_expr =
    match type_expr with
    | Typing_context.Int -> TInt
    | Typing_context.Bool -> TBool
    | Typing_context.Char -> TChar
    | Typing_context.String -> TString
    | Typing_context.Float -> TFloat
    | Typing_context.Unit -> TUnit
    | Typing_context.List element -> TList (loop element)
    | Typing_context.Option element -> TOption (loop element)
    | Typing_context.Tuple elements -> TTuple (List.map elements ~fn:loop)
    | Typing_context.Arrow { label; parameter; result } -> TArrow (
      import_arg_label label,
      loop parameter,
      loop result
    )
    | Typing_context.TypeConstructor { path; arguments } -> TCon (path, List.map arguments ~fn:loop)
    | Typing_context.Alias { type_; _ } -> loop type_
    | Typing_context.PolyVariant { bound; fields } -> TPolyVariant (
      import_poly_variant_bound bound,
      {
        tags = fields
        |> List.map
          ~fn:(fun field ->
            { tag = field.Typing_context.tag; payload = Option.map field.payload ~fn:loop })
        |> normalized_poly_variant_tags
      }
    )
    | Typing_context.Package package -> TPackage {
      binder = package.binder;
      module_type = package.module_type;
      constraints = package.constraints
      |> List.map
        ~fn:(fun constraint_ ->
          { type_name = constraint_.Typing_context.type_name; manifest = loop constraint_.manifest })
    }
    | Typing_context.Var id -> TVar { var = Generic id }
  in
  let _ = scheme.Typing_context.forall in
  loop scheme.body

and import_poly_variant_bound = function
  | Typing_context.Exact -> Exact
  | Typing_context.Upper -> Upper
  | Typing_context.Lower -> Lower

and import_arg_label = function
  | Typing_context.Nolabel -> Nolabel
  | Typing_context.Labelled label -> Labelled label
  | Typing_context.Optional label -> Optional label

let env_of_typing_context = fun typing_context ->
  List.fold_left
    typing_context.Typing_context.values
    ~init:[]
    ~fn:(fun env (value_binding: Typing_context.value_binding) ->
      {
        binding_id = value_binding.binding_id;
        entity_id = value_binding.entity_id;
        ty = import_scheme value_binding.scheme
      }
      :: env)

let rec lookup_env_binding = fun env surface_path ->
  match env with
  | [] -> None
  | binding :: rest ->
      if SurfacePath.equal (EntityId.surface_path binding.entity_id) surface_path then
        Some binding
      else
        lookup_env_binding rest surface_path

let lookup_value_type = fun env surface_path ->
  match lookup_env_binding env surface_path with
  | Some binding -> Some binding.ty
  | None -> lookup_builtin surface_path builtin_bindings

let lookup_surface_path = fun state env ~level ~at surface_path ->
  match lookup_value_type env surface_path with
  | Some ty -> instantiate state ~level ty
  | None -> (
      add_diagnostic
        state
        (unsupported_type at ("unbound value " ^ SurfacePath.to_string surface_path));
      fresh_tyvar state ~level
    )

let path_last_segment = fun path ->
  match List.reverse (SurfacePath.to_segments path) with
  | segment :: _ -> Some segment
  | [] -> None

let record_label_matches = fun requested actual ->
  SurfacePath.equal requested actual
  || match SurfacePath.to_segments requested, path_last_segment actual with
  | [ requested ], Some actual -> String.equal requested actual
  | _ -> false

let lookup_record_labels = fun state label ->
  List.filter
    state.record_labels
    ~fn:(fun record_label -> record_label_matches label record_label.label)

let lookup_record_label = fun state label ->
  match lookup_record_labels state label with
  | label :: _ -> Some label
  | [] -> None

let lookup_record_label_for_owner = fun state label owner_ty ->
  let candidates = lookup_record_labels state label in
  match prune owner_ty with
  | TCon (owner_path, _) -> (
      match
        List.find candidates
          ~fn:(fun candidate ->
            match prune candidate.owner_ty with
            | TCon (candidate_path, _) -> SurfacePath.equal owner_path candidate_path
            | _ -> false)
      with
      | Some candidate -> Some candidate
      | None -> (
          match candidates with
          | candidate :: _ -> Some candidate
          | [] -> None
        )
    )
  | _ -> (
      match candidates with
      | candidate :: _ -> Some candidate
      | [] -> None
    )

let literal_type = function
  | TypAst.Int -> TInt
  | TypAst.Float -> TFloat
  | TypAst.Char -> TChar
  | TypAst.String -> TString
  | TypAst.Bool -> TBool
  | TypAst.Unit
  | TypAst.Unknown -> TUnit

let rec lookup_type_var = fun vars name ->
  match !vars with
  | [] -> None
  | (other_name, ty) :: rest ->
      if SurfacePath.equal name other_name then
        Some ty
      else (
        vars := rest;
        let result = lookup_type_var vars name in
        vars := (other_name, ty) :: rest;
        result
      )

let bind_type_var = fun vars name ty -> vars := (name, ty) :: !vars

let lookup_locally_abstract_type = fun state name ->
  match
    List.find state.locally_abstract_types
      ~fn:(fun (other_name, _) ->
        SurfacePath.equal name other_name)
  with
  | Some (_, ty) -> Some ty
  | None -> None

let lookup_lower_type_var = fun state vars name ->
  match lookup_type_var vars name with
  | Some ty -> Some ty
  | None -> lookup_locally_abstract_type state name

let bind_locally_abstract_type = fun state ~level name ->
  let path = SurfacePath.from_name name in
  match lookup_locally_abstract_type state path with
  | Some _ -> ()
  | None -> state.locally_abstract_types <- (path, fresh_tyvar state ~level) :: state.locally_abstract_types

let resolve_type_path = fun state path ->
  match
    List.find state.type_aliases
      ~fn:(fun (source_path, _) ->
        SurfacePath.equal source_path path)
  with
  | Some (_, target_path) -> target_path
  | None -> path

let type_of_constructor = fun state ~level ~at path arguments ->
  let resolved_path = resolve_type_path state path in
  match arguments with
  | [] when SurfacePath.equal resolved_path path_int ->
      TInt
  | [] when SurfacePath.equal resolved_path path_bool ->
      TBool
  | [] when SurfacePath.equal resolved_path path_char ->
      TChar
  | [] when SurfacePath.equal resolved_path path_string ->
      TString
  | [] when SurfacePath.equal resolved_path path_float ->
      TFloat
  | [] when SurfacePath.equal resolved_path path_unit ->
      TUnit
  | [ element ] when SurfacePath.equal resolved_path path_list ->
      TList element
  | [ element ] when SurfacePath.equal resolved_path path_option ->
      TOption element
  | _ ->
      let _ = (state, level, at) in
      TCon (resolved_path, arguments)

let rec lower_apply_type = fun state ~level vars (type_expr: TypAst.core_type) ->
  let rec loop arguments (current: TypAst.core_type) =
    match current.kind with
    | TypAst.Apply { argument; constructor } ->
        let lowered_arguments = lower_type_application_arguments state ~level vars argument in
        loop (List.append (List.reverse lowered_arguments) arguments) constructor
    | TypAst.Path path ->
        type_of_constructor state ~level ~at:current.origin path (List.reverse arguments)
    | _ ->
        add_diagnostic
          state
          (unsupported_type (TypAst.core_type_origin current) "type application constructor");
        fresh_tyvar state ~level
  in
  loop [] type_expr

and lower_type_application_arguments = fun state ~level vars (type_expr: TypAst.core_type) ->
  match type_expr.kind with
  | TypAst.Parenthesized inner -> lower_type_application_arguments state ~level vars inner
  | TypAst.Tuple { separator=`Comma; elements } -> List.map
    elements
    ~fn:(lower_core_type state ~level vars)
  | _ -> [ lower_core_type state ~level vars type_expr ]

and lower_labeled_type = fun state ~level vars (type_expr: TypAst.core_type) ->
  match type_expr.kind with
  | TypAst.Labeled annotation -> lower_core_type state ~level vars annotation
  | _ -> lower_core_type state ~level vars type_expr

and lower_core_type = fun state ~level vars (type_expr: TypAst.core_type) ->
  match type_expr.kind with
  | TypAst.Wildcard ->
      fresh_tyvar state ~level
  | TypAst.Var (Some name) ->
      let name = SurfacePath.from_name name in
      (
        match lookup_lower_type_var state vars name with
        | Some ty -> ty
        | None ->
            let ty = fresh_tyvar state ~level in
            bind_type_var vars name ty;
            ty
      )
  | TypAst.Var None ->
      add_diagnostic state (unsupported_type type_expr.origin "missing type variable");
      fresh_tyvar state ~level
  | TypAst.Path path -> (
      match lookup_lower_type_var state vars path with
      | Some ty -> ty
      | None -> type_of_constructor state ~level ~at:type_expr.origin path []
    )
  | TypAst.Apply _ ->
      lower_apply_type state ~level vars type_expr
  | TypAst.Arrow { left; right } ->
      TArrow (
        Nolabel,
        lower_labeled_type state ~level vars left,
        lower_core_type state ~level vars right
      )
  | TypAst.Tuple { elements; _ } ->
      TTuple (List.map elements ~fn:(lower_core_type state ~level vars))
  | TypAst.Labeled _ ->
      lower_labeled_type state ~level vars type_expr
  | TypAst.Parenthesized inner ->
      lower_core_type state ~level vars inner
  | TypAst.Poly { parameters; body } ->
      let local_vars = ref !vars in
      parameters
      |> List.for_each
        ~fn:(fun parameter ->
          bind_type_var
            local_vars
            (SurfacePath.from_name parameter)
            (fresh_tyvar state ~level:(level + 1)));
      lower_core_type state ~level:(level + 1) local_vars body
  | TypAst.PolyVariant fields ->
      TPolyVariant (
        Exact,
        {
          tags = fields
          |> List.map
            ~fn:(fun (field: TypAst.poly_variant_type_field) ->
              {
                tag = field.tag;
                payload = Option.map field.payload ~fn:(lower_core_type state ~level vars)
              })
          |> normalized_poly_variant_tags
        }
      )
  | TypAst.Package package ->
      lower_package_type state ~level vars package

and lower_package_type = fun state ~level vars (package: TypAst.package_type) ->
  TPackage {
    binder = package.binder;
    module_type = package.module_type;
    constraints = package.constraints
    |> List.map
      ~fn:(fun (constraint_: TypAst.package_type_constraint) ->
        {
          type_name = constraint_.type_name;
          manifest = lower_core_type state ~level vars constraint_.manifest
        })
  }

let extend_mono = fun (env: env) (bindings: binding list) ->
  List.fold_left bindings ~init:env ~fn:(fun extended_env binding -> binding :: extended_env)

let extend_generalized = fun (env: env) ~level (bindings: binding list) ->
  List.fold_left
    bindings
    ~init:env
    ~fn:(fun extended_env binding -> { binding with ty = generalize level binding.ty } :: extended_env)

let generalized_bindings = fun ~level (bindings: binding list) ->
  List.map bindings ~fn:(fun binding -> { binding with ty = generalize level binding.ty })

let is_uppercase_name = fun name ->
  match String.get name ~at:0 with
  | Some char -> char >= 'A' && char <= 'Z'
  | None -> false

let simple_path_name = fun path ->
  match List.reverse (SurfacePath.to_segments path) with
  | name :: _ -> Some name
  | [] -> None

let split_field_path = fun path ->
  match List.reverse (SurfacePath.to_segments path) with
  | field :: receiver when not (List.is_empty receiver) -> Some (
    SurfacePath.from_segments (List.reverse receiver),
    SurfacePath.from_name field
  )
  | _ -> None

let flatten_pattern_application = fun pattern ->
  let rec loop arguments (pattern: TypAst.pattern) =
    match pattern.kind with
    | TypAst.Apply { callee; argument } -> loop (argument :: arguments) callee
    | _ -> (pattern, arguments)
  in
  loop [] pattern

let rec infer_pattern = fun state env ~level (pattern: TypAst.pattern) ->
  match pattern.kind with
  | TypAst.Path path -> (
      match simple_path_name path with
      | Some name when not (is_uppercase_name name) ->
          let ty = fresh_tyvar state ~level in
          let binding = make_binding state ~name:(SurfacePath.from_name name) ~ty in
          (ty, [ binding ])
      | Some _ ->
          (lookup_surface_path state env ~level ~at:pattern.origin path, [])
      | None ->
          add_diagnostic state (unsupported_syntax pattern.origin "path pattern");
          (fresh_tyvar state ~level, [])
    )
  | TypAst.Wildcard ->
      (fresh_tyvar state ~level, [])
  | TypAst.Literal literal ->
      (literal_type literal, [])
  | TypAst.PolyVariant { tag; payload } ->
      let payload_ty, bindings =
        match payload with
        | Some payload ->
            let payload_ty, bindings = infer_pattern state env ~level payload in
            (Some payload_ty, bindings)
        | None -> (None, [])
      in
      (TPolyVariant (Upper, { tags = [ { tag; payload = payload_ty } ] }), bindings)
  | TypAst.Tuple elements ->
      let element_types, binding_groups = elements
      |> List.map ~fn:(fun child -> infer_pattern state env ~level child)
      |> List.unzip in
      (TTuple element_types, List.concat binding_groups)
  | TypAst.List elements ->
      let element_ty = fresh_tyvar state ~level in
      let bindings =
        elements
        |> List.flat_map
          ~fn:(fun child ->
            let inferred_ty, bindings = infer_pattern state env ~level child in
            unify state ~at:(TypAst.pattern_origin child) element_ty inferred_ty;
            bindings)
      in
      (TList element_ty, bindings)
  | TypAst.Cons { head; tail } ->
      let head_ty, head_bindings = infer_pattern state env ~level head in
      let tail_ty, tail_bindings = infer_pattern state env ~level tail in
      unify state ~at:(TypAst.pattern_origin tail) tail_ty (TList head_ty);
      (TList head_ty, List.append head_bindings tail_bindings)
  | TypAst.Record fields ->
      let owner_ty = fresh_tyvar state ~level in
      let bindings = fields
      |> List.flat_map ~fn:(infer_record_pattern_field state env ~level owner_ty) in
      (owner_ty, bindings)
  | TypAst.Or { left; right } ->
      let left_ty, left_bindings = infer_pattern state env ~level left in
      let right_ty, right_bindings = infer_pattern state env ~level right in
      unify state ~at:pattern.origin left_ty right_ty;
      (left_ty, merge_or_pattern_bindings state pattern.origin left_bindings right_bindings)
  | TypAst.Apply _ ->
      infer_constructor_pattern_application state env ~level pattern
  | TypAst.Constraint { pattern=inner; annotation } ->
      let pattern_ty, bindings = infer_pattern state env ~level inner in
      let annotated = lower_core_type state ~level (ref []) annotation in
      unify state ~at:pattern.origin pattern_ty annotated;
      (pattern_ty, bindings)
  | TypAst.Alias { pattern=inner; alias } ->
      let pattern_ty, bindings = infer_pattern state env ~level inner in
      (
        match alias.kind with
        | TypAst.Path path -> (
            match simple_path_name path with
            | Some alias_name ->
                let alias_binding = make_binding state ~name:(SurfacePath.from_name alias_name) ~ty:pattern_ty in
                (pattern_ty, List.append bindings [ alias_binding ])
            | None -> (pattern_ty, bindings)
          )
        | _ -> (pattern_ty, bindings)
      )
  | TypAst.Attribute inner
  | TypAst.Parenthesized inner ->
      infer_pattern state env ~level inner
  | TypAst.LabeledParameter parameter
  | TypAst.OptionalParameter parameter
  | TypAst.OptionalParameterDefault parameter ->
      infer_parameter state env ~level parameter
  | TypAst.LocallyAbstractType names ->
      names |> List.for_each ~fn:(bind_locally_abstract_type state ~level);
      (TUnit, [])
  | TypAst.FirstClassModule { binder; package_type } ->
      let package_ty =
        match package_type with
        | Some package -> lower_package_type state ~level (ref []) package
        | None ->
            add_diagnostic
              state
              (unsupported_type pattern.origin "missing first-class module package type");
            TPackage { binder; module_type = SurfacePath.empty; constraints = [] }
      in
      let bindings =
        match binder, prune package_ty with
        | Some name, TPackage package -> bind_first_class_module_pattern state ~level name package
        | _ -> []
      in
      (package_ty, bindings)

and infer_constructor_pattern_application = fun state env ~level (pattern: TypAst.pattern) ->
  let callee, arguments = flatten_pattern_application pattern in
  match callee.kind with
  | TypAst.Path path ->
      let constructor_ty = lookup_surface_path state env ~level ~at:callee.origin path in
      let runtime_arguments = constructor_runtime_pattern_arguments state ~level arguments in
      (
        match runtime_arguments with
        | [] ->
            (constructor_ty, [])
        | [ argument ] ->
            let argument_ty, bindings = infer_pattern state env ~level argument in
            let result_ty = fresh_tyvar state ~level in
            unify state ~at:pattern.origin constructor_ty (arrow argument_ty result_ty);
            (result_ty, bindings)
        | arguments ->
            let argument: TypAst.pattern = { origin = pattern.origin; kind = TypAst.Tuple arguments } in
            let argument_ty, bindings = infer_pattern state env ~level argument in
            let result_ty = fresh_tyvar state ~level in
            unify state ~at:pattern.origin constructor_ty (arrow argument_ty result_ty);
            (result_ty, bindings)
      )
  | _ ->
      add_diagnostic state (unsupported_syntax pattern.origin "constructor pattern");
      (fresh_tyvar state ~level, [])

and constructor_runtime_pattern_arguments = fun state ~level arguments ->
  arguments |> List.filter
    ~fn:(fun (argument: TypAst.pattern) ->
      match argument.kind with
      | TypAst.LocallyAbstractType names ->
          names |> List.for_each ~fn:(bind_locally_abstract_type state ~level);
          false
      | _ -> true)

and qualify_path = fun path_prefix path ->
  SurfacePath.from_segments (List.append path_prefix (SurfacePath.to_segments path))

and bind_first_class_module_pattern = fun state ~level name (package: package_ty) ->
  let module_prefix = [ name ] in
  let manifests = package_manifests_for_module ~module_prefix package in
  state.type_manifests <- List.append manifests state.type_manifests;
  bindings_for_module_type state ~level ~module_prefix ~module_type_path:package.module_type

and package_manifests_for_module = fun ~module_prefix (package: package_ty) ->
  package.constraints
  |> List.map
    ~fn:(fun constraint_ -> (qualify_path module_prefix constraint_.type_name, constraint_.manifest))

and infer_record_pattern_field = fun state env ~level owner_ty (field: TypAst.record_pattern_field) ->
  match lookup_record_label_for_owner state field.name owner_ty with
  | None ->
      add_diagnostic
        state
        (unsupported_type field.origin ("unbound record field " ^ SurfacePath.to_string field.name));
      []
  | Some label ->
      let label_owner_ty, label_field_ty = instantiate_pair state ~level label.owner_ty label.field_ty in
      unify state ~at:field.origin owner_ty label_owner_ty;
      (
        match field.pattern with
        | Some pattern ->
            let pattern_ty, bindings = infer_pattern state env ~level pattern in
            unify state ~at:(TypAst.pattern_origin pattern) label_field_ty pattern_ty;
            bindings
        | None -> (
            match simple_path_name field.name with
            | Some name when not (is_uppercase_name name) -> [
              make_binding state ~name:(SurfacePath.from_name name) ~ty:label_field_ty
            ]
            | _ -> []
          )
      )

and merge_or_pattern_bindings = fun state origin left_bindings right_bindings ->
  let binding_name binding = EntityId.surface_path binding.entity_id in
  let find_binding name bindings =
    List.find bindings
      ~fn:(fun binding ->
        SurfacePath.equal (binding_name binding) name)
  in
  List.for_each left_bindings
    ~fn:(fun left ->
      let name = binding_name left in
      match find_binding name right_bindings with
      | Some right -> unify state ~at:origin left.ty right.ty
      | None -> add_diagnostic
        state
        (unsupported_type
          origin
          ("or-pattern binding missing on right: " ^ SurfacePath.to_string name)));
  List.for_each right_bindings
    ~fn:(fun right ->
      let name = binding_name right in
      if Option.is_none (find_binding name left_bindings) then
        add_diagnostic
          state
          (unsupported_type
            origin
            ("or-pattern binding missing on left: " ^ SurfacePath.to_string name)));
  left_bindings

and infer_parameter = fun state env ~level (parameter: TypAst.parameter) ->
  match parameter.kind with
  | TypAst.Labeled { label; pattern } ->
      infer_labeled_parameter state env ~level label pattern
  | TypAst.Optional { label; pattern } ->
      infer_labeled_parameter state env ~level label pattern
  | TypAst.OptionalDefault { label; pattern; default } ->
      let ty, bindings = infer_labeled_parameter state env ~level label pattern in
      let default_ty = infer_expression state env ~level default in
      unify state ~at:parameter.origin ty default_ty;
      (ty, bindings)

and infer_function_parameter = fun state env ~level (pattern: TypAst.pattern) ->
  match pattern.kind with
  | TypAst.LabeledParameter ({ kind=TypAst.Labeled { label; _ }; _ } as parameter) ->
      let ty, bindings = infer_parameter state env ~level parameter in
      (Labelled label, ty, bindings)
  | TypAst.OptionalParameter ({ kind=TypAst.Optional { label; _ }; _ } as parameter)
  | TypAst.OptionalParameterDefault ({ kind=TypAst.OptionalDefault { label; _ }; _ } as parameter) ->
      let ty, bindings = infer_parameter state env ~level parameter in
      (Optional label, ty, bindings)
  | _ ->
      let ty, bindings = infer_pattern state env ~level pattern in
      (Nolabel, ty, bindings)

and infer_labeled_parameter = fun state env ~level label pattern ->
  match pattern with
  | Some pattern -> infer_pattern state env ~level pattern
  | None ->
      let ty = fresh_tyvar state ~level in
      let binding = make_binding state ~name:(SurfacePath.from_name label) ~ty in
      (ty, [ binding ])

and infer_path_expression = fun state env ~level ~at path ->
  match lookup_value_type env path with
  | Some ty -> instantiate state ~level ty
  | None -> (
      match split_field_path path with
      | Some (receiver_path, field) when Option.is_some (lookup_record_label state field) ->
          let receiver_ty = infer_path_expression state env ~level ~at receiver_path in
          infer_record_field state ~level ~at receiver_ty field
      | _ ->
          add_diagnostic state (unsupported_type at ("unbound value " ^ SurfacePath.to_string path));
          fresh_tyvar state ~level
    )

and infer_expression = fun state env ~level (expression: TypAst.expression) ->
  let inferred =
    match expression.kind with
    | TypAst.Literal literal ->
        literal_type literal
    | TypAst.Path path ->
        infer_path_expression state env ~level ~at:expression.origin path
    | TypAst.Tuple elements ->
        TTuple (List.map elements ~fn:(infer_expression state env ~level))
    | TypAst.List elements ->
        let element_ty = fresh_tyvar state ~level in
        elements |> List.for_each
          ~fn:(fun child ->
            let child_ty = infer_expression state env ~level child in
            unify state ~at:child.origin element_ty child_ty);
        TList element_ty
    | TypAst.PolyVariant { tag; payload } ->
        let payload = Option.map payload ~fn:(infer_expression state env ~level) in
        TPolyVariant (Lower, { tags = [ { tag; payload } ] })
    | TypAst.Record fields ->
        infer_record state env ~level ~at:expression.origin fields
    | TypAst.RecordUpdate { base; fields } ->
        infer_record_update state env ~level ~at:expression.origin base fields
    | TypAst.FieldAccess { receiver; field } ->
        infer_field_access state env ~level ~at:expression.origin receiver field
    | TypAst.Assign { target; value } ->
        infer_assignment state env ~level ~at:expression.origin target value
    | TypAst.Sequence { left; right } ->
        let _ = infer_expression state env ~level left in
        infer_expression state env ~level right
    | TypAst.If { condition; then_branch; else_branch } ->
        let condition_ty = infer_expression state env ~level condition in
        unify state ~at:condition.origin condition_ty TBool;
        let then_ty = infer_expression state env ~level then_branch in
        (
          match else_branch with
          | Some else_branch ->
              let else_ty = infer_expression state env ~level else_branch in
              unify state ~at:expression.origin then_ty else_ty
          | None -> unify state ~at:expression.origin then_ty TUnit
        );
        then_ty
    | TypAst.Match { scrutinee; cases } ->
        infer_match state env ~level scrutinee cases
    | TypAst.Function { parameters; body } ->
        infer_function state env ~level parameters body
    | TypAst.Apply _ ->
        infer_apply state env ~level expression
    | TypAst.Infix { left; operator; right } ->
        let callee_ty = lookup_surface_path state env ~level ~at:expression.origin operator in
        let left_ty = infer_expression state env ~level left in
        let right_ty = infer_expression state env ~level right in
        let result_ty = fresh_tyvar state ~level in
        unify state ~at:expression.origin callee_ty (arrow left_ty (arrow right_ty result_ty));
        result_ty
    | TypAst.Let { first_binding; body } ->
        let extended_env, _ = infer_let_binding state env ~level ~recursive:false first_binding in
        infer_expression state extended_env ~level body
    | TypAst.LetModule {
      name;
      items;
      alias;
      unpack;
      body
    } ->
        infer_local_module state env ~level ~name ~items ~alias ~unpack body
    | TypAst.LocalOpen { module_path; body } ->
        infer_local_open state env ~level ~at:expression.origin module_path body
    | TypAst.FirstClassModule { module_path; package_type } -> (
        match package_type with
        | Some package -> lower_package_type state ~level (ref []) package
        | None -> TPackage { binder = None; module_type = module_path; constraints = [] }
      )
    | TypAst.Assert argument ->
        let inferred = infer_expression state env ~level argument in
        unify state ~at:expression.origin inferred TBool;
        TUnit
  in
  match expression.type_hint with
  | Some hint -> (
      let annotated = lower_core_type state ~level (ref []) hint.TypAst.type_ in
      match hint.kind with
      | TypAst.Annotation ->
          unify state ~at:expression.origin inferred annotated;
          inferred
      | TypAst.Coercion ->
          coerce state ~at:expression.origin inferred annotated;
          annotated
    )
  | None -> inferred

and infer_local_module = fun state env ~level ~name ~items ~alias ~unpack body ->
  let previous_module_value_bindings = state.module_value_bindings in
  let previous_module_summaries = state.module_summaries in
  let previous_record_labels = state.record_labels in
  let previous_type_manifests = state.type_manifests in
  let module_prefix = [ name ] in
  let extended_env, unpack_manifests =
    match unpack with
    | Some unpack -> infer_module_unpack state env ~level ~module_prefix unpack
    | None -> (
        match alias with
        | Some source_path -> (bind_module_alias state env ~module_prefix ~source_path, [])
        | None -> (bind_module_structure state env ~level ~module_prefix items, [])
      )
  in
  let local_manifests = List.append
    unpack_manifests
    (collect_local_type_manifests state ~level ~module_prefix items) in
  let result = infer_expression state extended_env ~level body |> expand_local_type_manifests local_manifests in
  state.module_value_bindings <- previous_module_value_bindings;
  state.module_summaries <- previous_module_summaries;
  state.record_labels <- previous_record_labels;
  state.type_manifests <- previous_type_manifests;
  result

and infer_module_unpack = fun state env ~level ~module_prefix (unpack: TypAst.module_unpack) ->
  let expression_ty = infer_expression state env ~level unpack.expression in
  let package_ty =
    match unpack.package_type with
    | Some package ->
        let ascribed = lower_package_type state ~level (ref []) package in
        unify state ~at:unpack.origin expression_ty ascribed;
        ascribed
    | None -> expression_ty
  in
  match prune package_ty with
  | TPackage package ->
      let manifests = package_manifests_for_module ~module_prefix package in
      state.type_manifests <- List.append manifests state.type_manifests;
      let bindings = bindings_for_module_type state ~level ~module_prefix ~module_type_path:package.module_type in
      (extend_mono env bindings, manifests)
  | _ ->
      add_diagnostic state (unsupported_type unpack.origin "first-class module unpack package type");
      (env, [])

and infer_local_open = fun state env ~level ~at module_path body ->
  match find_module_summary state module_path with
  | Some (summary: module_summary) ->
      let source_prefix = SurfacePath.to_segments summary.path in
      let copied_bindings = summary.env_bindings
      |> List.filter_map
        ~fn:(copy_binding_prefix_to_local state ~source_prefix ~target_prefix:source_prefix) in
      infer_expression state (extend_mono env copied_bindings) ~level body
  | None ->
      add_diagnostic
        state
        (unsupported_type at ("unbound opened module " ^ SurfacePath.to_string module_path));
      infer_expression state env ~level body

and collect_local_type_manifests = fun state ~level ~module_prefix items ->
  items |> List.flat_map
    ~fn:(fun (item: TypAst.structure_item) ->
      match item.kind with
      | TypAst.Type declarations ->
          declarations |> List.filter_map
            ~fn:(fun (declaration: TypAst.type_declaration) ->
              match declaration.parameters, declaration.definition.kind with
              | [], TypAst.Alias manifest -> Some (
                qualify_name module_prefix declaration.name,
                lower_core_type state ~level (ref []) manifest
              )
              | _ -> None)
      | _ -> [])

and expand_local_type_manifests = fun manifests ty ->
  let rec loop ty =
    match prune ty with
    | TList element ->
        TList (loop element)
    | TOption element ->
        TOption (loop element)
    | TTuple elements ->
        TTuple (List.map elements ~fn:loop)
    | TArrow (label, parameter, result) ->
        TArrow (label, loop parameter, loop result)
    | TCon (path, []) -> (
        match
          List.find manifests
            ~fn:(fun (manifest_path, _) ->
              SurfacePath.equal manifest_path path)
        with
        | Some (_, manifest) -> manifest
        | None -> TCon (path, [])
      )
    | TCon (path, arguments) ->
        TCon (path, List.map arguments ~fn:loop)
    | TPolyVariant (bound, tags) ->
        TPolyVariant (
          bound,
          {
            tags = tags.tags
            |> List.map ~fn:(fun field -> { field with payload = Option.map field.payload ~fn:loop })
            |> normalized_poly_variant_tags
          }
        )
    | TPackage package ->
        TPackage {
          package
          with constraints = package.constraints
          |> List.map
            ~fn:(fun constraint_ -> { constraint_ with manifest = loop constraint_.manifest })
        }
    | ty ->
        ty
  in
  loop ty

and infer_apply = fun state env ~level (expression: TypAst.expression) ->
  let rec collect arguments (current: TypAst.expression) =
    match current.kind with
    | TypAst.Apply { callee; arguments=current_arguments } -> collect
      (List.append current_arguments arguments)
      callee
    | _ -> (current, arguments)
  in
  let callee, arguments = collect [] expression in
  let callee_ty = infer_expression state env ~level callee in
  List.fold_left arguments ~init:callee_ty
    ~fn:(fun function_ty argument ->
      let argument_label, argument_ty = infer_apply_argument state env ~level argument in
      apply_argument_to_function state ~level ~at:expression.origin function_ty argument_label argument_ty)

and infer_apply_argument = fun state env ~level (argument: TypAst.argument) ->
  match argument.kind with
  | TypAst.Positional expression ->
      (Nolabel, infer_expression state env ~level expression)
  | TypAst.Labeled { label; value=Some value } ->
      (Labelled label, infer_expression state env ~level value)
  | TypAst.Optional { label; value=Some value } ->
      (Optional label, infer_expression state env ~level value)
  | TypAst.Labeled { value=None; _ }
  | TypAst.Optional { value=None; _ } ->
      add_diagnostic state (unsupported_syntax argument.origin "missing argument value");
      (Nolabel, fresh_tyvar state ~level)

and apply_label_matches = fun parameter_label argument_label ->
  match parameter_label, argument_label with
  | Nolabel, Nolabel -> true
  | (Labelled left, Labelled right)
  | (Optional left, Optional right)
  | (Optional left, Labelled right) -> String.equal left right
  | _ -> false

and is_labeled_argument = function
  | Labelled _
  | Optional _ -> true
  | Nolabel -> false

and replace_type_path = fun ~source ~replacement ty ->
  let rec loop ty =
    match ty with
    | TList element -> TList (loop element)
    | TOption element -> TOption (loop element)
    | TTuple elements -> TTuple (List.map elements ~fn:loop)
    | TArrow (label, parameter, result) -> TArrow (label, loop parameter, loop result)
    | TCon (path, []) when SurfacePath.equal path source -> replacement
    | TCon (path, arguments) -> TCon (path, List.map arguments ~fn:loop)
    | TPolyVariant (bound, tags) -> TPolyVariant (
      bound,
      {
        tags = tags.tags
        |> List.map ~fn:(fun field -> { field with payload = Option.map field.payload ~fn:loop })
        |> normalized_poly_variant_tags
      }
    )
    | TPackage package -> TPackage {
      package
      with constraints = package.constraints
      |> List.map ~fn:(fun constraint_ -> { constraint_ with manifest = loop constraint_.manifest })
    }
    | ty -> ty
  in
  loop ty

and replace_package_binder_result = fun (package: package_ty) result_ty ->
  match package.binder with
  | None -> result_ty
  | Some binder -> package.constraints
  |> List.fold_left
    ~init:result_ty
    ~fn:(fun result_ty constraint_ ->
      replace_type_path
        ~source:(qualify_path [ binder ] constraint_.type_name)
        ~replacement:constraint_.manifest
        result_ty)

and same_type_variable = fun left right ->
  match left, right with
  | TVar left, TVar right -> Ptr.equal left right
  | _ -> false

and prefer_argument_alias_result = fun state ~at result_ty argument_ty ->
  match prune argument_ty with
  | TCon (path, []) when has_type_manifest state path -> (
      match resolve_type_manifest state path with
      | Some manifest ->
          unify state ~at result_ty manifest;
          argument_ty
      | None -> result_ty
    )
  | _ -> result_ty

and apply_argument_to_function = fun state ~level ~at function_ty argument_label argument_ty ->
  match prune function_ty with
  | TArrow (parameter_label, parameter_ty, result_ty) when apply_label_matches parameter_label argument_label ->
      let result_tracks_parameter = same_type_variable parameter_ty result_ty in
      unify state ~at parameter_ty argument_ty;
      let result_ty =
        match prune parameter_ty with
        | TPackage package -> replace_package_binder_result package result_ty
        | _ -> result_ty
      in
      if result_tracks_parameter then
        prefer_argument_alias_result state ~at result_ty argument_ty
      else
        result_ty
  | TArrow (Optional _, _, result_ty) when arg_label_equal argument_label Nolabel ->
      apply_argument_to_function state ~level ~at result_ty argument_label argument_ty
  | TArrow (parameter_label, parameter_ty, result_ty) when is_labeled_argument argument_label ->
      let result_ty = apply_argument_to_function state ~level ~at result_ty argument_label argument_ty in
      TArrow (parameter_label, parameter_ty, result_ty)
  | TVar { var=Unbound _ } ->
      let result_ty = fresh_tyvar state ~level in
      unify state ~at function_ty (TArrow (argument_label, argument_ty, result_ty));
      result_ty
  | _ ->
      let result_ty = fresh_tyvar state ~level in
      unify state ~at function_ty (TArrow (argument_label, argument_ty, result_ty));
      result_ty

and infer_record = fun state env ~level ~at fields ->
  match fields with
  | [] ->
      add_diagnostic state (unsupported_syntax at "empty record expression");
      fresh_tyvar state ~level
  | fields ->
      let owner_ty = ref None in
      List.for_each fields
        ~fn:(fun (field: TypAst.record_expression_field) ->
          let value_ty = infer_expression state env ~level field.value in
          let label =
            match !owner_ty with
            | Some owner_ty -> lookup_record_label_for_owner state field.name owner_ty
            | None -> lookup_record_label state field.name
          in
          match label with
          | None -> add_diagnostic
            state
            (unsupported_type
              field.origin
              ("unbound record field " ^ SurfacePath.to_string field.name))
          | Some label ->
              let label_owner_ty, label_field_ty = instantiate_pair
                state
                ~level
                label.owner_ty
                label.field_ty in
              unify state ~at:field.origin label_field_ty value_ty;
              (
                match !owner_ty with
                | Some owner_ty -> unify state ~at:field.origin owner_ty label_owner_ty
                | None -> owner_ty := Some label_owner_ty
              ));
      (
        match !owner_ty with
        | Some owner_ty -> owner_ty
        | None -> fresh_tyvar state ~level
      )

and infer_record_update = fun state env ~level ~at base fields ->
  let base_ty = infer_expression state env ~level base in
  List.for_each fields
    ~fn:(fun (field: TypAst.record_expression_field) ->
      let value_ty = infer_expression state env ~level field.value in
      match lookup_record_label_for_owner state field.name base_ty with
      | None -> add_diagnostic
        state
        (unsupported_type field.origin ("unbound record field " ^ SurfacePath.to_string field.name))
      | Some label ->
          let owner_ty, field_ty = instantiate_pair state ~level label.owner_ty label.field_ty in
          unify state ~at:field.origin base_ty owner_ty;
          unify state ~at:field.origin field_ty value_ty);
  if List.is_empty fields then
    add_diagnostic state (unsupported_syntax at "empty record update");
  base_ty

and infer_record_field = fun state ~level ~at receiver_ty field ->
  match lookup_record_label_for_owner state field receiver_ty with
  | None ->
      add_diagnostic
        state
        (unsupported_type at ("unbound record field " ^ SurfacePath.to_string field));
      fresh_tyvar state ~level
  | Some label ->
      let owner_ty, field_ty = instantiate_pair state ~level label.owner_ty label.field_ty in
      unify state ~at:at receiver_ty owner_ty;
      field_ty

and infer_field_access = fun state env ~level ~at receiver field ->
  let receiver_ty = infer_expression state env ~level receiver in
  infer_record_field state ~level ~at receiver_ty field

and infer_assignment = fun state env ~level ~at target value ->
  let value_ty = infer_expression state env ~level value in
  (
    match target.kind with
    | TypAst.FieldAccess { receiver; field } ->
        let receiver_ty = infer_expression state env ~level receiver in
        let field_ty = infer_record_field state ~level ~at:target.origin receiver_ty field in
        unify state ~at:value.origin field_ty value_ty
    | _ ->
        add_diagnostic state (unsupported_syntax target.origin "assignment target");
        let target_ty = infer_expression state env ~level target in
        unify state ~at:target.origin target_ty value_ty
  );
  TUnit

and infer_match = fun state env ~level scrutinee cases ->
  let scrutinee_ty = infer_expression state env ~level scrutinee in
  let result_ty = fresh_tyvar state ~level in
  List.for_each cases
    ~fn:(fun (case: TypAst.match_case) ->
      let pattern_ty, bindings = infer_pattern state env ~level case.pattern in
      unify state ~at:case.pattern.origin scrutinee_ty pattern_ty;
      let extended_env = extend_mono env bindings in
      (
        match case.guard with
        | Some guard ->
            let guard_ty = infer_expression state extended_env ~level guard in
            unify state ~at:guard.origin guard_ty TBool
        | None -> ()
      );
      let body_ty = infer_expression state extended_env ~level case.body in
      unify state ~at:case.body.origin result_ty body_ty);
  result_ty

and infer_function = fun state env ~level parameters body ->
  match parameters with
  | [] ->
      infer_function_body state env ~level body
  | ({ TypAst.kind=TypAst.LocallyAbstractType names; _ }: TypAst.pattern) :: rest ->
      let previous = state.locally_abstract_types in
      names |> List.for_each ~fn:(bind_locally_abstract_type state ~level);
      let result = infer_function state env ~level rest body in
      state.locally_abstract_types <- previous;
      result
  | parameter :: rest ->
      let label, parameter_ty, parameter_bindings = infer_function_parameter state env ~level parameter in
      let extended_env = extend_mono env parameter_bindings in
      let result_ty = infer_function state extended_env ~level rest body in
      TArrow (label, parameter_ty, result_ty)

and infer_function_body = fun state env ~level body ->
  match body with
  | TypAst.Body body -> infer_expression state env ~level body
  | TypAst.Cases cases -> infer_function_cases state env ~level cases

and simple_pattern_identifier = fun (pattern: TypAst.pattern) ->
  match pattern.kind with
  | TypAst.Path path -> (
      match simple_path_name path with
      | Some name when not (is_uppercase_name name) -> Some name
      | _ -> None
    )
  | TypAst.Attribute inner
  | TypAst.Parenthesized inner ->
      simple_pattern_identifier inner
  | _ ->
      None

and simple_expression_identifier = fun (expression: TypAst.expression) ->
  match expression.kind with
  | TypAst.Path path -> (
      match simple_path_name path with
      | Some name when not (is_uppercase_name name) -> Some name
      | _ -> None
    )
  | _ -> None

and row_identity_tag_case = fun (case: TypAst.match_case) ->
  match case.guard, case.pattern.kind, case.body.kind with
  | (None, TypAst.PolyVariant { tag=pattern_tag; payload=None }, TypAst.PolyVariant {
    tag=body_tag;
    payload=None
  }) when String.equal pattern_tag body_tag -> Some pattern_tag
  | _ -> None

and is_row_identity_catch_all_case = fun (case: TypAst.match_case) ->
  match case.guard, simple_pattern_identifier case.pattern, simple_expression_identifier case.body with
  | None, Some pattern_name, Some body_name -> String.equal pattern_name body_name
  | _ -> false

and infer_row_identity_function_cases = fun cases ->
  let tags = ref [] in
  let catch_all = ref false in
  let rec loop = function
    | [] -> !catch_all && not (List.is_empty !tags)
    | case :: rest -> (
        match row_identity_tag_case case with
        | Some tag ->
            tags := tag :: !tags;
            loop rest
        | None when is_row_identity_catch_all_case case ->
            catch_all := true;
            loop rest
        | None ->
            false
      )
  in
  if loop cases then
    let row_tags = {
      tags = !tags |> List.map ~fn:(fun tag -> { tag; payload = None }) |> normalized_poly_variant_tags
    } in
    let row = TPolyVariant (Lower, row_tags) in
    Some (TArrow (Nolabel, row, row))
  else
    None

and infer_function_cases = fun state env ~level cases ->
  match infer_row_identity_function_cases cases with
  | Some ty -> ty
  | None ->
      let parameter_ty = fresh_tyvar state ~level in
      let result_ty = fresh_tyvar state ~level in
      List.for_each cases
        ~fn:(fun (case: TypAst.match_case) ->
          let case_parameter_ty, bindings = infer_pattern state env ~level case.pattern in
          unify state ~at:case.origin parameter_ty case_parameter_ty;
          (
            match case.guard with
            | Some guard ->
                let guard_ty = infer_expression state env ~level guard in
                unify state ~at:guard.origin guard_ty TBool
            | None -> ()
          );
          let body_ty = infer_expression state (extend_mono env bindings) ~level case.body in
          unify state ~at:case.body.origin result_ty body_ty);
      TArrow (Nolabel, parameter_ty, result_ty)

and infer_lambda = fun state env ~level parameters body ->
  match parameters with
  | [] ->
      infer_expression state env ~level body
  | ({ TypAst.kind=TypAst.LocallyAbstractType names; _ }: TypAst.pattern) :: rest ->
      let previous = state.locally_abstract_types in
      names |> List.for_each ~fn:(bind_locally_abstract_type state ~level);
      let result = infer_lambda state env ~level rest body in
      state.locally_abstract_types <- previous;
      result
  | parameter :: rest ->
      let label, parameter_ty, parameter_bindings = infer_function_parameter state env ~level parameter in
      let extended_env = extend_mono env parameter_bindings in
      let result_ty = infer_lambda state extended_env ~level rest body in
      TArrow (label, parameter_ty, result_ty)

and infer_annotated_lambda = fun state env ~level parameters body annotation ->
  match parameters with
  | [] ->
      let body_ty = infer_expression state env ~level body in
      let annotated = lower_core_type state ~level (ref []) annotation in
      unify state ~at:annotation.origin body_ty annotated;
      annotated
  | ({ TypAst.kind=TypAst.LocallyAbstractType names; _ }: TypAst.pattern) :: rest ->
      let previous = state.locally_abstract_types in
      names |> List.for_each ~fn:(bind_locally_abstract_type state ~level);
      let result = infer_annotated_lambda state env ~level rest body annotation in
      state.locally_abstract_types <- previous;
      result
  | parameter :: rest ->
      let label, parameter_ty, parameter_bindings = infer_function_parameter state env ~level parameter in
      let extended_env = extend_mono env parameter_bindings in
      let result_ty = infer_annotated_lambda state extended_env ~level rest body annotation in
      TArrow (label, parameter_ty, result_ty)

and is_constructor_path = fun path ->
  match simple_path_name path with
  | Some name -> is_uppercase_name name
  | None -> false

and is_nonexpansive_expression = fun (expression: TypAst.expression) ->
  match expression.kind with
  | TypAst.Literal _
  | TypAst.Path _
  | TypAst.PolyVariant _
  | TypAst.FirstClassModule _
  | TypAst.Function _ -> true
  | TypAst.Tuple elements
  | TypAst.List elements -> List.all elements ~fn:is_nonexpansive_expression
  | TypAst.Record fields -> List.all
    fields
    ~fn:(fun (field: TypAst.record_expression_field) -> is_nonexpansive_expression field.value)
  | TypAst.RecordUpdate { base; fields } -> is_nonexpansive_expression base
  && List.all
    fields
    ~fn:(fun (field: TypAst.record_expression_field) -> is_nonexpansive_expression field.value)
  | TypAst.FieldAccess { receiver; _ } -> is_nonexpansive_expression receiver
  | TypAst.Assign _ -> false
  | TypAst.Apply { callee; arguments } -> is_constructor_expression callee
  && List.all arguments ~fn:is_nonexpansive_argument
  | TypAst.Sequence _
  | TypAst.If _
  | TypAst.Match _
  | TypAst.Infix _
  | TypAst.Let _
  | TypAst.LetModule _
  | TypAst.Assert _ -> false
  | TypAst.LocalOpen { body; _ } -> is_nonexpansive_expression body

and is_constructor_expression = fun (expression: TypAst.expression) ->
  match expression.kind with
  | TypAst.Path path -> is_constructor_path path
  | _ -> false

and is_nonexpansive_argument = fun (argument: TypAst.argument) ->
  match argument.kind with
  | TypAst.Positional expression -> is_nonexpansive_expression expression
  | TypAst.Labeled { value=Some value; _ }
  | TypAst.Optional { value=Some value; _ } -> is_nonexpansive_expression value
  | TypAst.Labeled { value=None; _ }
  | TypAst.Optional { value=None; _ } -> false

and is_nonexpansive_let_binding = fun (binding: TypAst.let_binding) ->
  (not (List.is_empty binding.parameters)) || is_nonexpansive_expression binding.body

and infer_let_binding_value = fun state env ~level (binding: TypAst.let_binding) ->
  let value_ty =
    match binding.parameters, binding.type_annotation with
    | [], _ -> infer_expression state env ~level:(level + 1) binding.body
    | _, Some annotation -> infer_annotated_lambda
      state
      env
      ~level:(level + 1)
      binding.parameters
      binding.body
      annotation
    | _, None -> infer_lambda state env ~level:(level + 1) binding.parameters binding.body
  in
  match binding.parameters, binding.type_annotation with
  | [], Some annotation ->
      let annotated = lower_core_type state ~level:(level + 1) (ref []) annotation in
      unify state ~at:binding.origin value_ty annotated;
      lower_core_type state ~level:(level + 1) (ref []) annotation
  | _ -> value_ty

and runtime_parameter_count = fun parameters ->
  parameters |> List.filter
    ~fn:(fun (parameter: TypAst.pattern) ->
      match parameter.kind with
      | TypAst.LocallyAbstractType _ -> false
      | _ -> true) |> List.length

and unify_function_result_annotation = fun state ~at ~arity function_ty annotation ->
  match arity, prune function_ty with
  | n, TArrow (_, _, result) when n > 0 -> unify_function_result_annotation
    state
    ~at
    ~arity:(n - 1)
    result
    annotation
  | _, result -> unify state ~at result annotation

and infer_let_binding = fun state env ~level ~recursive (binding: TypAst.let_binding) ->
  if recursive then
    add_diagnostic state (unsupported_syntax binding.origin "recursive let binding");
  let value_ty = infer_let_binding_value state env ~level binding in
  let pattern_ty, bindings = infer_pattern state env ~level:(level + 1) binding.pattern in
  unify state ~at:binding.origin pattern_ty value_ty;
  let exported_bindings =
    if is_nonexpansive_let_binding binding then
      generalized_bindings ~level bindings
    else
      bindings
  in
  let extended_env = extend_mono env exported_bindings in
  (extended_env, exported_bindings)

and simple_let_binding_name = fun (binding: TypAst.let_binding) ->
  let rec loop (pattern: TypAst.pattern) =
    match pattern.kind with
    | TypAst.Path path -> (
        match simple_path_name path with
        | Some name when not (is_uppercase_name name) -> Some (SurfacePath.from_name name)
        | _ -> None
      )
    | TypAst.Constraint { pattern; _ }
    | TypAst.Attribute pattern
    | TypAst.Parenthesized pattern ->
        loop pattern
    | _ ->
        None
  in
  loop binding.pattern

and make_recursive_placeholder = fun state ~level (binding: TypAst.let_binding) ->
  match simple_let_binding_name binding with
  | Some name -> Some (make_binding state ~name ~ty:(fresh_tyvar state ~level:(level + 1)))
  | None ->
      add_diagnostic state (unsupported_syntax binding.origin "recursive let pattern");
      None

and recursive_placeholder_for_binding = fun placeholders (binding: TypAst.let_binding) ->
  match simple_let_binding_name binding with
  | None -> None
  | Some name ->
      List.find placeholders
        ~fn:(fun placeholder ->
          SurfacePath.equal (EntityId.surface_path placeholder.entity_id) name)

and infer_recursive_let_binding = fun state recursive_env ~level placeholders binding ->
  match recursive_placeholder_for_binding placeholders binding with
  | None -> ()
  | Some placeholder ->
      let value_ty = infer_let_binding_value state recursive_env ~level binding in
      unify state ~at:binding.origin placeholder.ty value_ty

and public_recursive_let_binding = fun state ~level placeholders binding ->
  match recursive_placeholder_for_binding placeholders binding with
  | None -> None
  | Some placeholder ->
      let ty =
        match binding.type_annotation with
        | Some annotation -> lower_core_type state ~level:(level + 1) (ref []) annotation
        | None -> placeholder.ty
      in
      Some { placeholder with ty = generalize level ty }

and infer_let_declaration = fun state env ~level (declaration: TypAst.let_declaration) ->
  if declaration.recursive then
    let placeholders =
      List.fold_left declaration.bindings ~init:[]
        ~fn:(fun placeholders binding ->
          match make_recursive_placeholder state ~level binding with
          | Some placeholder -> placeholder :: placeholders
          | None -> placeholders)
      |> List.reverse
    in
    let recursive_env = extend_mono env placeholders in
    List.for_each
      declaration.bindings
      ~fn:(infer_recursive_let_binding state recursive_env ~level placeholders);
    let public_bindings = declaration.bindings
    |> List.filter_map ~fn:(public_recursive_let_binding state ~level placeholders) in
    (extend_mono env public_bindings, public_bindings)
  else
    List.fold_left declaration.bindings ~init:(env, [])
      ~fn:(fun (env, public_bindings) binding ->
        let next_env, item_bindings = infer_let_binding
          state
          env
          ~level
          ~recursive:declaration.recursive
          binding in
        (next_env, List.append public_bindings item_bindings))

and bind_declared_value = fun state env ~level name annotation ->
  let ty = lower_core_type state ~level (ref []) annotation in
  let name = SurfacePath.from_name name in
  let binding = make_binding state ~name ~ty in
  let binding = { binding with ty = generalize level ty } in
  let extended_env = binding :: env in
  (extended_env, [ binding ])

and type_parameter_name = function
  | Some name -> SurfacePath.from_name name
  | None -> SurfacePath.from_name "_"

and type_parameter_bindings = fun parameters ->
  let index = ref 0 in
  let vars = ref [] in
  let arguments = ref [] in
  List.for_each parameters
    ~fn:(fun parameter ->
      let ty = generic_var !index in
      vars := (type_parameter_name parameter, ty) :: !vars;
      arguments := ty :: !arguments;
      index := !index + 1);
  (!vars, List.reverse !arguments)

and qualify_name = fun path_prefix name ->
  match path_prefix with
  | [] -> SurfacePath.from_name name
  | prefix -> SurfacePath.from_segments (List.append prefix [ name ])

and strip_prefix = fun prefix segments ->
  match prefix, segments with
  | [], rest -> Some rest
  | prefix :: prefixes, segment :: segments when String.equal prefix segment -> strip_prefix
    prefixes
    segments
  | _ -> None

and path_has_prefix = fun prefix path ->
  match strip_prefix prefix (SurfacePath.to_segments path) with
  | Some _ -> true
  | None -> false

and replace_path_prefix = fun ~source_prefix ~target_prefix path ->
  match strip_prefix source_prefix (SurfacePath.to_segments path) with
  | Some rest -> SurfacePath.from_segments (List.append target_prefix rest)
  | None -> path

and replace_ty_prefix = fun ~source_prefix ~target_prefix ty ->
  match prune ty with
  | TList element -> TList (replace_ty_prefix ~source_prefix ~target_prefix element)
  | TOption element -> TOption (replace_ty_prefix ~source_prefix ~target_prefix element)
  | TTuple elements -> TTuple (List.map
    elements
    ~fn:(replace_ty_prefix ~source_prefix ~target_prefix))
  | TArrow (label, parameter, result) -> TArrow (
    label,
    replace_ty_prefix ~source_prefix ~target_prefix parameter,
    replace_ty_prefix ~source_prefix ~target_prefix result
  )
  | TCon (path, arguments) -> TCon (
    replace_path_prefix ~source_prefix ~target_prefix path,
    List.map arguments ~fn:(replace_ty_prefix ~source_prefix ~target_prefix)
  )
  | TPolyVariant (bound, tags) -> TPolyVariant (
    bound,
    {
      tags = tags.tags
      |> List.map
        ~fn:(fun field ->
          {
            field
            with payload = Option.map
              field.payload
              ~fn:(replace_ty_prefix ~source_prefix ~target_prefix)
          })
      |> normalized_poly_variant_tags
    }
  )
  | TPackage package -> TPackage {
    package
    with module_type = replace_path_prefix ~source_prefix ~target_prefix package.module_type;
    constraints = package.constraints
    |> List.map
      ~fn:(fun constraint_ ->
        {
          type_name = replace_path_prefix ~source_prefix ~target_prefix constraint_.type_name;
          manifest = replace_ty_prefix ~source_prefix ~target_prefix constraint_.manifest
        })
  }
  | ty -> ty

and replace_ty_prefixes = fun substitutions ty ->
  List.fold_left
    substitutions
    ~init:ty
    ~fn:(fun ty (source_prefix, target_prefix) -> replace_ty_prefix ~source_prefix ~target_prefix ty)

and qualify_binding = fun state path_prefix binding ->
  let name = EntityId.surface_path binding.entity_id |> SurfacePath.to_segments in
  make_binding state ~name:(SurfacePath.from_segments (List.append path_prefix name)) ~ty:binding.ty

and copy_binding_prefix = fun state ~source_prefix ~target_prefix binding ->
  let source_path = EntityId.surface_path binding.entity_id in
  match strip_prefix source_prefix (SurfacePath.to_segments source_path) with
  | Some rest -> Some (make_binding
    state
    ~name:(SurfacePath.from_segments (List.append target_prefix rest))
    ~ty:(replace_ty_prefix ~source_prefix ~target_prefix binding.ty))
  | None -> None

and copy_binding_prefix_with_substitutions = fun state ~source_prefix ~target_prefix ~substitutions binding ->
  let source_path = EntityId.surface_path binding.entity_id in
  match strip_prefix source_prefix (SurfacePath.to_segments source_path) with
  | Some rest -> Some (make_binding
    state
    ~name:(SurfacePath.from_segments (List.append target_prefix rest))
    ~ty:(binding.ty |> replace_ty_prefix ~source_prefix ~target_prefix |> replace_ty_prefixes substitutions))
  | None -> None

and copy_binding_prefix_to_local = fun state ~source_prefix ~target_prefix binding ->
  let source_path = EntityId.surface_path binding.entity_id in
  match strip_prefix source_prefix (SurfacePath.to_segments source_path) with
  | Some rest -> Some (make_binding
    state
    ~name:(SurfacePath.from_segments rest)
    ~ty:(replace_ty_prefix ~source_prefix ~target_prefix binding.ty))
  | None -> None

and binding_has_path_prefix = fun path_prefix binding ->
  path_has_prefix path_prefix (EntityId.surface_path binding.entity_id)

and find_module_summary = fun state path ->
  List.find ((state.module_summaries: module_summary list))
    ~fn:(fun (summary: module_summary) ->
      SurfacePath.equal summary.path path)

and find_module_type_summary = fun state path ->
  List.find ((state.module_type_summaries: module_type_summary list))
    ~fn:(fun (summary: module_type_summary) ->
      SurfacePath.equal summary.path path)

and find_functor_summary = fun state path ->
  List.find ((state.functor_summaries: functor_summary list))
    ~fn:(fun (summary: functor_summary) ->
      SurfacePath.equal summary.path path)

and remove_bindings_with_prefix = fun path_prefix bindings ->
  List.filter bindings ~fn:(fun binding -> not (binding_has_path_prefix path_prefix binding))

and binding_path_in = fun paths binding ->
  let path = EntityId.surface_path binding.entity_id in
  List.exists
    (fun other ->
      SurfacePath.equal path other)
    paths

and remove_bindings_by_paths = fun paths bindings ->
  List.filter bindings ~fn:(fun binding -> not (binding_path_in paths binding))

and remove_module_summary = fun path summaries ->
  List.filter
    summaries
    ~fn:(fun (summary: module_summary) -> not (SurfacePath.equal summary.path path))

and bind_type_alias = fun state ~name_path ~type_path ->
  state.type_aliases <- (name_path, type_path) :: state.type_aliases

and bind_record_field_declaration = fun state ~level ~path_prefix ~owner_ty vars (
  field: TypAst.record_field_declaration
) ->
  let field_ty = lower_record_field_type state ~level vars field.type_annotation in
  state.record_labels <- { label = qualify_name path_prefix field.name; owner_ty; field_ty }
  :: state.record_labels

and lower_record_field_type = fun state ~level vars (type_annotation: TypAst.core_type) ->
  let field_ty = lower_core_type state ~level (ref vars) type_annotation in
  match type_annotation.kind with
  | TypAst.Poly _ -> generalize level field_ty
  | _ -> field_ty

and inline_record_owner_ty = fun type_path constructor_name arguments ->
  TCon (
    SurfacePath.from_segments (List.append (SurfacePath.to_segments type_path) [ constructor_name ]),
    arguments
  )

and constructor_payload_ty = fun state ~level ~path_prefix ~type_path ~result_arguments vars (
  constructor: TypAst.type_constructor
) ->
  match constructor.inline_record, constructor.payload with
  | Some fields, _ ->
      let owner_ty = inline_record_owner_ty type_path constructor.name result_arguments in
      fields
      |> List.for_each ~fn:(bind_record_field_declaration state ~level ~path_prefix ~owner_ty vars);
      Some owner_ty
  | None, Some payload ->
      Some (lower_core_type state ~level (ref vars) payload)
  | None, None ->
      None

and constructor_binding_of_declaration = fun state ~level ~path_prefix ~type_path ~result_ty ~result_arguments vars (
  constructor: TypAst.type_constructor
) ->
  let ty =
    match constructor.result with
    | Some result -> (
        let vars = ref vars in
        let constructor_level = level + 1 in
        let result_ty = lower_core_type state ~level:constructor_level vars result in
        match constructor.inline_record, constructor.payload with
        | None, Some payload ->
            let payload_ty = lower_core_type state ~level:constructor_level vars payload in
            arrow payload_ty result_ty
        | _ -> result_ty
      )
    | None -> (
        match constructor_payload_ty state ~level ~path_prefix ~type_path ~result_arguments vars constructor with
        | None -> result_ty
        | Some payload_ty -> arrow payload_ty result_ty
      )
  in
  make_binding state ~name:(qualify_name path_prefix constructor.name) ~ty

and bind_type_declaration = fun state env ~level ~type_path_prefix ~name_path_prefix (
  declaration: TypAst.type_declaration
) ->
  let vars, arguments = type_parameter_bindings declaration.parameters in
  let type_path = qualify_name type_path_prefix declaration.name in
  bind_type_alias state ~name_path:(qualify_name name_path_prefix declaration.name) ~type_path;
  let result_ty = TCon (type_path, arguments) in
  match declaration.definition.kind with
  | TypAst.Variant constructors ->
      constructors
      |> List.map
        ~fn:(constructor_binding_of_declaration
          state
          ~level
          ~path_prefix:name_path_prefix
          ~type_path
          ~result_ty
          ~result_arguments:arguments
          vars)
      |> extend_generalized env ~level
  | TypAst.Alias type_ ->
      let manifest = lower_core_type state ~level (ref vars) type_ in
      state.type_manifests <- (type_path, manifest) :: state.type_manifests;
      env
  | TypAst.Record fields ->
      fields
      |> List.for_each
        ~fn:(bind_record_field_declaration
          state
          ~level
          ~path_prefix:name_path_prefix
          ~owner_ty:result_ty
          vars);
      env
  | TypAst.Abstract ->
      env

and bind_module_type_item_aliases = fun state ~module_prefix (item: TypAst.signature_item) ->
  match item.kind with
  | TypAst.Type declarations -> List.for_each
    declarations
    ~fn:(fun declaration ->
      bind_type_alias
        state
        ~name_path:(SurfacePath.from_name declaration.name)
        ~type_path:(qualify_name module_prefix declaration.name))
  | TypAst.Value _
  | TypAst.External _ -> ()

and ascribed_binding_for_value_declaration = fun state ~level ~module_prefix name type_annotation ->
  let previous_type_manifests = state.type_manifests in
  state.type_manifests <- [];
  let ty = lower_core_type state ~level (ref []) type_annotation in
  state.type_manifests <- previous_type_manifests;
  let binding = make_binding state ~name:(qualify_name module_prefix name) ~ty in
  { binding with ty = generalize level ty }

and ascribed_bindings_for_signature_item = fun state ~level ~module_prefix (
  item: TypAst.signature_item
) ->
  match item.kind with
  | TypAst.Value declaration -> [
    ascribed_binding_for_value_declaration state ~level ~module_prefix declaration.name declaration.type_annotation
  ]
  | TypAst.External declaration -> [
    ascribed_binding_for_value_declaration state ~level ~module_prefix declaration.name declaration.type_annotation
  ]
  | TypAst.Type _ -> []

and bindings_for_module_type = fun state ~level ~module_prefix ~module_type_path ->
  match find_module_type_summary state module_type_path with
  | None -> []
  | Some summary ->
      let previous_type_aliases = state.type_aliases in
      List.for_each summary.items ~fn:(bind_module_type_item_aliases state ~module_prefix);
      let bindings = summary.items
      |> List.flat_map ~fn:(ascribed_bindings_for_signature_item state ~level ~module_prefix) in
      state.type_aliases <- previous_type_aliases;
      bindings

and ascribe_module_bindings = fun state env ~level ~module_prefix ~module_type_path items ->
  match find_module_type_summary state module_type_path with
  | None -> env
  | Some summary ->
      let ascribed_bindings = bindings_for_module_type state ~level ~module_prefix ~module_type_path in
      let ascribed_paths =
        List.map ascribed_bindings ~fn:(fun binding -> EntityId.surface_path binding.entity_id)
      in
      let module_path = SurfacePath.from_segments module_prefix in
      let base_env_bindings =
        match find_module_summary state module_path with
        | Some existing -> existing.env_bindings
        | None -> []
      in
      let env_bindings = extend_mono (remove_bindings_by_paths ascribed_paths base_env_bindings) ascribed_bindings in
      state.module_value_bindings <- List.append
        (remove_bindings_with_prefix module_prefix state.module_value_bindings)
        ascribed_bindings;
      state.module_summaries <- {
        path = module_path;
        items;
        env_bindings;
        value_bindings = ascribed_bindings
      }
      :: remove_module_summary module_path state.module_summaries;
      extend_mono (remove_bindings_by_paths ascribed_paths env) ascribed_bindings

and bind_module_type_declarations = fun state ~level ~module_prefix local_env exported_env declarations ->
  let local_env =
    List.fold_left
      declarations
      ~init:local_env
      ~fn:(fun env declaration ->
        bind_type_declaration state env ~level ~type_path_prefix:module_prefix ~name_path_prefix:[] declaration)
  in
  let exported_env =
    List.fold_left
      declarations
      ~init:exported_env
      ~fn:(fun env declaration ->
        bind_type_declaration
          state
          env
          ~level
          ~type_path_prefix:module_prefix
          ~name_path_prefix:module_prefix
          declaration)
  in
  (local_env, exported_env)

and infer_structure_item = fun state env ~level ~path_prefix (item: TypAst.structure_item) ->
  match item.kind with
  | TypAst.Let declaration ->
      let env, bindings = infer_let_declaration state env ~level declaration in
      (env, bindings, [])
  | TypAst.Type declarations ->
      let env =
        List.fold_left
          declarations
          ~init:env
          ~fn:(fun env declaration ->
            bind_type_declaration
              state
              env
              ~level
              ~type_path_prefix:path_prefix
              ~name_path_prefix:path_prefix
              declaration)
      in
      (env, [], declarations)
  | TypAst.Expression expression ->
      let _ = infer_expression state env ~level expression in
      (env, [], [])
  | TypAst.External declaration ->
      let env, bindings = bind_declared_value state env ~level declaration.name declaration.type_annotation in
      (env, bindings, [])
  | TypAst.Module declarations ->
      let env =
        List.fold_left
          declarations
          ~init:env
          ~fn:(fun env declaration -> bind_module_declaration state env ~level ~path_prefix declaration)
      in
      (env, [], [])
  | TypAst.ModuleType declaration ->
      state.module_type_summaries <- {
        path = qualify_name path_prefix declaration.name;
        items = declaration.items
      }
      :: state.module_type_summaries;
      (env, [], [])
  | TypAst.Include path -> (
      match find_module_summary state path with
      | Some summary ->
          let source_prefix = SurfacePath.to_segments summary.path in
          let target_prefix = path_prefix in
          let copied = summary.env_bindings
          |> List.filter_map ~fn:(copy_binding_prefix state ~source_prefix ~target_prefix) in
          (extend_mono env copied, [], [])
      | None ->
          add_diagnostic
            state
            (unsupported_type item.origin ("unbound included module " ^ SurfacePath.to_string path));
          (env, [], [])
    )

and bind_module_declaration = fun state env ~level ~path_prefix (
  declaration: TypAst.module_declaration
) ->
  let module_prefix = List.append path_prefix [ declaration.name ] in
  match declaration.parameters, declaration.application, declaration.alias with
  | _ :: _, _, _ ->
      bind_functor_declaration state env ~level ~module_prefix declaration
  | [], Some application, _ ->
      bind_module_application state env ~module_prefix application
  | [], None, Some source_path ->
      bind_module_alias state env ~module_prefix ~source_path
  | [], None, None ->
      let env = bind_module_structure state env ~level ~module_prefix declaration.items in
      (
        match declaration.module_type with
        | Some module_type_path -> ascribe_module_bindings
          state
          env
          ~level
          ~module_prefix
          ~module_type_path
          declaration.items
        | None -> env
      )

and bind_functor_parameter = fun state env ~level (parameter: TypAst.functor_parameter) ->
  match parameter.module_type with
  | Some module_type_path ->
      let bindings = bindings_for_module_type state ~level ~module_prefix:[ parameter.name ] ~module_type_path in
      extend_mono env bindings
  | None -> env

and bind_functor_parameters = fun state env ~level parameters ->
  List.fold_left
    parameters
    ~init:env
    ~fn:(fun env parameter -> bind_functor_parameter state env ~level parameter)

and bind_functor_declaration = fun state env ~level ~module_prefix (
  declaration: TypAst.module_declaration
) ->
  let parameter_env = bind_functor_parameters state env ~level declaration.parameters in
  let _ = bind_module_structure state parameter_env ~level ~module_prefix declaration.items in
  let env =
    match declaration.module_type with
    | Some module_type_path -> ascribe_module_bindings
      state
      env
      ~level
      ~module_prefix
      ~module_type_path
      declaration.items
    | None -> env
  in
  let module_path = SurfacePath.from_segments module_prefix in
  let env_bindings, value_bindings =
    match find_module_summary state module_path with
    | Some summary -> (summary.env_bindings, summary.value_bindings)
    | None -> ([], [])
  in
  state.functor_summaries <- {
    path = module_path;
    parameters = declaration.parameters;
    items = declaration.items;
    env_bindings;
    value_bindings;
  } :: state.functor_summaries;
  env

and bind_module_application = fun state env ~module_prefix (application: TypAst.module_application) ->
  match find_functor_summary state application.callee with
  | None -> env
  | Some summary ->
      let source_prefix = SurfacePath.to_segments summary.path in
      let substitutions =
        match summary.parameters with
        | parameter :: _ -> [ ([ parameter.name ], SurfacePath.to_segments application.argument) ]
        | [] -> []
      in
      let copied_env_bindings = summary.env_bindings
      |> List.filter_map
        ~fn:(copy_binding_prefix_with_substitutions
          state
          ~source_prefix
          ~target_prefix:module_prefix
          ~substitutions) in
      let copied_value_bindings = summary.value_bindings
      |> List.filter_map
        ~fn:(copy_binding_prefix_with_substitutions
          state
          ~source_prefix
          ~target_prefix:module_prefix
          ~substitutions) in
      state.module_value_bindings <- List.append state.module_value_bindings copied_value_bindings;
      state.module_summaries <- {
        path = SurfacePath.from_segments module_prefix;
        items = summary.items;
        env_bindings = copied_env_bindings;
        value_bindings = copied_value_bindings
      }
      :: state.module_summaries;
      extend_mono env copied_env_bindings

and bind_module_alias = fun state env ~module_prefix ~source_path ->
  match find_module_summary state source_path with
  | Some summary ->
      let source_prefix = SurfacePath.to_segments summary.path in
      let copied_env_bindings = summary.env_bindings
      |> List.filter_map ~fn:(copy_binding_prefix state ~source_prefix ~target_prefix:module_prefix) in
      let copied_value_bindings = summary.value_bindings
      |> List.filter_map ~fn:(copy_binding_prefix state ~source_prefix ~target_prefix:module_prefix) in
      state.module_value_bindings <- List.append state.module_value_bindings copied_value_bindings;
      state.module_summaries <- {
        path = SurfacePath.from_segments module_prefix;
        items = summary.items;
        env_bindings = copied_env_bindings;
        value_bindings = copied_value_bindings
      }
      :: state.module_summaries;
      extend_mono env copied_env_bindings
  | None -> env

and bind_include = fun state ~level ~module_prefix local_env exported_env path ->
  match find_module_summary state path with
  | Some summary ->
      let source_prefix = SurfacePath.to_segments summary.path in
      let local_env, exported_env =
        List.fold_left summary.items ~init:(local_env, exported_env)
          ~fn:(fun (local_env, exported_env) item ->
            match item.kind with
            | TypAst.Type declarations -> bind_module_type_declarations
              state
              ~level
              ~module_prefix
              local_env
              exported_env
              declarations
            | _ -> (local_env, exported_env))
      in
      let copied_local_bindings = summary.env_bindings
      |> List.filter_map
        ~fn:(copy_binding_prefix_to_local state ~source_prefix ~target_prefix:module_prefix) in
      let copied_exported_bindings = summary.env_bindings
      |> List.filter_map ~fn:(copy_binding_prefix state ~source_prefix ~target_prefix:module_prefix) in
      let copied_value_bindings = summary.value_bindings
      |> List.filter_map ~fn:(copy_binding_prefix state ~source_prefix ~target_prefix:module_prefix) in
      state.module_value_bindings <- List.append state.module_value_bindings copied_value_bindings;
      (
        extend_mono local_env copied_local_bindings,
        extend_mono exported_env copied_exported_bindings
      )
  | None -> (local_env, exported_env)

and bind_module_structure = fun state env ~level ~module_prefix items ->
  let previous_type_aliases = state.type_aliases in
  let _, exported_env =
    List.fold_left items ~init:(env, env)
      ~fn:(fun (local_env, exported_env) item ->
        match item.kind with
        | TypAst.Type declarations ->
            bind_module_type_declarations state ~level ~module_prefix local_env exported_env declarations
        | TypAst.Let declaration ->
            let local_env, local_bindings = infer_let_declaration state local_env ~level declaration in
            let qualified_bindings = List.map
              local_bindings
              ~fn:(qualify_binding state module_prefix) in
            state.module_value_bindings <- List.append state.module_value_bindings qualified_bindings;
            (local_env, extend_mono exported_env qualified_bindings)
        | TypAst.External declaration ->
            let local_env, local_bindings = bind_declared_value
              state
              local_env
              ~level
              declaration.name
              declaration.type_annotation in
            let qualified_bindings = List.map
              local_bindings
              ~fn:(qualify_binding state module_prefix) in
            state.module_value_bindings <- List.append state.module_value_bindings qualified_bindings;
            (local_env, extend_mono exported_env qualified_bindings)
        | TypAst.Module declarations ->
            let local_env, exported_env =
              List.fold_left declarations ~init:(local_env, exported_env)
                ~fn:(fun (local_env, exported_env) declaration ->
                  let exported_env = bind_module_declaration
                    state
                    exported_env
                    ~level
                    ~path_prefix:module_prefix
                    declaration in
                  (local_env, exported_env))
            in
            (local_env, exported_env)
        | TypAst.Include path ->
            bind_include state ~level ~module_prefix local_env exported_env path
        | TypAst.ModuleType declaration ->
            state.module_type_summaries <- {
              path = qualify_name module_prefix declaration.name;
              items = declaration.items
            }
            :: state.module_type_summaries;
            (local_env, exported_env)
        | TypAst.Expression _ ->
            (local_env, exported_env))
  in
  state.type_aliases <- previous_type_aliases;
  let module_path = SurfacePath.from_segments module_prefix in
  let env_bindings = List.filter exported_env ~fn:(binding_has_path_prefix module_prefix) in
  let value_bindings = List.filter
    state.module_value_bindings
    ~fn:(binding_has_path_prefix module_prefix) in
  state.module_summaries <- { path = module_path; items; env_bindings; value_bindings } :: state.module_summaries;
  exported_env

let check_implementation = fun ~ast ~typing_context items ->
  let state = make_state ~next_binding_stamp:typing_context.Typing_context.next_binding_stamp in
  let env = env_of_typing_context typing_context in
  let _, bindings, type_declarations =
    List.fold_left items ~init:(env, [], [])
      ~fn:(fun (env, bindings, type_declarations) item ->
        let next_env, item_bindings, item_type_declarations = infer_structure_item
          state
          env
          ~level:0
          ~path_prefix:[]
          item in
        (
          next_env,
          List.append bindings item_bindings,
          List.append type_declarations item_type_declarations
        ))
  in
  let public_bindings = List.map bindings ~fn:public_binding_of_binding in
  let public_module_bindings = List.map state.module_value_bindings ~fn:public_binding_of_binding in
  {
    File.ast;
    diagnostics = List.reverse state.diagnostics;
    type_declarations;
    bindings = public_bindings;
    typing_context = {
      Typing_context.next_binding_stamp = state.next_binding_stamp;
      values = List.append typing_context.values (List.append public_bindings public_module_bindings)
    };
  }

let check_signature_item = fun state env ~level (item: TypAst.signature_item) ->
  match item.kind with
  | TypAst.Value declaration ->
      let env, bindings = bind_declared_value state env ~level declaration.name declaration.type_annotation in
      (env, bindings, [])
  | TypAst.Type declarations ->
      let env =
        List.fold_left
          declarations
          ~init:env
          ~fn:(fun env declaration ->
            bind_type_declaration state env ~level ~type_path_prefix:[] ~name_path_prefix:[] declaration)
      in
      (env, [], declarations)
  | TypAst.External declaration ->
      let env, bindings = bind_declared_value state env ~level declaration.name declaration.type_annotation in
      (env, bindings, [])

let check_interface = fun ~ast ~typing_context items ->
  let state = make_state ~next_binding_stamp:typing_context.Typing_context.next_binding_stamp in
  let env = env_of_typing_context typing_context in
  let _, bindings, type_declarations =
    List.fold_left items ~init:(env, [], [])
      ~fn:(fun (env, bindings, type_declarations) item ->
        let next_env, item_bindings, item_type_declarations = check_signature_item
          state
          env
          ~level:0
          item in
        (
          next_env,
          List.append bindings item_bindings,
          List.append type_declarations item_type_declarations
        ))
  in
  let public_bindings = List.map bindings ~fn:public_binding_of_binding in
  {
    File.ast;
    diagnostics = List.reverse state.diagnostics;
    type_declarations;
    bindings = public_bindings;
    typing_context = {
      Typing_context.next_binding_stamp = state.next_binding_stamp;
      values = List.append typing_context.values public_bindings
    };
  }

let check_source_file = fun ~typing_context ast ->
  match ast.TypAst.kind with
  | TypAst.Implementation items -> check_implementation ~ast ~typing_context items
  | TypAst.Interface items -> check_interface ~ast ~typing_context items
  | TypAst.Empty _ -> File.empty ~ast ~typing_context

let check_expression = fun expression ->
  let state = make_state ~next_binding_stamp:0 in
  let _ = infer_expression state [] ~level:0 expression in
  List.reverse state.diagnostics

let check_pattern = fun pattern ->
  let state = make_state ~next_binding_stamp:0 in
  let _ = infer_pattern state [] ~level:0 pattern in
  List.reverse state.diagnostics

let check_let_binding = fun binding ->
  let state = make_state ~next_binding_stamp:0 in
  let _ = infer_let_binding state [] ~level:0 ~recursive:false binding in
  List.reverse state.diagnostics

let check_core_type = fun core_type ->
  let state = make_state ~next_binding_stamp:0 in
  let _ = lower_core_type state ~level:0 (ref []) core_type in
  List.reverse state.diagnostics

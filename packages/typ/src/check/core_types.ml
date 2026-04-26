open Std
open Std.Collections

module TypAst = Ast
module SurfacePath = Model.Surface_path
module BindingId = Model.Binding_id
module EntityId = Model.Entity_id

(* Internal inference type language.

   This deliberately does not reuse [Typing_context.type_expr]. The checker
   needs mutable union-find variables, level tracking, shared row objects, and
   manifest expansion while solving. Only exported bindings are converted to the
   immutable public type language.
*)

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
  | NoLabel
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

and tyvar_cell = { mutable var: tvar }

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

(* Per-check mutable state.

   The one-shot checker keeps mutation query-local: each file check allocates a
   fresh [state], imports any caller-provided [Typing_context.t], and exports a
   new public context at the end. Nothing here is shared across checks.
*)

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
    summary;
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
  TVar {
    var = Unbound (id, level);
  }

let fresh_binding_id = fun state ~name ->
  let stamp = state.next_binding_stamp in
  state.next_binding_stamp <- stamp + 1;
  BindingId.local ~stamp ~name

let make_binding = fun state ~name ~ty ->
  let binding_id = fresh_binding_id state ~name in
  let entity_id = EntityId.resolved ~binding_id ~surface_path:name in
  { binding_id; entity_id; ty }

let rec prune = function
  | TVar ({ var = Link linked_ty } as cell) ->
      let linked_ty = prune linked_ty in
      cell.var <- Link linked_ty;
      linked_ty
  | ty -> ty

let normalized_poly_variant_tags = fun tags ->
  tags
  |> List.sort ~compare:(fun left right -> String.compare left.tag right.tag)
  |> List.fold_left
    ~init:[]
    ~fn:(fun fields field ->
      match fields with
      | previous :: rest when String.equal previous.tag field.tag -> previous :: rest
      | _ -> field :: fields)
  |> List.reverse

let find_poly_variant_field = fun tag fields ->
  List.find
    fields
    ~fn:(fun field -> String.equal field.tag tag)

let poly_variant_tag_names = fun tags ->
  tags
  |> List.map ~fn:(fun field -> field.tag)

let poly_variant_tags_subset = fun left right ->
  let right_names = poly_variant_tag_names right in
  List.all
    (poly_variant_tag_names left)
    ~fn:(fun tag -> List.exists (fun other -> String.equal tag other) right_names)

let same_poly_variant_tags = fun left right ->
  poly_variant_tags_subset left right && poly_variant_tags_subset right left

let copy_poly_variant_tags = fun copies ~map_payload tags ->
  match List.find !copies ~fn:(fun (source, _) -> Ptr.equal source tags) with
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
  | TInt -> "int"
  | TBool -> "bool"
  | TChar -> "char"
  | TString -> "string"
  | TFloat -> "float"
  | TUnit -> "unit"
  | TList element -> string_of_ty element ^ " list"
  | TOption element -> string_of_ty element ^ " option"
  | TTuple elements ->
      elements
      |> List.map ~fn:string_of_ty
      |> String.concat " * "
  | TArrow (_, parameter, result) -> string_of_ty parameter ^ " -> " ^ string_of_ty result
  | TCon (path, []) -> SurfacePath.to_string path
  | TCon (path, [ argument ]) -> string_of_ty argument ^ " " ^ SurfacePath.to_string path
  | TCon (path, arguments) ->
      "("
      ^ (
        arguments
        |> List.map ~fn:string_of_ty
        |> String.concat ", "
      )
      ^ ") "
      ^ SurfacePath.to_string path
  | TPolyVariant (Exact, tags) -> "[ " ^ render_poly_variant_tags tags.tags ^ " ]"
  | TPolyVariant (Upper, tags) -> "[< " ^ render_poly_variant_tags tags.tags ^ " ]"
  | TPolyVariant (Lower, tags) -> "[> " ^ render_poly_variant_tags tags.tags ^ " ]"
  | TPackage package ->
      let constraints =
        package.constraints
        |> List.map
          ~fn:(fun constraint_ ->
            " with type "
            ^ SurfacePath.to_string constraint_.type_name
            ^ " = "
            ^ string_of_ty constraint_.manifest)
        |> String.concat ""
      in
      "(module " ^ SurfacePath.to_string package.module_type ^ constraints ^ ")"
  | TVar { var = Unbound (id, _) } -> "'_" ^ Int.to_string id
  | TVar { var = Generic id } -> "'a" ^ Int.to_string id
  | TVar { var = Link linked_ty } -> string_of_ty linked_ty

and render_poly_variant_payload = fun ty ->
  match prune ty with
  | TArrow _ -> "(" ^ string_of_ty ty ^ ")"
  | _ -> string_of_ty ty

and render_poly_variant_field = fun field ->
  match field.payload with
  | None -> "`" ^ field.tag
  | Some payload -> "`" ^ field.tag ^ " of " ^ render_poly_variant_payload payload

and render_poly_variant_tags = fun tags ->
  tags
  |> normalized_poly_variant_tags
  |> List.map ~fn:render_poly_variant_field
  |> String.concat " | "

exception Occurs

let arg_label_equal = fun left right ->
  match (left, right) with
  | (NoLabel, NoLabel) -> true
  | (Labelled left, Labelled right)
  | (Optional left, Optional right) -> String.equal left right
  | _ -> false

let row_tags_seen = fun seen tags -> List.exists (fun other_tags -> Ptr.equal other_tags tags) !seen

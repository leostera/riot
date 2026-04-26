open Std
open Std.Collections
open Core_types

(* Occurs check plus level adjustment.

   When a variable at [level] is linked to a type containing younger variables,
   those younger variables must be lowered to [level]. That preserves the
   generalization invariant: a binding may only quantify variables created
   deeper than the surrounding environment. Shared polymorphic-variant rows can
   be cyclic through links, so row objects are tracked by identity while walking.
*)

let rec occurs_adjust_levels = fun id level ty ->
  let seen_rows = ref [] in
  let rec loop ty =
    match prune ty with
    | TVar ({ var = Unbound (other_id, other_level) } as cell) ->
        if Int.equal id other_id then
          raise Occurs;
        if other_level > level then
          cell.var <- Unbound (other_id, level)
    | TVar { var = Generic _ } -> ()
    | TList element -> loop element
    | TOption element -> loop element
    | TTuple elements -> List.for_each elements ~fn:loop
    | TArrow (_, parameter, result) ->
        loop parameter;
        loop result
    | TCon (_, arguments) -> List.for_each arguments ~fn:loop
    | TPolyVariant (_, tags) ->
        if not (row_tags_seen seen_rows tags) then (
          seen_rows := tags :: !seen_rows;
          tags.tags
          |> List.for_each ~fn:(fun field -> Option.for_each field.payload ~fn:loop)
        )
    | TPackage package ->
        package.constraints
        |> List.for_each ~fn:(fun constraint_ -> loop constraint_.manifest)
    | TInt
    | TBool
    | TChar
    | TString
    | TFloat
    | TUnit -> ()
    | TVar { var = Link linked_ty } -> loop linked_ty
  in
  loop ty

let rec ty_mentions_unbound_var = fun id ty ->
  let seen_rows = ref [] in
  let rec loop ty =
    match prune ty with
    | TVar { var = Unbound (other_id, _) } -> Int.equal id other_id
    | TVar { var = Generic _ } -> false
    | TList element
    | TOption element -> loop element
    | TTuple elements -> List.exists loop elements
    | TArrow (_, parameter, result) -> loop parameter || loop result
    | TCon (_, arguments) -> List.exists loop arguments
    | TPackage package ->
        List.exists (fun constraint_ -> loop constraint_.manifest) package.constraints
    | TPolyVariant (_, tags) ->
        if row_tags_seen seen_rows tags then
          false
        else (
          seen_rows := tags :: !seen_rows;
          List.exists
            (fun field ->
              Option.map field.payload ~fn:loop
              |> Option.unwrap_or ~default:false)
            tags.tags
        )
    | TInt
    | TBool
    | TChar
    | TString
    | TFloat
    | TUnit -> false
    | TVar { var = Link linked_ty } -> loop linked_ty
  in
  loop ty

let supports_recursive_occurrence = fun ty ->
  match prune ty with
  | TPolyVariant _ -> true
  | _ -> false

let resolve_type_manifest = fun state path ->
  match List.find
    state.type_manifests
    ~fn:(fun (manifest_path, _) -> SurfacePath.equal manifest_path path) with
  | Some (_, manifest) -> Some manifest
  | None -> None

let has_type_manifest = fun state path -> Option.is_some (resolve_type_manifest state path)

(* Unification is intentionally permissive around manifest aliases.

   For source compatibility with OCaml interfaces, exported named aliases should
   often survive in public types even when their manifest is known. The special
   [TCon(path, [])] cases use manifests to check compatibility, but prefer to
   keep the named constructor linked when doing so is sound.
*)

let rec unify = fun state ~at left right ->
  match (left, right) with
  | (TVar ({ var = Link linked_ty } as cell), TCon (path, [])) when has_type_manifest state path -> (
      match resolve_type_manifest state path with
      | Some manifest ->
          unify state ~at linked_ty manifest;
          cell.var <- Link (TCon (path, []))
      | None -> ()
    )
  | (TCon (path, []), TVar ({ var = Link linked_ty } as cell)) when has_type_manifest state path -> (
      match resolve_type_manifest state path with
      | Some manifest ->
          unify state ~at linked_ty manifest;
          cell.var <- Link (TCon (path, []))
      | None -> ()
    )
  | _ -> unify_pruned state ~at left right

and unify_pruned = fun state ~at left right ->
  match (prune left, prune right) with
  | (TVar left_cell, TVar right_cell) when Ptr.equal left_cell right_cell -> ()
  | (TInt, TInt)
  | (TBool, TBool)
  | (TChar, TChar)
  | (TString, TString)
  | (TFloat, TFloat)
  | (TUnit, TUnit) -> ()
  | (TList left, TList right) -> unify state ~at left right
  | (TOption left, TOption right) -> unify state ~at left right
  | (TTuple left, TTuple right) ->
      if Int.equal (List.length left) (List.length right) then
        List.zip left right
        |> List.for_each ~fn:(fun (left, right) -> unify state ~at left right)
      else
        add_diagnostic
          state
          (unsupported_type
            at
            ("tuple arity mismatch: expected "
            ^ Int.to_string (List.length left)
            ^ " but got "
            ^ Int.to_string (List.length right)))
  | (
    TArrow (left_label, left_parameter, left_result),
    TArrow (right_label, right_parameter, right_result)
  ) when arg_label_equal left_label right_label ->
      unify state ~at left_parameter right_parameter;
      unify state ~at left_result right_result
  | (TCon (left_path, left_arguments), TCon (right_path, right_arguments)) when SurfacePath.equal
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
  | (TPolyVariant (left_bound, left_tags), TPolyVariant (right_bound, right_tags)) ->
      (* Row objects are mutable and may be shared by multiple occurrences of
         the same row variable. Merging updates both sides for compatible open
         rows so later constraints see the accumulated tag set.
      *)
      let left = normalized_poly_variant_tags left_tags.tags in
      let right = normalized_poly_variant_tags right_tags.tags in
      let unify_payloads left right =
        left
        |> List.for_each
          ~fn:(fun left_field ->
            match find_poly_variant_field left_field.tag right with
            | None -> ()
            | Some right_field -> (
                match (left_field.payload, right_field.payload) with
                | (Some left_payload, Some right_payload) ->
                    unify state ~at left_payload right_payload
                | (None, None) -> ()
                | (Some _, None)
                | (None, Some _) ->
                    add_diagnostic
                      state
                      (unsupported_type
                        at
                        ("polymorphic variant payload mismatch for `" ^ left_field.tag))
              ))
      in
      let merged =
        right
        |> List.fold_left
          ~init:left
          ~fn:(fun fields right_field ->
            match find_poly_variant_field right_field.tag fields with
            | None -> right_field :: fields
            | Some left_field ->
                unify_payloads [ left_field ] [ right_field ];
                fields)
        |> normalized_poly_variant_tags
      in
      let ok =
        match (left_bound, right_bound) with
        | (Upper, Upper)
        | (Lower, Lower) ->
            left_tags.tags <- merged;
            right_tags.tags <- merged;
            true
        | (Exact, Exact) -> same_poly_variant_tags left right
        | (Upper, Lower) -> poly_variant_tags_subset right left
        | (Lower, Upper) -> poly_variant_tags_subset left right
        | (Exact, Upper) -> poly_variant_tags_subset left right
        | (Upper, Exact) -> poly_variant_tags_subset right left
        | (Exact, Lower) -> poly_variant_tags_subset right left
        | (Lower, Exact) -> poly_variant_tags_subset left right
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
  | (TPackage left, TPackage right) -> unify_package state ~at left right
  | (TVar ({ var = Unbound (id, _) } as cell), TCon (path, [])) when has_type_manifest state path -> (
      match resolve_type_manifest state path with
      | Some manifest when ty_mentions_unbound_var id manifest -> ()
      | Some _
      | None -> cell.var <- Link (TCon (path, []))
    )
  | (TCon (path, []), TVar ({ var = Unbound (id, _) } as cell)) when has_type_manifest state path -> (
      match resolve_type_manifest state path with
      | Some manifest when ty_mentions_unbound_var id manifest -> ()
      | Some _
      | None -> cell.var <- Link (TCon (path, []))
    )
  | (TCon (path, []), other) when has_type_manifest state path -> (
      match resolve_type_manifest state path with
      | Some manifest -> unify state ~at manifest other
      | None -> ()
    )
  | (other, TCon (path, [])) when has_type_manifest state path -> (
      match resolve_type_manifest state path with
      | Some manifest -> unify state ~at other manifest
      | None -> ()
    )
  | (TVar ({ var = Unbound (id, level) } as cell), ty)
  | (ty, TVar ({ var = Unbound (id, level) } as cell)) -> (
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
  | (TVar { var = Generic _ }, _)
  | (_, TVar { var = Generic _ }) ->
      add_diagnostic state (unsupported_type at "unexpected generic type variable")
  | (left, right) ->
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
  left.constraints
  |> List.for_each
    ~fn:(fun left_constraint ->
      match List.find
        right.constraints
        ~fn:(fun right_constraint ->
          SurfacePath.equal
            left_constraint.type_name
            right_constraint.type_name) with
      | Some right_constraint -> unify state ~at left_constraint.manifest right_constraint.manifest
      | None -> ())

and coerce = fun state ~at source target ->
  (* Coercion differs from unification only where the language has directional
     pressure today: polymorphic variants. Everything else falls back to normal
     equality-style unification after manifest expansion.
  *)
  match (prune source, prune target) with
  | (TCon (source_path, []), TCon (target_path, [])) when SurfacePath.equal source_path target_path ->
      ()
  | (TCon (path, []), target) when has_type_manifest state path -> (
      match resolve_type_manifest state path with
      | Some manifest -> coerce state ~at manifest target
      | None -> ()
    )
  | (source, TCon (path, [])) when has_type_manifest state path -> (
      match resolve_type_manifest state path with
      | Some manifest -> coerce state ~at source manifest
      | None -> ()
    )
  | (TPolyVariant (_, source_tags), TPolyVariant (_, target_tags)) ->
      coerce_poly_variant state ~at source_tags target_tags
  | (source, target) -> unify state ~at source target

and coerce_poly_variant = fun state ~at source_tags target_tags ->
  let source = normalized_poly_variant_tags source_tags.tags in
  let target = normalized_poly_variant_tags target_tags.tags in
  if poly_variant_tags_subset source target then
    source
    |> List.for_each
      ~fn:(fun source_field ->
        match find_poly_variant_field source_field.tag target with
        | None -> ()
        | Some target_field -> (
            match (source_field.payload, target_field.payload) with
            | (Some source_payload, Some target_payload) ->
                unify state ~at source_payload target_payload
            | (None, None) -> ()
            | (Some _, None)
            | (None, Some _) ->
                add_diagnostic
                  state
                  (unsupported_type
                    at
                    ("polymorphic variant payload mismatch for `" ^ source_field.tag))
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
  (* Turn all variables born below [level] into generic variables. Polymorphic
     variant rows are copied instead of reused so a generalized scheme cannot be
     mutated by a later instantiation.
  *)
  let poly_variant_copies = ref [] in
  let rec loop ty =
    match prune ty with
    | TVar ({ var = Unbound (id, other_level) } as cell) when other_level > level ->
        cell.var <- Generic id;
        TVar cell
    | TList element -> TList (loop element)
    | TOption element -> TOption (loop element)
    | TTuple elements -> TTuple (List.map elements ~fn:loop)
    | TArrow (label, parameter, result) -> TArrow (label, loop parameter, loop result)
    | TCon (path, arguments) -> TCon (path, List.map arguments ~fn:loop)
    | TPolyVariant (bound, tags) ->
        TPolyVariant (bound, copy_poly_variant_tags poly_variant_copies ~map_payload:loop tags)
    | TPackage package ->
        TPackage {
          package with
          constraints =
            package.constraints
            |> List.map
              ~fn:(fun constraint_ -> { constraint_ with manifest = loop constraint_.manifest });
        }
    | ty -> ty
  in
  loop ty

let instantiate = fun state ~level ty ->
  (* Every use of a generalized binding receives fresh flexible variables.
     Shared rows are copied consistently within the instantiation so aliases in
     a single scheme remain aliases after freshening.
  *)
  let subst = ref [] in
  let poly_variant_copies = ref [] in
  let rec loop ty =
    match prune ty with
    | TVar { var = Generic id } -> (
        match List.find !subst ~fn:(fun (other_id, _) -> Int.equal id other_id) with
        | Some (_, replacement) -> replacement
        | None ->
            let replacement = fresh_tyvar state ~level in
            subst := (id, replacement) :: !subst;
            replacement
      )
    | TList element -> TList (loop element)
    | TOption element -> TOption (loop element)
    | TTuple elements -> TTuple (List.map elements ~fn:loop)
    | TArrow (label, parameter, result) -> TArrow (label, loop parameter, loop result)
    | TCon (path, arguments) -> TCon (path, List.map arguments ~fn:loop)
    | TPolyVariant (bound, tags) ->
        TPolyVariant (bound, copy_poly_variant_tags poly_variant_copies ~map_payload:loop tags)
    | TPackage package ->
        TPackage {
          package with
          constraints =
            package.constraints
            |> List.map
              ~fn:(fun constraint_ -> { constraint_ with manifest = loop constraint_.manifest });
        }
    | ty -> ty
  in
  loop ty

let instantiate_pair = fun state ~level left right ->
  let subst = ref [] in
  let poly_variant_copies = ref [] in
  let rec loop ty =
    match prune ty with
    | TVar { var = Generic id } -> (
        match List.find !subst ~fn:(fun (other_id, _) -> Int.equal id other_id) with
        | Some (_, replacement) -> replacement
        | None ->
            let replacement = fresh_tyvar state ~level in
            subst := (id, replacement) :: !subst;
            replacement
      )
    | TList element -> TList (loop element)
    | TOption element -> TOption (loop element)
    | TTuple elements -> TTuple (List.map elements ~fn:loop)
    | TArrow (label, parameter, result) -> TArrow (label, loop parameter, loop result)
    | TCon (path, arguments) -> TCon (path, List.map arguments ~fn:loop)
    | TPolyVariant (bound, tags) ->
        TPolyVariant (bound, copy_poly_variant_tags poly_variant_copies ~map_payload:loop tags)
    | TPackage package ->
        TPackage {
          package with
          constraints =
            package.constraints
            |> List.map
              ~fn:(fun constraint_ -> { constraint_ with manifest = loop constraint_.manifest });
        }
    | ty -> ty
  in
  (loop left, loop right)

open Std
open Std.Collections
open Core_types

type row_alias = {
  row_tags: poly_variant_tags;
  row_alias_id: int;
  mutable row_alias_emitted: bool;
}

let shared_poly_variant_row_aliases = fun ty ->
  (* Public rendering needs to preserve shared row identity. If the same row
     object appears more than once in a type, expose the first occurrence as an
     alias and later occurrences as the alias variable. *)
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
  (* Convert internal mutable types into the immutable public type language.
     [vars] assigns stable, dense public ids in occurrence order; those ids are
     local to the returned scheme. *)
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
  | NoLabel -> Typing_context.NoLabel
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
  (* Importing a caller-provided public context goes through [Generic] variables.
     Later lookup instantiates those generics, so values from previous checks
     keep normal HM polymorphism. *)
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
  | Typing_context.NoLabel -> NoLabel
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

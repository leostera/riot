open Std
open Ast

module HashMap = Std.Collections.HashMap
module SurfacePath = Model.Surface_path
module InferenceEnv = State.InferenceEnv

let surface_path parts =
  SurfacePath.from_parts parts
  |> Result.expect ~msg:"type path must contain at least one segment"

let is_builtin_type name =
  List.any
    [ "int"; "bool"; "float"; "char"; "string"; "unit"; "exn"; "list"; "array"; "ref"; "option"; "result" ]
    ~fn:(String.equal name)

let unresolved_type state (annotation: core_type) ident =
  State.add_diagnostic
    state
    (Diagnostics.Diagnostic.unresolved_name
      ~span:annotation.origin.span
      ~namespace:"type"
      ~name:(SurfacePath.to_string ident))

let resolve_type_identifier state (annotation: core_type) ident =
  match State.get_type state ~name:ident with
  | Some declaration -> declaration.name
  | None -> (
      let resolved =
        match List.rev (SurfacePath.to_segments ident) with
        | [] -> None
        | [ name ] when is_builtin_type name -> Some ident
        | [ _ ] -> None
        | type_name :: reversed_module_path ->
            let module_path = reversed_module_path |> List.rev |> surface_path in
            let type_name = surface_path [ type_name ] in
            (match State.get_module_path state ~path:module_path with
            | None -> None
            | Some summary ->
                Option.map
                  (InferenceEnv.module_get_type summary ~name:type_name)
                  ~fn:(fun declaration -> declaration.name))
      in
      match resolved with
      | Some ident -> ident
      | None ->
          unresolved_type state annotation ident;
          ident
    )

let unsupported_type state (annotation: core_type) summary =
  State.add_diagnostic
    state
    (Diagnostics.Diagnostic.UnsupportedType {
      span = annotation.origin.span;
      summary;
    })

let rec core_type_variables (annotation: core_type) =
  let variables_of_types types =
    List.fold_left
      types
      ~init:[]
      ~fn:(fun variables type_ ->
        List.fold_left
          (core_type_variables type_)
          ~init:variables
          ~fn:(fun variables name ->
            if List.any variables ~fn:(String.equal name) then variables
            else List.append variables [ name ]))
  in
  match annotation.kind with
  | Var (Some name) -> [ name ]
  | Apply { constructor; arguments } -> variables_of_types (constructor :: arguments)
  | Arrow { parameter; result; _ } -> variables_of_types [ parameter; result ]
  | Tuple parts -> variables_of_types parts
  | ForAll { parameters; body } ->
      List.fold_left
        (core_type_variables body)
        ~init:parameters
        ~fn:(fun variables name ->
          if List.any variables ~fn:(String.equal name) then variables
          else List.append variables [ name ])
  | PolyVariant fields ->
      fields
      |> List.filter_map ~fn:(fun (field: poly_variant_type_field) -> field.payload)
      |> variables_of_types
  | Package { constraints; _ } ->
      constraints
      |> List.map ~fn:(fun constraint_ -> constraint_.manifest)
      |> variables_of_types
  | Parenthesized inner -> core_type_variables inner
  | Wildcard
  | Var None
  | TypeIdent _ -> []

(**
   Source arrow labels and solver arrow labels are intentionally separate
   types. The source shape comes from `Typ.Ast.core_type`; the solver shape is
   what unification compares. This conversion is pure because labels carry no
   source-local state after lowering.
*)
let arrow_label_to_type_label = fun __tmp1 ->
  match __tmp1 with
  | NoLabel -> Type.Label.NoLabel
  | Labelled label -> Type.Label.Labelled label
  | Optional label -> Type.Label.Optional label

let type_constructor_arity state ident =
  match State.get_type state ~name:ident with
  | Some declaration -> Some (List.length declaration.parameters)
  | None -> (
      match List.rev (SurfacePath.to_segments ident) with
      | [ name ] when is_builtin_type name ->
          (match name with
          | "list" | "array" | "ref" | "option" -> Some 1
          | "result" -> Some 2
          | _ -> Some 0)
      | _ -> None
    )

let validate_type_constructor_arity state (annotation: core_type) ident actual =
  match type_constructor_arity state ident with
  | Some expected when not (Int.equal expected actual) ->
      State.add_diagnostic
        state
        (Diagnostics.Diagnostic.type_constructor_arity_mismatch
          ~span:annotation.origin.span
          ~name:(SurfacePath.to_string ident)
          ~expected
          ~actual)
  | Some _
  | None -> ()

(**
   Convert a source type expression into the solver algebra.

   The function also writes the converted type back onto the `core_type` node.
   That mutation is the same annotation strategy used for expressions and
   patterns: later diagnostics, LSP hovers, and tests can inspect the typed tree
   without rebuilding the conversion. Constructor positions defer arity checks
   until all source type arguments have been applied.
*)
let rec core_type_to_type_internal (state: State.t) ~as_constructor (annotation: core_type) =
  let type_ =
    match annotation.kind with
    (* A bare type name such as `int` or `Result.t`. *)
    | TypeIdent ident ->
        Type.Apply { ident = resolve_type_identifier state annotation ident; arguments = [] }
    | Apply { constructor; arguments } -> (
        (*
           Type application is represented source-first as "constructor plus
           arguments". If the constructor itself lowers to a nominal type, append
           the new arguments. This handles both `int list` and curried-looking
           type application shapes without introducing a separate solver node.
        *)
        let constructor = core_type_to_type_internal state ~as_constructor:true constructor in
        let arguments =
          List.map arguments ~fn:(core_type_to_type_internal state ~as_constructor:false)
        in
        match Unifier.resolve constructor with
        | Type.Apply { ident; arguments = existing_arguments } ->
            Type.Apply { ident; arguments = List.append existing_arguments arguments }
        | _ ->
            unsupported_type state annotation "type application to a non-nominal type";
            State.fresh_var state
      )
    | Arrow { label; parameter; result } ->
        Type.Arrow {
          label = arrow_label_to_type_label label;
          parameter = core_type_to_type_internal state ~as_constructor:false parameter;
          result = core_type_to_type_internal state ~as_constructor:false result;
        }

    (* Tuple syntax is already semantic enough for the current solver. *)
    | Tuple parts ->
        Type.Tuple (List.map parts ~fn:(core_type_to_type_internal state ~as_constructor:false))
    | Parenthesized inner ->
        (*
           Parentheses are syntax-only at this stage. They may still exist in
           the typed tree because the tree is source-shaped, but the solver
           should not see a separate parenthesized type node.
        *)
        core_type_to_type_internal state ~as_constructor inner
    | ForAll { body = inner; _ } ->
        (*
           TODO(@leostera): model explicit quantifier scope. For now we type
           the body so annotated examples can continue through the checker;
           real scoped quantification belongs here when that feature slice
           lands.
        *)
        core_type_to_type_internal state ~as_constructor:false inner
    | Var (Some name) -> (
        (*
           Type variables inside a declaration first consult the active
           declaration parameter scope. If the name was not declared, use a fresh
           hole so the checker can continue and report later constraints.
        *)
        match State.get_type_param state ~name with
        | Some type_ -> type_
        | None ->
            State.add_diagnostic
              state
              (Diagnostics.Diagnostic.unresolved_name
                ~span:annotation.origin.span
                ~namespace:"type variable"
                ~name);
            State.fresh_var state
      )
    | Var None -> State.fresh_var state
    | Wildcard -> State.fresh_var state
    | PolyVariant _ ->
        unsupported_type state annotation "polymorphic variant types are not supported yet";
        State.fresh_var state
    | Package _ ->
        unsupported_type state annotation "first-class module package types are not supported yet";
        State.fresh_var state
  in
  annotation.type_ <- Some type_;
  (match (as_constructor, Unifier.resolve type_) with
  | (false, Type.Apply { ident; arguments }) ->
      validate_type_constructor_arity state annotation ident (List.length arguments)
  | _ -> ());
  type_

let core_type_to_type state annotation =
  core_type_to_type_internal state ~as_constructor:false annotation

(**
   Convert a source annotation with one shared solver variable per named type
   variable. This preserves equations such as `'a -> 'a` while leaving the
   resulting variables available for the surrounding let/signature rule to
   generalize or constrain.
*)
let core_type_to_type_scoped state annotation =
  let names = core_type_variables annotation in
  let scope = HashMap.with_capacity ~size:(List.length names) in
  List.for_each names ~fn:(fun name ->
    let type_ = State.fresh_var state in
    let _ = HashMap.insert scope ~key:name ~value:type_ in
    ());
  State.with_type_params
    state
    scope
    (fun state -> core_type_to_type state annotation)

let type_declaration_to_type ?(arguments = []) (decl: type_declaration) =
  Type.Apply { ident = decl.name; arguments }

(**
   Allocate one fresh solver variable per declared type parameter.

   The map is installed only while converting the declaration that introduced
   those parameters. This makes `type 'a t = 'a list` preserve the same variable
   for both uses of `'a`, while avoiding a global table of type parameters.
*)
let fresh_type_parameters state parameters =
  let scope = HashMap.with_capacity ~size:(List.length parameters) in
  let arguments =
    List.map
      parameters
      ~fn:(fun param ->
        match param with
        | Some name ->
            let type_ = State.fresh_var state in
            let _ = HashMap.insert scope ~key:name ~value:type_ in
            type_
        | None -> State.fresh_var state)
  in
  (scope, arguments)

(**
   Record field lookup must instantiate the owning record declaration at the
   lookup site.

   For `type 'a box = { value : 'a }`, every use of `value` needs a fresh owner
   type `'x box` and field type `'x`; otherwise two unrelated field accesses
   would accidentally share the same solver variable.
*)
let instantiate_record_field state (info: State.InferenceEnv.record_field_info) =
  let (scope, arguments) = fresh_type_parameters state info.owner.parameters in
  State.with_type_params
    state
    scope
    (fun state ->
      let owner_type = type_declaration_to_type ~arguments info.owner in
      let field_type = core_type_to_type state info.field.type_annotation in
      (owner_type, field_type))

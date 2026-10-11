open Std
open Ast
open TypeScheme

module HashMap = Std.Collections.HashMap
module InferenceEnv = State.InferenceEnv
module SurfacePath = Model.Surface_path

(**
   Inline-record constructors behave as if they own a hidden record payload
   type. The hidden name is deterministic and scoped under the parent type name
   so snapshots stay stable and future summaries can serialize it.
*)
let inline_record_payload_ident (decl: type_declaration) (ctr: type_constructor) =
  let owner = SurfacePath.to_segments decl.name in
  let constructor = SurfacePath.to_string ctr.name in
  SurfacePath.from_parts (owner @ [ "$" ^ constructor ])
  |> Result.expect ~msg:"inline record constructor payload paths are non-empty"

(**
   Constructor descriptions store generalized shapes in the environment. We only
   need the generalized body here because the description itself is not a value
   scheme; it has separate slots for result type and argument metadata.
*)
let generalize_type type_ = (Quantifier.generalize_all type_).body

(**
   Generalize constructor argument metadata in lockstep with the callable
   constructor scheme. This keeps inline-record field types reusable without
   leaking solver variables allocated during type registration.
*)
let generalize_constructor_arguments arguments =
  match arguments with
  | InferenceEnv.Tuple arguments -> InferenceEnv.Tuple (List.map arguments ~fn:generalize_type)
  | InferenceEnv.InlineRecord inline_record ->
      InferenceEnv.InlineRecord {
        inline_record with
        payload_type = generalize_type inline_record.payload_type;
        fields = List.map
          inline_record.fields
          ~fn:(fun (field: InferenceEnv.inline_record_field) -> {
            field with
            type_ = generalize_type field.type_;
          });
      }

(**
   Instantiate a constructor description at each use site.

   The description may contain `Type.Generic n` variables from registration.
   Every occurrence of the same generic id must become the same fresh variable
   for this use, while different uses of the constructor must not share those
   variables. The `substitutions` table is the per-instantiation memo that gives
   us both properties.
*)
let instantiate state (description: InferenceEnv.constructor_description) =
  let substitutions = HashMap.with_capacity ~size:8 in
  let fresh_for_generic id =
    match HashMap.get substitutions ~key:id with
    | Some type_ -> type_
    | None ->
        let type_ = State.fresh_var state in
        let _ = HashMap.insert substitutions ~key:id ~value:type_ in
        type_
  in
  let rec instantiate_type type_ =
    match Unifier.resolve type_ with
    (*
       Generics are the variables that were generalized when the constructor was
       registered. Each generic id gets one fresh variable for this
       instantiation, memoized by `fresh_for_generic`.
    *)
    | Type.Generic id -> fresh_for_generic id

    (*
       Plain solver variables are already local to this checking run. Do not
       clone them here; cloning would hide constraints already learned before
       the constructor description was read.
    *)
    | Type.Var _ as type_ -> type_
    | Type.Tuple parts -> Type.Tuple (List.map parts ~fn:instantiate_type)
    | Type.Arrow arrow ->
        Type.Arrow {
          arrow with
          parameter = instantiate_type arrow.parameter;
          result = instantiate_type arrow.result;
        }
    | Type.Apply application ->
        Type.Apply {
          application with
          arguments = List.map application.arguments ~fn:instantiate_type;
        }
  in
  let instantiate_arguments arguments =
    match arguments with
    | InferenceEnv.Tuple arguments -> InferenceEnv.Tuple (List.map arguments ~fn:instantiate_type)
    | InferenceEnv.InlineRecord inline_record ->
        InferenceEnv.InlineRecord {
          inline_record with
          payload_type = instantiate_type inline_record.payload_type;
          fields = List.map
            inline_record.fields
            ~fn:(fun (field: InferenceEnv.inline_record_field) -> {
              field with
              type_ = instantiate_type field.type_;
            });
        }
  in
  {
    description with
    scheme = TypeScheme.monomorphic (instantiate_type description.scheme.body);
    result = instantiate_type description.result;
    arguments = instantiate_arguments description.arguments;
  }

let builtin_constructor ident =
  let first_id = TypeVar.first in
  let second_id = TypeVar.next first_id in
  let first = Type.Generic first_id in
  let second = Type.Generic second_id in
  let description ~result ~arguments body: InferenceEnv.constructor_description = {
    name = ident;
    scheme = TypeScheme.monomorphic body;
    result;
    arguments = InferenceEnv.Tuple arguments;
  }
  in
  if SurfacePath.equal ident Builtin.none_ident then
    Some (description ~result:(Builtin.option first) ~arguments:[] (Builtin.option first))
  else if SurfacePath.equal ident Builtin.some_ident then
    Some (
      description
        ~result:(Builtin.option first)
        ~arguments:[ first ]
        (Ast.Type.arrow first (Builtin.option first)))
  else if SurfacePath.equal ident Builtin.ok_ident then
    Some (
      description
        ~result:(Builtin.result first second)
        ~arguments:[ first ]
        (Ast.Type.arrow first (Builtin.result first second)))
  else if SurfacePath.equal ident Builtin.error_ident then
    Some (
      description
        ~result:(Builtin.result first second)
        ~arguments:[ second ]
        (Ast.Type.arrow second (Builtin.result first second)))
  else
    None

let instantiate_from_state state ident =
  match State.get_constructor state ~name:ident with
  | Some constructor -> Some (instantiate state constructor)
  | None -> Option.map (builtin_constructor ident) ~fn:(instantiate state)

(**
   Build the environment payload for one constructor declaration.

   Registration happens while type parameters for the owning type are in scope.
   The resulting constructor has three related views:

   - `scheme`: the callable function type of the constructor.
   - `result`: the type produced by the constructor.
   - `arguments`: richer argument metadata used for inline-record constructor
     expressions and patterns.
*)
let from_type_constructor state (decl: type_declaration) (ctr: type_constructor) =
  let (scope, arguments) = TypeExpr.fresh_type_parameters state decl.parameters in
  State.with_type_params
    state
    scope
    (fun state ->
      let result =
        match ctr.result with
        | Some result -> TypeExpr.core_type_to_type state result
        | None -> TypeExpr.type_declaration_to_type ~arguments decl
      in
      let constructor_arguments =
        match ctr.arguments with
        (*
           Ordinary constructor payloads become tuple metadata. A unary payload
           stays a single argument in the callable scheme, while multiple
           payloads are represented as one tuple parameter.
        *)
        | Tuple arguments ->
            InferenceEnv.Tuple (List.map arguments ~fn:(TypeExpr.core_type_to_type state))
        | Record fields ->
            (*
               Inline records are constructor-specific payload records. They do
               not go into the ordinary record-field namespace, but they do need
               a nominal payload type so expressions and patterns can unify
               against a stable owner.
            *)
            let payload_type = Type.Apply {
              ident = inline_record_payload_ident decl ctr;
              arguments;
            }
            in
            let inline_record: InferenceEnv.inline_record = {
              owner = decl;
              constructor = ctr;
              payload_type;
              fields = List.map
                fields
                ~fn:(fun field ->
                  ({
                    declaration = field;
                    type_ = TypeExpr.core_type_to_type state field.type_annotation;
                  }: InferenceEnv.inline_record_field));
            }
            in
            InferenceEnv.InlineRecord inline_record
      in
      let body =
        match constructor_arguments with
        (*
           The callable scheme is what makes constructors usable as expressions.
           `None` has type `'a option`, `Some` has type `'a -> 'a option`, and
           multi-argument constructors consume one tuple-shaped payload.
        *)
        | InferenceEnv.Tuple [] -> result
        | InferenceEnv.Tuple [ argument ] -> Ast.Type.arrow argument result
        | InferenceEnv.Tuple arguments -> Ast.Type.arrow (Type.Tuple arguments) result
        | InferenceEnv.InlineRecord inline_record ->
            Ast.Type.arrow inline_record.payload_type result
      in
      ({
        name = ctr.name;
        scheme = Quantifier.generalize_all body;
        result = generalize_type result;
        arguments = generalize_constructor_arguments constructor_arguments;
      }: InferenceEnv.constructor_description))

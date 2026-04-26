open Std
open Std.Result.Syntax
open Ast

type error =
  | TypeMismatch of {
      expected: Type.t;
      actual: Type.t;
    }
  | InfiniteSubstitution of {
      var: Type.variable;
      type_: Type.t;
    }

let type_mismatch ~expected ~actual = Error (TypeMismatch { expected; actual })

let error_to_string error =
  match error with
  | TypeMismatch { expected; actual } ->
      "Expected " ^ Type.to_string expected ^ " but got " ^ Type.to_string actual
  | InfiniteSubstitution { var; type_ } ->
      "Type variable "
      ^ TypeVar.to_string var.id
      ^ " cannot be substituted with "
      ^ Type.to_string type_

(**
   Resolve a type by following links and updating the final type pointed by the
   link chain at all levels
*)
let rec resolve type_ =
  match type_ with
  | Type.Var ({ link = Some linked; _ } as variable) ->
      let resolved = resolve linked in
      variable.link <- Some resolved;
      resolved
  | _ -> type_

(** Checks if a type variable `var` appears inside a type expression `type_` *)
let rec occurs_in var type_ =
  match resolve type_ with
  | Type.Var other -> Type.same_var var other
  | Tuple elements -> List.any elements ~fn:(occurs_in var)
  | Arrow { parameter; result; _ } -> occurs_in var parameter || occurs_in var result
  | Constructor { arguments; _ } -> List.any arguments ~fn:(occurs_in var)
  | Generic _ -> false

(** Solve a type variable by linking it to a known type *)
let solve_var var type_ =
  match resolve type_ with
  | Type.Var other when Type.same_var var other -> Ok ()
  | resolved when occurs_in var resolved -> Error (InfiniteSubstitution { var; type_ })
  | resolved ->
      var.link <- Some resolved;
      Ok ()

(**
   Attempt to make two `Type.t` values represent the same type. Whenever a
   variable is found on either side, it'll try to be solved against its matching
   type.
*)
let rec unify ~expected ~actual =
  match (resolve expected, resolve actual) with
  | (Generic a, Generic b) when TypeVar.equal a b -> Ok ()
  | (Var a, Var b) when Type.same_var a b -> Ok ()
  | (Var var, type_)
  | (type_, Var var) -> solve_var var type_
  | (Tuple a, Tuple b) -> unify_many a b
  | (Arrow a, Arrow b) -> unify_arrow a b
  | (Constructor a, Constructor b) -> unify_constructors a b
  | (expected, actual) -> type_mismatch ~expected ~actual

and unify_many expected actual =
  let expected_len = List.length expected in
  let actual_len = List.length actual in
  if Int.(expected_len != actual_len) then
    type_mismatch ~expected:(Type.Tuple expected) ~actual:(Type.Tuple actual)
  else
    List.zip expected actual
    |> List.fold_left
      ~init:(Ok ())
      ~fn:(fun acc (expected, actual) ->
        let* acc = acc in
        unify ~expected ~actual)

and unify_arrow expected actual =
  if not (Type.Label.equal expected.label actual.label) then
    type_mismatch ~expected:(Type.Arrow expected) ~actual:(Type.Arrow actual)
  else
    let* () = unify ~expected:expected.parameter ~actual:actual.parameter in
    unify ~expected:expected.result ~actual:actual.result

and unify_constructors expected actual =
  let same_constructor = Model.Surface_path.equal expected.ident actual.ident in
  let same_args_length = Int.(List.length expected.arguments = List.length actual.arguments) in
  if same_constructor && same_args_length then
    unify_many expected.arguments actual.arguments
  else
    type_mismatch ~expected:(Type.Constructor expected) ~actual:(Type.Constructor actual)

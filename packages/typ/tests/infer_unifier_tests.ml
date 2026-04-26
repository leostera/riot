open Std
open Std.Result.Syntax

module State = Typ.Infer.State
module Unifier = Typ.Infer.Unifier
module Type = Typ.Ast.Type
module SurfacePath = Typ.Model.Surface_path

let mk_path name = SurfacePath.from_name name

let int_type = Type.Constructor { ident = mk_path "int"; arguments = [] }

let bool_type = Type.Constructor { ident = mk_path "bool"; arguments = [] }

let arrow ?(label = Type.Label.NoLabel) parameter result = Type.Arrow { label; parameter; result }

let fresh_variable state =
  match State.fresh_var state with
  | Type.Var variable -> Ok variable
  | _ -> Error "State.fresh_var returned non-variable"

let type_var variable = Type.Var variable

let assert_type expected type_ =
  let resolved = Unifier.resolve type_ in
  if Type.equal resolved expected then
    Ok ()
  else
    Error ("expected " ^ Type.to_string expected ^ " but found " ^ Type.to_string resolved)

let assert_type_is_int type_ = assert_type int_type type_

let assert_type_is_bool type_ = assert_type bool_type type_

let assert_type_mismatch result =
  match result with
  | Error (Unifier.TypeMismatch _) -> Ok ()
  | Error error -> Error ("expected type mismatch, got " ^ Unifier.error_to_string error)
  | Ok () -> Error "expected type mismatch"

let test_resolve_follows_links _ctx =
  let state = State.create () in
  let* a = fresh_variable state in
  a.link <- Some int_type;
  assert_type_is_int (type_var a)

let test_resolve_compresses_links _ctx =
  let state = State.create () in
  let* a = fresh_variable state in
  let* b = fresh_variable state in
  let* c = fresh_variable state in
  let* d = fresh_variable state in
  a.link <- Some (type_var b);
  b.link <- Some (type_var c);
  c.link <- Some (type_var d);
  d.link <- Some int_type;
  let _ = Unifier.resolve (type_var a) in
  match (a.link, b.link, c.link, d.link) with
  | (Some a_type, Some b_type, Some c_type, Some d_type) ->
      let* () = assert_type_is_int a_type in
      let* () = assert_type_is_int b_type in
      let* () = assert_type_is_int c_type in
      assert_type_is_int d_type
  | _ -> Error "expected both links to be compressed"

let test_solve_var_links_unsolved_variable _ctx =
  let state = State.create () in
  let* a = fresh_variable state in
  let* _ =
    Unifier.solve_var a bool_type
    |> Result.map_err ~fn:Unifier.error_to_string
  in
  match a.link with
  | Some linked when Type.equal (Unifier.resolve linked) bool_type -> Ok ()
  | Some _ -> Error "expected variable to link to bool"
  | None -> Error "expected variable to be linked"

let test_solve_var_accepts_self _ctx =
  let state = State.create () in
  let* a = fresh_variable state in
  Unifier.solve_var a (type_var a)
  |> Result.map ~fn:(fun _ -> ())
  |> Result.map_err ~fn:Unifier.error_to_string

let test_solve_var_rejects_infinite_type _ctx =
  let state = State.create () in
  let* a = fresh_variable state in
  let infinite = arrow (type_var a) int_type in
  match Unifier.solve_var a infinite with
  | Error (Unifier.InfiniteSubstitution _) -> Ok ()
  | _ -> Error "expected infinite type to be rejected"

let test_unify_same_constructor _ctx =
  Unifier.unify ~expected:int_type ~actual:int_type
  |> Result.map_err ~fn:Unifier.error_to_string

let test_unify_constructor_mismatch _ctx =
  Unifier.unify ~expected:int_type ~actual:bool_type
  |> assert_type_mismatch

let test_unify_var_with_constructor _ctx =
  let state = State.create () in
  let* a = fresh_variable state in
  let* () =
    Unifier.unify ~expected:(type_var a) ~actual:int_type
    |> Result.map_err ~fn:Unifier.error_to_string
  in
  assert_type_is_int (type_var a)

let test_unify_tuple_links_nested_var _ctx =
  let state = State.create () in
  let* a = fresh_variable state in
  let expected = Type.Tuple [ type_var a; bool_type ] in
  let actual = Type.Tuple [ int_type; bool_type ] in
  let* () =
    Unifier.unify ~expected ~actual
    |> Result.map_err ~fn:Unifier.error_to_string
  in
  assert_type_is_int (type_var a)

let test_unify_tuple_crosslinks_nested_vars _ctx =
  let state = State.create () in
  let* a = fresh_variable state in
  let* b = fresh_variable state in
  let expected = Type.Tuple [ type_var a; bool_type ] in
  let actual = Type.Tuple [ int_type; type_var b ] in
  let* () =
    Unifier.unify ~expected ~actual
    |> Result.map_err ~fn:Unifier.error_to_string
  in
  let* () = assert_type int_type (type_var a) in
  let* () = assert_type bool_type (type_var b) in
  Ok ()

let test_unify_tuple_arity_mismatch _ctx =
  let expected = Type.Tuple [ int_type ] in
  let actual = Type.Tuple [ int_type; bool_type ] in
  Unifier.unify ~expected ~actual
  |> assert_type_mismatch

let test_unify_arrow_links_parameter_and_result _ctx =
  let state = State.create () in
  let* a = fresh_variable state in
  let* b = fresh_variable state in
  let expected = arrow (type_var a) bool_type in
  let actual = arrow int_type (type_var b) in
  let* () =
    Unifier.unify ~expected ~actual
    |> Result.map_err ~fn:Unifier.error_to_string
  in
  let* () = assert_type_is_int (type_var a) in
  assert_type_is_bool (type_var b)

let tests =
  Test.[
    case "resolve follows links" test_resolve_follows_links;
    case "resolve compresses links" test_resolve_compresses_links;
    case "solve_var links unsolved variable" test_solve_var_links_unsolved_variable;
    case "solve_var accepts self" test_solve_var_accepts_self;
    case "solve_var rejects infinite type" test_solve_var_rejects_infinite_type;
    case "unify same constructor" test_unify_same_constructor;
    case "unify constructor mismatch" test_unify_constructor_mismatch;
    case "unify var with constructor" test_unify_var_with_constructor;
    case "unify tuple links nested var" test_unify_tuple_links_nested_var;
    case "unify tuple arity mismatch" test_unify_tuple_arity_mismatch;
    case "unify arrow links parameter and result" test_unify_arrow_links_parameter_and_result;
  ]

let main ~args = Test.Cli.main ~name:"typ:infer" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()

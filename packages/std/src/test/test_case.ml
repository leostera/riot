open Global

type ctx = Test_context.t

type test_result =
  Pass
  | Fail of string
  | Error of exn

type test_type =
  | UnitTest
  | Property of { examples: int }

type t = {
  name: string;
  test_type: test_type;
  fn: ctx -> (unit, string) result;
  skip: bool;
}

let case = fun name fn -> { name; test_type = UnitTest; fn; skip = false }

let property = fun name ~examples fn -> { name; test_type = Property { examples }; fn; skip = false }

let skip = fun name fn -> { name; test_type = UnitTest; fn; skip = true }

let todo = fun name ->
  { name; test_type = UnitTest; fn = (fun _ctx -> Result.Error "todo"); skip = false }

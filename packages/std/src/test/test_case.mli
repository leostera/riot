open Global

type ctx = Test_context.t

(** The type of test: regular unit test or property test with example count. *)
type test_result =
  | Pass
  | Fail of string
  | Error of exn
type test_type =
  | UnitTest
  | Property of { examples: int }
(** [case name fn] creates a regular unit test. *)
type t = {
  name: string;
  test_type: test_type;
  fn: ctx -> (unit, string) result;
  skip: bool;
}
val case: string -> (ctx -> (unit, string) result) -> t

(** [property name ~examples fn] creates a property test that ran [examples] test cases. *)
val property: string -> examples:int -> (ctx -> (unit, string) result) -> t

(** [skip name fn] creates a skipped test. *)
val skip: string -> (ctx -> (unit, string) result) -> t

(** [todo name] creates a placeholder test marked as todo. *)
val todo: string -> t

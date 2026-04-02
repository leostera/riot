open Global

type ctx = Test_context.t = {
  suite_name: string;
  test_name: string;
  test_index: int;
  source_file: string option;
  binary_path: string option;
  workspace_root: Path.t option;
  package_name: string option;
  fixture: Test_context.fixture option;
}

module Context: sig
  type t = ctx
  type fixture = Test_context.fixture = {
    path: Path.t;
    relpath: string;
    name: string;
  }

  val with_fixture: t -> fixture -> t
end

module Snapshot = Snapshot

module FixtureRunner = Fixture_runner

(** The type of test: regular unit test or property test with example count. *)
type test_type =
  | UnitTest
  | Property of { examples: int }
(** [case name fn] creates a regular unit test. *)
type test_case = Test_case.t
val case: string -> (ctx -> (unit, string) result) -> test_case

(** [property name ~examples fn] creates a property test.
    Use this for property-based tests to show the number of examples tested.
    
    Example:
    {[
      Test.property "my property" ~examples:1000 (fun _ctx ->
        (* property checking logic *)
        Ok ()
      )
    ]}
*)
val property: string -> examples:int -> (ctx -> (unit, string) result) -> test_case

(** [skip name fn] creates a skipped test. *)
val skip: string -> (ctx -> (unit, string) result) -> test_case

val todo: string -> test_case

(** [todo name] creates a placeholder test marked as todo. *)
val assert_equal: expected:'a -> actual:'a -> unit

val assert_error: ('a, 'b) result -> unit

val assert_false: bool -> unit

val assert_ok: ('a, 'b) result -> unit

val assert_true: bool -> unit

module Cli: sig
  val main:
    name:string -> tests:test_case list -> args:string list -> (unit, Miniriot.Process.exit_reason) result
end

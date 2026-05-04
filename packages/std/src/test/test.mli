open Global

(** Shared test execution context. *)
type ctx = Test_context.t = {
  suite_name: string;
  test_name: string;
  test_index: int;
  source_file: Path.t option;
  binary_path: Path.t option;
  built_binaries: Test_context.built_binary list;
  workspace_root: Path.t option;
  package_name: string option;
  fixture: Test_context.fixture option;
  progress_handler: Test_context.progress_handler;
}

(** Helpers for working with test contexts and fixtures. *)
module Context: sig
  type t = ctx
  type built_binary = Test_context.built_binary = {
    name: string;
    path: Path.t;
  }
  type fixture = Test_context.fixture = {
    path: Path.t;
    relpath: Path.t;
    name: string;
    snapshot_path: Path.t option;
  }
  type snapshot_mode = Test_context.snapshot_mode =
    | External
    | Inline
  type snapshot_format = Test_context.snapshot_format =
    | Text
    | Json
  type snapshot_mismatch_reason = Test_context.snapshot_mismatch_reason =
    | Missing_approved
    | Pending_exists
    | Mismatch
  type progress = Test_context.progress =
    | PropertyIterationPassed of { current: int; total: int; size: int }
    | PropertyAssumptionRejected of { current: int; total: int; size: int; rejected_count: int }
    | PropertyCounterExampleFound of { current: int; total: int; size: int }
    | PropertyShrinkStep of { current: int; total: int; step: int; max_steps: int }
    | SnapshotAssertionStarted of {
        mode: snapshot_mode;
        format: snapshot_format;
        approved_path: Path.t option;
        pending_path: Path.t option;
      }
    | SnapshotAssertionMatched of {
        mode: snapshot_mode;
        format: snapshot_format;
        approved_path: Path.t option;
      }
    | SnapshotAssertionMismatch of {
        mode: snapshot_mode;
        format: snapshot_format;
        approved_path: Path.t option;
        pending_path: Path.t option;
        reason: snapshot_mismatch_reason;
      }

  val with_fixture: t -> fixture -> t

  val with_progress_handler: t -> Test_context.progress_handler -> t

  val emit_progress: t -> progress -> unit

  val no_progress_handler: Test_context.progress_handler

  val find_binary: t -> string -> Path.t option

  val require_binary: t -> string -> (Path.t, string) result
end

(** Snapshot assertions for golden-file and inline snapshots. *)
module Snapshot = Snapshot

(** Shared fixture discovery and test expansion helpers. *)
module FixtureRunner = Fixture_runner

(** Fuzzing corpus and mutator metadata helpers. *)
module Fuzz = Fuzz

(** The type of test: regular unit test or property test with example count. *)
type test_type =
  | UnitTest
  | Property of { examples: int }
  | Fuzz of { seeds: int }
type size = Test_case.size =
  | Small
  | Large
type reliability = Test_case.reliability =
  | Stable
  | Flaky of { retry_attempts: int }
(** Public representation of a test case. *)
type test_case = Test_case.t

(** [case name fn] creates a regular unit test. *)
val case:
  ?size:size ->
  ?reliability:reliability ->
  string ->
  (ctx -> (unit, string) result) ->
  test_case

(**
   [property name ~examples fn] creates a property test.
   Use this for property-based tests to show the number of examples tested.

   Example:
   {[
     Test.property "my property" ~examples:1000 (fun _ctx ->
       (* property checking logic *)
       Ok ()
     )
   ]}
*)
val property:
  ?size:size ->
  ?reliability:reliability ->
  string ->
  examples:int ->
  (ctx -> (unit, string) result) ->
  test_case

(**
   [fuzz name fn] creates a fuzz case.

   Fuzz cases are replayed by [riot test] with their seed inputs and selected
   by [riot fuzz] for generated input campaigns.
*)
val fuzz:
  ?size:size ->
  ?reliability:reliability ->
  ?seeds:string list ->
  ?corpus:Fuzz.Corpus.t ->
  ?mutator:Fuzz.Mutator.t ->
  string ->
  (ctx -> string -> (unit, string) result) ->
  test_case

(** [skip name fn] creates a skipped test. *)
val skip:
  ?size:size ->
  ?reliability:reliability ->
  string ->
  (ctx -> (unit, string) result) ->
  test_case

(** [todo name] creates a placeholder test marked as todo. *)
val todo: ?size:size -> ?reliability:reliability -> string -> test_case

(** Assert that [actual] equals [expected]. Raises on failure. *)
val assert_equal: expected:'a -> actual:'a -> unit

(** Assert that a result is [Error _]. Raises on success. *)
val assert_error: ('a, 'b) result -> unit

(** Assert that a boolean is [false]. Raises otherwise. *)
val assert_false: bool -> unit

(** Assert that a result is [Ok _]. Raises on error. *)
val assert_ok: ('a, 'b) result -> unit

(** Assert that a boolean is [true]. Raises otherwise. *)
val assert_true: bool -> unit

(** CLI helpers for test binaries. *)
module Cli: sig
  type execution_mode = Cli.execution_mode =
    | Concurrent
    | Linear
  type suite_hook = Cli.suite_hook

  val main:
    ?execution_mode:execution_mode ->
    ?setup:suite_hook ->
    ?teardown:suite_hook ->
    name:string ->
    tests:test_case list ->
    args:string list ->
    unit ->
    (unit, Runtime.Actor.exit_reason) result
end

open Global

(** Shared test execution context passed into test functions. *)
type ctx = Test_context.t
(** Internal result of running a test case. *)
type test_result =
  | Pass
  | Fail of string
  | Error of exn
(** The type of test: regular unit test or property test with example count. *)
type test_type =
  | UnitTest
  | Property of { examples: int }
  | Fuzz of { seeds: int }
(** Coarse execution policy bucket for a test. *)
type size =
  | Small
  | Large
type reliability =
  | Stable
  | Flaky of { retry_attempts: int }
(** Public representation of a test case. *)
type t = {
  (** Human-readable test name. *)
  name: string;
  (** Test kind. *)
  test_type: test_type;
  (** Execution size bucket. *)
  size: size;
  (** Reliability metadata used by the runner. *)
  reliability: reliability;
  (** Test implementation. *)
  fn: ctx -> (unit, string) result;
  (** Fuzz implementation, when [test_type] is [Fuzz]. *)
  fuzz_fn: (ctx -> string -> (unit, string) result) option;
  (** Declared fuzz corpus, when [test_type] is [Fuzz]. *)
  fuzz_corpus: Fuzz.Corpus.t option;
  (** Declared fuzz mutator hints, when [test_type] is [Fuzz]. *)
  fuzz_mutator: Fuzz.Mutator.t option;
  (** Whether the test should be skipped. *)
  skip: bool;
}

(** [case name fn] creates a regular unit test. *)
val case: ?size:size -> ?reliability:reliability -> string -> (ctx -> (unit, string) result) -> t

(**
   [property name ~examples fn] creates a property test that ran [examples]
   test cases.
*)
val property:
  ?size:size ->
  ?reliability:reliability ->
  string ->
  examples:int ->
  (ctx -> (unit, string) result) ->
  t

(**
   [fuzz name ~seeds fn] creates a fuzz case.

   The normal test runner replays [seeds]. The fuzz runner invokes [fn] with
   generated inputs through the test binary's [run-fuzz-case] command.
*)
val fuzz:
  ?size:size ->
  ?reliability:reliability ->
  ?seeds:string list ->
  ?corpus:Fuzz.Corpus.t ->
  ?mutator:Fuzz.Mutator.t ->
  string ->
  (ctx -> string -> (unit, string) result) ->
  t

(** [skip name fn] creates a skipped test. *)
val skip: ?size:size -> ?reliability:reliability -> string -> (ctx -> (unit, string) result) -> t

(** [todo name] creates a placeholder test marked as todo. *)
val todo: ?size:size -> ?reliability:reliability -> string -> t

open Std

(** Coverage-guided fuzzing support for Riot test binaries. *)
type error = Error.t =
  | Native_error of int
  | Io_error of string
  | Random_error of string
  | Test_error of string
  | Runtime_error of string

val error_message: error -> string

module Afl = Afl

module Coverage = Coverage

type target = Types.target = {
  program: string;
  args: input_path:Path.t -> string list;
  env: (string * string) list;
  cwd: Path.t option;
}
type fuzz_case = Types.fuzz_case = {
  suite: Riot_test.Test_runtime.suite_binary;
  case: Riot_test.Test_runtime.listed_test_case;
  binary_path: Path.t;
  source_path: Path.t option;
}
type corpus = Types.corpus = {
  inputs: string list;
  files: Path.t list;
}
type mutator = Types.mutator = {
  dictionary: string list;
  max_len: int option;
  splicing: bool;
}
type event = Types.event =
  | Campaign_started of {
      runs: int;
      max_len: int;
      duration_ms: int option;
      dir: Path.t;
    }
  | Campaign_progress of {
      run: int;
      runs: int;
      elapsed_ms: int;
      total_edges: int;
      corpus_size: int;
    }
  | Input_executed of {
      run: int;
      status: Afl.status;
      hit_edges: int;
      new_edges: int;
    }
  | Corpus_saved of {
      run: int;
      path: Path.t;
      new_edges: int;
    }
  | Crash_found of {
      run: int;
      path: Path.t;
      status: Afl.status;
    }
  | Crash_triaged of {
      run: int;
      input_path: Path.t;
      stdout_path: Path.t;
      stderr_path: Path.t;
      status_path: Path.t;
      status: Afl.status;
    }
  | Campaign_completed of {
      runs: int;
      crash_path: Path.t option;
      total_edges: int;
      elapsed_ms: int;
    }
  | Replay_completed of {
      input_path: Path.t;
      status: Afl.status;
      hit_edges: int;
    }
  | Corpus_minimized of {
      dir: Path.t;
      kept: int;
      removed: int;
    }
type request = Types.request = {
  case_dir: Path.t;
  target: target;
  corpus: corpus;
  mutator: mutator;
  runs: int;
  max_len: int;
  duration: Time.Duration.t option;
  timeout_ms: int;
  seed: string option;
  on_event: event -> unit;
}
type result = Types.result = {
  runs: int;
  crash_path: Path.t option;
  total_edges: int;
  elapsed_ms: int;
}
type campaign_result = Types.campaign_result = {
  index: int;
  result: (result, error) Result.t;
}
type many_result = Types.many_result = {
  campaigns: campaign_result list;
}
type replay_result = Types.replay_result = {
  input_path: Path.t;
  status: Afl.status;
  hit_edges: int;
}
type minimize_request = Types.minimize_request = {
  case_dir: Path.t;
  target: target;
  timeout_ms: int;
  on_event: event -> unit;
}
type minimize_result = Types.minimize_result = {
  dir: Path.t;
  kept: int;
  removed: int;
}

val collect_cases:
  ?on_event:(Riot_test.Test_runtime.test_event -> unit) ->
  workspace:Riot_model.Workspace.t ->
  package_filters:Riot_model.Package_name.t list ->
  filter:string option ->
  unit ->
  (fuzz_case list, error) Result.t

val case_dir: workspace:Riot_model.Workspace.t -> fuzz_case -> Path.t

val target_for_case: workspace:Riot_model.Workspace.t -> fuzz_case -> target

val corpus_for_case: workspace:Riot_model.Workspace.t -> fuzz_case -> corpus

val mutator_for_case: fuzz_case -> mutator

(**
   Run one fuzz campaign against a single fuzz-case binary.

   The target process receives `RIOT_SCHEDULERS=1` by default so each fuzz case
   runs without extra scheduler domains. Callers may pass an explicit
   `RIOT_SCHEDULERS` entry in [target.env] to override that default while still
   running multiple campaigns in parallel at the command level.
*)
val run: request -> (result, error) Result.t

(** Replay one saved input against one fuzz case under the AFL forkserver. *)
val replay: target:target -> input_path:Path.t -> timeout_ms:int -> (replay_result, error) Result.t

(**
   Minimize a case corpus by replaying existing corpus files in size order and
   deleting coverage-redundant inputs from [case_dir]/corpus.
*)
val minimize_corpus: minimize_request -> (minimize_result, error) Result.t

(**
   Run multiple fuzz campaigns with controlled campaign-level concurrency.

   This parallelizes independent fuzz-case binaries. Each target process still
   receives `RIOT_SCHEDULERS=1` by default through [run], so campaign-level
   parallelism does not make individual fuzz cases run in multicore mode.
*)
val run_many: ?concurrency:int -> request list -> many_result

(**
   Serialize fuzzing commands for one workspace with [target_dir_root]/fuzz.lock.
*)
val with_lock:
  workspace:Riot_model.Workspace.t ->
  on_waiting:(Path.t -> unit) ->
  (unit -> ('a, error) Result.t) ->
  ('a, error) Result.t

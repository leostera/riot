open Std

(** Coverage-guided fuzzing support for Riot test binaries. *)
type error = Error.t =
  | Native_error of int
  | Io_error of string
  | Random_error of string
  | Test_error of string
  | Runtime_error of string

let error_message = Error.message

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

let collect_cases = Case.collect_cases

let case_dir = Case.case_dir

let target_for_case = Case.target_for_case

let corpus_for_case = Case.corpus_for_case

let mutator_for_case = Case.mutator_for_case

let run = Runner.run

let replay = Runner.replay

let minimize_corpus = Runner.minimize_corpus

let run_many = Runner.run_many

let with_lock = Lock.with_lock

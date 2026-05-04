open Std

type target = {
  program: string;
  args: input_path:Path.t -> string list;
  env: (string * string) list;
  cwd: Path.t option;
}

type fuzz_case = {
  suite: Riot_test.Test_runtime.suite_binary;
  case: Riot_test.Test_runtime.listed_test_case;
  binary_path: Path.t;
  source_path: Path.t option;
}

type corpus = {
  inputs: string list;
  files: Path.t list;
}

type mutator = {
  dictionary: string list;
  max_len: int option;
  splicing: bool;
}

type event =
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

type request = {
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

type result = {
  runs: int;
  crash_path: Path.t option;
  total_edges: int;
  elapsed_ms: int;
}

type campaign_result = {
  index: int;
  result: (result, Error.t) Result.t;
}

type many_result = {
  campaigns: campaign_result list;
}

type replay_result = {
  input_path: Path.t;
  status: Afl.status;
  hit_edges: int;
}

type minimize_request = {
  case_dir: Path.t;
  target: target;
  timeout_ms: int;
  on_event: event -> unit;
}

type minimize_result = {
  dir: Path.t;
  kept: int;
  removed: int;
}

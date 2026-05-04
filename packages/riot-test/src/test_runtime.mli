open Std
open Riot_model

type suite_binary = {
  package_name: Package_name.t;
  suite_name: string;
}
type test_request = {
  workspace: Workspace.t;
  package_filters: Package_name.t list;
  suite_filter: string option;
  profile: string;
  extra_args: string list;
}
type test_case_type =
  | Test
  | Property of { examples: int }
  | Fuzz of { seeds: int }
type test_case_size =
  | Small
  | Large
type test_case_reliability =
  | Stable
  | Flaky of { retry_attempts: int }
type fuzz_corpus = {
  inputs: string list;
  files: Path.t list;
}
type fuzz_mutator = {
  dictionary: string list;
  max_len: int option;
  splicing: bool;
}
type test_case_status =
  | Passed
  | Failed of string
  | Timed_out of { timeout_ms: int }
  | Skipped
type test_case_result = {
  index: int;
  name: string;
  test_type: test_case_type;
  size: test_case_size;
  reliability: test_case_reliability;
  attempts: int;
  result: test_case_status;
  duration_us: int;
}
type listed_test_case = {
  index: int;
  name: string;
  test_type: test_case_type;
  size: test_case_size;
  reliability: test_case_reliability;
  fuzz_corpus: fuzz_corpus option;
  fuzz_mutator: fuzz_mutator option;
  skip: bool;
}
type listed_test_suite = {
  suite: suite_binary;
  source_path: Path.t option;
  binary_path: Path.t option;
  tests: listed_test_case list;
}
type failed_test = {
  suite: suite_binary;
  name: string;
  message: string;
  duration_us: int;
}
type test_suite_summary = {
  total: int;
  passed: int;
  failed: int;
  skipped: int;
  duration_us: int;
  results: test_case_result list;
}
type test_event =
  | Build of Riot_build.Event.t
  | NoSuitesFound of {
      package_name: Package_name.t option;
      suite_name: string option;
    }
  | TestSuitesCollected of {
      package_name: Package_name.t option;
      suite_name: string option;
      suite_count: int;
    }
  | ResolvingSuiteBinary of suite_binary
  | SuiteBinaryResolved of {
      suite: suite_binary;
      binary_path: Path.t;
    }
  | RunningSuite of suite_binary
  | ExecutingSuiteBinary of {
      suite: suite_binary;
      binary_path: Path.t;
      args: string list;
    }
  | SuiteHeartbeat of {
      suite: suite_binary;
      binary_path: Path.t;
      elapsed_us: int;
    }
  | SuiteBinaryFinished of {
      suite: suite_binary;
      binary_path: Path.t;
      status: int;
      stdout_bytes: int;
      stderr_bytes: int;
    }
  | SuiteProgress of {
      suite: suite_binary;
      event: Data.Json.t;
    }
  | ParsingSuiteOutput of {
      suite: suite_binary;
      binary_path: Path.t;
    }
  | SuiteCompleted of {
      suite: suite_binary;
      status: int;
      stdout: string;
      stderr: string;
      started_at_us: int option;
      completed_at_us: int option;
      duration_us: int option;
      summary: test_suite_summary;
    }
  | Summary of {
      total: int;
      passed: int;
      failed: int;
      skipped: int;
      failed_tests: failed_test list;
    }
type test_error =
  | BuildFailed of Riot_build.error
  | SuiteArtifactNotFound of {
      suite: suite_binary;
      reason: string;
    }
  | SuiteExecutionError of {
      suite: suite_binary;
      reason: string;
    }
  | SuitesFailed of int

val collect_suite_binaries:
  Workspace.t ->
  ?package_filters:Package_name.t list ->
  ?suite_filter:string ->
  unit ->
  suite_binary list

val test_error_message: test_error -> string

val test_event_to_json: test_event -> Data.Json.t option

val suite_context_json:
  workspace_root:Path.t ->
  package_name:Package_name.t ->
  ?source_file:Path.t ->
  binary_path:Path.t ->
  built_binaries:Test.Context.built_binary list ->
  unit ->
  string

val suite_progress_test_case_result: Data.Json.t -> (test_case_result option, string) result

val list_tests:
  ?on_event:(test_event -> unit) ->
  ?on_suite:(listed_test_suite -> unit) ->
  ?on_suite_error:(suite_binary -> test_error -> unit) ->
  test_request ->
  (listed_test_suite list, test_error) result

val test: ?on_event:(test_event -> unit) -> test_request -> (unit, test_error) result

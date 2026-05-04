open Std
open Std.Result.Syntax
open Riot_model

module Vector = Collections.Vector

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

type suite_run_output = {
  suite: suite_binary;
  status: int;
  stdout: string;
  stderr: string;
  started_at_us: int option;
  completed_at_us: int option;
  duration_us: int option;
  summary: test_suite_summary;
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

type Message.t +=
  | ListedTestsReady of (suite_binary * (listed_test_suite, test_error) result)

let no_event: test_event -> unit = fun _ -> ()

let no_listed_suite: listed_test_suite -> unit = fun _ -> ()

let no_list_error: suite_binary -> test_error -> unit = fun _ _ -> ()

let ctx_json_arg = "--ctx"

let upsert_json_field = fun name value fields ->
  let filtered =
    List.filter fields ~fn:(fun (field_name, _) -> not (String.equal field_name name))
  in
  filtered @ [ (name, value); ]

let json_event_type = fun json ->
  match Data.Json.get_field "type" json with
  | Some (Data.Json.String value) -> Some value
  | _ -> None

let suite_progress_event_of_line = fun line ->
  let trimmed = String.trim line in
  if String.equal trimmed "" then
    None
  else
    match Data.Json.from_string trimmed with
    | Ok (Data.Json.Object _ as json) -> (
        match json_event_type json with
        | Some "TestSummary"
        | None -> None
        | Some _ -> Some json
      )
    | Ok _
    | Error _ -> None

let strip_progress_json_lines = fun lines ->
  lines
  |> List.filter ~fn:(fun line -> Option.is_none (suite_progress_event_of_line line))

let suite_progress_json = fun (suite: suite_binary) (json: Data.Json.t) ->
  match json with
  | Data.Json.Object fields ->
      Data.Json.Object (
        fields
        |> upsert_json_field
          "package"
          (Data.Json.String (Package_name.to_string suite.package_name))
        |> upsert_json_field "suite" (Data.Json.String suite.suite_name)
      )
  | other -> other

let is_test_binary_name = fun name ->
  String.ends_with ~suffix:"_tests" name || String.ends_with ~suffix:"-tests" name

let compare_suite_binary = fun left right ->
  match Package_name.compare left.package_name right.package_name with
  | Order.EQ -> String.compare left.suite_name right.suite_name
  | cmp -> cmp

let requested_packages = fun suites ->
  suites
  |> List.map ~fn:(fun (suite: suite_binary) -> suite.package_name)
  |> List.unique ~compare:Package_name.compare

let profile_of_name = fun __tmp1 ->
  match __tmp1 with
  | "release" -> Riot_model.Profile.release
  | "fuzz" -> Riot_model.Profile.fuzz
  | _ -> Riot_model.Profile.debug

let matches_package_filters = fun package_filters package_name ->
  List.is_empty package_filters
  || List.exists
    (fun package_filter -> Package_name.equal package_filter package_name)
    package_filters

let selected_package_name = fun __tmp1 ->
  match __tmp1 with
  | [ package_name ] -> Some package_name
  | _ -> None

let realized_test_packages = fun ?(package_filters = []) (workspace: Workspace.t) ->
  Workspace.realize_packages ~intent:Package.Test workspace
  |> List.filter ~fn:Package.is_workspace_member
  |> List.filter ~fn:(fun (pkg: Package.t) -> matches_package_filters package_filters pkg.name)

let collect_suite_binaries = fun
  (workspace: Workspace.t) ?(package_filters = []) ?suite_filter () ->
  realized_test_packages ~package_filters workspace
  |> List.flat_map
    ~fn:(fun (pkg: Package.t) ->
      List.filter_map
        pkg.binaries
        ~fn:(fun (bin: Package.binary) ->
          if is_test_binary_name bin.name && (
            match suite_filter with
            | None -> true
            | Some suite_name -> String.equal bin.name suite_name
          ) then
            Some { package_name = pkg.name; suite_name = bin.name }
          else
            None))
  |> List.sort ~compare:compare_suite_binary

let find_suite_source_path = fun ~(workspace:Workspace.t) (suite: suite_binary) ->
  match List.find
    (realized_test_packages workspace)
    ~fn:(fun (pkg: Package.t) -> Package_name.equal pkg.name suite.package_name) with
  | None -> None
  | Some pkg ->
      List.find
        pkg.binaries
        ~fn:(fun (bin: Package.binary) -> String.equal bin.name suite.suite_name)
      |> Option.map ~fn:(fun (bin: Package.binary) -> Path.(pkg.path / bin.path))

let test_error_message = fun __tmp1 ->
  match __tmp1 with
  | BuildFailed err -> Riot_build.error_message err
  | SuiteArtifactNotFound { reason; _ } -> reason
  | SuiteExecutionError { reason; _ } -> reason
  | SuitesFailed count -> Int.to_string count ^ " test suite(s) failed"

let rec json_type_name = fun __tmp1 ->
  match __tmp1 with
  | Data.Json.Null -> "null"
  | Bool _ -> "bool"
  | Int _ -> "int"
  | Float _ -> "float"
  | String _ -> "string"
  | Array _ -> "array"
  | Object _ -> "object"
  | Embed json -> json_type_name json

let error_expected = fun expected actual ->
  Error ("expected " ^ expected ^ " but got " ^ json_type_name actual)

let get_object = fun __tmp1 ->
  match __tmp1 with
  | Data.Json.Object fields -> Ok fields
  | other -> error_expected "object" other

let get_array = fun __tmp1 ->
  match __tmp1 with
  | Data.Json.Array values -> Ok values
  | other -> error_expected "array" other

let get_string = fun __tmp1 ->
  match __tmp1 with
  | Data.Json.String value -> Ok value
  | other -> error_expected "string" other

let get_int = fun __tmp1 ->
  match __tmp1 with
  | Data.Json.Int value -> Ok value
  | other -> error_expected "int" other

let get_bool = fun __tmp1 ->
  match __tmp1 with
  | Data.Json.Bool value -> Ok value
  | other -> error_expected "bool" other

let optional_field = fun name fields ->
  match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name name) with
  | Some (_, value) -> Some value
  | None -> None

let field = fun name fields ->
  match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name name) with
  | Some (_, value) -> Ok value
  | None -> Error ("missing field " ^ name)

let optional_int_field = fun name fields ->
  match optional_field name fields with
  | Some value ->
      get_int value
      |> Result.map ~fn:Option.some
  | None -> Ok None

let split_json_stdout = fun stdout ->
  let lines = String.split stdout ~by:"\n" in
  let indexed = List.enumerate lines in
  match indexed
  |> List.reverse
  |> List.find ~fn:(fun (_, line) -> not (String.equal (String.trim line) "")) with
  | None -> Error "missing JSON output"
  | Some (json_idx, json_line) ->
      let prefix =
        indexed
        |> List.filter_map
          ~fn:(fun (idx, line) ->
            if idx < json_idx then
              Some line
            else
              None)
        |> String.concat "\n"
      in
      Ok (prefix, json_line)

let remove_json_args = fun args ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> List.reverse acc
    | "--json" :: rest -> loop acc rest
    | "--format" :: _value :: rest -> loop acc rest
    | arg :: rest when String.starts_with ~prefix:"--format=" arg -> loop acc rest
    | arg :: rest -> loop (arg :: acc) rest
  in
  loop [] args

let remove_list_args = fun args ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> List.reverse acc
    | "--json" :: rest -> loop acc rest
    | "--format" :: _value :: rest -> loop acc rest
    | arg :: rest when String.starts_with ~prefix:"--format=" arg -> loop acc rest
    | "--shuffle" :: rest -> loop acc rest
    | "--concurrency" :: _value :: rest -> loop acc rest
    | "--small-timeout-ms" :: _value :: rest -> loop acc rest
    | "--flaky-max-retries" :: _value :: rest -> loop acc rest
    | arg :: rest -> loop (arg :: acc) rest
  in
  loop [] args

let test_type_of_json = fun json ->
  let* fields = get_object json in
  let* type_json = field "type" fields in
  let* kind = get_string type_json in
  match kind with
  | "test" -> Ok Test
  | "property" ->
      let* examples_json = field "examples" fields in
      let* examples = get_int examples_json in
      Ok (Property { examples })
  | "fuzz" ->
      let seeds =
        match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name "seeds") with
        | Some (_, value) -> get_int value
        | None -> Ok 0
      in
      seeds
      |> Result.map ~fn:(fun seeds -> Fuzz { seeds })
  | other -> Error ("unknown test type " ^ other)

let test_size_of_json = fun fields ->
  match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name "size") with
  | None -> Ok Small
  | Some (_, value) ->
      let* size = get_string value in
      match size with
      | "small" -> Ok Small
      | "large" -> Ok Large
      | other -> Error ("unknown test size " ^ other)

let test_reliability_of_json = fun fields ->
  match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name "reliability") with
  | None -> Ok Stable
  | Some (_, value) ->
      let* reliability = get_string value in
      match reliability with
      | "stable" -> Ok Stable
      | "flaky" ->
          let retry_attempts =
            match List.find
              fields
              ~fn:(fun (field_name, _) -> String.equal field_name "retry_attempts") with
            | Some (_, value) -> get_int value
            | None -> Ok 0
          in
          retry_attempts
          |> Result.map ~fn:(fun retry_attempts -> Flaky { retry_attempts })
      | other -> Error ("unknown test reliability " ^ other)

let string_list_of_json = fun json ->
  let* values = get_array json in
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | value :: rest ->
        let* value = get_string value in
        loop (value :: acc) rest
  in
  loop [] values

let path_list_of_json = fun json ->
  let* values = string_list_of_json json in
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | value :: rest -> (
        match Path.from_string value with
        | Ok path -> loop (path :: acc) rest
        | Error _ -> Error ("invalid fuzz corpus path " ^ value)
      )
  in
  loop [] values

let fuzz_corpus_of_json = fun fields ->
  match optional_field "corpus" fields with
  | None -> Ok None
  | Some json ->
      let* corpus_fields = get_object json in
      let* inputs =
        match optional_field "inputs" corpus_fields with
        | Some json -> string_list_of_json json
        | None -> Ok []
      in
      let* files =
        match optional_field "files" corpus_fields with
        | Some json -> path_list_of_json json
        | None -> Ok []
      in
      Ok (Some { inputs; files })

let fuzz_mutator_of_json = fun fields ->
  match optional_field "mutator" fields with
  | None -> Ok None
  | Some json ->
      let* mutator_fields = get_object json in
      let* dictionary =
        match optional_field "dictionary" mutator_fields with
        | Some json -> string_list_of_json json
        | None -> Ok []
      in
      let* splicing =
        match optional_field "splicing" mutator_fields with
        | Some json -> get_bool json
        | None -> Ok true
      in
      let* max_len =
        match optional_field "max_len" mutator_fields with
        | Some Data.Json.Null
        | None -> Ok None
        | Some json ->
            get_int json
            |> Result.map ~fn:Option.some
      in
      Ok (Some { dictionary; max_len; splicing })

let test_status_of_json = fun json ->
  let* fields = get_object json in
  let* status_json = field "status" fields in
  let* status = get_string status_json in
  match status with
  | "passed" -> Ok Passed
  | "skipped" -> Ok Skipped
  | "failed" ->
      let* message_json = field "message" fields in
      let* message = get_string message_json in
      Ok (Failed message)
  | "timed_out" ->
      let* timeout_json = field "timeout_ms" fields in
      let* timeout_ms = get_int timeout_json in
      Ok (Timed_out { timeout_ms })
  | other -> Error ("unknown test status " ^ other)

let test_result_of_json = fun index json ->
  let* fields = get_object json in
  let* name_json = field "name" fields in
  let* name = get_string name_json in
  let* test_type = test_type_of_json json in
  let* size = test_size_of_json fields in
  let* reliability = test_reliability_of_json fields in
  let* attempts = optional_int_field "attempts" fields in
  let* result = test_status_of_json json in
  let* duration_us = optional_int_field "duration_us" fields in
  Ok {
    index;
    name;
    test_type;
    size;
    reliability;
    attempts = Option.unwrap_or ~default:1 attempts;
    result;
    duration_us = Option.unwrap_or ~default:0 duration_us;
  }

let test_case_type_field_of_json = fun fields ->
  match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name "test_type") with
  | None -> Ok Test
  | Some (_, Data.Json.String "test") -> Ok Test
  | Some (_, Data.Json.String "property") ->
      let* examples =
        match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name "examples") with
        | Some (_, value) -> get_int value
        | None -> Ok 0
      in
      Ok (Property { examples })
  | Some (_, Data.Json.String "fuzz") ->
      let* seeds =
        match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name "seeds") with
        | Some (_, value) -> get_int value
        | None -> Ok 0
      in
      Ok (Fuzz { seeds })
  | Some (_, Data.Json.String other) -> Error ("unknown test type " ^ other)
  | Some (_, other) -> error_expected "string" other

let suite_progress_test_case_result = fun json ->
  match json_event_type json with
  | Some "TestCaseCompleted" ->
      let* fields = get_object json in
      let index =
        match Data.Json.get_field "index" json with
        | Some (Data.Json.Int value) -> value
        | _ -> 0
      in
      let* name_json = field "name" fields in
      let* name = get_string name_json in
      let* test_type = test_case_type_field_of_json fields in
      let* size = test_size_of_json fields in
      let* reliability = test_reliability_of_json fields in
      let* attempts = optional_int_field "attempts" fields in
      let* result = test_status_of_json json in
      let* duration_us = optional_int_field "duration_us" fields in
      Ok (
        Some {
          index;
          name;
          test_type;
          size;
          reliability;
          attempts = Option.unwrap_or ~default:1 attempts;
          result;
          duration_us = Option.unwrap_or ~default:0 duration_us;
        }
      )
  | Some _
  | None -> Ok None

let listed_test_case_of_json = fun json ->
  let* fields = get_object json in
  let* index_json = field "index" fields in
  let* name_json = field "name" fields in
  let* index = get_int index_json in
  let* name = get_string name_json in
  let* test_type = test_type_of_json json in
  let* size = test_size_of_json fields in
  let* reliability = test_reliability_of_json fields in
  let* fuzz_corpus = fuzz_corpus_of_json fields in
  let* fuzz_mutator = fuzz_mutator_of_json fields in
  let skip =
    match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name "skip") with
    | Some (_, value) -> get_bool value
    | None -> Ok false
  in
  let* skip = skip in
  Ok {
    index;
    name;
    test_type;
    size;
    reliability;
    fuzz_corpus;
    fuzz_mutator;
    skip;
  }

let test_summary_of_json = fun json ->
  let* fields = get_object json in
  let* total_json = field "total" fields in
  let* passed_json = field "passed" fields in
  let* failed_json = field "failed" fields in
  let* skipped_json = field "skipped" fields in
  let* total = get_int total_json in
  let* passed = get_int passed_json in
  let* failed = get_int failed_json in
  let* skipped = get_int skipped_json in
  let* duration_us = optional_int_field "duration_us" fields in
  Ok (total, passed, failed, skipped, Option.unwrap_or ~default:0 duration_us)

let parse_test_suite_output = fun stdout ->
  let* (prefix_stdout, json_line) = split_json_stdout stdout in
  let prefix_stdout =
    prefix_stdout
    |> String.split ~by:"\n"
    |> strip_progress_json_lines
    |> String.concat "\n"
  in
  let* json =
    Data.Json.from_string json_line
    |> Result.map_err ~fn:Data.Json.error_to_string
  in
  let* fields = get_object json in
  let* tests_json = field "tests" fields in
  let* summary_json = field "summary" fields in
  let* tests = get_array tests_json in
  let rec parse_results index acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | test_json :: rest ->
        let* result = test_result_of_json index test_json in
        parse_results (index + 1) (result :: acc) rest
  in
  let* results = parse_results 1 [] tests in
  let* (total, passed, failed, skipped, summary_duration_us) = test_summary_of_json summary_json in
  let* started_at_us = optional_int_field "started_at_us" fields in
  let* completed_at_us = optional_int_field "completed_at_us" fields in
  let* duration_us = optional_int_field "duration_us" fields in
  Ok (
    prefix_stdout,
    started_at_us,
    completed_at_us,
    duration_us,
    {
      total;
      passed;
      failed;
      skipped;
      duration_us = summary_duration_us;
      results;
    }
  )

let parse_listed_tests_output = fun stdout ->
  let* (_prefix_stdout, json_line) = split_json_stdout stdout in
  let* json =
    Data.Json.from_string json_line
    |> Result.map_err ~fn:Data.Json.error_to_string
  in
  let* fields = get_object json in
  let* tests_json = field "tests" fields in
  let* tests = get_array tests_json in
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | json :: rest ->
        let* listed = listed_test_case_of_json json in
        loop (listed :: acc) rest
  in
  loop [] tests

let empty_suite_summary = {
  total = 0;
  passed = 0;
  failed = 0;
  skipped = 0;
  duration_us = 0;
  results = [];
}

let is_blank = fun s -> String.equal (String.trim s) ""

let suite_event_fields = fun (suite: suite_binary) -> [
  ("package", Data.Json.String (Package_name.to_string suite.package_name));
  ("suite", Data.Json.String suite.suite_name);
]

let summarize_output = fun value ->
  let trimmed = String.trim value in
  if String.equal trimmed "" then
    "<empty>"
  else
    let limit = 200 in
    if String.length trimmed <= limit then
      trimmed
    else
      String.sub trimmed ~offset:0 ~len:limit ^ "..."

let parse_failure_reason = fun ~suite ~(output:Command.output) reason ->
  String.concat
    ""
    [
      "failed to parse test results from suite '";
      suite.suite_name;
      "': ";
      reason;
      " (status=";
      Int.to_string output.status;
      ", stdout=";
      summarize_output output.stdout;
      ", stderr=";
      summarize_output output.stderr;
      ")";
    ]

let test_event_to_json = fun __tmp1 ->
  match __tmp1 with
  | Build event -> Riot_build.Event.to_json event
  | NoSuitesFound { package_name; suite_name } ->
      Some (
        Data.Json.Object [
          ("type", Data.Json.String "NoSuitesFound");
          ("package_name", match package_name with
          | Some name -> Data.Json.String (Riot_model.Package_name.to_string name)
          | None -> Data.Json.Null);
          ("suite_name", match suite_name with
          | Some name -> Data.Json.String name
          | None -> Data.Json.Null);
        ]
      )
  | TestSuitesCollected { package_name; suite_name; suite_count } ->
      Some (
        Data.Json.Object [
          ("type", Data.Json.String "TestSuitesCollected");
          ("package_name", match package_name with
          | Some name -> Data.Json.String (Riot_model.Package_name.to_string name)
          | None -> Data.Json.Null);
          ("suite_name", match suite_name with
          | Some name -> Data.Json.String name
          | None -> Data.Json.Null);
          ("suite_count", Data.Json.Int suite_count);
        ]
      )
  | ResolvingSuiteBinary suite ->
      Some (Data.Json.Object ([ ("type", Data.Json.String "ResolvingSuiteBinary"); ]
      @ suite_event_fields suite))
  | SuiteBinaryResolved { suite; binary_path } ->
      Some (Data.Json.Object ([
        ("type", Data.Json.String "SuiteBinaryResolved");
        ("binary_path", Data.Json.String (Path.to_string binary_path));
      ]
      @ suite_event_fields suite))
  | RunningSuite { package_name; suite_name } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "RunningSuite");
        ("package", Data.Json.String (Riot_model.Package_name.to_string package_name));
        ("suite", Data.Json.String suite_name);
      ])
  | ExecutingSuiteBinary { suite; binary_path; args } ->
      Some (Data.Json.Object ([
        ("type", Data.Json.String "ExecutingSuiteBinary");
        ("binary_path", Data.Json.String (Path.to_string binary_path));
        ("args", Data.Json.Array (List.map args ~fn:(fun arg -> Data.Json.String arg)));
      ]
      @ suite_event_fields suite))
  | SuiteHeartbeat { suite; binary_path; elapsed_us } ->
      Some (Data.Json.Object ([
        ("type", Data.Json.String "SuiteHeartbeat");
        ("binary_path", Data.Json.String (Path.to_string binary_path));
        ("elapsed_us", Data.Json.Int elapsed_us);
      ]
      @ suite_event_fields suite))
  | SuiteBinaryFinished {
      suite;
      binary_path;
      status;
      stdout_bytes;
      stderr_bytes;
    } ->
      Some (Data.Json.Object ([
        ("type", Data.Json.String "SuiteBinaryFinished");
        ("binary_path", Data.Json.String (Path.to_string binary_path));
        ("status", Data.Json.Int status);
        ("stdout_bytes", Data.Json.Int stdout_bytes);
        ("stderr_bytes", Data.Json.Int stderr_bytes);
      ]
      @ suite_event_fields suite))
  | SuiteProgress { suite; event } -> Some (suite_progress_json suite event)
  | ParsingSuiteOutput { suite; binary_path } ->
      Some (Data.Json.Object ([
        ("type", Data.Json.String "ParsingSuiteOutput");
        ("binary_path", Data.Json.String (Path.to_string binary_path));
      ]
      @ suite_event_fields suite))
  | SuiteCompleted {
      suite;
      status;
      stdout;
      stderr;
      started_at_us;
      completed_at_us;
      duration_us;
      summary;
    } ->
      let test_results =
        summary.results
        |> List.map
          ~fn:(fun (result: test_case_result) ->
            let status_fields =
              match result.result with
              | Passed -> [ ("status", Data.Json.String "passed"); ]
              | Skipped -> [ ("status", Data.Json.String "skipped"); ]
              | Timed_out { timeout_ms } ->
                  [
                    ("status", Data.Json.String "timed_out");
                    ("timeout_ms", Data.Json.Int timeout_ms);
                  ]
              | Failed message ->
                  [ ("status", Data.Json.String "failed"); ("message", Data.Json.String message); ]
            in
            let size_fields =
              match result.size with
              | Small -> [ ("size", Data.Json.String "small"); ]
              | Large -> [ ("size", Data.Json.String "large"); ]
            in
            let reliability_fields =
              match result.reliability with
              | Stable -> [ ("reliability", Data.Json.String "stable"); ]
              | Flaky { retry_attempts } ->
                  [
                    ("reliability", Data.Json.String "flaky");
                    ("retry_attempts", Data.Json.Int retry_attempts);
                  ]
            in
            let type_fields =
              match result.test_type with
              | Test -> [ ("type", Data.Json.String "test"); ]
              | Property { examples } ->
                  [ ("type", Data.Json.String "property"); ("examples", Data.Json.Int examples); ]
              | Fuzz { seeds } ->
                  [ ("type", Data.Json.String "fuzz"); ("seeds", Data.Json.Int seeds); ]
            in
            Data.Json.Object (((([
              ("name", Data.Json.String result.name);
              ("index", Data.Json.Int result.index);
              ("duration_us", Data.Json.Int result.duration_us);
              ("attempts", Data.Json.Int result.attempts);
            ]
            @ status_fields)
            @ size_fields)
            @ reliability_fields)
            @ type_fields))
      in
      Some (
        Data.Json.Object [
          ("type", Data.Json.String "SuiteCompleted");
          ("package", Data.Json.String (Package_name.to_string suite.package_name));
          ("suite", Data.Json.String suite.suite_name);
          ("status", Data.Json.Int status);
          ("stdout", Data.Json.String stdout);
          ("stderr", Data.Json.String stderr);
          ("started_at_us", match started_at_us with
          | Some value -> Data.Json.Int value
          | None -> Data.Json.Null);
          ("completed_at_us", match completed_at_us with
          | Some value -> Data.Json.Int value
          | None -> Data.Json.Null);
          ("duration_us", match duration_us with
          | Some value -> Data.Json.Int value
          | None -> Data.Json.Int summary.duration_us);
          ("tests", Data.Json.Array test_results);
          (
            "summary",
            Data.Json.Object [
              ("total", Data.Json.Int summary.total);
              ("passed", Data.Json.Int summary.passed);
              ("failed", Data.Json.Int summary.failed);
              ("skipped", Data.Json.Int summary.skipped);
              ("duration_us", Data.Json.Int summary.duration_us);
            ]
          );
        ]
      )
  | Summary {
      total;
      passed;
      failed;
      skipped;
      failed_tests;
    } ->
      let failed_tests =
        failed_tests
        |> List.map
          ~fn:(fun (failed_test: failed_test) ->
            Data.Json.Object [
              ("package", Data.Json.String (Package_name.to_string failed_test.suite.package_name));
              ("suite", Data.Json.String failed_test.suite.suite_name);
              ("name", Data.Json.String failed_test.name);
              ("message", Data.Json.String failed_test.message);
              ("duration_us", Data.Json.Int failed_test.duration_us);
            ])
      in
      Some (Data.Json.Object [
        ("type", Data.Json.String "TestSummary");
        ("total", Data.Json.Int total);
        ("passed", Data.Json.Int passed);
        ("failed", Data.Json.Int failed);
        ("skipped", Data.Json.Int skipped);
        ("failed_tests", Data.Json.Array failed_tests);
      ])

let ensure_executable_binary_path = fun ~kind path ->
  match Fs.metadata path with
  | Error err -> Error ("failed to read " ^ kind ^ " metadata: " ^ IO.error_message err)
  | Ok metadata ->
      let mode = Fs.Metadata.mode metadata in
      if mode land 0o111 != 0 then
        Ok path
      else
        Fs.set_permissions path (Fs.Permissions.from_mode (mode lor 0o111))
        |> Result.map ~fn:(fun () -> path)
        |> Result.map_err
          ~fn:(fun err -> "failed to mark " ^ kind ^ " executable: " ^ IO.error_message err)

let materialized_export_path = fun ~(workspace:Workspace.t) ~profile ~package_name ~export_name ->
  let out_dir =
    Riot_model.Riot_dirs.out_dir_in_workspace
      ~workspace
      ~profile
      ~target:(Riot_model.Riot_dirs.host_target ())
  in
  Path.(out_dir / Path.v (Package_name.to_string package_name) / Path.v export_name)

let find_export_path_in_output = fun
  ~(workspace:Workspace.t)
  ~profile
  ~(store:Riot_store.Store.t)
  ~kind
  ~package_name
  ~export_name
  (output: Riot_build.Build_result.t) ->
  let fallback_path = materialized_export_path ~workspace ~profile ~package_name ~export_name in
  let ensure_materialized_fallback () =
    match Fs.exists fallback_path with
    | Ok true ->
        ensure_executable_binary_path ~kind fallback_path
        |> Result.map_err ~fn:(fun reason -> reason)
    | Ok false
    | Error _ -> Error (kind ^ " '" ^ export_name ^ "' was not produced by build output")
  in
  match Riot_build.Build_result.find_package output package_name
  |> Option.and_then
    ~fn:(fun package_output -> Riot_build.Build_result.find_export package_output export_name) with
  | None -> ensure_materialized_fallback ()
  | Some export_entry -> (
      match Riot_store.Store.export_source_path store export_entry with
      | Some path ->
          ensure_executable_binary_path ~kind path
          |> Result.map_err ~fn:(fun reason -> reason)
      | None -> ensure_materialized_fallback ()
    )

let find_suite_binary_path_in_output = fun
  ~(workspace:Workspace.t)
  ~profile
  ~(store:Riot_store.Store.t)
  ~(suite:suite_binary)
  (output: Riot_build.Build_result.t) ->
  find_export_path_in_output
    ~workspace
    ~profile
    ~store
    ~kind:"suite binary"
    ~package_name:suite.package_name
    ~export_name:suite.suite_name
    output
  |> Result.map_err ~fn:(fun reason -> SuiteArtifactNotFound { suite; reason })

let suite_ctx_json_value = fun
  ~workspace_root ~package_name ?source_file ~binary_path ~built_binaries () ->
  let built_binaries_json =
    built_binaries
    |> List.map
      ~fn:(fun (binary: Test.Context.built_binary) ->
        Data.Json.Object [
          ("name", Data.Json.String binary.name);
          ("path", Data.Json.String (Path.to_string binary.path));
        ])
  in
  Data.Json.Object [
    ("workspace_root", Data.Json.String (Path.to_string workspace_root));
    ("package_name", Data.Json.String (Package_name.to_string package_name));
    ("binary_path", Data.Json.String (Path.to_string binary_path));
    ("source_file", match source_file with
    | Some source_file -> Data.Json.String (Path.to_string source_file)
    | None -> Data.Json.Null);
    ("built_binaries", Data.Json.Array built_binaries_json);
  ]
  |> Data.Json.to_string

let suite_context_json = suite_ctx_json_value

let runtime_output_packages = fun ~(workspace:Workspace.t) (output: Riot_build.Build_result.t) ->
  Workspace.realize_packages ~intent:Package.Runtime workspace
  |> List.filter
    ~fn:(fun (pkg: Package.t) ->
      Option.is_some
        (Riot_build.Build_result.find_package output pkg.name))

let reachable_runtime_packages = fun packages start_package_name ->
  let find_package package_name =
    List.find packages ~fn:(fun (pkg: Package.t) -> Package_name.equal pkg.name package_name)
  in
  let seen = Collections.HashSet.create () in
  let rec visit acc package_name =
    if Collections.HashSet.contains seen ~value:package_name then
      acc
    else
      let _ = Collections.HashSet.insert seen ~value:package_name in
      match find_package package_name with
      | None -> acc
      | Some (pkg: Package.t) ->
          let acc = pkg :: acc in
          List.fold_left
            pkg.dependencies
            ~init:acc
            ~fn:(fun acc (dep: Package.dependency) -> visit acc dep.name)
  in
  visit [] start_package_name
  |> List.reverse

let runtime_output_built_binaries = fun
  ~(workspace:Workspace.t)
  ~package_name
  ~profile
  ~(store:Riot_store.Store.t)
  (output: Riot_build.Build_result.t) ->
  let packages = runtime_output_packages ~workspace output in
  reachable_runtime_packages packages package_name
  |> List.flat_map
    ~fn:(fun (pkg: Package.t) ->
      List.filter_map
        pkg.binaries
        ~fn:(fun (bin: Package.binary) ->
          match find_export_path_in_output
            ~workspace
            ~profile
            ~store
            ~kind:"built binary"
            ~package_name:pkg.name
            ~export_name:bin.name
            output with
          | Ok path -> Some Test.Context.{ name = bin.name; path }
          | Error _ -> None))

let run_suite_args = fun extra_args -> ("run-tests" :: remove_json_args extra_args) @ [ "--json" ]

let run_suite = fun
  ~on_event ~workspace_root ~suite ?source_file ~built_binaries ~extra_args binary_path ->
  let ctx_json =
    suite_ctx_json_value
      ~workspace_root
      ~package_name:suite.package_name
      ?source_file
      ~binary_path
      ~built_binaries
      ()
  in
  let args = run_suite_args extra_args @ [ ctx_json_arg; ctx_json ] in
  on_event (ExecutingSuiteBinary { suite; binary_path; args });
  let cmd = Command.make (Path.to_string binary_path) ~args in
  match Command.output
    cmd
    ~on_idle:(fun elapsed ->
      on_event
        (SuiteHeartbeat { suite; binary_path; elapsed_us = Time.Duration.to_micros elapsed }))
    ~on_stdout_line:(fun line ->
      suite_progress_event_of_line line
      |> Option.for_each ~fn:(fun event -> on_event (SuiteProgress { suite; event }))) with
  | Error (Command.SystemError reason) -> Error (SuiteExecutionError { suite; reason })
  | Ok output -> (
      on_event
        (
          SuiteBinaryFinished {
            suite;
            binary_path;
            status = output.status;
            stdout_bytes = String.length output.stdout;
            stderr_bytes = String.length output.stderr;
          }
        );
      on_event (ParsingSuiteOutput { suite; binary_path });
      let parsed_output =
        match parse_test_suite_output output.stdout with
        | Ok parsed -> Ok parsed
        | Error _reason when Int.equal output.status 0
        && is_blank output.stdout
        && is_blank output.stderr -> Ok ("", None, None, None, empty_suite_summary)
        | Error reason -> Error reason
      in
      match parsed_output with
      | Error reason ->
          Error (SuiteExecutionError {
            suite;
            reason = parse_failure_reason ~suite ~output reason;
          })
      | Ok (stdout, started_at_us, completed_at_us, duration_us, summary) ->
          Ok {
            suite;
            status = output.status;
            stdout;
            stderr = output.stderr;
            started_at_us;
            completed_at_us;
            duration_us;
            summary;
          }
    )

let list_suite_binary_capture = fun ~(suite:suite_binary) ~extra_args binary_path ->
  let extra_args = remove_list_args extra_args @ [ "--json" ] in
  let cmd = Command.make (Path.to_string binary_path) ~args:("list-tests" :: extra_args) in
  Command.output cmd

let list_suite = fun ~(workspace:Workspace.t) ~suite ~extra_args binary_path ->
  match list_suite_binary_capture ~suite ~extra_args binary_path with
  | Error (Command.SystemError reason) -> Error (SuiteExecutionError { suite; reason })
  | Ok output -> (
      match parse_listed_tests_output output.stdout with
      | Error reason ->
          Error (SuiteExecutionError {
            suite;
            reason = parse_failure_reason ~suite ~output reason;
          })
      | Ok tests ->
          Ok {
            suite;
            source_path = find_suite_source_path ~workspace suite;
            binary_path = Some binary_path;
            tests;
          }
    )

let build_output = fun ~(workspace:Workspace.t) ~packages ~profile ?on_event () ->
  Riot_build.Request.make
    ~workspace
    ~packages
    ~targets:Target.Host
    ~scope:Riot_build.Request.Dev
    ~profile:(profile_of_name profile)
    ()
  |> Riot_build.build ?on_event

let store_for_request = fun (request: test_request) ->
  Riot_store.Store.create_for_lane
    ~workspace:request.workspace
    ~profile:request.profile
    ~target:(Riot_dirs.host_target ())

let resolve_suite_binaries = fun ~(workspace:Workspace.t) ~profile ~store ~suites output ->
  let rec loop resolved missing = fun __tmp1 ->
    match __tmp1 with
    | [] -> (List.reverse resolved, List.reverse missing)
    | suite :: rest -> (
        match find_suite_binary_path_in_output ~workspace ~profile ~store ~suite output with
        | Ok binary_path -> loop ((suite, binary_path) :: resolved) missing rest
        | Error err -> loop resolved ((suite, err) :: missing) rest
      )
  in
  loop [] [] suites

let list_tests = fun
  ?(on_event = no_event)
  ?(on_suite = no_listed_suite)
  ?(on_suite_error = no_list_error)
  (request: test_request) ->
  let suites =
    collect_suite_binaries
      request.workspace
      ~package_filters:request.package_filters
      ?suite_filter:request.suite_filter
      ()
  in
  if suites = [] then
    Ok []
  else
    match build_output
      ~workspace:request.workspace
      ~packages:(requested_packages suites)
      ~profile:request.profile
      ~on_event:(fun event -> on_event (Build event))
      () with
    | Error err -> Error (BuildFailed err)
    | Ok output ->
        let store = store_for_request request in
        let (suite_binaries, missing_suites) =
          resolve_suite_binaries
            ~workspace:request.workspace
            ~profile:request.profile
            ~store
            ~suites
            output
        in
        List.for_each missing_suites ~fn:(fun (suite, err) -> on_suite_error suite err);
        if suite_binaries = [] then
          Ok []
        else
          let concurrency = Int.max 1 (Int.min 8 Thread.available_parallelism) in
          let parent = self () in
          let rec spawn_initial active remaining =
            if active >= concurrency then
              (active, remaining)
            else
              match remaining with
              | [] -> (active, [])
              | (suite, binary_path) :: rest ->
                  let _worker =
                    spawn
                      (fun () ->
                        let result =
                          try list_suite
                            ~workspace:request.workspace
                            ~suite
                            ~extra_args:request.extra_args
                            binary_path with
                          | exn ->
                              Error (SuiteExecutionError {
                                suite;
                                reason = Exception.to_string exn;
                              })
                        in
                        send parent (ListedTestsReady (suite, result));
                        Ok ())
                  in
                  spawn_initial (active + 1) rest
          in
          let rec collect active remaining acc =
            if active <= 0 then
              Ok (List.reverse acc)
            else
              let (suite, result) =
                receive
                  ~selector:(fun (msg: Message.t) ->
                    match msg with
                    | ListedTestsReady payload -> Select payload
                    | _ -> Skip)
                  ()
              in
              let acc =
                match result with
                | Ok listed ->
                    on_suite listed;
                    (suite, listed) :: acc
                | Error err ->
                    on_suite_error suite err;
                    acc
              in
              match remaining with
              | [] -> collect (active - 1) [] acc
              | (next_suite, next_binary_path) :: rest ->
                  let _worker =
                    spawn
                      (fun () ->
                        let result =
                          try list_suite
                            ~workspace:request.workspace
                            ~suite:next_suite
                            ~extra_args:request.extra_args
                            next_binary_path with
                          | exn ->
                              Error (SuiteExecutionError {
                                suite = next_suite;
                                reason = Exception.to_string exn;
                              })
                        in
                        send parent (ListedTestsReady (next_suite, result));
                        Ok ())
                  in
                  collect active rest acc
          in
          let (initial_active, remaining) = spawn_initial 0 suite_binaries in
          collect initial_active remaining []
          |> Result.map
            ~fn:(fun collected ->
              collected
              |> List.sort ~compare:(fun (left, _) (right, _) -> compare_suite_binary left right)
              |> List.map ~fn:(fun (_, value) -> value))

let test = fun ?(on_event = no_event) (request: test_request) ->
  let suites =
    collect_suite_binaries
      request.workspace
      ~package_filters:request.package_filters
      ?suite_filter:request.suite_filter
      ()
  in
  if suites = [] then (
    on_event
      (NoSuitesFound {
        package_name = selected_package_name request.package_filters;
        suite_name = request.suite_filter;
      });
    Ok ()
  ) else
    let () =
      on_event
        (TestSuitesCollected {
          package_name = selected_package_name request.package_filters;
          suite_name = request.suite_filter;
          suite_count = List.length suites;
        })
    in
    match build_output
      ~workspace:request.workspace
      ~packages:(requested_packages suites)
      ~profile:request.profile
      ~on_event:(fun event -> on_event (Build event))
      () with
    | Error err -> Error (BuildFailed err)
    | Ok output ->
        let store = store_for_request request in
        let total = ref 0 in
        let passed = ref 0 in
        let failed = ref 0 in
        let skipped = ref 0 in
        let failed_tests = Vector.with_capacity ~size:8 in
        let rec loop = fun __tmp1 ->
          match __tmp1 with
          | [] ->
              on_event
                (
                  Summary {
                    total = !total;
                    passed = !passed;
                    failed = !failed;
                    skipped = !skipped;
                    failed_tests =
                      Vector.to_array failed_tests
                      |> Array.to_list;
                  }
                );
              if !failed > 0 then
                Error (SuitesFailed !failed)
              else
                Ok ()
          | suite :: rest -> (
              on_event (ResolvingSuiteBinary suite);
              match find_suite_binary_path_in_output
                ~workspace:request.workspace
                ~profile:request.profile
                ~store
                ~suite
                output with
              | Error _ as err -> err
              | Ok binary_path ->
                  let source_file = find_suite_source_path ~workspace:request.workspace suite in
                  let built_binaries =
                    runtime_output_built_binaries
                      ~workspace:request.workspace
                      ~package_name:suite.package_name
                      ~profile:request.profile
                      ~store
                      output
                  in
                  on_event (SuiteBinaryResolved { suite; binary_path });
                  on_event (RunningSuite suite);
                  match run_suite
                    ~on_event
                    ~workspace_root:request.workspace.root
                    ~suite
                    ?source_file
                    ~built_binaries
                    ~extra_args:request.extra_args
                    binary_path with
                  | Error _ as err -> err
                  | Ok suite_output ->
                      total := !total + suite_output.summary.total;
                      passed := !passed + suite_output.summary.passed;
                      failed := !failed + suite_output.summary.failed;
                      skipped := !skipped + suite_output.summary.skipped;
                      suite_output.summary.results
                      |> List.for_each
                        ~fn:(fun (result: test_case_result) ->
                          match result.result with
                          | Failed message ->
                              Vector.push
                                failed_tests
                                ~value:{
                                  suite = suite_output.suite;
                                  name = result.name;
                                  message;
                                  duration_us = result.duration_us;
                                }
                          | Timed_out { timeout_ms } ->
                              Vector.push
                                failed_tests
                                ~value:{
                                  suite = suite_output.suite;
                                  name = result.name;
                                  message = "timed out after " ^ Int.to_string timeout_ms ^ "ms";
                                  duration_us = result.duration_us;
                                }
                          | Passed
                          | Skipped -> ());
                      on_event
                        (
                          SuiteCompleted {
                            suite = suite_output.suite;
                            status = suite_output.status;
                            stdout = suite_output.stdout;
                            stderr = suite_output.stderr;
                            started_at_us = suite_output.started_at_us;
                            completed_at_us = suite_output.completed_at_us;
                            duration_us = suite_output.duration_us;
                            summary = suite_output.summary;
                          }
                        );
                      loop rest
            )
        in
        loop suites

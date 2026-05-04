open Global
open Collections
open Arg_parser

type execution_mode =
  | Concurrent
  | Linear

type suite_hook = unit -> (unit, string) result

let test_type_fields = fun __tmp1 ->
  match __tmp1 with
  | Test_case.UnitTest -> [ ("type", Data.Json.String "test"); ]
  | Test_case.Property { examples } ->
      [ ("type", Data.Json.String "property"); ("examples", Data.Json.Int examples); ]
  | Test_case.Fuzz { seeds } ->
      [ ("type", Data.Json.String "fuzz"); ("seeds", Data.Json.Int seeds); ]

let string_array = fun values ->
  Data.Json.Array (List.map values ~fn:(fun value -> Data.Json.String value))

let fuzz_corpus_json = fun (corpus: Fuzz.Corpus.t) ->
  Data.Json.Object [
    ("inputs", string_array (Fuzz.Corpus.inline_inputs corpus));
    (
      "files",
      Fuzz.Corpus.file_paths corpus
      |> List.map ~fn:(fun path -> Path.to_string path)
      |> string_array
    );
  ]

let fuzz_mutator_json = fun (mutator: Fuzz.Mutator.t) ->
  Data.Json.Object [
    ("dictionary", string_array mutator.dictionary);
    ("splicing", Data.Json.Bool mutator.splicing);
    ("max_len", match mutator.max_len with
    | Some max_len -> Data.Json.Int max_len
    | None -> Data.Json.Null);
  ]

let fuzz_metadata_fields = fun (test: Test_case.t) ->
  match (test.fuzz_corpus, test.fuzz_mutator) with
  | (None, None) -> []
  | (corpus, mutator) ->
      (
        match corpus with
        | Some corpus -> [ ("corpus", fuzz_corpus_json corpus); ]
        | None -> []
      ) @ (
        match mutator with
        | Some mutator -> [ ("mutator", fuzz_mutator_json mutator); ]
        | None -> []
      )

let event_test_type_fields = fun __tmp1 ->
  match __tmp1 with
  | Test_case.UnitTest -> [ ("test_type", Data.Json.String "test"); ]
  | Test_case.Property { examples } ->
      [ ("test_type", Data.Json.String "property"); ("examples", Data.Json.Int examples); ]
  | Test_case.Fuzz { seeds } ->
      [ ("test_type", Data.Json.String "fuzz"); ("seeds", Data.Json.Int seeds); ]

let size_to_json = fun __tmp1 ->
  match __tmp1 with
  | Test_case.Small -> Data.Json.String "small"
  | Test_case.Large -> Data.Json.String "large"

let reliability_fields = fun __tmp1 ->
  match __tmp1 with
  | Test_case.Stable -> [ ("reliability", Data.Json.String "stable"); ]
  | Test_case.Flaky { retry_attempts } ->
      [
        ("reliability", Data.Json.String "flaky");
        ("retry_attempts", Data.Json.Int retry_attempts);
      ]

let descriptor_fields = fun (test: Runner.test_descriptor) ->
  ([
    ("index", Data.Json.Int test.index);
    ("name", Data.Json.String test.name);
    ("size", size_to_json test.size);
  ]
  @ event_test_type_fields test.test_type)
  @ reliability_fields test.reliability

let snapshot_mode_to_json = fun __tmp1 ->
  match __tmp1 with
  | Test_context.External -> Data.Json.String "external"
  | Test_context.Inline -> Data.Json.String "inline"

let snapshot_format_to_json = fun __tmp1 ->
  match __tmp1 with
  | Test_context.Text -> Data.Json.String "text"
  | Test_context.Json -> Data.Json.String "json"

let snapshot_reason_to_json = fun __tmp1 ->
  match __tmp1 with
  | Test_context.Missing_approved -> Data.Json.String "missing_approved"
  | Test_context.Pending_exists -> Data.Json.String "pending_exists"
  | Test_context.Mismatch -> Data.Json.String "mismatch"

let optional_path_fields = fun name value ->
  match value with
  | Some path -> [ (name, Data.Json.String (Path.to_string path)); ]
  | None -> []

let progress_fields = fun __tmp1 ->
  match __tmp1 with
  | Test_context.PropertyIterationPassed { current; total; size } ->
      [
        ("progress_type", Data.Json.String "property_iteration_passed");
        ("current", Data.Json.Int current);
        ("total", Data.Json.Int total);
        ("size_value", Data.Json.Int size);
      ]
  | Test_context.PropertyAssumptionRejected {
      current;
      total;
      size;
      rejected_count;
    } ->
      [
        ("progress_type", Data.Json.String "property_assumption_rejected");
        ("current", Data.Json.Int current);
        ("total", Data.Json.Int total);
        ("size_value", Data.Json.Int size);
        ("rejected_count", Data.Json.Int rejected_count);
      ]
  | Test_context.PropertyCounterExampleFound { current; total; size } ->
      [
        ("progress_type", Data.Json.String "property_counter_example_found");
        ("current", Data.Json.Int current);
        ("total", Data.Json.Int total);
        ("size_value", Data.Json.Int size);
      ]
  | Test_context.PropertyShrinkStep {
      current;
      total;
      step;
      max_steps;
    } ->
      [
        ("progress_type", Data.Json.String "property_shrink_step");
        ("current", Data.Json.Int current);
        ("total", Data.Json.Int total);
        ("step", Data.Json.Int step);
        ("max_steps", Data.Json.Int max_steps);
      ]
  | Test_context.SnapshotAssertionStarted {
      mode;
      format;
      approved_path;
      pending_path;
    } ->
      ([
        ("progress_type", Data.Json.String "snapshot_assertion_started");
        ("snapshot_mode", snapshot_mode_to_json mode);
        ("snapshot_format", snapshot_format_to_json format);
      ]
      @ optional_path_fields "approved_path" approved_path)
      @ optional_path_fields "pending_path" pending_path
  | Test_context.SnapshotAssertionMatched { mode; format; approved_path } ->
      [
        ("progress_type", Data.Json.String "snapshot_assertion_matched");
        ("snapshot_mode", snapshot_mode_to_json mode);
        ("snapshot_format", snapshot_format_to_json format);
      ]
      @ optional_path_fields "approved_path" approved_path
  | Test_context.SnapshotAssertionMismatch {
      mode;
      format;
      approved_path;
      pending_path;
      reason;
    } ->
      ([
        ("progress_type", Data.Json.String "snapshot_assertion_mismatch");
        ("snapshot_mode", snapshot_mode_to_json mode);
        ("snapshot_format", snapshot_format_to_json format);
        ("reason", snapshot_reason_to_json reason);
      ]
      @ optional_path_fields "approved_path" approved_path)
      @ optional_path_fields "pending_path" pending_path

let single_result_fields = fun __tmp1 ->
  match __tmp1 with
  | Test_result.Passed -> [ ("status", Data.Json.String "passed"); ]
  | Test_result.Skipped -> [ ("status", Data.Json.String "skipped"); ]
  | Test_result.Failed message ->
      [ ("status", Data.Json.String "failed"); ("message", Data.Json.String message); ]
  | Test_result.Timed_out { timeout } ->
      [
        ("status", Data.Json.String "timed_out");
        ("timeout_ms", Data.Json.Int (Time.Duration.to_millis timeout));
      ]

let event_started_at = ref None

let event_elapsed_us = fun () ->
  match !event_started_at with
  | Some started_at ->
      Time.Instant.elapsed started_at
      |> Time.Duration.to_micros
  | None -> 0

let write_json_line = fun json -> println (Data.Json.to_string json)

let event_to_json = fun __tmp1 ->
  match __tmp1 with
  | Runner.SuiteStarted { suite_name; total } ->
      event_started_at := Some (Time.Instant.now ());
      Data.Json.Object [
        ("type", Data.Json.String "TestSuiteStarted");
        ("suite", Data.Json.String suite_name);
        ("total", Data.Json.Int total);
        ("started_at_us", Data.Json.Int 0);
      ]
  | Runner.TestStarted test ->
      Data.Json.Object ([
        ("type", Data.Json.String "TestCaseStarted");
        ("emitted_at_us", Data.Json.Int (event_elapsed_us ()));
      ]
      @ descriptor_fields test)
  | Runner.TestProgress { test; attempt; progress } ->
      Data.Json.Object (([
        ("type", Data.Json.String "TestCaseProgress");
        ("attempt", Data.Json.Int attempt);
        ("emitted_at_us", Data.Json.Int (event_elapsed_us ()));
      ]
      @ descriptor_fields test)
      @ progress_fields progress)
  | Runner.TestAttemptStarted { test; attempt; timeout } ->
      let timeout_fields =
        match timeout with
        | Some timeout -> [ ("timeout_ms", Data.Json.Int (Time.Duration.to_millis timeout)); ]
        | None -> []
      in
      Data.Json.Object (([
        ("type", Data.Json.String "TestCaseAttemptStarted");
        ("attempt", Data.Json.Int attempt);
        ("emitted_at_us", Data.Json.Int (event_elapsed_us ()));
      ]
      @ descriptor_fields test)
      @ timeout_fields)
  | Runner.TestHeartbeat { test; attempt; elapsed } ->
      Data.Json.Object ([
        ("type", Data.Json.String "TestCaseHeartbeat");
        ("attempt", Data.Json.Int attempt);
        ("elapsed_us", Data.Json.Int (Time.Duration.to_micros elapsed));
        ("emitted_at_us", Data.Json.Int (event_elapsed_us ()));
      ]
      @ descriptor_fields test)
  | Runner.TestAttemptFinished {
      test;
      attempt;
      result;
      duration;
    } ->
      Data.Json.Object (([
        ("type", Data.Json.String "TestCaseAttemptFinished");
        ("attempt", Data.Json.Int attempt);
        ("duration_us", Data.Json.Int (Time.Duration.to_micros duration));
        ("emitted_at_us", Data.Json.Int (event_elapsed_us ()));
      ]
      @ descriptor_fields test)
      @ single_result_fields result)
  | Runner.TestFinished result ->
      Data.Json.Object (((([
        ("type", Data.Json.String "TestCaseCompleted");
        ("index", Data.Json.Int result.index);
        ("name", Data.Json.String result.name);
        ("attempts", Data.Json.Int result.attempts);
        ("duration_us", Data.Json.Int (Time.Duration.to_micros result.duration));
        ("emitted_at_us", Data.Json.Int (event_elapsed_us ()));
      ]
      @ event_test_type_fields result.test_type)
      @ [ ("size", size_to_json result.size); ])
      @ reliability_fields result.reliability)
      @ single_result_fields result.result)

let json_event_handler = fun event ->
  event_to_json event
  |> write_json_line

let matches_query = fun query (test: Test_case.t) ->
  match query with
  | None -> true
  | Some query -> String.contains test.name query

let matches_size = fun ~small_only ~large_only (test: Test_case.t) ->
  match (small_only, large_only, test.size) with
  | (false, false, _)
  | (true, false, Test_case.Small)
  | (false, true, Test_case.Large) -> true
  | _ -> false

let matches_flaky = fun ~flaky_only (test: Test_case.t) ->
  not flaky_only || match test.reliability with
  | Test_case.Stable -> false
  | Test_case.Flaky _ -> true

let filtered_tests = fun ~query ~small_only ~large_only ~flaky_only tests ->
  List.filter
    tests
    ~fn:(fun (test: Test_case.t) ->
      matches_query query test
      && matches_size ~small_only ~large_only test
      && matches_flaky ~flaky_only test)

let write_tests_json = fun tests ->
  let rec to_json_items index = fun __tmp1 ->
    match __tmp1 with
    | [] -> []
    | (test: Test_case.t) :: rest ->
        let base_fields = [
          ("index", Data.Json.Int index);
          ("name", Data.Json.String test.name);
          ("size", size_to_json test.size);
          ("skip", Data.Json.Bool test.skip);
        ]
        in
        Data.Json.Object (((base_fields @ test_type_fields test.test_type)
        @ reliability_fields test.reliability)
        @ fuzz_metadata_fields test)
        :: to_json_items (index + 1) rest
  in
  let tests_json = to_json_items 1 tests in
  println (Data.Json.to_string (Data.Json.Object [ ("tests", Data.Json.Array tests_json); ]))

let ctx_json_arg = "--ctx"

type suite_ctx = {
  source_file: Path.t option;
  binary_path: Path.t option;
  workspace_root: Path.t option;
  package_name: string option;
  built_binaries: Test_context.built_binary list;
}

let empty_suite_ctx = {
  source_file = None;
  binary_path = None;
  workspace_root = None;
  package_name = None;
  built_binaries = [];
}

let suite_ctx_of_json = fun value ->
  let built_binary_of_json = fun __tmp1 ->
    match __tmp1 with
    | Data.Json.Object fields -> (
        match (
          List.find fields ~fn:(fun (name, _) -> String.equal name "name"),
          List.find fields ~fn:(fun (name, _) -> String.equal name "path")
        ) with
        | (Some (_, Data.Json.String name), Some (_, Data.Json.String path)) ->
            Some Test_context.{ name; path = Path.v path }
        | _ -> None
      )
    | _ -> None
  in
  match Data.Json.from_string value with
  | Ok (Data.Json.Object fields) ->
      let path_field name =
        match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name name) with
        | Some (_, Data.Json.String path) -> Some (Path.v path)
        | _ -> None
      in
      let string_field name =
        match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name name) with
        | Some (_, Data.Json.String value) -> Some value
        | _ -> None
      in
      let built_binaries =
        match List.find fields ~fn:(fun (name, _) -> String.equal name "built_binaries") with
        | Some (_, Data.Json.Array items) -> List.filter_map items ~fn:built_binary_of_json
        | _ -> []
      in
      {
        source_file = path_field "source_file";
        binary_path = path_field "binary_path";
        workspace_root = path_field "workspace_root";
        package_name = string_field "package_name";
        built_binaries;
      }
  | Error _
  | Ok _ -> empty_suite_ctx

let suite_ctx_from_args = fun args ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> empty_suite_ctx
    | flag :: value :: _ when String.equal flag ctx_json_arg -> suite_ctx_of_json value
    | arg :: _ when String.starts_with ~prefix:(ctx_json_arg ^ "=") arg ->
        let prefix_len = String.length ctx_json_arg + 1 in
        let len = String.length arg - prefix_len in
        suite_ctx_of_json (String.sub arg ~offset:prefix_len ~len)
    | _ :: rest -> loop rest
  in
  loop args

let list_tests = fun ~json tests ->
  if json then
    write_tests_json tests
  else
    List.for_each tests ~fn:(fun (test: Test_case.t) -> println test.name);
  Ok ()

let parse_format_to_reporter = fun __tmp1 ->
  match __tmp1 with
  | "tap" -> Ok (module Reporter.TAP : Reporter.Intf)
  | "json" -> Ok (module Reporter.JSON : Reporter.Intf)
  | "junit" -> Ok (module Reporter.JUnit : Reporter.Intf)
  | "pretty" -> Ok (module Reporter.Pretty : Reporter.Intf)
  | "minimal" -> Ok (module Reporter.Minimal : Reporter.Intf)
  | other -> Error ("Unknown format: " ^ other)

let default_concurrency = Int.max 1 Thread.available_parallelism

let run_tests_cmd =
  let open Arg_parser.Arg in
  command "run-tests"
  |> about "Run tests that match query"
  |> args
    [
      positional "query"
      |> required false
      |> help "Test name substring to filter by";
      flag "json"
      |> long "json"
      |> help "Emit machine-readable JSON output";
      option "format"
      |> long "format"
      |> help "Output format: tap, json, junit, pretty, minimal"
      |> default "pretty"
      |> possible_values [ "tap"; "json"; "junit"; "pretty"; "minimal"; ];
      flag "shuffle"
      |> long "shuffle"
      |> help "Run tests in random order";
      option "concurrency"
      |> long "concurrency"
      |> help "Number of concurrent workers";
      flag "small"
      |> long "small"
      |> help "Run only tests marked small";
      flag "large"
      |> long "large"
      |> help "Run only tests marked large";
      flag "flaky"
      |> long "flaky"
      |> help "Run only tests marked flaky";
      option "flaky-max-retries"
      |> long "flaky-max-retries"
      |> help "Retry budget for tests marked flaky";
      option "pattern"
      |> long "pattern"
      |> help "Deprecated alias for the positional query argument";
      option "ctx"
      |> long "ctx"
      |> help "Structured runner context JSON";
    ]

let list_tests_cmd =
  let open Arg_parser.Arg in
  command "list-tests"
  |> about "List all tests"
  |> args
    [
      positional "query"
      |> required false
      |> help "Test name substring to filter by";
      flag "json"
      |> long "json"
      |> help "Emit machine-readable JSON output";
      flag "small"
      |> long "small"
      |> help "List only tests marked small";
      flag "large"
      |> long "large"
      |> help "List only tests marked large";
      flag "flaky"
      |> long "flaky"
      |> help "List only tests marked flaky";
      option "pattern"
      |> long "pattern"
      |> help "Deprecated alias for the positional query argument";
      option "ctx"
      |> long "ctx"
      |> help "Structured runner context JSON";
    ]

let run_fuzz_case_cmd =
  let open Arg_parser.Arg in
  command "run-fuzz-case"
  |> about "Run one fuzz case with one input"
  |> args
    [
      positional "query"
      |> required true
      |> help "Fuzz case name substring";
      option "input"
      |> long "input"
      |> help "Path to the fuzz input file";
      flag "json"
      |> long "json"
      |> help "Emit machine-readable JSON output";
      option "ctx"
      |> long "ctx"
      |> help "Structured runner context JSON";
    ]

let get_suite_info name args: Reporter.suite_info =
  let suite_ctx = suite_ctx_from_args args in
  let fallback_binary_path =
    Env.args
    |> List.head
    |> Option.unwrap_or ~default:name
    |> Path.v
  in
  {
    name;
    source_file = suite_ctx.source_file;
    binary_path = Option.or_ suite_ctx.binary_path (Some fallback_binary_path);
    workspace_root = suite_ctx.workspace_root;
    package_name = suite_ctx.package_name;
    built_binaries = suite_ctx.built_binaries;
  }

let render_hook_exception = fun hook_name exn ->
  let exn = Exception.to_string exn in
  let bt = Exception.raw_backtrace_to_string (Exception.get_raw_backtrace ()) in
  hook_name ^ " hook raised: " ^ exn ^ "\n\n" ^ bt

let run_suite_hook = fun hook_name hook ->
  match hook with
  | None -> Ok ()
  | Some hook -> (
      match hook () with
      | Ok () -> Ok ()
      | Error message -> Error message
      | exception exn -> Error (render_hook_exception hook_name exn)
    )

let suite_setup_failure_result = fun message ->
  Test_result.{
    index = 1;
    name = "suite setup";
    test_type = Test_case.UnitTest;
    size = Test_case.Small;
    reliability = Test_case.Stable;
    attempts = 1;
    result = Failed message;
    duration = Time.Duration.zero;
  }

let report_setup_failure = fun ~(reporter:(module Reporter.Intf)) ~suite_info message ->
  let module R = (val reporter : Reporter.Intf) in
  let result = suite_setup_failure_result message in
  let summary = Test_result.make_summary [ result ] in
  R.init suite_info 1;
  R.on_result 1 result;
  R.finalize summary;
  summary

let report_teardown_failure = fun ~(reporter:(module Reporter.Intf)) message ->
  let module R = (val reporter : Reporter.Intf) in
  R.warn ("test suite teardown failed: " ^ message)

let fuzz_ctx = fun ~(suite_info:Reporter.suite_info) ~index (test: Test_case.t) ->
  Test_context.{
    suite_name = suite_info.name;
    test_name = test.name;
    test_index = index;
    source_file = suite_info.source_file;
    binary_path = suite_info.binary_path;
    built_binaries = suite_info.built_binaries;
    workspace_root = suite_info.workspace_root;
    package_name = suite_info.package_name;
    fixture = None;
    progress_handler = Test_context.no_progress_handler;
  }

let fuzz_input_from_matches = fun sub_matches ->
  match get_one sub_matches "input" with
  | None -> Ok ""
  | Some path ->
      Fs.read (Path.v path)
      |> Result.map_err ~fn:IO.error_message

let write_fuzz_case_json = fun ~name ~status ?message () ->
  let fields = [
    ("type", Data.Json.String "FuzzCaseCompleted");
    ("name", Data.Json.String name);
    ("status", Data.Json.String status);
  ]
  in
  let fields =
    match message with
    | None -> fields
    | Some message -> fields @ [ ("message", Data.Json.String message); ]
  in
  println (Data.Json.to_string (Data.Json.Object fields))

let run_fuzz_case = fun ~suite_info ~json ~query ~input tests ->
  let fuzz_tests =
    tests
    |> List.enumerate
    |> List.filter_map
      ~fn:(fun (idx, (test: Test_case.t)) ->
        match test.fuzz_fn with
        | Some fuzz_fn when String.contains test.name query -> Some (idx + 1, test, fuzz_fn)
        | Some _
        | None -> None)
  in
  match fuzz_tests with
  | [] ->
      let message = "no fuzz case matched '" ^ query ^ "'" in
      if json then
        write_fuzz_case_json ~name:query ~status:"not_found" ~message ()
      else
        eprintln message;
      System.exit 2
  | _ :: _ :: _ ->
      let message = "fuzz case query matched multiple cases: " ^ query in
      if json then
        write_fuzz_case_json ~name:query ~status:"ambiguous" ~message ()
      else
        eprintln message;
      System.exit 2
  | [ (index, test, fuzz_fn) ] ->
      let ctx = fuzz_ctx ~suite_info ~index test in
      let result =
        try fuzz_fn ctx input with
        | exn -> Error (Exception.to_string exn)
      in
      match result with
      | Ok () ->
          if json then
            write_fuzz_case_json ~name:test.name ~status:"passed" ();
          Ok ()
      | Error message ->
          if json then
            write_fuzz_case_json ~name:test.name ~status:"failed" ~message ()
          else
            eprintln message;
          System.exit 1

let run_tests_with_hooks = fun ?setup ?teardown ~(config:Runner.config) tests ->
  let selected_tests = Runner.filter_tests config.target tests in
  if List.is_empty selected_tests then
    Runner.run_tests ~config tests
  else
    match run_suite_hook "setup" setup with
    | Error message ->
        report_setup_failure ~reporter:config.reporter ~suite_info:config.suite_info message
    | Ok () ->
        let teardown_ran = ref false in
        let run_teardown_once () =
          if not !teardown_ran then (
            teardown_ran := true;
            match run_suite_hook "teardown" teardown with
            | Ok () -> ()
            | Error message -> report_teardown_failure ~reporter:config.reporter message
          )
        in
        let summary =
          match Runner.run_tests ~config tests with
          | summary -> summary
          | exception exn ->
              run_teardown_once ();
              raise exn
        in
        run_teardown_once ();
        summary

let main = fun ?(execution_mode = Concurrent) ?setup ?teardown ~name ~tests ~args () ->
  let suite_info = get_suite_info name args in
  let cmd =
    command name
    |> about ("Test runner for " ^ name)
    |> subcommands [ list_tests_cmd; run_tests_cmd; run_fuzz_case_cmd ]
  in
  match get_matches cmd args with
  | Error err ->
      print_error err;
      Error (Failure (error_message err))
  | Ok matches -> (
      match get_subcommand matches with
      | Some ("list-tests", sub_matches) ->
          let small_only = get_flag sub_matches "small" in
          let large_only = get_flag sub_matches "large" in
          if small_only && large_only then
            Error (Failure "Cannot combine --small and --large")
          else
            let flaky_only = get_flag sub_matches "flaky" in
            let query =
              match get_one sub_matches "query" with
              | Some query -> Some query
              | None -> get_one sub_matches "pattern"
            in
            filtered_tests ~query ~small_only ~large_only ~flaky_only tests
            |> list_tests ~json:(get_flag sub_matches "json")
      | Some ("run-tests", sub_matches) -> (
          let format_str =
            if get_flag sub_matches "json" then
              "json"
            else
              get_one sub_matches "format"
              |> Option.unwrap_or ~default:"pretty"
          in
          match parse_format_to_reporter format_str with
          | Error msg ->
              println ("Error: " ^ msg);
              Error (Failure msg)
          | Ok reporter ->
              let shuffle = get_flag sub_matches "shuffle" in
              let requested_concurrency =
                get_int sub_matches "concurrency"
                |> Option.unwrap_or ~default:default_concurrency
              in
              let small_only = get_flag sub_matches "small" in
              let large_only = get_flag sub_matches "large" in
              let flaky_only = get_flag sub_matches "flaky" in
              let query =
                match get_one sub_matches "query" with
                | Some query -> Some query
                | None -> get_one sub_matches "pattern"
              in
              if small_only && large_only then
                Error (Failure "Cannot combine --small and --large")
              else
                let size_filter =
                  if small_only then
                    Runner.Only_small
                  else if large_only then
                    Runner.Only_large
                  else
                    Runner.All_sizes
                in
                let flaky_max_retries =
                  get_int sub_matches "flaky-max-retries"
                  |> Option.unwrap_or ~default:0
                in
                let target = Runner.{ query; size_filter; flaky_only } in
                let mode =
                  match execution_mode with
                  | Linear -> Runner.Shuffle
                  | Concurrent ->
                      if shuffle then
                        Runner.Shuffle
                      else
                        Runner.Sequential
                in
                let concurrency =
                  match execution_mode with
                  | Linear -> 1
                  | Concurrent -> requested_concurrency
                in
                let config =
                  Runner.{
                    concurrency;
                    reporter;
                    mode;
                    target;
                    policy = { Runner.default_policy with flaky_max_retries };
                    suite_info;
                    event_handler =
                      if String.equal format_str "json" then
                        json_event_handler
                      else
                        Runner.no_event_handler;
                  }
                in
                let summary = run_tests_with_hooks ?setup ?teardown ~config tests in
                if summary.failed > 0 then
                  System.exit 1;
              Ok ()
        )
      | Some ("run-fuzz-case", sub_matches) -> (
          let query =
            get_one sub_matches "query"
            |> Option.unwrap_or ~default:""
          in
          let json = get_flag sub_matches "json" in
          match fuzz_input_from_matches sub_matches with
          | Error message ->
              if json then
                write_fuzz_case_json ~name:query ~status:"input_error" ~message ()
              else
                eprintln message;
              Error (Failure message)
          | Ok input -> run_fuzz_case ~suite_info ~json ~query ~input tests
        )
      | _ ->
          let reporter = (module Reporter.Pretty : Reporter.Intf) in
          let mode =
            match execution_mode with
            | Linear -> Runner.Shuffle
            | Concurrent -> Runner.Sequential
          in
          let concurrency =
            match execution_mode with
            | Linear -> 1
            | Concurrent -> default_concurrency
          in
          let config =
            Runner.{
              concurrency;
              reporter;
              mode;
              target = { query = None; size_filter = All_sizes; flaky_only = false };
              policy = default_policy;
              suite_info;
              event_handler = Runner.no_event_handler;
            }
          in
          let summary = run_tests_with_hooks ?setup ?teardown ~config tests in
          if summary.failed > 0 then
            System.exit 1;
          Ok ()
    )

open Std
open Propane
module Test = Std.Test

let flaky_counter = Sync.Atomic.make 0

let make_overlap_probe = fun counter name ->
  Test.case ~size:Test.Large name
    (fun _ctx ->
      let _ = Sync.Atomic.fetch_and_add counter 1 in
      let started = Time.Instant.now () in
      let rec wait_for_peer () =
        if Sync.Atomic.get counter >= 2 then
          Ok ()
        else if Time.Duration.to_millis (Time.Instant.elapsed started) >= 250 then
          Error "expected tests to overlap under concurrency"
        else (
          sleep (Time.Duration.from_millis 5);
          wait_for_peer ()
        )
      in
      wait_for_peer ())

let overlap_probe_counter = Sync.Atomic.make 0

let make_linear_probe = fun counter name ->
  Test.case ~size:Test.Large name
    (fun _ctx ->
      let previous = Sync.Atomic.fetch_and_add counter 1 in
      let result =
        if previous = 0 then
          (
            sleep (Time.Duration.from_millis 20);
            Ok ()
          )
        else
          Error "expected linear suite to run one test at a time"
      in
      let _ = Sync.Atomic.fetch_and_add counter (-1) in
      result)

let linear_probe_counter = Sync.Atomic.make 0

let sample_tests = [
  Test.case ~size:Test.Large "alpha_large" (fun _ctx -> Ok ());
  Test.case ~size:Test.Large "large_timeout_probe"
    (fun _ctx ->
      sleep (Time.Duration.from_secs 2);
      Ok ());
  Test.case "beta" (fun _ctx -> Ok ());
  Propane.property "gamma_property" Arbitrary.int (fun _ -> true);
  Test.case
    "inline_snapshot_probe"
    (fun ctx -> Test.Snapshot.assert_inline_text ~ctx ~actual:"inline snapshot\n" ~expected:"inline snapshot\n");
  Test.case ~size:Test.Large "middle_large_case" (fun _ctx -> Ok ());
  make_overlap_probe overlap_probe_counter "concurrency_probe_alpha";
  make_overlap_probe overlap_probe_counter "concurrency_probe_beta";
  Test.case ~size:Test.Large "ordered_slow_first"
    (fun _ctx ->
      sleep (Time.Duration.from_millis 30);
      Ok ());
  Test.case ~size:Test.Large "ordered_fast_second" (fun _ctx -> Ok ());
  Test.case ~reliability:Test.(Flaky { retry_attempts = 2 }) "flaky_then_ok"
    (fun _ctx ->
      if Sync.Atomic.fetch_and_add flaky_counter 1 = 0 then
        Error "transient failure"
      else
        Ok ());
  Test.case "timeout_probe"
    (fun _ctx ->
      sleep (Time.Duration.from_secs 2);
      Ok ());
  Test.case "ctx_probe"
    (fun ctx ->
      match (ctx.package_name, ctx.workspace_root, Test.Context.find_binary ctx "demo") with
      | Some "demo", Some workspace_root, Some binary_path when String.equal
        (Path.to_string workspace_root)
        "/tmp/demo-workspace"
      && String.equal (Path.to_string binary_path) "/tmp/demo-bin" -> Ok ()
      | _ -> Error "expected ctx_probe to receive structured context from --ctx");
  Test.case "after_timeout" (fun _ctx -> Ok ());
]

let linear_tests = [
  make_linear_probe linear_probe_counter "linear_probe_alpha";
  make_linear_probe linear_probe_counter "linear_probe_beta";
]

let self_executable = fun () ->
  match Env.args with
  | exe :: _ -> exe
  | [] -> panic "missing argv[0] for std_test_cli_tests"

let split_lines = fun output ->
  output |> String.split ~by:"\n" |> List.filter ~fn:(fun line -> not (String.equal line ""))

let parse_json_output = fun stdout -> Data.Json.of_string stdout |> Result.expect ~msg:"failed to parse json output"

let parse_json_lines = fun stdout ->
  split_lines stdout
  |> List.map
    ~fn:(fun line -> Data.Json.of_string line |> Result.expect ~msg:"failed to parse jsonl line")

let json_type = fun json ->
  match Data.Json.get_field "type" json with
  | Some (Data.Json.String value) -> Some value
  | _ -> None

let last_summary_json = fun stdout ->
  parse_json_lines stdout
  |> List.reverse
  |> List.find ~fn:(fun json -> json_type json = Some "TestSummary")
  |> Option.expect ~msg:"expected a final TestSummary json line"

let assoc_value = fun key entries ->
  match
    List.find entries
      ~fn:(fun (entry_key, _) ->
        String.equal entry_key key)
  with
  | Some (_, value) -> Some value
  | None -> None

let test_names_from_json = fun stdout ->
  let json = last_summary_json stdout in
  match Data.Json.get_field "tests" json with
  | Some (Data.Json.Array tests) ->
      tests |> List.filter_map
        ~fn:(fun test_json ->
          match Data.Json.get_field "name" test_json with
          | Some (Data.Json.String name) -> Some name
          | _ -> None)
  | _ -> []

let listed_test_fields_from_json = fun stdout ->
  let json = parse_json_output stdout in
  match Data.Json.get_field "tests" json with
  | Some (Data.Json.Array tests) ->
      tests |> List.filter_map
        ~fn:(
          function
          | Data.Json.Object fields -> Some fields
          | _ -> None
        )
  | _ -> []

let run_sample_capture = fun args ->
  let cmd = Command.make
    (self_executable ())
    ~env:[ ("PROPANE_TESTS", "7") ]
    ~args:("sample" :: args) in
  Command.output cmd |> Result.expect ~msg:"failed to run sample test cli"

let run_linear_sample_capture = fun args ->
  let cmd = Command.make
    (self_executable ())
    ~env:[ ("PROPANE_TESTS", "7") ]
    ~args:("sample-linear" :: args) in
  Command.output cmd |> Result.expect ~msg:"failed to run linear sample test cli"

let test_list_tests_lists_all_cases = fun _ctx ->
  let output = run_sample_capture [ "list-tests" ] in
  if not (Int.equal output.status 0) then
    Error ("expected list-tests to succeed, got " ^ Int.to_string output.status)
  else
    let names = split_lines output.stdout |> List.sort ~compare:String.compare in
    let expected =
      [
        "alpha_large";
        "large_timeout_probe";
        "beta";
        "gamma_property";
        "inline_snapshot_probe";
        "middle_large_case";
        "concurrency_probe_alpha";
        "concurrency_probe_beta";
        "ordered_slow_first";
        "ordered_fast_second";
        "flaky_then_ok";
        "timeout_probe";
        "after_timeout"
      ]
      |> List.sort ~compare:String.compare
    in
    if names = expected then
      Ok ()
    else
      Error ("unexpected listed test names: " ^ String.concat ", " names)

let test_list_tests_json_includes_metadata = fun _ctx ->
  let output = run_sample_capture [ "list-tests"; "--json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected list-tests --json to succeed, got " ^ Int.to_string output.status)
  else
    match listed_test_fields_from_json output.stdout with
    | first :: _ ->
        let has name value = assoc_value name first = Some value in
        if
          has "index" (Data.Json.Int 1)
          && has "name" (Data.Json.String "alpha_large")
          && has "type" (Data.Json.String "test")
          && has "size" (Data.Json.String "large")
          && has "reliability" (Data.Json.String "stable")
          && has "skip" (Data.Json.Bool false)
        then
          Ok ()
        else
          Error "expected list-tests --json to include metadata fields"
    | [] -> Error "expected list-tests --json to include tests"

let test_list_tests_respects_filters = fun _ctx ->
  let output = run_sample_capture [ "list-tests"; "--json"; "--flaky"; "flaky" ] in
  if not (Int.equal output.status 0) then
    Error ("expected filtered list-tests --json to succeed, got " ^ Int.to_string output.status)
  else
    let names =
      listed_test_fields_from_json output.stdout
      |> List.filter_map ~fn:(assoc_value "name")
      |> List.filter_map
        ~fn:(
          function
          | Data.Json.String name -> Some name
          | _ -> None
        )
    in
    if names = [ "flaky_then_ok" ] then
      Ok ()
    else
      Error ("unexpected filtered list test names: " ^ String.concat ", " names)

let test_run_tests_pattern_matches_suffix_substring = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "_large"; "--format"; "json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected filtered run to succeed, got " ^ Int.to_string output.status)
  else
    let names = test_names_from_json output.stdout |> List.sort ~compare:String.compare in
    let expected = [ "alpha_large"; "middle_large_case" ] |> List.sort ~compare:String.compare in
    if names = expected then
      Ok ()
    else
      Error ("unexpected filtered names for _large: " ^ String.concat ", " names)

let test_run_tests_pattern_matches_middle_substring = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "large_case"; "--format"; "json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected filtered run to succeed, got " ^ Int.to_string output.status)
  else
    let names = test_names_from_json output.stdout in
    if names = [ "middle_large_case" ] then
      Ok ()
    else
      Error ("unexpected filtered names for large_case: " ^ String.concat ", " names)

let test_run_tests_returns_success_with_zero_matches = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "missing_case"; "--format"; "json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected filtered run with no matches to succeed, got " ^ Int.to_string output.status)
  else if test_names_from_json output.stdout = [] then
    Ok ()
  else
    Error "expected filtered run with no matches to report an empty test list"

let test_run_tests_json_flag_alias_emits_json = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "_large"; "--json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected --json run to succeed, got " ^ Int.to_string output.status)
  else
    let names = test_names_from_json output.stdout |> List.sort ~compare:String.compare in
    let expected = [ "alpha_large"; "middle_large_case" ] |> List.sort ~compare:String.compare in
    if names = expected then
      Ok ()
    else
      Error ("unexpected filtered names for --json: " ^ String.concat ", " names)

let test_run_tests_small_flag_filters_small_tests = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "--small"; "--format"; "json" ] in
  if Int.equal output.status 0 then
    Error "expected --small run to fail because timeout_probe is small and exceeds 500ms"
  else
    let names = test_names_from_json output.stdout |> List.sort ~compare:String.compare in
    let expected = [
      "beta";
      "gamma_property";
      "inline_snapshot_probe";
      "flaky_then_ok";
      "timeout_probe";
      "after_timeout"
    ]
    |> List.sort ~compare:String.compare in
    if names = expected then
      Ok ()
    else
      Error ("unexpected filtered names for --small: " ^ String.concat ", " names)

let test_run_tests_large_flag_filters_large_tests = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "--large"; "--format"; "json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected --large run to succeed, got " ^ Int.to_string output.status)
  else
    let names = test_names_from_json output.stdout |> List.sort ~compare:String.compare in
    let expected = [
      "alpha_large";
      "large_timeout_probe";
      "middle_large_case";
      "concurrency_probe_alpha";
      "concurrency_probe_beta";
      "ordered_slow_first";
      "ordered_fast_second";
    ]
    |> List.sort ~compare:String.compare in
    if names = expected then
      Ok ()
    else
      Error ("unexpected filtered names for --large: " ^ String.concat ", " names)

let test_run_tests_flaky_flag_filters_flaky_tests = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "--flaky"; "--format"; "json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected --flaky run to succeed, got " ^ Int.to_string output.status)
  else if test_names_from_json output.stdout = [ "flaky_then_ok" ] then
    Ok ()
  else
    Error ("unexpected filtered names for --flaky: "
    ^ String.concat ", " (test_names_from_json output.stdout))

let test_run_tests_json_includes_timing_fields = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "_large"; "--json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected --json run to succeed, got " ^ Int.to_string output.status)
  else
    let json = last_summary_json output.stdout in
    let has_int_field name json =
      match Data.Json.get_field name json with
      | Some (Data.Json.Int _) -> true
      | _ -> false
    in
    let tests_have_duration =
      match Data.Json.get_field "tests" json with
      | Some (Data.Json.Array tests) -> List.all
        tests
        ~fn:(fun test_json -> has_int_field "duration_us" test_json)
      | _ -> false
    in
    let summary_has_duration =
      match Data.Json.get_field "summary" json with
      | Some summary_json -> has_int_field "duration_us" summary_json
      | None -> false
    in
    if
      has_int_field "started_at_us" json
      && has_int_field "completed_at_us" json
      && has_int_field "duration_us" json
      && summary_has_duration
      && tests_have_duration
    then
      Ok ()
    else
      Error "expected test json output to include timing fields"

let test_run_tests_json_includes_reliability_metadata = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "flaky_then_ok"; "--json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected flaky --json run to succeed, got " ^ Int.to_string output.status)
  else
    let json = last_summary_json output.stdout in
    match Data.Json.get_field "tests" json with
    | Some (Data.Json.Array [ Data.Json.Object fields ]) ->
        let has name value = assoc_value name fields = Some value in
        if
          has "size" (Data.Json.String "small")
          && has "reliability" (Data.Json.String "flaky")
          && has "retry_attempts" (Data.Json.Int 2)
          && has "attempts" (Data.Json.Int 2)
          && has "status" (Data.Json.String "passed")
        then
          Ok ()
        else
          Error "expected flaky json output to include reliability metadata"
    | _ -> Error "expected exactly one flaky test in json output"

let test_run_tests_small_timeout_reports_timed_out = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "timeout_probe"; "--small"; "--json" ] in
  if Int.equal output.status 0 then
    Error "expected timeout probe to fail"
  else
    let json = last_summary_json output.stdout in
    match Data.Json.get_field "tests" json with
    | Some (Data.Json.Array [ Data.Json.Object fields ]) ->
        let has name value = assoc_value name fields = Some value in
        if has "status" (Data.Json.String "timed_out") && has "timeout_ms" (Data.Json.Int 500) then
          Ok ()
        else
          Error "expected timeout probe json output to report a timeout"
    | _ -> Error "expected exactly one timeout probe test in json output"

let test_run_tests_json_emits_lifecycle_events = fun _ctx ->
  let events = parse_json_lines (run_sample_capture [ "run-tests"; "gamma_property"; "--json" ]).stdout in
  let event_types = events |> List.filter_map ~fn:json_type in
  let has_event name = List.contains event_types ~value:name in
  if
    has_event "TestSuiteStarted"
    && has_event "TestCaseStarted"
    && has_event "TestCaseAttemptStarted"
    && has_event "TestCaseAttemptFinished"
    && has_event "TestCaseCompleted"
    && has_event "TestSummary"
  then
    Ok ()
  else
    Error ("expected lifecycle json events, got: " ^ String.concat ", " event_types)

let test_run_tests_json_emits_property_metadata = fun _ctx ->
  let events = parse_json_lines (run_sample_capture [ "run-tests"; "gamma_property"; "--json" ]).stdout in
  match List.find events ~fn:(fun json -> json_type json = Some "TestCaseStarted") with
  | Some (Data.Json.Object fields) ->
      let has name value = assoc_value name fields = Some value in
      if
        has "name" (Data.Json.String "gamma_property")
        && has "test_type" (Data.Json.String "property")
        && has "examples" (Data.Json.Int 7)
      then
        Ok ()
      else
        Error "expected property test metadata in TestCaseStarted event"
  | _ -> Error "expected a TestCaseStarted event for gamma_property"

let test_run_tests_json_emits_property_progress = fun _ctx ->
  let events = parse_json_lines (run_sample_capture [ "run-tests"; "gamma_property"; "--json" ]).stdout in
  let progress_events =
    events
    |> List.filter_map
      ~fn:(fun json ->
        match (json_type json, Data.Json.get_field "progress_type" json) with
        | (Some "TestCaseProgress", Some (Data.Json.String progress_type)) -> Some progress_type
        | _ -> None)
  in
  let property_passes =
    List.filter progress_events
      ~fn:(fun progress_type ->
        String.equal progress_type "property_iteration_passed")
  in
  if Int.equal (List.length property_passes) 7 then
    Ok ()
  else
    Error ("expected 7 property progress events, got: " ^ String.concat ", " progress_events)

let test_run_tests_json_emits_snapshot_progress = fun _ctx ->
  let events = parse_json_lines
    (run_sample_capture [ "run-tests"; "inline_snapshot_probe"; "--json" ]).stdout in
  let progress_events =
    events
    |> List.filter_map
      ~fn:(fun json ->
        match (json_type json, Data.Json.get_field "progress_type" json) with
        | (Some "TestCaseProgress", Some (Data.Json.String progress_type)) -> Some progress_type
        | _ -> None)
  in
  if
    List.contains progress_events ~value:"snapshot_assertion_started"
    && List.contains progress_events ~value:"snapshot_assertion_matched"
  then
    Ok ()
  else
    Error ("expected snapshot progress events, got: " ^ String.concat ", " progress_events)

let test_run_tests_json_emits_heartbeat_for_long_tests = fun _ctx ->
  let events = parse_json_lines
    (run_sample_capture [ "run-tests"; "large_timeout_probe"; "--json" ]).stdout in
  if List.exists (fun json -> json_type json = Some "TestCaseHeartbeat") events then
    Ok ()
  else
    Error "expected a TestCaseHeartbeat event for a long-running test"

let test_run_tests_ctx_flag_populates_structured_context = fun _ctx ->
  let ctx_json = "{\"workspace_root\":\"/tmp/demo-workspace\",\
     \"package_name\":\"demo\",\
     \"binary_path\":\"/tmp/sample-suite\",\
     \"source_file\":\"/tmp/sample-suite.ml\",\
     \"built_binaries\":[{\"name\":\"demo\",\"path\":\"/tmp/demo-bin\"}]}"
  in
  let output = run_sample_capture [ "run-tests"; "ctx_probe"; "--ctx"; ctx_json ] in
  if Int.equal output.status 0 then
    Ok ()
  else
    Error ("expected --ctx run to succeed, got " ^ Int.to_string output.status ^ ": " ^ output.stdout)

let test_run_tests_timeout_does_not_abort_suite = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "--small"; "--json" ] in
  if Int.equal output.status 0 then
    Error "expected timed out small-test run to fail overall"
  else
    let names = test_names_from_json output.stdout |> List.sort ~compare:String.compare in
    let expected = [
      "beta";
      "gamma_property";
      "inline_snapshot_probe";
      "flaky_then_ok";
      "timeout_probe";
      "after_timeout"
    ]
    |> List.sort ~compare:String.compare in
    if not (names = expected) then
      Error ("expected suite to continue after timeout, got: " ^ String.concat ", " names)
    else
      let json = last_summary_json output.stdout in
      match Data.Json.get_field "tests" json with
      | Some (Data.Json.Array tests) ->
          let find_status name =
            tests
            |> List.filter_map
              ~fn:(fun test_json ->
                match (Data.Json.get_field "name" test_json, Data.Json.get_field "status" test_json) with
                | (Some (Data.Json.String test_name), Some (Data.Json.String status)) when String.equal
                  test_name
                  name -> Some status
                | _ -> None)
            |> List.head
          in
          if
            find_status "timeout_probe" = Some "timed_out" && find_status "after_timeout" = Some "passed"
          then
            Ok ()
          else
            Error "expected timeout_probe to time out and after_timeout to still run"
      | _ -> Error "expected tests array in final TestSummary"

let test_run_tests_concurrency_flag_allows_overlap = fun _ctx ->
  let output = run_sample_capture
    [ "run-tests"; "concurrency_probe"; "--json"; "--concurrency"; "2" ] in
  if not (Int.equal output.status 0) then
    Error ("expected concurrency probe run to succeed, got " ^ Int.to_string output.status)
  else
    let names = test_names_from_json output.stdout |> List.sort ~compare:String.compare in
    let expected = [ "concurrency_probe_alpha"; "concurrency_probe_beta" ] in
    if names = expected then
      Ok ()
    else
      Error ("unexpected concurrency probe names: " ^ String.concat ", " names)

let test_run_tests_concurrency_keeps_summary_order = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "ordered_"; "--json"; "--concurrency"; "2" ] in
  if not (Int.equal output.status 0) then
    Error ("expected ordered concurrency run to succeed, got " ^ Int.to_string output.status)
  else
    let names = test_names_from_json output.stdout in
    if names = [ "ordered_slow_first"; "ordered_fast_second" ] then
      Ok ()
    else
      Error ("expected ordered summary results, got: " ^ String.concat ", " names)

let test_run_tests_linear_suite_forces_serial_execution = fun _ctx ->
  let output = run_linear_sample_capture
    [ "run-tests"; "linear_probe"; "--json"; "--concurrency"; "2" ] in
  if not (Int.equal output.status 0) then
    Error ("expected linear suite run to succeed, got " ^ Int.to_string output.status)
  else
    let names = test_names_from_json output.stdout |> List.sort ~compare:String.compare in
    if names = [ "linear_probe_alpha"; "linear_probe_beta" ] then
      Ok ()
    else
      Error ("unexpected linear probe names: " ^ String.concat ", " names)

let meta_tests = [
  Test.case ~size:Large "list-tests lists all sample cases" test_list_tests_lists_all_cases;
  Test.case ~size:Large "list-tests --json includes metadata" test_list_tests_json_includes_metadata;
  Test.case ~size:Large "list-tests respects filters" test_list_tests_respects_filters;
  Test.case ~size:Large "run-tests pattern matches suffix substring" test_run_tests_pattern_matches_suffix_substring;
  Test.case ~size:Large "run-tests pattern matches middle substring" test_run_tests_pattern_matches_middle_substring;
  Test.case ~size:Large "run-tests succeeds when the query matches no tests" test_run_tests_returns_success_with_zero_matches;
  Test.case ~size:Large "run-tests --json alias emits json" test_run_tests_json_flag_alias_emits_json;
  Test.case ~size:Large "run-tests --small filters small tests" test_run_tests_small_flag_filters_small_tests;
  Test.case ~size:Large "run-tests --large filters large tests" test_run_tests_large_flag_filters_large_tests;
  Test.case ~size:Large "run-tests --flaky filters flaky tests" test_run_tests_flaky_flag_filters_flaky_tests;
  Test.case ~size:Large "run-tests --json includes timing fields" test_run_tests_json_includes_timing_fields;
  Test.case ~size:Large "run-tests --json includes reliability metadata" test_run_tests_json_includes_reliability_metadata;
  Test.case ~size:Large "run-tests applies the default small-test timeout" test_run_tests_small_timeout_reports_timed_out;
  Test.case ~size:Large "run-tests --json emits lifecycle events" test_run_tests_json_emits_lifecycle_events;
  Test.case ~size:Large "run-tests --json emits property metadata" test_run_tests_json_emits_property_metadata;
  Test.case ~size:Large "run-tests --json emits property progress" test_run_tests_json_emits_property_progress;
  Test.case ~size:Large "run-tests --json emits snapshot progress" test_run_tests_json_emits_snapshot_progress;
  Test.case ~size:Large "run-tests --json emits heartbeat for long tests" test_run_tests_json_emits_heartbeat_for_long_tests;
  Test.case ~size:Large "run-tests --ctx populates structured context" test_run_tests_ctx_flag_populates_structured_context;
  Test.case ~size:Large "run-tests timeout does not abort suite" test_run_tests_timeout_does_not_abort_suite;
  Test.case ~size:Large "run-tests --concurrency allows overlapping tests" test_run_tests_concurrency_flag_allows_overlap;
  Test.case ~size:Large "run-tests --concurrency keeps summary order" test_run_tests_concurrency_keeps_summary_order;
  Test.case ~size:Large "run-tests linear suites force serial execution" test_run_tests_linear_suite_forces_serial_execution;
]

let sample_main = fun ~args ->
  match args with
  | exe :: _sample :: rest -> Test.Cli.main ~name:"sample" ~tests:sample_tests ~args:(exe :: rest) ()
  | _ -> Error (Failure "expected sample subcommand arguments")

let linear_sample_main = fun ~args ->
  match args with
  | exe :: _sample :: rest -> Test.Cli.main
    ~execution_mode:Test.Cli.Linear
    ~name:"sample_linear"
    ~tests:linear_tests
    ~args:(exe :: rest)
    ()
  | _ -> Error (Failure "expected sample-linear subcommand arguments")

let meta_main = fun ~args ->
  let normalize_args = function
    | [] -> [ "std_test_cli_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  Test.Cli.main ~name:"std_test_cli_tests" ~tests:meta_tests ~args:(normalize_args args) ()

let main = fun ~args ->
  match args with
  | _ :: "sample" :: _ -> sample_main ~args
  | _ :: "sample-linear" :: _ -> linear_sample_main ~args
  | _ -> meta_main ~args

let () = Runtime.run ~main ~args:Env.args ()

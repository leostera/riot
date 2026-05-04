open Std
open Std.Result.Syntax
open Riot_model
open Riot_build
open ArgParser

module Vector = Collections.Vector

let command =
  let open ArgParser in
  let open ArgParser.Arg in
  command "test"
  |> about "Run tests with optional case filtering"
  |> ArgParser.allow_trailing_args
  |> args
    [
      option "package"
      |> short 'p'
      |> long "package"
      |> multiple
      |> help "Run tests from a specific package. Repeat to run multiple packages.";
      option "filter"
      |> short 'f'
      |> long "filter"
      |> help "Filter test suites and cases by substring within the selected packages";
      flag "list"
      |> long "list"
      |> help "List test suites and cases without running them";
      flag "release"
      |> long "release"
      |> help "Use the release build profile";
      flag "small"
      |> long "small"
      |> help "Run only tests marked small";
      flag "large"
      |> long "large"
      |> help "Run only tests marked large";
      flag "flaky"
      |> long "flaky"
      |> help "Run only tests marked flaky";
      flag "json"
      |> long "json"
      |> help "Emit machine-readable JSONL events";
      flag "verbose"
      |> short 'v'
      |> long "verbose"
      |> help "Enable verbose output for tests"
      |> count;
    ]

let trailing_args = fun matches ->
  let args = ArgParser.trailing_args matches in
  match args with
  | "--" :: rest -> rest
  | _ -> args

let profile_of_matches = fun matches ->
  if ArgParser.get_flag matches "release" then
    "release"
  else
    "debug"

let parse_package_names = fun package_names ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | package_name :: rest -> (
        match Riot_model.Package_name.from_string package_name with
        | Ok package_name -> loop (package_name :: acc) rest
        | Error error ->
            Error (Failure ("invalid package name '"
            ^ package_name
            ^ "': "
            ^ Riot_model.Package_name.error_message error))
      )
  in
  loop [] package_names

let print_command_output = fun (output: Command.output) ->
  if not (String.equal output.stdout "") then
    print output.stdout;
  if not (String.equal output.stderr "") then
    eprint output.stderr

let print_empty_hint = fun package_filter suite_filter ->
  match (package_filter, suite_filter) with
  | (Some package_name, Some suite_name) ->
      println
        ("No test suite '"
        ^ suite_name
        ^ "' found in package '"
        ^ Riot_model.Package_name.to_string package_name
        ^ "'")
  | (Some package_name, None) ->
      println
        ("No test suites found in package '" ^ Riot_model.Package_name.to_string package_name ^ "'")
  | (None, Some suite_name) -> println ("No test suites named '" ^ suite_name ^ "' found")
  | (None, None) -> println "No test binaries found"

let print_empty_list_hint = fun package_filter suite_filter query ->
  match query with
  | Some query -> println ("No tests matched query '" ^ query ^ "'")
  | None -> print_empty_hint package_filter suite_filter

type slow_test = {
  suite_label: string;
  test_name: string;
  duration_us: int;
  size: Test_runtime.test_case_size;
}

type failed_test = { suite_label: string; test_name: string; message: string; duration_us: int }

type timing_summary = {
  mutable measured_duration_us: int;
  mutable measured_test_count: int;
  mutable slowest_tests: slow_test list;
  failed_tests: failed_test Vector.t;
}

let empty_timing_summary = fun () ->
  {
    measured_duration_us = 0;
    measured_test_count = 0;
    slowest_tests = [];
    failed_tests = Vector.with_capacity ~size:8;
  }

let take = fun limit values ->
  let rec loop remaining acc rest =
    match (remaining, rest) with
    | (0, _) -> List.reverse acc
    | (_, []) -> List.reverse acc
    | (_, value :: tail) -> loop (remaining - 1) (value :: acc) tail
  in
  loop limit [] values

let format_duration_us = fun duration_us ->
  if duration_us < 1_000 then
    Int.to_string duration_us ^ "µs"
  else if duration_us < 1_000_000 then
    Float.to_string ~precision:2 (Float.from_int duration_us /. 1_000.0) ^ "ms"
  else
    Float.to_string ~precision:2 (Float.from_int duration_us /. 1_000_000.0) ^ "s"

let ansi_reset = "\027[0m"

let ansi_gray = "\027[38;5;245m"

let ansi_bold_red = "\027[1;31m"

let ansi_bold_yellow = "\027[1;33m"

let slow_small_threshold_us = 500_000

let failed_status = ansi_bold_red ^ "FAILED" ^ ansi_reset

let duration_suffix = fun size duration_us ->
  let text = "(" ^ format_duration_us duration_us ^ ")" in
  let color =
    match size with
    | Test_runtime.Small when duration_us > slow_small_threshold_us -> ansi_bold_yellow
    | Test_runtime.Small
    | Test_runtime.Large -> ansi_gray
  in
  match size with
  | Test_runtime.Small
  | Test_runtime.Large -> " " ^ color ^ text ^ ansi_reset

let metadata_labels = fun size reliability ->
  let size_labels =
    match size with
    | Test_runtime.Small -> []
    | Test_runtime.Large -> [ "large" ]
  in
  let reliability_labels =
    match reliability with
    | Test_runtime.Stable -> []
    | Test_runtime.Flaky { retry_attempts } -> [ "flaky/" ^ Int.to_string retry_attempts ]
  in
  List.append size_labels reliability_labels

let metadata_suffix = fun size reliability ->
  match metadata_labels size reliability with
  | [] -> ""
  | labels -> " [" ^ String.concat " " labels ^ "]"

let attempts_suffix = fun attempts ->
  if attempts <= 1 then
    ""
  else
    " after " ^ Int.to_string attempts ^ " attempts"

let timeout_message = fun timeout_ms -> "timed out after " ^ Int.to_string timeout_ms ^ "ms"

let sort_summary_tests = fun ~small_only ~large_only tests ->
  let compare =
    if large_only && not small_only then
      fun (left: slow_test) (right: slow_test) -> Int.compare left.duration_us right.duration_us
    else
      fun (left: slow_test) (right: slow_test) -> Int.compare right.duration_us left.duration_us
  in
  List.sort tests ~compare

let filter_summary_tests = fun ~small_only ~large_only tests ->
  if small_only then
    List.filter tests ~fn:(fun (test: slow_test) -> test.size = Test_runtime.Small)
  else if large_only then
    List.filter tests ~fn:(fun (test: slow_test) -> test.size = Test_runtime.Large)
  else
    tests

let summary_section_title = fun ~small_only:_ ~large_only ->
  if large_only then
    "  Fastest tests:"
  else
    "  Slowest tests:"

let record_suite_timing = fun
  ~small_only
  ~large_only
  (timing: timing_summary)
  ~suite_label
  (summary: Test_runtime.test_suite_summary) ->
  timing.measured_duration_us <- timing.measured_duration_us + summary.duration_us;
  timing.measured_test_count <- timing.measured_test_count + summary.total;
  let slow_suite_tests: slow_test list =
    summary.results
    |> List.map
      ~fn:(fun (result: Test_runtime.test_case_result) ->
        ({
          suite_label;
          test_name = result.name;
          duration_us = result.duration_us;
          size = result.size;
        }: slow_test))
  in
  let slowest_tests: slow_test list =
    List.append timing.slowest_tests slow_suite_tests
    |> filter_summary_tests ~small_only ~large_only
    |> sort_summary_tests ~small_only ~large_only
    |> take 5
  in
  timing.slowest_tests <- slowest_tests;
  summary.results
  |> List.for_each
    ~fn:(fun (result: Test_runtime.test_case_result) ->
      match result.result with
      | Test_runtime.Failed message ->
          Vector.push
            timing.failed_tests
            ~value:{
              suite_label;
              test_name = result.name;
              message;
              duration_us = result.duration_us;
            }
      | Test_runtime.Timed_out { timeout_ms } ->
          Vector.push
            timing.failed_tests
            ~value:{
              suite_label;
              test_name = result.name;
              message = timeout_message timeout_ms;
              duration_us = result.duration_us;
            }
      | Test_runtime.Passed
      | Test_runtime.Skipped -> ())

let print_summary = fun
  ~small_only ~large_only ~label ~total ~passed ~failed ~skipped ~(timing:timing_summary) ->
  println "";
  println label;
  println ("  Total test cases: " ^ Int.to_string total);
  println ("  Passed: " ^ Int.to_string passed);
  println ("  Failed: " ^ Int.to_string failed);
  println ("  Skipped: " ^ Int.to_string skipped);
  if timing.measured_test_count > 0 then (
    println ("  Measured test time: " ^ format_duration_us timing.measured_duration_us);
    println
      ("  Average per test: "
      ^ format_duration_us (timing.measured_duration_us / timing.measured_test_count));
    if not (List.is_empty timing.slowest_tests) then (
      println (summary_section_title ~small_only ~large_only);
      timing.slowest_tests
      |> List.enumerate
      |> List.for_each
        ~fn:(fun (idx, (test: slow_test)) ->
          println
            ("    "
            ^ Int.to_string (idx + 1)
            ^ ". "
            ^ test.suite_label
            ^ " :: "
            ^ test.test_name
            ^ " ("
            ^ format_duration_us test.duration_us
            ^ ")"))
    )
  );
  if not (Vector.is_empty timing.failed_tests) then (
    println "  Failed tests:";
    timing.failed_tests
    |> Vector.to_array
    |> Array.to_list
    |> List.enumerate
    |> List.for_each
      ~fn:(fun (idx, (test: failed_test)) ->
        println
          ("    "
          ^ Int.to_string (idx + 1)
          ^ ". "
          ^ test.suite_label
          ^ " :: "
          ^ test.test_name
          ^ " ("
          ^ format_duration_us test.duration_us
          ^ ")");
        if not (String.equal test.message "") then
          println ("       " ^ test.message))
  )

let event_elapsed_us = fun ~command_started_at ->
  Time.Instant.elapsed command_started_at
  |> Time.Duration.to_micros

let json_int_field = fun name fields ->
  match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name name) with
  | Some (_, Data.Json.Int value) -> Some value
  | _ -> None

let upsert_int_field = fun name value fields ->
  let filtered =
    List.filter fields ~fn:(fun (field_name, _) -> not (String.equal field_name name))
  in
  filtered @ [ (name, Data.Json.Int value); ]

let stamp_json_event = fun
  ~command_started_at ~duration_us (event: Test_runtime.test_event) (json: Data.Json.t) ->
  match json with
  | Data.Json.Object fields ->
      let elapsed_us = event_elapsed_us ~command_started_at in
      let normalized_duration_us =
        match duration_us with
        | Some duration_us -> duration_us
        | None -> Option.unwrap_or ~default:0 (json_int_field "duration_us" fields)
      in
      let fields =
        match event with
        | Test_runtime.SuiteProgress _ -> upsert_int_field "command_emitted_at_us" elapsed_us fields
        | _ -> upsert_int_field "duration_us" normalized_duration_us fields
      in
      let fields =
        match event with
        | Test_runtime.RunningSuite _ -> upsert_int_field "started_at_us" elapsed_us fields
        | Test_runtime.SuiteCompleted _ ->
            fields
            |> upsert_int_field "started_at_us" (Int.max 0 (elapsed_us - normalized_duration_us))
            |> upsert_int_field "completed_at_us" elapsed_us
        | Test_runtime.Summary _ ->
            fields
            |> upsert_int_field "started_at_us" 0
            |> upsert_int_field "completed_at_us" elapsed_us
        | Test_runtime.NoSuitesFound _ -> upsert_int_field "completed_at_us" elapsed_us fields
        | Test_runtime.Build _ -> fields
        | Test_runtime.TestSuitesCollected _
        | Test_runtime.ResolvingSuiteBinary _
        | Test_runtime.SuiteBinaryResolved _
        | Test_runtime.ExecutingSuiteBinary _
        | Test_runtime.SuiteHeartbeat _
        | Test_runtime.SuiteBinaryFinished _
        | Test_runtime.ParsingSuiteOutput _ -> upsert_int_field "emitted_at_us" elapsed_us fields
        | Test_runtime.SuiteProgress _ -> fields
      in
      Data.Json.Object fields
  | other -> other

let write_json_event = fun ~command_started_at ~duration_us event (json: Data.Json.t) ->
  println
    (Data.Json.to_string (stamp_json_event ~command_started_at ~duration_us event json))

let summary_duration_us = fun ~command_started_at (event: Test_runtime.test_event) ->
  match event with
  | Test_runtime.Summary _ ->
      Some (
        Time.Instant.elapsed command_started_at
        |> Time.Duration.to_micros
      )
  | _ -> None

let write_test_event_json = fun ~command_started_at (event: Test_runtime.test_event) ->
  Test_runtime.test_event_to_json event
  |> Option.for_each
    ~fn:(fun json ->
      write_json_event
        ~command_started_at
        ~duration_us:(summary_duration_us ~command_started_at event)
        event
        json)

type suite_source_label_entry = {
  package_name: Riot_model.Package_name.t;
  suite_name: string;
  label: string;
}

let source_path_label = fun ~(workspace:Riot_model.Workspace.t) path ->
  match Path.strip_prefix path ~prefix:workspace.root with
  | Ok relative_path -> Path.to_string relative_path
  | Error _ -> Path.to_string path

let suite_source_labels = fun ~(workspace:Riot_model.Workspace.t) ->
  Riot_model.Workspace.realize_packages ~intent:Riot_model.Package.Test workspace
  |> List.flat_map
    ~fn:(fun (pkg: Riot_model.Package.t) ->
      pkg.binaries
      |> List.filter_map
        ~fn:(fun (bin: Riot_model.Package.binary) ->
          if
            String.ends_with ~suffix:"_tests" bin.name || String.ends_with ~suffix:"-tests" bin.name
          then
            Some {
              package_name = pkg.name;
              suite_name = bin.name;
              label = source_path_label ~workspace Path.(pkg.path / bin.path);
            }
          else
            None))

let suite_source_label = fun
  ~(suite_labels:suite_source_label_entry list) (suite: Test_runtime.suite_binary) ->
  match suite_labels
  |> List.find
    ~fn:(fun (entry: suite_source_label_entry) ->
      Riot_model.Package_name.equal entry.package_name suite.package_name
      && String.equal entry.suite_name suite.suite_name) with
  | Some entry -> entry.label
  | None -> Riot_model.Package_name.to_string suite.package_name ^ "/" ^ suite.suite_name

let listed_suite_source_label = fun
  ~(workspace:Riot_model.Workspace.t)
  ~(suite_labels:suite_source_label_entry list)
  (suite: Test_runtime.listed_test_suite) ->
  match suite.source_path with
  | Some path -> source_path_label ~workspace path
  | None -> suite_source_label ~suite_labels suite.suite

let listed_test_selector = fun
  (suite: Test_runtime.suite_binary) (test: Test_runtime.listed_test_case) ->
  Riot_model.Package_name.to_string suite.package_name ^ ":" ^ suite.suite_name ^ ":" ^ test.name

let listed_test_json = fun
  (suite: Test_runtime.suite_binary) (test: Test_runtime.listed_test_case) ->
  let type_fields =
    match test.test_type with
    | Test_runtime.Test -> [ ("type", Data.Json.String "test"); ]
    | Test_runtime.Property { examples } ->
        [ ("type", Data.Json.String "property"); ("examples", Data.Json.Int examples); ]
    | Test_runtime.Fuzz { seeds } ->
        [ ("type", Data.Json.String "fuzz"); ("seeds", Data.Json.Int seeds); ]
  in
  let reliability_fields =
    match test.reliability with
    | Test_runtime.Stable -> [ ("reliability", Data.Json.String "stable"); ]
    | Test_runtime.Flaky { retry_attempts } ->
        [
          ("reliability", Data.Json.String "flaky");
          ("retry_attempts", Data.Json.Int retry_attempts);
        ]
  in
  let size =
    match test.size with
    | Test_runtime.Small -> Data.Json.String "small"
    | Test_runtime.Large -> Data.Json.String "large"
  in
  Data.Json.Object (([
    ("index", Data.Json.Int test.index);
    ("name", Data.Json.String test.name);
    ("selector", Data.Json.String (listed_test_selector suite test));
    ("size", size);
    ("skip", Data.Json.Bool test.skip);
  ]
  @ type_fields)
  @ reliability_fields)

let listed_suite_path_json = fun
  ~(workspace:Riot_model.Workspace.t) (suite: Test_runtime.listed_test_suite) ->
  match suite.source_path with
  | Some path -> (
      match Path.strip_prefix path ~prefix:workspace.root with
      | Ok relative_path -> Data.Json.String (Path.to_string relative_path)
      | Error _ -> Data.Json.String (Path.to_string path)
    )
  | None -> Data.Json.Null

let listed_suite_selector = fun (suite: Test_runtime.suite_binary) ->
  Riot_model.Package_name.to_string suite.package_name ^ ":" ^ suite.suite_name

let write_json_line = fun json -> println (Data.Json.to_string json)

let write_test_suite_listed_json = fun
  ~command_started_at
  ~(workspace:Riot_model.Workspace.t)
  (suite: Test_runtime.listed_test_suite) ->
  write_json_line
    (Data.Json.Object [
      ("type", Data.Json.String "TestSuiteListed");
      ("package", Data.Json.String (Riot_model.Package_name.to_string suite.suite.package_name));
      ("suite", Data.Json.String suite.suite.suite_name);
      ("path", listed_suite_path_json ~workspace suite);
      ("selector", Data.Json.String (listed_suite_selector suite.suite));
      ("emitted_at_us", Data.Json.Int (event_elapsed_us ~command_started_at));
    ])

let write_test_case_listed_json = fun
  ~command_started_at (suite: Test_runtime.suite_binary) (test: Test_runtime.listed_test_case) ->
  write_json_line
    (Data.Json.Object [
      ("type", Data.Json.String "TestCaseListed");
      ("package", Data.Json.String (Riot_model.Package_name.to_string suite.package_name));
      ("suite", Data.Json.String suite.suite_name);
      ("index", Data.Json.Int test.index);
      ("name", Data.Json.String test.name);
      ("selector", Data.Json.String (listed_test_selector suite test));
      ("case", listed_test_json suite test);
      ("emitted_at_us", Data.Json.Int (event_elapsed_us ~command_started_at));
    ])

let write_test_suite_list_failed_json = fun
  ~command_started_at (suite: Test_runtime.suite_binary) err ->
  write_json_line
    (Data.Json.Object [
      ("type", Data.Json.String "TestSuiteListFailed");
      ("package", Data.Json.String (Riot_model.Package_name.to_string suite.package_name));
      ("suite", Data.Json.String suite.suite_name);
      ("selector", Data.Json.String (listed_suite_selector suite));
      ("message", Data.Json.String (Test_runtime.test_error_message err));
      ("emitted_at_us", Data.Json.Int (event_elapsed_us ~command_started_at));
    ])

let write_test_list_completed_json = fun
  ~command_started_at ~suite_count ~test_count ~failed_suite_count ->
  write_json_line
    (Data.Json.Object [
      ("type", Data.Json.String "TestListCompleted");
      ("suite_count", Data.Json.Int suite_count);
      ("test_count", Data.Json.Int test_count);
      ("failed_suite_count", Data.Json.Int failed_suite_count);
      ("completed_at_us", Data.Json.Int (event_elapsed_us ~command_started_at));
    ])

let write_test_list = fun
  ~(workspace:Riot_model.Workspace.t) ~(suite_labels:suite_source_label_entry list) suites ->
  List.for_each
    suites
    ~fn:(fun (suite: Test_runtime.listed_test_suite) ->
      println "";
      println (listed_suite_source_label ~workspace ~suite_labels suite);
      suite.tests
      |> List.for_each
        ~fn:(fun (test: Test_runtime.listed_test_case) ->
          let metadata = metadata_suffix test.size test.reliability in
          let type_prefix =
            match test.test_type with
            | Test_runtime.Test -> "test"
            | Test_runtime.Property _ -> "prop"
            | Test_runtime.Fuzz _ -> "fuzz"
          in
          let skip_suffix =
            if test.skip then
              " [skip]"
            else
              ""
          in
          println
            ("  ["
            ^ Int.to_string test.index
            ^ "] "
            ^ type_prefix
            ^ " "
            ^ test.name
            ^ metadata
            ^ skip_suffix)))

type human_render_state = {
  streamed_suites: string Collections.HashSet.t;
}

let empty_human_render_state = fun () -> { streamed_suites = Collections.HashSet.create () }

let suite_stream_key = fun (suite: Test_runtime.suite_binary) ->
  Riot_model.Package_name.to_string suite.package_name ^ ":" ^ suite.suite_name

let qualified_test_name = fun
  (suite: Test_runtime.suite_binary) (result: Test_runtime.test_case_result) ->
  Riot_model.Package_name.to_string suite.package_name
  ^ "::"
  ^ suite.suite_name
  ^ "::"
  ^ result.name

let print_test_result = fun
  ~(suite:Test_runtime.suite_binary) (result: Test_runtime.test_case_result) ->
  let prefix =
    match result.test_type with
    | Test_runtime.Test -> "test"
    | Test_runtime.Property _ -> "prop"
    | Test_runtime.Fuzz _ -> "fuzz"
  in
  let metadata = metadata_suffix result.size result.reliability in
  let name = qualified_test_name suite result in
  match result.result with
  | Test_runtime.Passed ->
      let suffix =
        match result.test_type with
        | Test_runtime.Test -> "ok"
        | Test_runtime.Property { examples } -> Int.to_string examples ^ " examples ok"
        | Test_runtime.Fuzz { seeds } -> Int.to_string seeds ^ " seeds ok"
      in
      println
        (prefix
        ^ " "
        ^ name
        ^ metadata
        ^ " ... "
        ^ suffix
        ^ attempts_suffix result.attempts
        ^ duration_suffix result.size result.duration_us)
  | Test_runtime.Failed message ->
      println
        (prefix
        ^ " "
        ^ name
        ^ metadata
        ^ " ... "
        ^ failed_status
        ^ attempts_suffix result.attempts
        ^ duration_suffix result.size result.duration_us);
      if not (String.equal message "") then
        println ("       " ^ message)
  | Test_runtime.Timed_out { timeout_ms } ->
      println
        (prefix
        ^ " "
        ^ name
        ^ metadata
        ^ " ... TIMED OUT "
        ^ timeout_message timeout_ms
        ^ attempts_suffix result.attempts
        ^ duration_suffix result.size result.duration_us)
  | Test_runtime.Skipped ->
      println
        (prefix
        ^ " "
        ^ name
        ^ metadata
        ^ " ... skipped"
        ^ duration_suffix result.size result.duration_us)

let write_test_event = fun
  ~(suite_labels:suite_source_label_entry list)
  ~(timing:timing_summary)
  ~small_only
  ~large_only
  ~(state:human_render_state)
  ~verbose
  (event: Test_runtime.test_event) ->
  match event with
  | Test_runtime.Build _ -> ()
  | Test_runtime.NoSuitesFound { package_name; suite_name } ->
      print_empty_hint package_name suite_name
  | Test_runtime.TestSuitesCollected _
  | Test_runtime.ResolvingSuiteBinary _
  | Test_runtime.SuiteBinaryResolved _
  | Test_runtime.RunningSuite _ -> ()
  | Test_runtime.ExecutingSuiteBinary _
  | Test_runtime.SuiteHeartbeat _
  | Test_runtime.SuiteBinaryFinished _
  | Test_runtime.ParsingSuiteOutput _ -> ()
  | Test_runtime.SuiteProgress { suite; event } ->
      Test_runtime.suite_progress_test_case_result event
      |> Result.to_option
      |> Option.flatten
      |> Option.for_each
        ~fn:(fun result ->
          let key = suite_stream_key suite in
          let _ = Collections.HashSet.insert state.streamed_suites ~value:key in
          print_test_result ~suite result)
  | Test_runtime.SuiteCompleted {
      suite;
      stdout;
      stderr;
      summary;
      _;
    } ->
      if summary.total > 0 then (
        record_suite_timing
          ~small_only
          ~large_only
          timing
          ~suite_label:(suite_source_label ~suite_labels suite)
          summary;
        if
          not (Collections.HashSet.contains state.streamed_suites ~value:(suite_stream_key suite))
        then
          summary.results
          |> List.for_each ~fn:(print_test_result ~suite)
      );
      if verbose > 0 then
        print_command_output Command.{ stdout; stderr; status = 0 }
  | Test_runtime.Summary {
      total;
      passed;
      failed;
      skipped;
      failed_tests = _;
    } ->
      print_summary
        ~small_only
        ~large_only
        ~label:"Test Summary:"
        ~total
        ~passed
        ~failed
        ~skipped
        ~timing

let write_test_error = fun err -> println ("error: " ^ Test_runtime.test_error_message err)

let write_test_error_json = fun ~command_started_at err ->
  let event_json = Data.Json.Object [
    ("type", Data.Json.String "test.error");
    ("message", Data.Json.String (Test_runtime.test_error_message err));
  ]
  in
  print
    (
      Data.Json.to_string
        (
          match event_json with
          | Data.Json.Object fields ->
              Data.Json.Object (upsert_int_field
                "completed_at_us"
                (event_elapsed_us ~command_started_at)
                fields)
          | other -> other
        )
    );
  print "\n"

let run = fun ~(workspace:Riot_model.Workspace.t) matches ->
  let seen_registry_updates = Collections.HashSet.create () in
  let trailing = trailing_args matches in
  let verbose = ArgParser.get_count matches "verbose" in
  let _ = verbose in
  let output_mode =
    if ArgParser.get_flag matches "json" then
      Build.Json
    else
      Build.Human
  in
  let small_only = ArgParser.get_flag matches "small" in
  let large_only = ArgParser.get_flag matches "large" in
  let flaky_only = ArgParser.get_flag matches "flaky" in
  let filter = ArgParser.get_one matches "filter" in
  let package_filters = parse_package_names (ArgParser.get_many matches "package") in
  let list_mode = ArgParser.get_flag matches "list" in
  let profile = profile_of_matches matches in
  let command_started_at = Time.Instant.now () in
  if output_mode = Build.Json then
    Build.reset_json_clock ~started_at:command_started_at;
  if small_only && large_only then
    Error (Failure "Cannot combine --small and --large")
  else
    match Riot_model.Workspace_operational_config.load ~workspace_root:workspace.root with
    | Error err ->
        let message = Riot_model.Workspace_operational_config.message err in
        (
          match output_mode with
          | Build.Json ->
              print
                (Data.Json.to_string
                  (Data.Json.Object [
                    ("type", Data.Json.String "test.error");
                    ("message", Data.Json.String message);
                    ("completed_at_us", Data.Json.Int (event_elapsed_us ~command_started_at));
                  ]));
              print "\n"
          | Build.Human -> println ("error: " ^ message)
        );
        Error (Failure message)
    | Ok operational_config ->
        let suite_labels = suite_source_labels ~workspace in
        let size_filter =
          if small_only then
            Test_selection.Small
          else if large_only then
            Test_selection.Large
          else
            Test_selection.All
        in
        let* package_filters = package_filters in
        let* request =
          Test_selection.parse_request ~filter ~package_filters ~size_filter ~flaky_only
          |> Result.map_err ~fn:(fun error -> Failure error)
        in
        let extra_args =
          Test_selection.extra_args
            ~small_test_timeout:operational_config.test.small_test_timeout
            ~flaky_max_retries:operational_config.test.flaky_max_retries
            request
            trailing
        in
        if list_mode then
          let listed_suite_count = ref 0 in
          let listed_test_count = ref 0 in
          let failed_suite_count = ref 0 in
          let on_suite (suite: Test_runtime.listed_test_suite) =
            if not (List.is_empty suite.tests) then (
              listed_suite_count := !listed_suite_count + 1;
              listed_test_count := !listed_test_count + List.length suite.tests;
              write_test_suite_listed_json ~command_started_at ~workspace suite;
              List.for_each
                suite.tests
                ~fn:(write_test_case_listed_json ~command_started_at suite.suite)
            )
          in
          let on_suite_error (suite: Test_runtime.suite_binary) err =
            failed_suite_count := !failed_suite_count + 1;
            write_test_suite_list_failed_json ~command_started_at suite err
          in
          let on_event (event: Test_runtime.test_event) =
            match event with
            | Test_runtime.Build build_event ->
                Build.write_build_event ~mode:output_mode ~seen_registry_updates build_event
            | _ -> ()
          in
          match Test_runtime.list_tests
            ~on_event
            ?on_suite:(
              if output_mode = Build.Json then
                Some on_suite
              else
                None
            )
            ?on_suite_error:(
              if output_mode = Build.Json then
                Some on_suite_error
              else
                None
            )
            {
              workspace;
              package_filters = request.package_filters;
              suite_filter = request.suite_filter;
              profile;
              extra_args;
            } with
          | Ok suites ->
              let suites =
                List.filter
                  suites
                  ~fn:(fun (suite: Test_runtime.listed_test_suite) ->
                    not
                      (List.is_empty suite.tests))
              in
              (
                match output_mode with
                | Build.Json ->
                    write_test_list_completed_json
                      ~command_started_at
                      ~suite_count:!listed_suite_count
                      ~test_count:!listed_test_count
                      ~failed_suite_count:!failed_suite_count
                | Build.Human ->
                    if List.is_empty suites then
                      print_empty_list_hint
                        request.package_filter
                        request.suite_filter
                        request.query
                    else
                      write_test_list ~workspace ~suite_labels suites
              );
              Ok ()
          | Error err ->
              (
                match output_mode with
                | Build.Json -> write_test_error_json ~command_started_at err
                | Build.Human -> write_test_error err
              );
              Error (Failure (Test_runtime.test_error_message err))
        else
          let timing = empty_timing_summary () in
          let state = empty_human_render_state () in
          let on_event (event: Test_runtime.test_event) =
            match event with
            | Test_runtime.Build build_event ->
                Build.write_build_event ~mode:output_mode ~seen_registry_updates build_event
            | _ -> (
                match output_mode with
                | Build.Json -> write_test_event_json ~command_started_at event
                | Build.Human ->
                    write_test_event
                      ~suite_labels
                      ~timing
                      ~small_only
                      ~large_only
                      ~state
                      ~verbose
                      event
              )
          in
          match Test_runtime.test
            ~on_event
            {
              workspace;
              package_filters = request.package_filters;
              suite_filter = request.suite_filter;
              profile;
              extra_args;
            } with
          | Ok () -> Ok ()
          | Error err ->
              (
                match output_mode with
                | Build.Json -> write_test_error_json ~command_started_at err
                | Build.Human -> write_test_error err
              );
              Error (Failure (Test_runtime.test_error_message err))

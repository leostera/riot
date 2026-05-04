open Std
open Std.Result.Syntax

type output_mode =
  | Human
  | Json

let selection_args = fun () ->
  let open ArgParser.Arg in
  [
    option "package"
    |> short 'p'
    |> long "package"
    |> multiple
    |> help "Fuzz cases from a specific package. Repeat to select multiple packages.";
    option "filter"
    |> short 'f'
    |> long "filter"
    |> help "Filter fuzz suites and cases by substring or package:suite:case selector";
  ]

let output_args = fun () ->
  let open ArgParser.Arg in
  [
    flag "json"
    |> long "json"
    |> help "Emit machine-readable JSONL events";
  ]

let timeout_args = fun () ->
  let open ArgParser.Arg in
  [
    option "timeout-ms"
    |> long "timeout-ms"
    |> help "Maximum time for one generated input"
    |> default "1000";
  ]

let run_args = fun () ->
  let open ArgParser.Arg in
  (
    (
      selection_args () @ [
        flag "list"
        |> long "list"
        |> help "List fuzz cases without running a campaign";
        option "runs"
        |> long "runs"
        |> help "Maximum number of generated inputs to execute";
        option "duration"
        |> long "duration"
        |> help "Maximum campaign duration, such as 30s, 10m, or 1h";
        option "max-len"
        |> long "max-len"
        |> help "Maximum generated input length"
        |> default "4096";
        option "seed"
        |> long "seed"
        |> help "Deterministic fuzzer seed";
        option "concurrency"
        |> long "concurrency"
        |> help "Number of fuzz campaigns to run in parallel";
        option "replay"
        |> long "replay"
        |> help "Replay a saved input against one selected fuzz case";
        flag "minimize-corpus"
        |> long "minimize-corpus"
        |> help "Deprecated alias for `riot fuzz minimize-corpus`";
      ]
    ) @ timeout_args ()
  ) @ output_args ()

let minimize_args = fun () -> (selection_args () @ timeout_args ()) @ output_args ()

let command =
  let open ArgParser in
  command "fuzz"
  |> about "Run fuzz campaigns for fuzz test cases"
  |> args (run_args ())
  |> allow_no_subcommand
  |> subcommands
    [
      command "minimize-corpus"
      |> about "Delete coverage-redundant inputs from fuzz corpuses"
      |> args (minimize_args ());
    ]

let output_mode_of_matches = fun matches ->
  if ArgParser.get_flag matches "json" then
    Json
  else
    Human

let build_output_mode = fun __tmp1 ->
  match __tmp1 with
  | Human -> Build.Human
  | Json -> Build.Json

let render_discovery_event = fun ~mode ~seen_registry_updates event ->
  match event with
  | Riot_test.Test_runtime.Build build_event ->
      Build.write_build_event
        ~mode:(build_output_mode mode)
        ~seen_registry_updates
        build_event
  | _ -> ()

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

let parse_positive_int = fun ~name ~default matches ->
  match ArgParser.get_one matches name with
  | None -> Ok default
  | Some value -> (
      match Int.parse value with
      | Some parsed when parsed > 0 -> Ok parsed
      | Some _
      | None -> Error (Failure ("invalid --" ^ name ^ " value: " ^ value))
    )

let parse_optional_positive_int = fun ~name matches ->
  match ArgParser.get_one matches name with
  | None -> Ok None
  | Some value -> (
      match Int.parse value with
      | Some parsed when parsed > 0 -> Ok (Some parsed)
      | Some _
      | None -> Error (Failure ("invalid --" ^ name ^ " value: " ^ value))
    )

let parse_duration_value = fun value ->
  let parse_with_suffix suffix make =
    if String.ends_with ~suffix value then
      let len = String.length value - String.length suffix in
      let raw = String.sub value ~offset:0 ~len in
      match Int.parse raw with
      | Some n when n > 0 -> Some (make n)
      | Some _
      | None -> None
    else
      None
  in
  match parse_with_suffix "ms" Time.Duration.from_millis with
  | Some duration -> Ok duration
  | None -> (
      match parse_with_suffix "s" Time.Duration.from_secs with
      | Some duration -> Ok duration
      | None -> (
          match parse_with_suffix "m" Time.Duration.from_mins with
          | Some duration -> Ok duration
          | None -> (
              match parse_with_suffix "h" Time.Duration.from_hours with
              | Some duration -> Ok duration
              | None -> (
                  match Int.parse value with
                  | Some n when n > 0 -> Ok (Time.Duration.from_secs n)
                  | Some _
                  | None -> Error (Failure ("invalid --duration value: " ^ value))
                )
            )
        )
    )

let parse_duration = fun matches ->
  match ArgParser.get_one matches "duration" with
  | None -> Ok None
  | Some value ->
      parse_duration_value value
      |> Result.map ~fn:(fun duration -> Some duration)

let parse_runs = fun ~duration matches ->
  match ArgParser.get_one matches "runs" with
  | Some value -> (
      match Int.parse value with
      | Some parsed when parsed > 0 -> Ok parsed
      | Some _
      | None -> Error (Failure ("invalid --runs value: " ^ value))
    )
  | None ->
      if Option.is_some duration then
        Ok Int.max_int
      else
        Ok 1_000

let write_json_line = fun fields -> println (Data.Json.to_string (Data.Json.Object fields))

let write_case_json = fun event (fuzz_case: Riot_fuzz.fuzz_case) extra_fields ->
  write_json_line
    ([
      ("type", Data.Json.String event);
      ("package", Data.Json.String (Riot_model.Package_name.to_string fuzz_case.suite.package_name));
      ("suite", Data.Json.String fuzz_case.suite.suite_name);
      ("case", Data.Json.String fuzz_case.case.name);
    ]
    @ extra_fields)

let write_case_human = fun (fuzz_case: Riot_fuzz.fuzz_case) ->
  println
    (Riot_model.Package_name.to_string fuzz_case.suite.package_name
    ^ ":"
    ^ fuzz_case.suite.suite_name
    ^ ":"
    ^ fuzz_case.case.name)

let format_millis = fun millis ->
  if millis < 1_000 then
    Int.to_string millis ^ "ms"
  else
    Time.Duration.from_millis millis
    |> Time.Duration.to_secs_string ~precision:1
    |> fun secs -> secs ^ "s"

let status_to_string = Riot_fuzz.Afl.status_to_string

let write_command_json = fun event fields ->
  write_json_line
    (("type", Data.Json.String event) :: fields)

let render_lock_waiting = fun ~mode path ->
  match mode with
  | Human -> eprintln ("fuzz lock is held, waiting: " ^ Path.to_string path)
  | Json ->
      write_command_json
        "FuzzLockWaiting"
        [ ("lock_path", Data.Json.String (Path.to_string path)); ]

let status_to_json = fun __tmp1 ->
  match __tmp1 with
  | Riot_fuzz.Afl.Exited code ->
      Data.Json.Object [ ("kind", Data.Json.String "exited"); ("code", Data.Json.Int code); ]
  | Riot_fuzz.Afl.Signaled signal ->
      Data.Json.Object [ ("kind", Data.Json.String "signaled"); ("signal", Data.Json.Int signal); ]
  | Riot_fuzz.Afl.Stopped signal ->
      Data.Json.Object [ ("kind", Data.Json.String "stopped"); ("signal", Data.Json.Int signal); ]
  | Riot_fuzz.Afl.Timed_out signal ->
      Data.Json.Object [ ("kind", Data.Json.String "timed_out"); ("signal", Data.Json.Int signal); ]

let render_event = fun ~mode (fuzz_case: Riot_fuzz.fuzz_case) event ->
  match mode with
  | Human -> (
      match event with
      | Riot_fuzz.Campaign_started { runs; duration_ms; dir; _ } ->
          let budget =
            match duration_ms with
            | Some millis when Int.equal runs Int.max_int -> " for " ^ format_millis millis
            | Some millis -> " for up to " ^ Int.to_string runs ^ " runs or " ^ format_millis millis
            | None -> " for " ^ Int.to_string runs ^ " runs"
          in
          eprintln
            ("fuzzing "
            ^ Riot_model.Package_name.to_string fuzz_case.suite.package_name
            ^ ":"
            ^ fuzz_case.suite.suite_name
            ^ ":"
            ^ fuzz_case.case.name
            ^ budget);
          eprintln ("fuzz state: " ^ Path.to_string dir)
      | Riot_fuzz.Campaign_progress {
          run;
          runs;
          elapsed_ms;
          total_edges;
          corpus_size;
        } ->
          let run_budget =
            if Int.equal runs Int.max_int then
              Int.to_string run
            else
              Int.to_string run ^ "/" ^ Int.to_string runs
          in
          eprintln
            ("  run "
            ^ run_budget
            ^ ", "
            ^ Int.to_string total_edges
            ^ " edges, corpus "
            ^ Int.to_string corpus_size
            ^ ", "
            ^ format_millis elapsed_ms)
      | Riot_fuzz.Input_executed _ -> ()
      | Riot_fuzz.Corpus_saved { run; path; new_edges } ->
          eprintln
            ("new coverage on run "
            ^ Int.to_string run
            ^ " (+"
            ^ Int.to_string new_edges
            ^ " edges): "
            ^ Path.to_string path)
      | Riot_fuzz.Crash_found { run; path; status } ->
          eprintln
            ("crash found on run " ^ Int.to_string run ^ " (" ^ status_to_string status ^ ")");
          eprintln ("saved input: " ^ Path.to_string path)
      | Riot_fuzz.Crash_triaged {
          stdout_path;
          stderr_path;
          status_path;
          status;
          _;
        } ->
          eprintln ("triage status: " ^ status_to_string status);
          eprintln ("triage stdout: " ^ Path.to_string stdout_path);
          eprintln ("triage stderr: " ^ Path.to_string stderr_path);
          eprintln ("triage status file: " ^ Path.to_string status_path)
      | Riot_fuzz.Campaign_completed {
          runs;
          crash_path;
          total_edges;
          elapsed_ms;
        } ->
          if Option.is_none crash_path then
            eprintln
              ("completed "
              ^ Int.to_string runs
              ^ " fuzz runs without a crash; observed "
              ^ Int.to_string total_edges
              ^ " coverage edges in "
              ^ format_millis elapsed_ms)
      | Riot_fuzz.Replay_completed { input_path; status; hit_edges } ->
          eprintln
            ("replayed "
            ^ Path.to_string input_path
            ^ " with "
            ^ Int.to_string hit_edges
            ^ " coverage edges: "
            ^ status_to_string status)
      | Riot_fuzz.Corpus_minimized { dir; kept; removed } ->
          eprintln
            ("minimized "
            ^ Path.to_string dir
            ^ ": kept "
            ^ Int.to_string kept
            ^ ", deleted "
            ^ Int.to_string removed
            ^ " redundant inputs")
    )
  | Json -> (
      match event with
      | Riot_fuzz.Campaign_started {
          runs;
          max_len;
          duration_ms;
          dir;
        } ->
          write_case_json
            "FuzzCampaignStarted"
            fuzz_case
            [
              ("runs", Data.Json.Int runs);
              ("max_len", Data.Json.Int max_len);
              ("duration_ms", match duration_ms with
              | Some millis -> Data.Json.Int millis
              | None -> Data.Json.Null);
              ("dir", Data.Json.String (Path.to_string dir));
            ]
      | Riot_fuzz.Campaign_progress {
          run;
          runs;
          elapsed_ms;
          total_edges;
          corpus_size;
        } ->
          write_case_json
            "FuzzCampaignProgress"
            fuzz_case
            [
              ("run", Data.Json.Int run);
              ("runs", Data.Json.Int runs);
              ("elapsed_ms", Data.Json.Int elapsed_ms);
              ("total_edges", Data.Json.Int total_edges);
              ("corpus_size", Data.Json.Int corpus_size);
            ]
      | Riot_fuzz.Input_executed {
          run;
          status;
          hit_edges;
          new_edges;
        } ->
          write_case_json
            "FuzzInputExecuted"
            fuzz_case
            [
              ("run", Data.Json.Int run);
              ("status", status_to_json status);
              ("hit_edges", Data.Json.Int hit_edges);
              ("new_edges", Data.Json.Int new_edges);
            ]
      | Riot_fuzz.Corpus_saved { run; path; new_edges } ->
          write_case_json
            "FuzzCorpusSaved"
            fuzz_case
            [
              ("run", Data.Json.Int run);
              ("path", Data.Json.String (Path.to_string path));
              ("new_edges", Data.Json.Int new_edges);
            ]
      | Riot_fuzz.Crash_found { run; path; status } ->
          write_case_json
            "FuzzCrashFound"
            fuzz_case
            [
              ("run", Data.Json.Int run);
              ("path", Data.Json.String (Path.to_string path));
              ("status", status_to_json status);
            ]
      | Riot_fuzz.Crash_triaged {
          run;
          input_path;
          stdout_path;
          stderr_path;
          status_path;
          status;
        } ->
          write_case_json
            "FuzzCrashTriaged"
            fuzz_case
            [
              ("run", Data.Json.Int run);
              ("input_path", Data.Json.String (Path.to_string input_path));
              ("stdout_path", Data.Json.String (Path.to_string stdout_path));
              ("stderr_path", Data.Json.String (Path.to_string stderr_path));
              ("status_path", Data.Json.String (Path.to_string status_path));
              ("status", status_to_json status);
            ]
      | Riot_fuzz.Campaign_completed {
          runs;
          crash_path;
          total_edges;
          elapsed_ms;
        } ->
          write_case_json
            "FuzzCampaignCompleted"
            fuzz_case
            [
              ("runs", Data.Json.Int runs);
              ("crash_path", match crash_path with
              | Some path -> Data.Json.String (Path.to_string path)
              | None -> Data.Json.Null);
              ("total_edges", Data.Json.Int total_edges);
              ("elapsed_ms", Data.Json.Int elapsed_ms);
            ]
      | Riot_fuzz.Replay_completed { input_path; status; hit_edges } ->
          write_case_json
            "FuzzReplayCompleted"
            fuzz_case
            [
              ("input_path", Data.Json.String (Path.to_string input_path));
              ("status", status_to_json status);
              ("hit_edges", Data.Json.Int hit_edges);
            ]
      | Riot_fuzz.Corpus_minimized { dir; kept; removed } ->
          write_case_json
            "FuzzCorpusMinimized"
            fuzz_case
            [
              ("dir", Data.Json.String (Path.to_string dir));
              ("kept", Data.Json.Int kept);
              ("removed", Data.Json.Int removed);
            ]
    )

let request_for_case = fun ~workspace ~mode ~runs ~max_len ~duration ~timeout_ms ~seed fuzz_case ->
  let request: Riot_fuzz.request = {
    case_dir = Riot_fuzz.case_dir ~workspace fuzz_case;
    target = Riot_fuzz.target_for_case ~workspace fuzz_case;
    corpus = Riot_fuzz.corpus_for_case ~workspace fuzz_case;
    mutator = Riot_fuzz.mutator_for_case fuzz_case;
    runs;
    max_len;
    duration;
    timeout_ms;
    seed;
    on_event = render_event ~mode fuzz_case;
  }
  in
  request

let fuzz_case_at = fun cases index -> List.get cases ~at:index

let fuzz_case_label = fun (fuzz_case: Riot_fuzz.fuzz_case) ->
  Riot_model.Package_name.to_string fuzz_case.suite.package_name
  ^ ":"
  ^ fuzz_case.suite.suite_name
  ^ ":"
  ^ fuzz_case.case.name

let run_campaigns = fun
  ~workspace ~mode ?concurrency ~runs ~max_len ~duration ~timeout_ms ~seed cases ->
  let requests =
    cases
    |> List.map ~fn:(request_for_case ~workspace ~mode ~runs ~max_len ~duration ~timeout_ms ~seed)
  in
  let outcomes = Riot_fuzz.run_many ?concurrency requests in
  let errors =
    outcomes.campaigns
    |> List.filter_map
      ~fn:(fun (campaign: Riot_fuzz.campaign_result) ->
        match campaign.result with
        | Ok _ -> None
        | Error err ->
            fuzz_case_at cases campaign.index
            |> Option.map ~fn:(fun fuzz_case -> (fuzz_case, Riot_fuzz.error_message err)))
  in
  let crashes =
    outcomes.campaigns
    |> List.filter_map
      ~fn:(fun (campaign: Riot_fuzz.campaign_result) ->
        match campaign.result with
        | Ok result -> (
            match result.Riot_fuzz.crash_path with
            | Some path ->
                fuzz_case_at cases campaign.index
                |> Option.map ~fn:(fun fuzz_case -> (fuzz_case, path))
            | None -> None
          )
        | Error _ -> None)
  in
  match errors with
  | (fuzz_case, message) :: _ ->
      Error (Failure ("fuzz campaign failed for " ^ fuzz_case_label fuzz_case ^ ": " ^ message))
  | [] -> (
      match crashes with
      | [] -> Ok ()
      | (_fuzz_case, path) :: _ -> Error (Failure ("fuzz crash saved to " ^ Path.to_string path))
    )

let single_selected_case = fun cases action ->
  match cases with
  | [ fuzz_case ] -> Ok fuzz_case
  | [] -> Error (Failure ("no fuzz cases selected for " ^ action))
  | _ :: _ :: _ -> Error (Failure ("--" ^ action ^ " requires exactly one selected fuzz case"))

let replay_case = fun ~workspace ~mode ~timeout_ms input_path cases ->
  let* fuzz_case = single_selected_case cases "replay" in
  let target = Riot_fuzz.target_for_case ~workspace fuzz_case in
  let* result =
    Riot_fuzz.replay ~target ~input_path ~timeout_ms
    |> Result.map_err ~fn:(fun err -> Failure (Riot_fuzz.error_message err))
  in
  render_event
    ~mode
    fuzz_case
    (Riot_fuzz.Replay_completed {
      input_path = result.input_path;
      status = result.status;
      hit_edges = result.hit_edges;
    });
  let failed =
    match result.status with
    | Riot_fuzz.Afl.Exited 0 -> false
    | Riot_fuzz.Afl.Exited _
    | Riot_fuzz.Afl.Signaled _
    | Riot_fuzz.Afl.Stopped _
    | Riot_fuzz.Afl.Timed_out _ -> true
  in
  if failed then
    Error (Failure ("fuzz replay failed with " ^ status_to_string result.status))
  else
    Ok ()

let minimize_cases = fun ~workspace ~mode ~timeout_ms cases ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok ()
    | fuzz_case :: rest ->
        let request: Riot_fuzz.minimize_request = {
          case_dir = Riot_fuzz.case_dir ~workspace fuzz_case;
          target = Riot_fuzz.target_for_case ~workspace fuzz_case;
          timeout_ms;
          on_event = render_event ~mode fuzz_case;
        }
        in
        (
          match Riot_fuzz.minimize_corpus request with
          | Ok _ -> loop rest
          | Error err -> Error (Failure (Riot_fuzz.error_message err))
        )
  in
  loop cases

let collect_selected_cases = fun ~workspace ~mode ~seen_registry_updates matches ->
  let filter = ArgParser.get_one matches "filter" in
  let* package_filters = parse_package_names (ArgParser.get_many matches "package") in
  Riot_fuzz.collect_cases
    ~on_event:(render_discovery_event ~mode ~seen_registry_updates)
    ~workspace
    ~package_filters
    ~filter
    ()
  |> Result.map_err ~fn:(fun err -> Failure (Riot_fuzz.error_message err))

let with_fuzz_lock = fun ~workspace ~mode fn ->
  Riot_fuzz.with_lock
    ~workspace
    ~on_waiting:(render_lock_waiting ~mode)
    (fun () ->
      fn ()
      |> Result.map_err ~fn:(fun exn -> Riot_fuzz.Runtime_error (Exception.to_string exn)))
  |> Result.map_err ~fn:(fun err -> Failure (Riot_fuzz.error_message err))

let run_minimize = fun ~workspace matches ->
  let mode = output_mode_of_matches matches in
  let seen_registry_updates = Collections.HashSet.create () in
  if mode = Json then
    Build.reset_json_clock ~started_at:(Time.Instant.now ());
  let* timeout_ms = parse_positive_int ~name:"timeout-ms" ~default:1_000 matches in
  let* cases = collect_selected_cases ~workspace ~mode ~seen_registry_updates matches in
  with_fuzz_lock ~workspace ~mode (fun () -> minimize_cases ~workspace ~mode ~timeout_ms cases)

let run = fun ~(workspace:Riot_model.Workspace.t) matches ->
  match ArgParser.get_subcommand matches with
  | Some ("minimize-corpus", sub_matches) -> run_minimize ~workspace sub_matches
  | Some _ -> Error (Failure "unknown fuzz subcommand")
  | None ->
      let mode = output_mode_of_matches matches in
      let seen_registry_updates = Collections.HashSet.create () in
      if mode = Json then
        Build.reset_json_clock ~started_at:(Time.Instant.now ());
      let list_only = ArgParser.get_flag matches "list" in
      let minimize_only = ArgParser.get_flag matches "minimize-corpus" in
      let* duration = parse_duration matches in
      let* runs = parse_runs ~duration matches in
      let* max_len = parse_positive_int ~name:"max-len" ~default:4_096 matches in
      let* timeout_ms = parse_positive_int ~name:"timeout-ms" ~default:1_000 matches in
      let* concurrency = parse_optional_positive_int ~name:"concurrency" matches in
      let replay_path =
        ArgParser.get_one matches "replay"
        |> Option.map ~fn:Path.v
      in
      let seed = ArgParser.get_one matches "seed" in
      let* cases = collect_selected_cases ~workspace ~mode ~seen_registry_updates matches in
      if list_only then (
        (
          match mode with
          | Human ->
              if List.is_empty cases then
                println "No fuzz cases found"
              else
                List.for_each cases ~fn:write_case_human
          | Json ->
              List.for_each
                cases
                ~fn:(fun fuzz_case ->
                  write_case_json "FuzzCaseListed" fuzz_case []);
              write_json_line
                [
                  ("type", Data.Json.String "FuzzListCompleted");
                  ("case_count", Data.Json.Int (List.length cases));
                ]
        );
        Ok ()
      ) else if Option.is_some replay_path then
        with_fuzz_lock
          ~workspace
          ~mode
          (fun () ->
            replay_case ~workspace ~mode ~timeout_ms (Option.unwrap replay_path) cases)
      else if minimize_only then
        with_fuzz_lock
          ~workspace
          ~mode
          (fun () ->
            minimize_cases ~workspace ~mode ~timeout_ms cases)
      else
        match cases with
        | [] ->
            let message = "No fuzz cases found" in
            (
              match mode with
              | Human -> println message
              | Json ->
                  write_json_line
                    [
                      ("type", Data.Json.String "FuzzNoCasesFound");
                      ("message", Data.Json.String message);
                    ]
            );
            Ok ()
        | _ :: _ ->
            with_fuzz_lock
              ~workspace
              ~mode
              (fun () ->
                run_campaigns
                  ~workspace
                  ~mode
                  ?concurrency
                  ~runs
                  ~max_len
                  ~duration
                  ~timeout_ms
                  ~seed
                  cases)

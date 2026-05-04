open Global
open Collections

let suite_started_monotonic = ref None

let duration_us = fun duration -> Time.Duration.to_micros duration

let size_to_json = fun __tmp1 ->
  match __tmp1 with
  | Test_case.Small -> Data.Json.string "small"
  | Test_case.Large -> Data.Json.string "large"

let reliability_fields = fun __tmp1 ->
  match __tmp1 with
  | Test_case.Stable -> [ ("reliability", Data.Json.string "stable"); ]
  | Test_case.Flaky { retry_attempts } ->
      [
        ("reliability", Data.Json.string "flaky");
        ("retry_attempts", Data.Json.int retry_attempts);
      ]

let init = fun (_suite: Intf.suite_info) _total ->
  suite_started_monotonic := Some (Time.Instant.now ())

let on_result = fun _idx _result -> ()

let warn = fun message -> eprintln ("warning: " ^ message)

let finalize = fun (summary: Test_result.summary) ->
  let open Data.Json in
  let suite_duration =
    match !suite_started_monotonic with
    | Some start -> Time.Instant.elapsed start
    | None -> summary.duration
  in
  let test_results =
    List.map
      summary.results
      ~fn:(fun (r: Test_result.t) ->
        let type_fields =
          match r.test_type with
          | Test_case.UnitTest -> [ ("type", string "test"); ]
          | Test_case.Property { examples } ->
              [ ("type", string "property"); ("examples", int examples); ]
          | Test_case.Fuzz { seeds } -> [ ("type", string "fuzz"); ("seeds", int seeds); ]
        in
        let timing_fields =
          [
            ("duration_us", int (duration_us r.duration));
            ("attempts", int r.attempts);
            ("size", size_to_json r.size);
          ]
          @ reliability_fields r.reliability
        in
        let base_fields =
          match r.result with
          | Test_result.Passed -> [ ("name", string r.name); ("status", string "passed"); ]
          | Test_result.Failed msg ->
              [ ("name", string r.name); ("status", string "failed"); ("message", string msg); ]
          | Test_result.Timed_out { timeout } ->
              [
                ("name", string r.name);
                ("status", string "timed_out");
                ("timeout_ms", int (Time.Duration.to_millis timeout));
              ]
          | Test_result.Skipped -> [ ("name", string r.name); ("status", string "skipped"); ]
        in
        obj ((base_fields @ type_fields) @ timing_fields))
  in
  let summary_json =
    obj
      [
        ("total", int summary.total);
        ("passed", int summary.passed);
        ("failed", int summary.failed);
        ("skipped", int summary.skipped);
        ("duration_us", int (duration_us summary.duration));
      ]
  in
  let output =
    obj
      [
        ("type", string "TestSummary");
        ("tests", array test_results);
        ("summary", summary_json);
        ("started_at_us", int 0);
        ("completed_at_us", int (duration_us suite_duration));
        ("duration_us", int (duration_us suite_duration));
      ]
  in
  println (to_string output)

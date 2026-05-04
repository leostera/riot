open Global
open Collections

let metadata_labels = fun size reliability ->
  let size_labels =
    match size with
    | Test_case.Small -> []
    | Test_case.Large -> [ "large" ]
  in
  let reliability_labels =
    match reliability with
    | Test_case.Stable -> []
    | Test_case.Flaky { retry_attempts } -> [ "flaky/" ^ Int.to_string retry_attempts ]
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

let duration_suffix = fun size duration ->
  let duration_us = Time.Duration.to_micros duration in
  let text = "(" ^ format_duration_us duration_us ^ ")" in
  let color =
    match size with
    | Test_case.Small when duration_us > slow_small_threshold_us -> ansi_bold_yellow
    | Test_case.Small
    | Test_case.Large -> ansi_gray
  in
  match size with
  | Test_case.Small
  | Test_case.Large -> " " ^ color ^ text ^ ansi_reset

let init = fun (suite: Intf.suite_info) total ->
  (
    match (suite.source_file, suite.binary_path) with
    | (Some source, Some binary) ->
        println "";
        println ("     Running " ^ Path.to_string source ^ " (" ^ Path.to_string binary ^ ")")
    | (Some source, None) ->
        println "";
        println ("     Running " ^ Path.to_string source)
    | (None, _) -> ()
  );
  println "";
  println ("running " ^ Int.to_string total ^ " tests")

let on_result = fun _idx (result: Test_result.t) ->
  let prefix =
    match result.test_type with
    | Test_case.UnitTest -> "test"
    | Test_case.Property _ -> "prop"
    | Test_case.Fuzz _ -> "fuzz"
  in
  let metadata = metadata_suffix result.size result.reliability in
  match result.result with
  | Test_result.Passed ->
      let suffix =
        match result.test_type with
        | Test_case.UnitTest -> "ok"
        | Test_case.Property { examples } -> Int.to_string examples ^ " examples ok"
        | Test_case.Fuzz { seeds } -> Int.to_string seeds ^ " seeds ok"
      in
      println
        (prefix
        ^ " "
        ^ result.name
        ^ metadata
        ^ " ... "
        ^ suffix
        ^ attempts_suffix result.attempts
        ^ duration_suffix result.size result.duration)
  | Test_result.Failed msg ->
      println
        (prefix
        ^ " "
        ^ result.name
        ^ metadata
        ^ " ... "
        ^ failed_status
        ^ attempts_suffix result.attempts
        ^ duration_suffix result.size result.duration);
      println ("       " ^ msg)
  | Test_result.Timed_out { timeout } ->
      println
        (prefix
        ^ " "
        ^ result.name
        ^ metadata
        ^ " ... TIMED OUT after "
        ^ Int.to_string (Time.Duration.to_millis timeout)
        ^ "ms"
        ^ attempts_suffix result.attempts
        ^ duration_suffix result.size result.duration)
  | Test_result.Skipped ->
      println
        (prefix
        ^ " "
        ^ result.name
        ^ metadata
        ^ " ... skipped"
        ^ duration_suffix result.size result.duration)

let warn = fun message -> eprintln (ansi_bold_yellow ^ "warning" ^ ansi_reset ^ ": " ^ message)

let finalize = fun (summary: Test_result.summary) ->
  println "";
  let status =
    if summary.failed > 0 then
      failed_status
    else
      "ok"
  in
  println
    ("test result: "
    ^ status
    ^ ". "
    ^ Int.to_string summary.passed
    ^ " passed; "
    ^ Int.to_string summary.failed
    ^ " failed; "
    ^ Int.to_string summary.skipped
    ^ " skipped")

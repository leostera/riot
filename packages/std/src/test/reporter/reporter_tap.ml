open Global
open Collections

let init = fun (_suite: Intf.suite_info) total ->
  println "TAP version 14";
  println ("1.." ^ Int.to_string total)

let on_result = fun idx (result: Test_result.t) ->
  let idx_str = Int.to_string idx in
  let name_with_type =
    match result.test_type with
    | Test_case.UnitTest -> result.name
    | Test_case.Property { examples } -> result.name ^ " (" ^ Int.to_string examples ^ " examples)"
    | Test_case.Fuzz { seeds } -> result.name ^ " (" ^ Int.to_string seeds ^ " seeds)"
  in
  match result.result with
  | Test_result.Passed -> println ("ok " ^ idx_str ^ " - " ^ name_with_type)
  | Test_result.Failed msg ->
      println ("not ok " ^ idx_str ^ " - " ^ name_with_type);
      println "  ---";
      println ("  message: '" ^ msg ^ "'");
      println "  severity: fail";
      println "  ..."
  | Test_result.Timed_out { timeout } ->
      println ("not ok " ^ idx_str ^ " - " ^ name_with_type);
      println "  ---";
      println
        ("  message: 'timed out after " ^ Int.to_string (Time.Duration.to_millis timeout) ^ "ms'");
      println "  severity: fail";
      println "  ..."
  | Test_result.Skipped -> println ("ok " ^ idx_str ^ " - " ^ name_with_type ^ " # SKIP")

let warn = fun message -> eprintln ("warning: " ^ message)

let finalize = fun _summary -> ()

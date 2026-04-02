open Std
module Test = Std.Test

let sample_tests = [
  Test.case "alpha_long" (fun _ctx -> Ok ());
  Test.case "beta" (fun _ctx -> Ok ());
  Test.case "middle_long_case" (fun _ctx -> Ok ());
]

let self_executable = fun () ->
  match Env.args with
  | exe :: _ -> exe
  | [] -> panic "missing argv[0] for std_test_cli_tests"

let split_lines = fun output ->
  output |> String.split_on_char '\n' |> List.filter (fun line -> not (String.equal line ""))

let parse_json_output = fun stdout -> Data.Json.of_string stdout |> Result.expect ~msg:"failed to parse json output"

let test_names_from_json = fun stdout ->
  let json = parse_json_output stdout in
  match Data.Json.get_field "tests" json with
  | Some (Data.Json.Array tests) ->
      tests |> List.filter_map
        (fun test_json ->
          match Data.Json.get_field "name" test_json with
          | Some (Data.Json.String name) -> Some name
          | _ -> None)
  | _ -> []

let run_sample_capture = fun args ->
  let cmd = Command.make (self_executable ()) ~args:(("sample" :: args)) in
  Command.output cmd |> Result.expect ~msg:"failed to run sample test cli"

let test_list_tests_lists_all_cases = fun _ctx ->
  let output = run_sample_capture [ "list-tests" ] in
  if not (Int.equal output.status 0) then
    Error ("expected list-tests to succeed, got " ^ Int.to_string output.status)
  else
    let names = split_lines output.stdout |> List.sort String.compare in
    let expected = [ "alpha_long"; "beta"; "middle_long_case" ] |> List.sort String.compare in
    if names = expected then
      Ok ()
    else
      Error ("unexpected listed test names: " ^ String.concat ", " names)

let test_run_tests_pattern_matches_suffix_substring = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "_long"; "--format"; "json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected filtered run to succeed, got " ^ Int.to_string output.status)
  else
    let names = test_names_from_json output.stdout |> List.sort String.compare in
    let expected = [ "alpha_long"; "middle_long_case" ] |> List.sort String.compare in
    if names = expected then
      Ok ()
    else
      Error ("unexpected filtered names for _long: " ^ String.concat ", " names)

let test_run_tests_pattern_matches_middle_substring = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "long_case"; "--format"; "json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected filtered run to succeed, got " ^ Int.to_string output.status)
  else
    let names = test_names_from_json output.stdout in
    if names = [ "middle_long_case" ] then
      Ok ()
    else
      Error ("unexpected filtered names for long_case: " ^ String.concat ", " names)

let test_run_tests_returns_success_with_zero_matches = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "missing_case"; "--format"; "json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected filtered run with no matches to succeed, got " ^ Int.to_string output.status)
  else if test_names_from_json output.stdout = [] then
    Ok ()
  else
    Error "expected filtered run with no matches to report an empty test list"

let meta_tests = [
  Test.case "list-tests lists all sample cases" test_list_tests_lists_all_cases;
  Test.case "run-tests pattern matches suffix substring" test_run_tests_pattern_matches_suffix_substring;
  Test.case "run-tests pattern matches middle substring" test_run_tests_pattern_matches_middle_substring;
  Test.case "run-tests succeeds when the query matches no tests" test_run_tests_returns_success_with_zero_matches;
]

let sample_main = fun ~args ->
  match args with
  | exe :: _sample :: rest -> Test.Cli.main ~name:"sample" ~tests:sample_tests ~args:((exe :: rest))
  | _ -> Error (Failure "expected sample subcommand arguments")

let meta_main = fun ~args ->
  let normalize_args = function
    | [] -> [ "std_test_cli_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  Test.Cli.main ~name:"std_test_cli_tests" ~tests:meta_tests ~args:(normalize_args args)

let main = fun ~args ->
  match args with
  | _ :: "sample" :: _ -> sample_main ~args
  | _ -> meta_main ~args

let () = Miniriot.run ~main ~args:Env.args ()

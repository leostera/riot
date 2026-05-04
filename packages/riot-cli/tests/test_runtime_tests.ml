open Std
open Std.Result.Syntax

module Test = Std.Test

let package_name = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let make_test_package = fun name ->
  Riot_model.Package.make
    ~name:(package_name name)
    ~path:(Path.v ("/workspace/packages/" ^ name))
    ~relative_path:(Path.v ("packages/" ^ name))
    ~binaries:[ { name = name ^ "_tests"; path = Path.v ("tests/" ^ name ^ "_tests.ml") } ]
    ()

let sample_suite = {
  Riot_cli.Test_runtime.package_name = package_name "demo";
  suite_name = "demo_tests";
}

let expect_field = fun fields name expected ->
  match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name name) with
  | Some (_, actual) when String.equal (Data.Json.to_string actual) (Data.Json.to_string expected) ->
      Ok ()
  | Some (_, actual) ->
      Error ("expected field "
      ^ name
      ^ " to equal "
      ^ Data.Json.to_string expected
      ^ ", got "
      ^ Data.Json.to_string actual)
  | None -> Error ("missing field " ^ name)

let test_suite_heartbeat_event_to_json = fun _ctx ->
  match Riot_cli.Test_runtime.test_event_to_json
    (SuiteHeartbeat {
      suite = sample_suite;
      binary_path = Path.v "/tmp/demo_tests";
      elapsed_us = 1_234;
    }) with
  | Some (Data.Json.Object fields) ->
      let* () = expect_field fields "type" (Data.Json.String "SuiteHeartbeat") in
      let* () = expect_field fields "package" (Data.Json.String "demo") in
      let* () = expect_field fields "suite" (Data.Json.String "demo_tests") in
      let* () = expect_field fields "binary_path" (Data.Json.String "/tmp/demo_tests") in
      expect_field fields "elapsed_us" (Data.Json.Int 1_234)
  | Some json -> Error ("expected object json, got: " ^ Data.Json.to_string json)
  | None -> Error "expected suite heartbeat event to render json"

let test_suite_progress_test_case_result_parses_completed_case = fun _ctx ->
  let json = Data.Json.Object [
    ("type", Data.Json.String "TestCaseCompleted");
    ("index", Data.Json.Int 3);
    ("name", Data.Json.String "alpha");
    ("test_type", Data.Json.String "property");
    ("examples", Data.Json.Int 7);
    ("size", Data.Json.String "large");
    ("reliability", Data.Json.String "flaky");
    ("retry_attempts", Data.Json.Int 2);
    ("attempts", Data.Json.Int 3);
    ("status", Data.Json.String "passed");
    ("duration_us", Data.Json.Int 1_234);
  ]
  in
  match Riot_cli.Test_runtime.suite_progress_test_case_result json with
  | Ok (
    Some Riot_cli.Test_runtime.{
      index;
      name;
      test_type = Property { examples };
      size = Large;
      reliability = Flaky { retry_attempts };
      attempts;
      result = Passed;
      duration_us;
    }
  ) ->
      if
        Int.equal index 3
        && String.equal name "alpha"
        && Int.equal examples 7
        && Int.equal retry_attempts 2
        && Int.equal attempts 3
        && Int.equal duration_us 1_234
      then
        Ok ()
      else
        Error "expected parsed completed case metadata to round-trip"
  | Ok (Some _) -> Error "expected completed case to parse into a property test result"
  | Ok None -> Error "expected completed case progress to parse"
  | Error err -> Error ("expected completed case progress to parse: " ^ err)

let test_suite_progress_test_case_result_parses_fuzz_case = fun _ctx ->
  let json = Data.Json.Object [
    ("type", Data.Json.String "TestCaseCompleted");
    ("index", Data.Json.Int 4);
    ("name", Data.Json.String "fuzzer");
    ("test_type", Data.Json.String "fuzz");
    ("seeds", Data.Json.Int 2);
    ("status", Data.Json.String "passed");
  ]
  in
  match Riot_cli.Test_runtime.suite_progress_test_case_result json with
  | Ok (Some Riot_cli.Test_runtime.{ test_type = Fuzz { seeds }; name; result = Passed; _ }) when Int.equal
    seeds
    2
  && String.equal name "fuzzer" -> Ok ()
  | Ok (Some _) -> Error "expected completed case to parse into a fuzz test result"
  | Ok None -> Error "expected completed fuzz case progress to parse"
  | Error err -> Error ("expected completed fuzz case progress to parse: " ^ err)

let test_suite_progress_test_case_result_ignores_non_completed_event = fun _ctx ->
  let json = Data.Json.Object [
    ("type", Data.Json.String "TestCaseStarted");
    ("name", Data.Json.String "alpha");
  ]
  in
  match Riot_cli.Test_runtime.suite_progress_test_case_result json with
  | Ok None -> Ok ()
  | Ok (Some _) -> Error "expected non-completed progress event to be ignored"
  | Error err -> Error ("expected non-completed progress event to be ignored: " ^ err)

let test_collect_suite_binaries_keeps_multiple_package_filters = fun _ctx ->
  let workspace =
    Riot_model.Workspace.make_realized
      ~root:(Path.v "/workspace")
      ~packages:[ make_test_package "alpha"; make_test_package "beta"; make_test_package "gamma"; ]
      ()
  in
  let suites =
    Riot_cli.Test_runtime.collect_suite_binaries
      workspace
      ~package_filters:[ package_name "alpha"; package_name "gamma" ]
      ()
  in
  let actual =
    suites
    |> List.map
      ~fn:(fun (suite: Riot_cli.Test_runtime.suite_binary) ->
        Riot_model.Package_name.to_string suite.package_name ^ ":" ^ suite.suite_name)
  in
  Test.assert_equal ~expected:[ "alpha:alpha_tests"; "gamma:gamma_tests" ] ~actual;
  Ok ()

let tests = [
  Test.case "suite heartbeat event renders json" test_suite_heartbeat_event_to_json;
  Test.case
    "suite progress completed case parses into a test result"
    test_suite_progress_test_case_result_parses_completed_case;
  Test.case
    "suite progress completed fuzz case parses into a test result"
    test_suite_progress_test_case_result_parses_fuzz_case;
  Test.case
    "suite progress ignores non-completed events"
    test_suite_progress_test_case_result_ignores_non_completed_event;
  Test.case
    "collect suite binaries keeps multiple package filters"
    test_collect_suite_binaries_keeps_multiple_package_filters;
]

let main ~args = Test.Cli.main ~name:"test_runtime_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()

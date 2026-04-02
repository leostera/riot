open Std
module Test = Std.Test

let make_workspace = fun binaries ->
  let package =
    Tusk_model.Package.{
      name = "demo";
      path = Path.v "/workspace/packages/demo";
      relative_path = Path.v "packages/demo";
      dependencies = [];
      dev_dependencies = [];
      build_dependencies = [];
      foreign_dependencies = [];
      binaries;
      library = None;
      sources =
        {
          src = [];
          native = [];
          tests = [];
          examples = [];
          bench = [];
        };
      compiler = { profile_overrides = []; target_overrides = [] };
      commands = [];
      fix_providers = [];
      publish = { version = None; description = None; license = None; is_public = None };
    }
  in
  Tusk_model.Workspace.make ~root:(Path.v "/workspace") ~packages:[ package ] ()

let test_collect_test_suites_filters_workspace_binaries = fun _ctx ->
  let workspace = make_workspace
    [
      Tusk_model.Package.{ name = "alpha_tests"; path = Path.v "tests/alpha_tests.ml" };
      Tusk_model.Package.{ name = "main"; path = Path.v "src/main.ml" };
      Tusk_model.Package.{ name = "beta-tests"; path = Path.v "tests/beta-tests.ml" };
    ] in
  let actual = Tusk_build.collect_test_suites workspace () in
  Test.assert_equal
    ~expected:[
      Tusk_build.{ package_name = "demo"; suite_name = "alpha_tests" };
      Tusk_build.{ package_name = "demo"; suite_name = "beta-tests" };
    ]
    ~actual;
  Ok ()

let test_test_event_to_json_serializes_summary = fun _ctx ->
  match Tusk_build.test_event_to_json (Tusk_build.Summary { total = 3; passed = 2; failed = 1 }) with
  | Some (Data.Json.Object fields) ->
      Test.assert_equal
        ~expected:(Some (Data.Json.String "TestSummary"))
        ~actual:(List.assoc_opt "type" fields);
      Test.assert_equal ~expected:(Some (Data.Json.Int 3)) ~actual:(List.assoc_opt "total" fields);
      Ok ()
  | Some json ->
      Error ("expected JSON object, got " ^ Data.Json.to_string json)
  | None ->
      Error "expected JSON output for summary event"

let tests =
  let open Test in [
    case "test runtime: collect test suites" test_collect_test_suites_filters_workspace_binaries;
    case "test runtime: summary event json" test_test_event_to_json_serializes_summary;
  ]

let name = "Tusk Build Test Runtime Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()

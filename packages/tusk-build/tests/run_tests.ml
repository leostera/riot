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

let test_build_scope_for_test_binary_uses_dev = fun _ctx ->
  let workspace = make_workspace
    [ Tusk_model.Package.{ name = "pm_tests"; path = Path.v "tests/pm_tests.ml" } ] in
  Test.assert_equal
    ~expected:Tusk_build.Dev
    ~actual:(Tusk_build.build_scope_for_binary workspace ~package_name:"demo" ~binary_name:"pm_tests");
  Ok ()

let test_run_event_to_json_serializes_running_binary = fun _ctx ->
  match Tusk_build.run_event_to_json
    (Tusk_build.RunningBinary {
      package = "demo";
      binary = "pm_tests";
      args = [ "run-tests"; "query" ]
    }) with
  | Some (Data.Json.Object fields) ->
      Test.assert_equal
        ~expected:(Some (Data.Json.String "RunningBinary"))
        ~actual:(List.assoc_opt "type" fields);
      Test.assert_equal
        ~expected:(Some (Data.Json.String "demo"))
        ~actual:(List.assoc_opt "package" fields);
      Ok ()
  | Some json ->
      Error ("expected JSON object, got " ^ Data.Json.to_string json)
  | None ->
      Error "expected JSON output for running binary event"

let tests =
  let open Test in [
    case "run: test binaries use dev scope" test_build_scope_for_test_binary_uses_dev;
    case "run: running binary event json" test_run_event_to_json_serializes_running_binary;
  ]

let name = "Tusk Build Run Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()

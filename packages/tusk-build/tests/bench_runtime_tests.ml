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

let test_collect_bench_suites_filters_workspace_binaries = fun _ctx ->
  let workspace = make_workspace
    [
      Tusk_model.Package.{ name = "alpha_bench"; path = Path.v "bench/alpha_bench.ml" };
      Tusk_model.Package.{ name = "main"; path = Path.v "src/main.ml" };
      Tusk_model.Package.{ name = "beta-bench"; path = Path.v "bench/beta-bench.ml" };
    ] in
  let actual = Tusk_build.collect_bench_suites workspace () in
  Test.assert_equal
    ~expected:[
      Tusk_build.{ package_name = "demo"; suite_name = "alpha_bench" };
      Tusk_build.{ package_name = "demo"; suite_name = "beta-bench" };
    ]
    ~actual;
  Ok ()

let test_bench_event_to_json_serializes_summary = fun _ctx ->
  match Tusk_build.bench_event_to_json (Tusk_build.Summary { total = 3; passed = 2; failed = 1 }) with
  | Some (Data.Json.Object fields) ->
      Test.assert_equal
        ~expected:(Some (Data.Json.String "BenchSummary"))
        ~actual:(List.assoc_opt "type" fields);
      Test.assert_equal ~expected:(Some (Data.Json.Int 3)) ~actual:(List.assoc_opt "total" fields);
      Ok ()
  | Some json ->
      Error ("expected JSON object, got " ^ Data.Json.to_string json)
  | None ->
      Error "expected JSON output for summary event"

let tests =
  let open Test in [
    case "bench runtime: collect benchmark suites" test_collect_bench_suites_filters_workspace_binaries;
    case "bench runtime: summary event json" test_bench_event_to_json_serializes_summary;
  ]

let name = "Tusk Build Bench Runtime Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()

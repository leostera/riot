open Std
open Std.Collections
module Test = Std.Test

let test_workspace_completed_is_silent = fun _ctx ->
  let displayed_packages = HashSet.create () in
  let event = Tusk_executor.Telemetry_events.WorkspaceCompleted {
    session_id = Tusk_model.Session_id.make ();
    target = Tusk_planner.Workspace_planner.All;
    total_duration = Time.Duration.from_millis 42;
    cached_count = 1;
    built_count = 2;
    failed_count = 0;
  }
  in
  let rendered = Tusk_cli.Event_formatter.format ~displayed_packages event in
  if String.equal rendered "" then
    Ok ()
  else
    Error ("expected empty workspace summary, got: " ^ rendered)

let test_build_failed_prefixes_package_name = fun _ctx ->
  let displayed_packages = HashSet.create () in
  let package =
    Tusk_model.Package.{
      name = "syn";
      path = Path.v ".";
      relative_path = Path.v ".";
      dependencies = [];
      dev_dependencies = [];
      build_dependencies = [];
      foreign_dependencies = [];
      binaries = [];
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
  let event = Tusk_executor.Telemetry_events.BuildFailed {
    session_id = Tusk_model.Session_id.make ();
    package;
    target = Tusk_planner.Workspace_planner.Package package.name;
    error = Tusk_executor.Telemetry_events.ExecutionFailed { message = "Command failed" }
  } in
  let rendered = Tusk_cli.Event_formatter.format ~displayed_packages event in
  if String.contains rendered "syn: Execution failed: Command failed" then
    Ok ()
  else
    Error ("expected package-prefixed error, got: " ^ rendered)

let test_package_ocamlc_warnings_prefix_package_name = fun _ctx ->
  let displayed_packages = HashSet.create () in
  let package =
    Tusk_model.Package.{
      name = "tusk-eval";
      path = Path.v ".";
      relative_path = Path.v ".";
      dependencies = [];
      dev_dependencies = [];
      build_dependencies = [];
      foreign_dependencies = [];
      binaries = [];
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
  let event = Tusk_executor.Telemetry_events.PackageOcamlcWarnings {
    session_id = Tusk_model.Session_id.make ();
    package;
    target = Tusk_planner.Workspace_planner.Package package.name;
    source = `Cached;
    messages = [ "File \"x.ml\", line 1, characters 0-1:\nWarning: example" ];
  }
  in
  let rendered = Tusk_cli.Event_formatter.format ~displayed_packages event in
  if String.contains rendered "tusk-eval: File \"x.ml\"" then
    Ok ()
  else
    Error ("expected package-prefixed warning, got: " ^ rendered)

let tests =
  Test.[
    case "event formatter: workspace completed is silent" test_workspace_completed_is_silent;
    case "event formatter: build failed prefixes package name" test_build_failed_prefixes_package_name;
    case "event formatter: package ocamlc warnings prefix package name" test_package_ocamlc_warnings_prefix_package_name;
  ]

let name = "Tusk CLI Event Formatter Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()

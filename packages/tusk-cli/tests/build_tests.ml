open Std
module Test = Std.Test
module HashSet = Std.Collections.HashSet

let parse_build = fun args ->
  match ArgParser.get_matches Tusk_cli.Build.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let test_build_accepts_multiple_packages = fun _ctx ->
  match parse_build [ "build"; "syn"; "krasny"; "tusk-cli" ] with
  | Error err -> Error ("expected build args to parse: " ^ err)
  | Ok matches ->
      let actual = ArgParser.get_many matches "package" in
      Test.assert_equal ~expected:[ "syn"; "krasny"; "tusk-cli" ] ~actual;
      Ok ()

let test_build_usage_shows_variadic_packages = fun _ctx ->
  let usage = ArgParser.usage_string Tusk_cli.Build.command in
  if String.contains usage "package..." then
    Ok ()
  else
    Error ("expected variadic package usage, got: " ^ usage)

let test_build_accepts_json_flag = fun _ctx ->
  match parse_build [ "build"; "--json"; "syn" ] with
  | Error err -> Error ("expected build args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "json" then
        Ok ()
      else
        Error "expected --json flag to be parsed"

let test_build_accepts_release_flag = fun _ctx ->
  match parse_build [ "build"; "--release"; "syn" ] with
  | Error err -> Error ("expected build args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "release" then
        Ok ()
      else
        Error "expected --release flag to be parsed"

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

let test_run_build_scope_uses_runtime_for_runtime_binaries = fun _ctx ->
  let workspace = make_workspace
    [ Tusk_model.Package.{ name = "demo"; path = Path.v "src/demo.ml" } ] in
  Test.assert_equal
    ~expected:Tusk_cli.Build.Runtime
    ~actual:(Tusk_cli.Run.build_scope_for_binary workspace ~package_name:"demo" ~binary_name:"demo");
  Ok ()

let test_run_build_scope_uses_dev_for_test_binaries = fun _ctx ->
  let workspace = make_workspace
    [ Tusk_model.Package.{ name = "pm_tests"; path = Path.v "tests/pm_tests.ml" } ] in
  Test.assert_equal
    ~expected:Tusk_cli.Build.Dev
    ~actual:(Tusk_cli.Run.build_scope_for_binary workspace ~package_name:"demo" ~binary_name:"pm_tests");
  Ok ()

let test_run_build_scope_defaults_to_runtime_when_binary_is_missing = fun _ctx ->
  let workspace = make_workspace [] in
  Test.assert_equal
    ~expected:Tusk_cli.Build.Runtime
    ~actual:(Tusk_cli.Run.build_scope_for_binary workspace ~package_name:"demo" ~binary_name:"missing");
  Ok ()

let test_pm_event_hides_workspace_resolved_packages = fun _ctx ->
  let seen_registry_updates = HashSet.create () in
  let actual = Tusk_cli.Build.format_pm_event
    ~seen_registry_updates
    (Tusk_model.Event.PackageResolvedForBuild {
      package = "create-riot-app";
      version = None;
      path = "/workspace";
      workspace = true
    }) in
  Test.assert_equal ~expected:None ~actual;
  Ok ()

let test_pm_event_maps_materialization_to_fetching = fun _ctx ->
  let seen_registry_updates = HashSet.create () in
  let actual = Tusk_cli.Build.format_pm_event
    ~seen_registry_updates
    (Tusk_model.Event.PackageMaterializationStarted {
      package = "std";
      version = "0.1.0";
      path = "/cache/std"
    }) in
  match actual with
  | Some message ->
      Test.assert_true (String.contains message "Fetching");
      Test.assert_true (String.contains message "std 0.1.0");
      Ok ()
  | None -> Error "expected materialization event to render as a fetching message"

let test_pm_event_hides_manifest_fetch_chatter = fun _ctx ->
  let seen_registry_updates = HashSet.create () in
  let actual = Tusk_cli.Build.format_pm_event
    ~seen_registry_updates
    (Tusk_model.Event.PackageManifestFetchStarted { package = "std"; version = "0.1.0" }) in
  Test.assert_equal ~expected:None ~actual;
  Ok ()

let test_pm_event_hides_download_skipped = fun _ctx ->
  let seen_registry_updates = HashSet.create () in
  let actual = Tusk_cli.Build.format_pm_event
    ~seen_registry_updates
    (Tusk_model.Event.PackageDownloadSkipped {
      package = "std";
      version = "0.1.0";
      path = "/cache/std";
      reason = "already materialized"
    }) in
  Test.assert_equal ~expected:None ~actual;
  Ok ()

let tests =
  Test.[
    case "build: accept multiple package arguments" test_build_accepts_multiple_packages;
    case "build: usage shows variadic packages" test_build_usage_shows_variadic_packages;
    case "build: parse --json flag" test_build_accepts_json_flag;
    case "build: parse --release flag" test_build_accepts_release_flag;
    case "run: runtime binaries use runtime scope" test_run_build_scope_uses_runtime_for_runtime_binaries;
    case "run: test binaries use dev scope" test_run_build_scope_uses_dev_for_test_binaries;
    case "run: missing binaries default to runtime scope" test_run_build_scope_defaults_to_runtime_when_binary_is_missing;
    case "build: pm events hide workspace resolved packages" test_pm_event_hides_workspace_resolved_packages;
    case "build: pm materialization renders as fetching" test_pm_event_maps_materialization_to_fetching;
    case "build: pm manifest fetch chatter is hidden" test_pm_event_hides_manifest_fetch_chatter;
    case "build: pm download skipped is hidden" test_pm_event_hides_download_skipped;
  ]

let name = "Tusk CLI Build Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()

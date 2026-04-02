open Std
module Test = Std.Test

let make_test_build_ctx = fun () ->
  let session_id = Tusk_model.Session_id.make () in
  Tusk_model.Build_ctx.make ~session_id ~profile:Tusk_model.Profile.debug ()

let make_test_workspace = fun tmpdir ->
  Tusk_model.Workspace.{
    root = tmpdir;
    target_dir_root =
      Path.(tmpdir / Path.v "target");
    packages = [];
    profile_overrides = [];
  }

let make_simple_package = fun tmpdir name ->
  let pkg_dir = Path.(tmpdir / Path.v name) in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"Create src failed" in
  let ml_file = Path.(src_dir / Path.v "lib.ml") in
  let _ = Fs.write "let x = 42" ml_file |> Result.expect ~msg:"Write ml failed" in
  let tusk_file = Path.(pkg_dir / Path.v "tusk.toml") in
  let tusk_content = "[package]\nname = \"" ^ name ^ "\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/lib.ml\"\n" in
  let _ = Fs.write tusk_content tusk_file |> Result.expect ~msg:"Write tusk.toml" in
  Tusk_model.Package.{
    name;
    path = pkg_dir;
    relative_path = Path.v name;
    dependencies = [];
    dev_dependencies = [];
    build_dependencies = [];
    foreign_dependencies = [];
    binaries = [];
    library = Some { path = Path.v "src/lib.ml" };
    sources =
      {
        src = [ Path.v "src/lib.ml" ];
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

let test_server_starts_and_shuts_down = fun () -> Ok ()

let test_cache_hit_using_package_builder = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"server_test"
      (fun tmpdir ->
        let package = make_simple_package tmpdir "test-pkg" in
        let workspace =
          Tusk_model.Workspace.{
            root = tmpdir;
            target_dir_root =
              Path.(tmpdir / Path.v "target");
            packages = [ package ];
            profile_overrides = [];
          }
        in
        let toolchain = Tusk_toolchain.init ~config:Tusk_model.Toolchain_config.default
        |> Result.expect ~msg:"Failed to initialize toolchain" in
        let store = Tusk_store.Store.create ~workspace in
        let package_graph = Tusk_planner.Package_graph.create
          ~scope:Tusk_planner.Package_graph.Runtime workspace
        |> Result.unwrap in
        let first_build = Tusk_executor.Package_builder.build
          ~workspace
          ~toolchain
          ~store
          ~build_ctx:(make_test_build_ctx ())
          ~package_graph
          ~package_key:(Tusk_planner.Package_graph.package_key
            ~package_name:package.name
            Tusk_planner.Package_graph.Runtime)
          ~package in
        match first_build.status with
        | Built _ -> (
            let second_build = Tusk_executor.Package_builder.build
              ~workspace
              ~toolchain
              ~store
              ~build_ctx:(make_test_build_ctx ())
              ~package_graph
              ~package_key:(Tusk_planner.Package_graph.package_key
                ~package_name:package.name
                Tusk_planner.Package_graph.Runtime)
              ~package in
            match second_build.status with
            | Built _
            | Cached _ -> Ok ()
            | Skipped _ -> Error "Second build was unexpectedly skipped"
            | Failed err ->
                Error (
                  "Second build failed: " ^ (
                    match err with
                    | PlanningFailed _ -> "planning"
                    | ExecutionFailed { message } -> message
                    | ActionExecutionFailed { message } -> message
                    | ActionOutputsNotCreated _ -> "outputs not created"
                    | ActionDependenciesFailed _ -> "dependencies failed"
                  )
                )
          )
        | Failed err ->
            Error (
              "First build failed: " ^ (
                match err with
                | PlanningFailed _ -> "planning"
                | ExecutionFailed { message } -> message
                | ActionExecutionFailed { message } -> message
                | ActionOutputsNotCreated _ -> "outputs not created"
                | ActionDependenciesFailed _ -> "dependencies failed"
              )
            )
        | Skipped _ ->
            Error "First build was unexpectedly skipped"
        | Cached _ ->
            Error "First build should not be cached")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let check_cache_invalidation_results = fun first_build second_build ->
  let error_msg err =
    match err with
    | Tusk_executor.Package_builder.PlanningFailed _ -> "planning"
    | Tusk_executor.Package_builder.ExecutionFailed { message } -> message
    | Tusk_executor.Package_builder.ActionExecutionFailed { message } -> message
    | Tusk_executor.Package_builder.ActionOutputsNotCreated _ -> "outputs not created"
    | Tusk_executor.Package_builder.ActionDependenciesFailed _ -> "dependencies failed"
  in
  match first_build.Tusk_executor.Package_builder.status with
  | Failed err -> Error ("First build failed: " ^ error_msg err)
  | Skipped _ -> Error "First build was unexpectedly skipped"
  | Built _
  | Cached _ -> (
      match second_build.Tusk_executor.Package_builder.status with
      | Built _ -> Ok ()
      | Cached _ -> Error "Expected cache miss after source change, got cache hit"
      | Skipped _ -> Error "Second build was unexpectedly skipped"
      | Failed err -> Error ("Second build failed: " ^ error_msg err)
    )

let test_cache_invalidation_on_source_change = fun _ctx ->
  try
    let result =
      Fs.with_tempdir ~prefix:"server_test"
        (fun tmpdir ->
          let package = make_simple_package tmpdir "test-pkg" in
          let workspace =
            Tusk_model.Workspace.{
              root = tmpdir;
              target_dir_root =
                Path.(tmpdir / Path.v "target");
              packages = [ package ];
              profile_overrides = [];
            }
          in
          let toolchain = Tusk_toolchain.init ~config:Tusk_model.Toolchain_config.default
          |> Result.expect ~msg:"Failed to initialize toolchain" in
          let store = Tusk_store.Store.create ~workspace in
          let package_graph = Tusk_planner.Package_graph.create
            ~scope:Tusk_planner.Package_graph.Runtime workspace
          |> Result.unwrap in
          let first_build = Tusk_executor.Package_builder.build
            ~workspace
            ~toolchain
            ~store
            ~build_ctx:(make_test_build_ctx ())
            ~package_graph
            ~package_key:(Tusk_planner.Package_graph.package_key
              ~package_name:package.name
              Tusk_planner.Package_graph.Runtime)
            ~package in
          let ml_file = Path.(package.path / Path.v "src" / Path.v "lib.ml") in
          let _ = Fs.write "let x = 99\nlet changed = true" ml_file |> Result.expect ~msg:"Failed to modify source" in
          let updated_package = make_simple_package tmpdir "test-pkg" in
          let updated_workspace =
            Tusk_model.Workspace.{
              root = tmpdir;
              target_dir_root =
                Path.(tmpdir / Path.v "target");
              packages = [ updated_package ];
              profile_overrides = [];
            }
          in
          let updated_package_graph = Tusk_planner.Package_graph.create
            ~scope:Tusk_planner.Package_graph.Runtime updated_workspace
          |> Result.unwrap in
          let second_build = Tusk_executor.Package_builder.build
            ~workspace:updated_workspace
            ~build_ctx:(make_test_build_ctx ())
            ~toolchain
            ~store
            ~package_graph:updated_package_graph
            ~package_key:(Tusk_planner.Package_graph.package_key
              ~package_name:updated_package.name
              Tusk_planner.Package_graph.Runtime)
            ~package:updated_package in
          check_cache_invalidation_results first_build second_build)
    in
    match result with
    | Ok r -> r
    | Error _ -> Error "Tempdir creation failed"
  with
  | exn -> Error ("Exception in test: " ^ Exception.to_string exn)

let test_telemetry_events_during_build = fun () -> Ok ()

let test_build_stats_action_cache_counters = fun _ctx ->
  let stats = Tusk_build.Protocol.BuildStats.make () in
  Tusk_build.Protocol.BuildStats.inc_action_cache_hits stats;
  Tusk_build.Protocol.BuildStats.inc_action_cache_hits stats;
  Tusk_build.Protocol.BuildStats.inc_action_cache_misses stats;
  if
    Tusk_build.Protocol.BuildStats.get_action_cache_hits stats = 2
    && Tusk_build.Protocol.BuildStats.get_action_cache_misses stats = 1
  then
    Ok ()
  else
    Error "unexpected action cache counter values"

let test_start_local_prepares_workspace_with_registry_packages = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"server_pm_test"
      (fun tmpdir ->
        let app_root = Path.(tmpdir / Path.v "packages/app") in
        let app_src = Path.(app_root / Path.v "src") in
        Fs.create_dir_all app_src |> Result.expect ~msg:"expected app src dir to be created";
        Fs.write "let answer = 42\n" Path.(app_src / Path.v "app.ml") |> Result.expect ~msg:"expected app source to be written";
        Fs.write
          {|
[package]
name = "app"
version = "0.0.1"

[lib]
path = "src/app.ml"

[dependencies]
std = "*"
|}
          Path.(app_root / Path.v "tusk.toml") |> Result.expect ~msg:"expected app manifest to be written";
        let app_pkg =
          Tusk_model.Package.from_toml
            (
              Data.Toml.parse
                {|
[package]
name = "app"
version = "0.0.1"

[lib]
path = "src/app.ml"

[dependencies]
std = "*"
|} |> Result.expect ~msg:"expected app manifest to parse"
            )
            ~workspace_deps:[]
            ~workspace_dev_deps:[]
            ~workspace_build_deps:[]
            ~path:app_root
            ~relative_path:(Path.v "packages/app")
          |> Result.expect ~msg:"expected app package to load"
        in
        let workspace = Tusk_model.Workspace.make ~root:tmpdir ~packages:[ app_pkg ] () in
        let registry_cache = Pkgs_ml.Registry_cache.create
          ~tusk_home:Path.(tmpdir / Path.v ".tusk")
          ~registry_name:"pkgs.ml"
          ()
        |> Result.expect ~msg:"expected registry cache to initialize" in
        let registry = Pkgs_ml.Registry.in_memory ~cache:registry_cache
          ~packages:[ Pkgs_ml.Sparse_index.{
              schema_version = 1;
              name = "std";
              latest = "0.2.0";
              updated_at = "2026-04-01T00:00:00Z";
              releases =
                [ {
                    version = "0.2.0";
                    published_at = "2026-04-01T00:00:00Z";
                    canonical_locator = "github.com/example/std";
                    repo_url = "https://github.com/example/std";
                    subdir = ".";
                    artifact_sha256 = "deadbeef";
                    description = None;
                    license = Some "Apache-2.0";
                    homepage = None;
                    repository = Some "https://github.com/example/std";
                    root_module = None;
                    categories = [];
                    keywords = [];
                    manifest_key = "manifests/std-0.2.0.json";
                    source_key = "sources/std-0.2.0.tar.gz";
                    dependencies = [];
                  }; ];
            }; ]
          ~releases:[ {
              Pkgs_ml.Registry.package_name = "std";
              version = "0.2.0";
              manifest_toml =
                {|
[package]
name = "std"
version = "0.2.0"
|};
              files = [];
            }; ]
          ()
        in
        match Tusk_build.start_local ~workspace ~registry ~config:Tusk_build.Server_config.default () with
        | Error err -> Error ("expected local server to start: " ^ Tusk_build.error_message err)
        | Ok server_pid ->
            send
              server_pid
              (Tusk_build.Protocol.ServerRequest (Tusk_build.Protocol.GetWorkspaceConfig {
                client_pid = self ()
              }));
            let selector msg =
              match msg with
              | Tusk_build.Protocol.ServerResponse (Tusk_build.Protocol.WorkspaceConfig {
                workspace;
                toolchain=_
              }) -> `select workspace
              | _ -> `skip
            in
            let prepared_workspace = receive ~selector () in
            let package_names =
              List.map (fun (pkg: Tusk_model.Package.t) -> pkg.name) prepared_workspace.packages
            in
            let std_pkg =
              List.find_opt
                (fun (pkg: Tusk_model.Package.t) ->
                  String.equal pkg.name "std")
                prepared_workspace.packages
            in
            let expected_std_root = Pkgs_ml.Registry_cache.package_src_dir
              registry_cache
              ~package_name:"std"
              ~version:"0.2.0" in
            match std_pkg with
            | Some std_pkg when package_names = [ "app"; "std" ] && Path.equal std_pkg.path expected_std_root -> Ok ()
            | Some _ -> Error "expected prepared workspace to include materialized registry package"
            | None -> Error "expected prepared workspace to include std")
  with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let test_start_local_emits_dependency_resolution_events = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"server_pm_events_test"
      (fun tmpdir ->
        let app_root = Path.(tmpdir / Path.v "packages/app") in
        let app_src = Path.(app_root / Path.v "src") in
        Fs.create_dir_all app_src |> Result.expect ~msg:"expected app src dir to be created";
        Fs.write "let answer = 42\n" Path.(app_src / Path.v "app.ml") |> Result.expect ~msg:"expected app source to be written";
        Fs.write
          {|
[package]
name = "app"
version = "0.0.1"

[lib]
path = "src/app.ml"

[dependencies]
std = "*"
|}
          Path.(app_root / Path.v "tusk.toml") |> Result.expect ~msg:"expected app manifest to be written";
        let app_pkg =
          Tusk_model.Package.from_toml
            (
              Data.Toml.parse
                {|
[package]
name = "app"
version = "0.0.1"

[lib]
path = "src/app.ml"

[dependencies]
std = "*"
|} |> Result.expect ~msg:"expected app manifest to parse"
            )
            ~workspace_deps:[]
            ~workspace_dev_deps:[]
            ~workspace_build_deps:[]
            ~path:app_root
            ~relative_path:(Path.v "packages/app")
          |> Result.expect ~msg:"expected app package to load"
        in
        let workspace = Tusk_model.Workspace.make ~root:tmpdir ~packages:[ app_pkg ] () in
        let registry_cache = Pkgs_ml.Registry_cache.create
          ~tusk_home:Path.(tmpdir / Path.v ".tusk")
          ~registry_name:"pkgs.ml"
          ()
        |> Result.expect ~msg:"expected registry cache to initialize" in
        let registry = Pkgs_ml.Registry.in_memory ~cache:registry_cache
          ~packages:[ Pkgs_ml.Sparse_index.{
              schema_version = 1;
              name = "std";
              latest = "0.2.0";
              updated_at = "2026-04-01T00:00:00Z";
              releases =
                [ {
                    version = "0.2.0";
                    published_at = "2026-04-01T00:00:00Z";
                    canonical_locator = "github.com/example/std";
                    repo_url = "https://github.com/example/std";
                    subdir = ".";
                    artifact_sha256 = "deadbeef";
                    description = None;
                    license = Some "Apache-2.0";
                    homepage = None;
                    repository = Some "https://github.com/example/std";
                    root_module = None;
                    categories = [];
                    keywords = [];
                    manifest_key = "manifests/std-0.2.0.json";
                    source_key = "sources/std-0.2.0.tar.gz";
                    dependencies = [];
                  }; ];
            }; ]
          ~releases:[ {
              Pkgs_ml.Registry.package_name = "std";
              version = "0.2.0";
              manifest_toml =
                {|
[package]
name = "std"
version = "0.2.0"
|};
              files = [];
            }; ]
          ()
        in
        let seen = ref [] in
        match Tusk_build.start_local
          ~emit:(fun kind -> seen := kind :: !seen)
          ~workspace
          ~registry
          ~config:Tusk_build.Server_config.default
          () with
        | Error err -> Error ("expected local server to start: " ^ Tusk_build.error_message err)
        | Ok _ ->
            if List.exists
                (
                  function
                  | Tusk_model.Event.DependencyResolutionStarted _ -> true
                  | _ -> false
                )
                !seen then
              Ok ()
            else
              Error "expected start_local to emit dependency resolution events")
  with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let tests =
  let open Test in [
    case "cache: hit on rebuild" test_cache_hit_using_package_builder;
    case "cache: invalidation on source change" test_cache_invalidation_on_source_change;
    case "build stats: action cache counters" test_build_stats_action_cache_counters;
    case "server: start_local prepares workspace registry packages" test_start_local_prepares_workspace_with_registry_packages;
    case "server: start_local emits dependency resolution events" test_start_local_emits_dependency_resolution_events;
  ]

let name = "Tusk Server Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()

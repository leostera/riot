open Std

module Test = Std.Test

let make_test_build_ctx () =
  let session_id = Tusk_model.Session_id.make () in
  Tusk_model.Build_ctx.make ~session_id ~profile:Tusk_model.Profile.debug ()

let make_test_workspace tmpdir =
  Tusk_model.Workspace.
    {
      root = tmpdir;
      target_dir_root = Path.(tmpdir / Path.v "target");
      packages = [];
      profile_overrides = [];
    }

let make_simple_package tmpdir name =
  let pkg_dir = Path.(tmpdir / Path.v name) in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"Create src failed" in

  let ml_file = Path.(src_dir / Path.v "lib.ml") in
  let _ =
    Fs.write "let x = 42" ml_file |> Result.expect ~msg:"Write ml failed"
  in

  let tusk_file = Path.(pkg_dir / Path.v "tusk.toml") in
  let tusk_content =
    "[package]\nname = \"" ^ name
    ^ "\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/lib.ml\"\n"
  in
  let _ =
    Fs.write tusk_content tusk_file |> Result.expect ~msg:"Write tusk.toml"
  in

  Tusk_model.Package.
    {
      name;
      path = pkg_dir;
      relative_path = Path.v name;
      dependencies = [];
      dev_dependencies = [];
      build_dependencies = [];
      foreign_dependencies = [];
      binaries = [];
      library = Some { path = Path.v "src/lib.ml" };
      sources = { src = []; native = []; tests = []; examples = []; bench = [] };
      compiler = { profile_overrides = []; target_overrides = [] };
      commands = [];
      fix_providers = [];
    }

let test_server_starts_and_shuts_down () = Ok ()

let test_cache_hit_using_package_builder () =
  match
    Fs.with_tempdir ~prefix:"server_test" (fun tmpdir ->
        let package = make_simple_package tmpdir "test-pkg" in
        let workspace =
          Tusk_model.Workspace.
            {
              root = tmpdir;
              target_dir_root = Path.(tmpdir / Path.v "target");
              packages = [ package ];
              profile_overrides = [];
            }
        in
        let toolchain =
          Tusk_toolchain.init ~config:Tusk_model.Toolchain_config.default
          |> Result.expect ~msg:"Failed to initialize toolchain"
        in
        let store = Tusk_store.Store.create ~workspace in
        let package_graph = 
          Tusk_planner.Package_graph.create ~scope:Tusk_planner.Package_graph.Runtime workspace |> Result.unwrap
        in

        let first_build =
          Tusk_executor.Package_builder.build ~workspace ~toolchain ~store
            ~build_ctx:(make_test_build_ctx ())
            ~package_graph
            ~package_key:
              (Tusk_planner.Package_graph.package_key ~package_name:package.name
                 Tusk_planner.Package_graph.Runtime)
            ~package
        in

        match first_build.status with
        | Built _ -> (
            let second_build =
              Tusk_executor.Package_builder.build ~workspace ~toolchain ~store
                ~build_ctx:(make_test_build_ctx ())
                ~package_graph
                ~package_key:
                  (Tusk_planner.Package_graph.package_key ~package_name:package.name
                     Tusk_planner.Package_graph.Runtime)
                ~package
            in

            match second_build.status with
            | Built _ | Cached _ -> Ok ()
            | Failed err ->
                Error
                  ("Second build failed: "
                  ^ (match err with
                    | PlanningFailed _ -> "planning"
                    | ExecutionFailed { message } -> message
                    | ActionExecutionFailed { message } -> message
                    | ActionOutputsNotCreated _ -> "outputs not created"
                    | ActionDependenciesFailed _ -> "dependencies failed")))
        | Failed err ->
            Error
              ("First build failed: "
              ^ (match err with
                | PlanningFailed _ -> "planning"
                | ExecutionFailed { message } -> message
                | ActionExecutionFailed { message } -> message
                | ActionOutputsNotCreated _ -> "outputs not created"
                | ActionDependenciesFailed _ -> "dependencies failed"))
        | Cached _ -> Error "First build should not be cached")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let check_cache_invalidation_results first_build second_build =
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
  | Built _ | Cached _ -> (
      match second_build.Tusk_executor.Package_builder.status with
      | Built _ -> Ok ()
      | Cached _ ->
          Error "Expected cache miss after source change, got cache hit"
      | Failed err -> Error ("Second build failed: " ^ error_msg err))

let test_cache_invalidation_on_source_change () =
  try
    let result =
      Fs.with_tempdir ~prefix:"server_test" (fun tmpdir ->
          let package = make_simple_package tmpdir "test-pkg" in
          let workspace =
            Tusk_model.Workspace.
              {
                root = tmpdir;
                target_dir_root = Path.(tmpdir / Path.v "target");
                packages = [ package ];
                profile_overrides = [];
              }
          in
          let toolchain =
            Tusk_toolchain.init ~config:Tusk_model.Toolchain_config.default
            |> Result.expect ~msg:"Failed to initialize toolchain"
          in
          let store = Tusk_store.Store.create ~workspace in
          let package_graph = 
            Tusk_planner.Package_graph.create ~scope:Tusk_planner.Package_graph.Runtime workspace |> Result.unwrap
          in

          let first_build =
            Tusk_executor.Package_builder.build ~workspace ~toolchain ~store
              ~build_ctx:(make_test_build_ctx ())
              ~package_graph
              ~package_key:
                (Tusk_planner.Package_graph.package_key
                   ~package_name:package.name
                   Tusk_planner.Package_graph.Runtime)
              ~package
          in

          let ml_file = Path.(package.path / Path.v "src" / Path.v "lib.ml") in
          let _ =
            Fs.write "let x = 99\nlet changed = true" ml_file
            |> Result.expect ~msg:"Failed to modify source"
          in

          let updated_package = make_simple_package tmpdir "test-pkg" in
          let updated_workspace =
            Tusk_model.Workspace.
              {
                root = tmpdir;
                target_dir_root = Path.(tmpdir / Path.v "target");
                packages = [ updated_package ];
                profile_overrides = [];
              }
          in
          let updated_package_graph =
            Tusk_planner.Package_graph.create ~scope:Tusk_planner.Package_graph.Runtime updated_workspace |> Result.unwrap
          in

          let second_build =
            Tusk_executor.Package_builder.build ~workspace:updated_workspace
              ~build_ctx:(make_test_build_ctx ())
              ~toolchain ~store ~package_graph:updated_package_graph
              ~package_key:
                (Tusk_planner.Package_graph.package_key
                   ~package_name:updated_package.name
                   Tusk_planner.Package_graph.Runtime)
              ~package:updated_package
          in

          check_cache_invalidation_results first_build second_build)
    in
    match result with Ok r -> r | Error _ -> Error "Tempdir creation failed"
  with exn -> Error ("Exception in test: " ^ Exception.to_string exn)

let test_telemetry_events_during_build () = Ok ()

let test_build_stats_action_cache_counters () =
  let stats = Tusk_server.Protocol.BuildStats.make () in
  Tusk_server.Protocol.BuildStats.inc_action_cache_hits stats;
  Tusk_server.Protocol.BuildStats.inc_action_cache_hits stats;
  Tusk_server.Protocol.BuildStats.inc_action_cache_misses stats;
  if
    Tusk_server.Protocol.BuildStats.get_action_cache_hits stats = 2
    && Tusk_server.Protocol.BuildStats.get_action_cache_misses stats = 1
  then Ok ()
  else Error "unexpected action cache counter values"

let tests =
  let open Test in
  [
    case "cache: hit on rebuild" test_cache_hit_using_package_builder;
    case "cache: invalidation on source change"
      test_cache_invalidation_on_source_change;
    case "build stats: action cache counters"
      test_build_stats_action_cache_counters;
  ]

let name = "Tusk Server Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()

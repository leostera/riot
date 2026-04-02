open Std
module Test = Std.Test

let make_test_build_ctx = fun () ->
  let session_id = Tusk_model.Session_id.make () in
  Tusk_model.Build_ctx.make ~session_id ~profile:Tusk_model.Profile.debug ()

let make_test_workspace = fun tmpdir packages ->
  Tusk_model.Workspace.{
    root = tmpdir;
    target_dir_root =
      Path.(tmpdir / Path.v "target");
    packages;
    profile_overrides = [];
  }

let package_error_message = fun err ->
  match err with
  | Tusk_executor.Package_builder.PlanningFailed _ -> "planning"
  | Tusk_executor.Package_builder.ExecutionFailed { message } -> message
  | Tusk_executor.Package_builder.ActionExecutionFailed { message } -> message
  | Tusk_executor.Package_builder.ActionOutputsNotCreated _ -> "outputs not created"
  | Tusk_executor.Package_builder.ActionDependenciesFailed _ -> "dependencies failed"

let make_package = fun tmpdir name content ->
  let pkg_dir = Path.(tmpdir / Path.v name) in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"Create src failed" in
  let ml_file = Path.(src_dir / Path.v "lib.ml") in
  let _ = Fs.write content ml_file |> Result.expect ~msg:"Write ml failed" in
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

let test_fresh_build_no_cache = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"cache_test"
      (fun tmpdir ->
        let package = make_package tmpdir "test-pkg" "let x = 42" in
        let workspace = make_test_workspace tmpdir [ package ] in
        let toolchain = Tusk_toolchain.init ~config:Tusk_model.Toolchain_config.default
        |> Result.expect ~msg:"Failed to initialize toolchain" in
        let store = Tusk_store.Store.create ~workspace in
        let package_graph = Tusk_planner.Package_graph.create
          ~scope:Tusk_planner.Package_graph.Runtime workspace
        |> Result.unwrap in
        let build = Tusk_executor.Package_builder.build
          ~workspace
          ~toolchain
          ~store
          ~build_ctx:(make_test_build_ctx ())
          ~package_graph
          ~package_key:(Tusk_planner.Package_graph.package_key
            ~package_name:package.name
            Tusk_planner.Package_graph.Runtime)
          ~package in
        match build.status with
        | Tusk_executor.Package_builder.Built _ -> Ok ()
        | Tusk_executor.Package_builder.Cached _ -> Error "Fresh build should not be cached"
        | Tusk_executor.Package_builder.Skipped { reason } -> Error ("Build skipped: " ^ reason)
        | Tusk_executor.Package_builder.Failed err ->
            Error ("Build failed: " ^ package_error_message err))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_second_build_reuses_action_cache_path = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"cache_test"
      (fun tmpdir ->
        let package = make_package tmpdir "test-pkg" "let x = 42" in
        let workspace = make_test_workspace tmpdir [ package ] in
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
            | Skipped { reason } -> Error ("Second build skipped: " ^ reason)
            | Failed err ->
                Error ("Second build failed: " ^ package_error_message err)
          )
        | Skipped { reason } ->
            Error ("First build skipped: " ^ reason)
        | Cached _ ->
            Error "First build should not be cached"
        | Failed err ->
            Error ("First build failed: " ^ package_error_message err))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let tests =
  let open Test in [
    case "cache: fresh build, no cache" test_fresh_build_no_cache;
    case "cache: second build, action cache path" test_second_build_reuses_action_cache_path;
  ]

let name = "Cache Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()

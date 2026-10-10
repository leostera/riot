open Std
open Riot_build
module Test = Std.Test

let package_name = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let make_test_build_ctx = fun () ->
  let session_id = Riot_model.Session_id.make () in
  Riot_model.Build_ctx.make ~session_id ~profile:Riot_model.Profile.debug ()

let make_test_workspace = fun tmpdir packages ->
  Riot_model.Workspace.make_realized
    ~root:tmpdir
    ~packages
    ~target_dir:(Path.to_string Path.(Path.v "target"))
    ()

let package_error_message = fun err ->
  match err with
  | Package_builder.PlanningFailed _ -> "planning"
  | Package_builder.ExecutionFailed { message } -> message
  | Package_builder.ActionExecutionFailed { message } -> message
  | Package_builder.ActionOutputsNotCreated _ -> "outputs not created"
  | Package_builder.ActionDependenciesFailed _ -> "dependencies failed"

let make_package = fun tmpdir name content ->
  let pkg_dir = Path.(tmpdir / Path.v name) in
  let package_name = package_name name in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"Create src failed" in
  let ml_file = Path.(src_dir / Path.v "lib.ml") in
  let _ = Fs.write content ml_file |> Result.expect ~msg:"Write ml failed" in
  let riot_file = Path.(pkg_dir / Path.v "riot.toml") in
  let riot_content = "[package]\nname = \"" ^ name ^ "\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/lib.ml\"\n" in
  let _ = Fs.write riot_content riot_file |> Result.expect ~msg:"Write riot.toml" in
  Riot_model.Package.make ~name:package_name ~path:pkg_dir ~relative_path:(Path.v name) ~library:{
    path = Path.v "src/lib.ml"
  }
    ~sources:{
      src = [ Path.v "src/lib.ml" ];
      native = [];
      tests = [];
      examples = [];
      bench = [];
    }
    ()

let test_fresh_build_no_cache = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"cache_test"
      (fun tmpdir ->
        let package = make_package tmpdir "test-pkg" "let x = 42" in
        let workspace = make_test_workspace tmpdir [ package ] in
        let toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
        |> Result.expect ~msg:"Failed to initialize toolchain" in
        let store = Riot_store.Store.create ~workspace in
        let package_graph = Riot_planner.Package_graph.create
          ~scope:Riot_planner.Package_graph.Runtime workspace
        |> Result.unwrap in
        let build = Package_builder.build
          ~workspace
          ~toolchain
          ~store
          ~build_ctx:(make_test_build_ctx ())
          ~package_graph
          ~package_key:(Riot_planner.Package_graph.package_key
            ~package_name:(Riot_model.Package_name.to_string package.name)
            Riot_planner.Package_graph.Runtime)
          ~package in
        match build.status with
        | Package_builder.Built _ -> Ok ()
        | Package_builder.Cached _ -> Error "Fresh build should not be cached"
        | Package_builder.Skipped { reason } -> Error ("Build skipped: " ^ reason)
        | Package_builder.Failed err -> Error ("Build failed: "
        ^ package_error_message err))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_second_build_reuses_action_cache_path = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"cache_test"
      (fun tmpdir ->
        let package = make_package tmpdir "test-pkg" "let x = 42" in
        let workspace = make_test_workspace tmpdir [ package ] in
        let toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
        |> Result.expect ~msg:"Failed to initialize toolchain" in
        let store = Riot_store.Store.create ~workspace in
        let package_graph = Riot_planner.Package_graph.create
          ~scope:Riot_planner.Package_graph.Runtime workspace
        |> Result.unwrap in
        let first_build = Package_builder.build
          ~workspace
          ~toolchain
          ~store
          ~build_ctx:(make_test_build_ctx ())
          ~package_graph
          ~package_key:(Riot_planner.Package_graph.package_key
            ~package_name:(Riot_model.Package_name.to_string package.name)
            Riot_planner.Package_graph.Runtime)
          ~package in
        match first_build.status with
        | Built first_artifact -> (
            let second_build = Package_builder.build
              ~workspace
              ~toolchain
              ~store
              ~build_ctx:(make_test_build_ctx ())
              ~package_graph
              ~package_key:(Riot_planner.Package_graph.package_key
                ~package_name:(Riot_model.Package_name.to_string package.name)
                Riot_planner.Package_graph.Runtime)
              ~package in
            match second_build.status with
            | Cached artifact ->
                if String.equal (Crypto.Digest.hex first_artifact.hash) (Crypto.Digest.hex artifact.hash) then Ok ()
                else Error "Unchanged repeat build returned a different artifact hash"
            | Built _ -> Error "Unchanged repeat build did not use its package cache"
            | Skipped { reason } -> Error ("Second build skipped: " ^ reason)
            | Failed err -> Error ("Second build failed: " ^ package_error_message err)
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

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()

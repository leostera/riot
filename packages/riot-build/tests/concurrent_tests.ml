open Std
open Riot_build
module Test = Std.Test

let package_name = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let make_test_build_ctx = fun () ->
  let session_id = Riot_model.Session_id.make () in
  Riot_model.Build_ctx.make ~session_id ~profile:Riot_model.Profile.debug ()

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

type Message.t +=
  BuildComplete of (string * (unit, string) result)

type Message.t +=
  | BuildCompleteWithCache of (string * bool * (unit, string) result)

let test_concurrent_builds_different_packages = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"concurrent_test"
      (fun tmpdir ->
        let pkg1 = make_package tmpdir "pkg-1" "let x = 1" in
        let pkg2 = make_package tmpdir "pkg-2" "let x = 2" in
        let workspace =
          Riot_model.Workspace.make_realized
            ~root:tmpdir
            ~packages:[ pkg1; pkg2 ]
            ~target_dir:(Path.to_string Path.(Path.v "target"))
            ()
        in
        let toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
        |> Result.expect ~msg:"Failed to initialize toolchain" in
        let store = Riot_store.Store.create ~workspace in
        let package_graph = Riot_planner.Package_graph.create
          ~scope:Riot_planner.Package_graph.Runtime workspace
        |> Result.unwrap in
        let parent = self () in
        let _worker1 =
          spawn
            (fun () ->
              let result = Package_builder.build
                ~workspace
                ~toolchain
                ~store
                ~build_ctx:(make_test_build_ctx ())
                ~package_graph
                ~package_key:(Riot_planner.Package_graph.package_key
                  ~package_name:(Riot_model.Package_name.to_string pkg1.name)
                  Riot_planner.Package_graph.Runtime)
                ~package:pkg1 in
              let status =
                match result.status with
                | Built _
                | Cached _ -> Ok ()
                | Skipped _ -> Error "skipped"
                | Failed err ->
                    Error (
                      match err with
                      | PlanningFailed _ -> "planning"
                      | ExecutionFailed { message } -> message
                      | ActionExecutionFailed { message } -> message
                      | ActionOutputsNotCreated _ -> "outputs not created"
                      | ActionDependenciesFailed _ -> "dependencies failed"
                    )
              in
              send parent (BuildComplete ("pkg-1", status));
              Ok ())
        in
        let _worker2 =
          spawn
            (fun () ->
              let result = Package_builder.build
                ~workspace
                ~toolchain
                ~store
                ~build_ctx:(make_test_build_ctx ())
                ~package_graph
                ~package_key:(Riot_planner.Package_graph.package_key
                  ~package_name:(Riot_model.Package_name.to_string pkg2.name)
                  Riot_planner.Package_graph.Runtime)
                ~package:pkg2 in
              let status =
                match result.status with
                | Built _
                | Cached _ -> Ok ()
                | Skipped _ -> Error "skipped"
                | Failed err ->
                    Error (
                      match err with
                      | PlanningFailed _ -> "planning"
                      | ExecutionFailed { message } -> message
                      | ActionExecutionFailed { message } -> message
                      | ActionOutputsNotCreated _ -> "outputs not created"
                      | ActionDependenciesFailed _ -> "dependencies failed"
                    )
              in
              send parent (BuildComplete ("pkg-2", status));
              Ok ())
        in
        let selector msg =
          match msg with
          | BuildComplete _ -> `select msg
          | _ -> `skip
        in
        let result1 = receive ~selector () in
        let result2 = receive ~selector () in
        match (result1, result2) with
        | BuildComplete (name1, Ok ()), BuildComplete (name2, Ok ()) ->
            if (name1 = "pkg-1" && name2 = "pkg-2") || (name1 = "pkg-2" && name2 = "pkg-1") then
              Ok ()
            else
              Error ("Unexpected package names: " ^ name1 ^ ", " ^ name2)
        | BuildComplete (name, Error err), _ -> Error (name ^ " build failed: " ^ err)
        | _, BuildComplete (name, Error err) -> Error (name ^ " build failed: " ^ err)
        | _ -> Error "Unexpected message")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_concurrent_builds_same_package = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"concurrent_test"
      (fun tmpdir ->
        let package = make_package tmpdir "test-pkg" "let x = 42" in
        let workspace =
          Riot_model.Workspace.make_realized
            ~root:tmpdir
            ~packages:[ package ]
            ~target_dir:(Path.to_string Path.(Path.v "target"))
            ()
        in
        let toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
        |> Result.expect ~msg:"Failed to initialize toolchain" in
        let store = Riot_store.Store.create ~workspace in
        let package_graph = Riot_planner.Package_graph.create
          ~scope:Riot_planner.Package_graph.Runtime workspace
        |> Result.unwrap in
        let parent = self () in
        let _worker1 =
          spawn
            (fun () ->
              let result = Package_builder.build
                ~workspace
                ~toolchain
                ~store
                ~build_ctx:(make_test_build_ctx ())
                ~package_graph
                ~package_key:(Riot_planner.Package_graph.package_key
                  ~package_name:(Riot_model.Package_name.to_string package.name)
                  Riot_planner.Package_graph.Runtime)
                ~package in
              let status =
                match result.status with
                | Built _
                | Cached _ -> Ok ()
                | Skipped _ -> Error "skipped"
                | Failed err ->
                    Error (
                      match err with
                      | PlanningFailed _ -> "planning"
                      | ExecutionFailed { message } -> message
                      | ActionExecutionFailed { message } -> message
                      | ActionOutputsNotCreated _ -> "outputs not created"
                      | ActionDependenciesFailed _ -> "dependencies failed"
                    )
              in
              send parent (BuildComplete ("worker1", status));
              Ok ())
        in
        let _worker2 =
          spawn
            (fun () ->
              let result = Package_builder.build
                ~workspace
                ~toolchain
                ~store
                ~build_ctx:(make_test_build_ctx ())
                ~package_graph
                ~package_key:(Riot_planner.Package_graph.package_key
                  ~package_name:(Riot_model.Package_name.to_string package.name)
                  Riot_planner.Package_graph.Runtime)
                ~package in
              let status =
                match result.status with
                | Built _
                | Cached _ -> Ok ()
                | Skipped _ -> Error "skipped"
                | Failed err ->
                    Error (
                      match err with
                      | PlanningFailed _ -> "planning"
                      | ExecutionFailed { message } -> message
                      | ActionExecutionFailed { message } -> message
                      | ActionOutputsNotCreated _ -> "outputs not created"
                      | ActionDependenciesFailed _ -> "dependencies failed"
                    )
              in
              send parent (BuildComplete ("worker2", status));
              Ok ())
        in
        let selector msg =
          match msg with
          | BuildComplete _ -> `select msg
          | _ -> `skip
        in
        let result1 = receive ~selector () in
        let result2 = receive ~selector () in
        match (result1, result2) with
        | BuildComplete (_, Ok ()), BuildComplete (_, Ok ()) -> Ok ()
        | BuildComplete (name, Error err), _ -> Error (name ^ " build failed: " ^ err)
        | _, BuildComplete (name, Error err) -> Error (name ^ " build failed: " ^ err)
        | _ -> Error "Unexpected message")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_concurrent_builds_with_shared_cache = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"concurrent_test"
      (fun tmpdir ->
        let package = make_package tmpdir "test-pkg" "let x = 42" in
        let workspace =
          Riot_model.Workspace.make_realized
            ~root:tmpdir
            ~packages:[ package ]
            ~target_dir:(Path.to_string Path.(Path.v "target"))
            ()
        in
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
        | Built _ -> (
            let parent = self () in
            let _worker1 =
              spawn
                (fun () ->
                  let result = Package_builder.build
                    ~workspace
                    ~toolchain
                    ~store
                    ~build_ctx:(make_test_build_ctx ())
                    ~package_graph
                    ~package_key:(Riot_planner.Package_graph.package_key
                      ~package_name:(Riot_model.Package_name.to_string package.name)
                      Riot_planner.Package_graph.Runtime)
                    ~package in
                  let cached =
                    match result.status with
                    | Cached _ -> true
                    | Built _ -> false
                    | Skipped _ -> false
                    | Failed _ -> false
                  in
                  let status =
                    match result.status with
                    | Cached _
                    | Built _ -> Ok ()
                    | Skipped _ -> Error "skipped"
                    | Failed err ->
                        Error (
                          match err with
                          | PlanningFailed _ -> "planning"
                          | ExecutionFailed { message } -> message
                          | ActionExecutionFailed { message } -> message
                          | ActionOutputsNotCreated _ -> "outputs not created"
                          | ActionDependenciesFailed _ -> "dependencies failed"
                        )
                  in
                  send parent (BuildCompleteWithCache ("worker1", cached, status));
                  Ok ())
            in
            let _worker2 =
              spawn
                (fun () ->
                  let result = Package_builder.build
                    ~workspace
                    ~toolchain
                    ~store
                    ~build_ctx:(make_test_build_ctx ())
                    ~package_graph
                    ~package_key:(Riot_planner.Package_graph.package_key
                      ~package_name:(Riot_model.Package_name.to_string package.name)
                      Riot_planner.Package_graph.Runtime)
                    ~package in
                  let cached =
                    match result.status with
                    | Cached _ -> true
                    | Built _ -> false
                    | Skipped _ -> false
                    | Failed _ -> false
                  in
                  let status =
                    match result.status with
                    | Cached _
                    | Built _ -> Ok ()
                    | Skipped _ -> Error "skipped"
                    | Failed err ->
                        Error (
                          match err with
                          | PlanningFailed _ -> "planning"
                          | ExecutionFailed { message } -> message
                          | ActionExecutionFailed { message } -> message
                          | ActionOutputsNotCreated _ -> "outputs not created"
                          | ActionDependenciesFailed _ -> "dependencies failed"
                        )
                  in
                  send parent (BuildCompleteWithCache ("worker2", cached, status));
                  Ok ())
            in
            let selector msg =
              match msg with
              | BuildCompleteWithCache _ -> `select msg
              | _ -> `skip
            in
            let result1 = receive ~selector () in
            let result2 = receive ~selector () in
            match (result1, result2) with
            | (BuildCompleteWithCache (_, cached1, Ok ()), BuildCompleteWithCache (_, cached2, Ok ())) ->
                if cached1 && cached2 then Ok ()
                else Error "Concurrent unchanged builds must both reuse the shared package cache"
            | BuildCompleteWithCache (name, _, Error err), _ ->
                Error (name ^ " build failed: " ^ err)
            | _, BuildCompleteWithCache (name, _, Error err) ->
                Error (name ^ " build failed: " ^ err)
            | _ ->
                Error "Unexpected message"
          )
        | Cached _ ->
            Error "First build should not be cached"
        | Skipped _ ->
            Error "First build was unexpectedly skipped"
        | Failed err ->
            Error (
              "First build failed: " ^ match err with
              | PlanningFailed _ -> "planning"
              | ExecutionFailed { message } -> message
              | ActionExecutionFailed { message } -> message
              | ActionOutputsNotCreated _ -> "outputs not created"
              | ActionDependenciesFailed _ -> "dependencies failed"
            ))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let tests =
  let open Test in [
    case "concurrent: different packages don't interfere" test_concurrent_builds_different_packages;
    case "concurrent: same package builds safely" test_concurrent_builds_same_package;
    case "concurrent: shared cache works correctly" test_concurrent_builds_with_shared_cache;
  ]

let name = "Concurrent Build Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()

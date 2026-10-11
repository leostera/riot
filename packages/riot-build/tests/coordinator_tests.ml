open Std
open Riot_build
open Riot_model
module Test = Std.Test

let make_build_ctx = fun ?(parallelism = 2) () ->
  let session_id = Riot_model.Session_id.make () in
  Riot_model.Build_ctx.make
    ~session_id
    ~profile:Riot_model.Profile.debug
    ~available_parallelism:parallelism
    ()

let write_package = fun ~root ~name ~lib_body ~deps ->
  let pkg_dir = Path.(root / Path.v name) in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"create src failed" in
  let _ = Fs.write lib_body Path.(src_dir / Path.v "lib.ml") |> Result.expect ~msg:"write source failed" in
  let deps_block =
    match deps with
    | [] -> ""
    | _ -> "\n[dependencies]\n"
    ^ String.concat "\n" (List.map deps ~fn:(fun dep -> dep ^ " = \"*\""))
    ^ "\n"
  in
  let riot_toml = "[package]\nname = \""
  ^ name
  ^ "\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/lib.ml\"\n"
  ^ deps_block in
  let _ = Fs.write riot_toml Path.(pkg_dir / Path.v "riot.toml") |> Result.expect ~msg:"write riot.toml failed" in
  ()

let write_workspace = fun ~root members ->
  let riot_toml = "[workspace]\nmembers = ["
  ^ String.concat ", " (List.map members ~fn:(fun member -> "\"" ^ member ^ "\""))
  ^ "]\n" in
  let _ = Fs.write riot_toml Path.(root / Path.v "riot.toml") |> Result.expect ~msg:"write workspace riot.toml failed" in
  ()

let make_registry = fun tmpdir ->
  let cache =
    Pkgs_ml.Registry_cache.create
      ~riot_home:Path.(tmpdir / Path.v ".riot")
      ~registry_name:"pkgs.ml"
      ()
    |> Result.expect ~msg:"registry cache init failed"
  in
  Pkgs_ml.Registry.in_memory ~cache ~packages:[] ()

let with_workspace = fun tmpdir f ->
  let workspace_manager = Riot_model.Workspace_manager.create () in
  match Riot_model.Workspace_manager.scan workspace_manager tmpdir with
  | Error _ -> Error "workspace scan failed"
  | Ok (workspace_manifest, _load_errors) -> (
      match
        Riot_deps.ensure_workspace
          ~workspace_manager
          ~mode:Riot_deps.Dep_solver.Refresh
          ~registry:(make_registry tmpdir)
          ~workspace:workspace_manifest
          ()
      with
      | Ok workspace -> f workspace
      | Error err -> Error ("workspace ensure failed: " ^ Riot_model.Pm_error.message err)
    )

let result_status_to_string = fun (result: Package_builder.build_result) ->
  let status =
    match result.status with
    | Package_builder.Cached _ -> "cached"
    | Built _ -> "built"
    | Skipped { reason } -> "skipped(" ^ reason ^ ")"
    | Failed err -> "failed(" ^ Package_builder.package_error_to_string err ^ ")"
  in
  Package_name.to_string result.package.Riot_model.Package.name ^ ":" ^ status

let test_build_workspace_two_packages_success = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"coordinator_two_pkg_test"
      (fun tmpdir ->
        write_package ~root:tmpdir ~name:"a" ~lib_body:"let a = 1" ~deps:[];
        write_package ~root:tmpdir ~name:"b" ~lib_body:"let b = 1" ~deps:[ "a" ];
        write_workspace ~root:tmpdir [ "a"; "b" ];
        with_workspace tmpdir
          (fun workspace ->
            let toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
            |> Result.expect ~msg:"toolchain init failed" in
            let store = Riot_store.Store.create ~workspace in
            let build_ctx = make_build_ctx ~parallelism:2 () in
            match Coordinator.build_workspace
              ~workspace
              ~toolchain
              ~store
              ~target:Riot_planner.Workspace_planner.All
              ~scope:Riot_planner.Package_graph.Runtime
              ~concurrency:2
              ~build_ctx
              ~session_id:build_ctx.Riot_model.Build_ctx.session_id with
            | Error _ -> Error "workspace build failed"
            | Ok result ->
                if List.length result.results = 2 && result.failed_count = 0 then
                  Ok ()
                else
                  Error ("unexpected workspace result accounting: results="
                  ^ Int.to_string (List.length result.results)
                  ^ " failed_count="
                  ^ Int.to_string result.failed_count
                  ^ " statuses=["
                  ^ String.concat ", " (List.map result.results ~fn:result_status_to_string)
                  ^ "]")))
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_build_workspace_respects_serial_package_orchestration = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"coordinator_serial_pkg_test"
      (fun tmpdir ->
        write_package ~root:tmpdir ~name:"left" ~lib_body:"let x = 1" ~deps:[];
        write_package ~root:tmpdir ~name:"right" ~lib_body:"let y = 2" ~deps:[];
        write_workspace ~root:tmpdir [ "left"; "right" ];
        with_workspace tmpdir
          (fun workspace ->
            let toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
            |> Result.expect ~msg:"toolchain init failed" in
            let store = Riot_store.Store.create ~workspace in
            let build_ctx = make_build_ctx ~parallelism:1 () in
            match Coordinator.build_workspace
              ~workspace
              ~toolchain
              ~store
              ~target:Riot_planner.Workspace_planner.All
              ~scope:Riot_planner.Package_graph.Runtime
              ~concurrency:4
              ~build_ctx
              ~session_id:build_ctx.Riot_model.Build_ctx.session_id with
            | Error _ -> Error "workspace build failed"
            | Ok result ->
                if List.length result.results = 2 && result.failed_count = 0 then
                  Ok ()
                else
                  Error ("serial orchestration build should succeed: results="
                  ^ Int.to_string (List.length result.results)
                  ^ " failed_count="
                  ^ Int.to_string result.failed_count
                  ^ " statuses=["
                  ^ String.concat ", " (List.map result.results ~fn:result_status_to_string)
                  ^ "]")))
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_failed_dependency_updates_package_graph = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"coordinator_failed_dep_test"
      (fun tmpdir ->
        write_package ~root:tmpdir ~name:"a" ~lib_body:"let broken =" ~deps:[];
        write_package ~root:tmpdir ~name:"b" ~lib_body:"let b = 1" ~deps:[ "a" ];
        write_workspace ~root:tmpdir [ "a"; "b" ];
        with_workspace tmpdir
          (fun workspace ->
            let toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
            |> Result.expect ~msg:"toolchain init failed" in
            let store = Riot_store.Store.create ~workspace in
            let build_ctx = make_build_ctx ~parallelism:2 () in
            match Coordinator.build_workspace
              ~workspace
              ~toolchain
              ~store
              ~target:Riot_planner.Workspace_planner.All
              ~scope:Riot_planner.Package_graph.Runtime
              ~concurrency:2
              ~build_ctx
              ~session_id:build_ctx.Riot_model.Build_ctx.session_id with
            | Error _ -> Error "workspace build failed"
            | Ok result ->
                let package_key = Riot_planner.Package_graph.package_key
                  ~package_name:"b"
                  Riot_planner.Package_graph.Runtime in
                match Riot_planner.Package_graph.get_node_by_key result.package_graph package_key with
                | None ->
                    let graph_nodes = Riot_planner.Package_graph.topological_sort result.package_graph
                    |> List.map
                      ~fn:(fun node -> Riot_planner.Package_graph.get_key node |> Riot_model.Package.key_to_string) in
                    Error ("missing package graph node for failed package; graph keys=["
                    ^ String.concat ", " graph_nodes
                    ^ "]")
                | Some node -> (
                    match node.value with
                    | Riot_planner.Package_graph.Failed _ -> Ok ()
                    | Riot_planner.Package_graph.Unplanned _ ->
                        Error "package graph left dependent package unplanned after \
                           dependency failure"
                    | Riot_planner.Package_graph.Planned _ ->
                        Error "package graph left dependent package planned after \
                           dependency failure"
                    | Riot_planner.Package_graph.Cached _ ->
                        Error "package graph left dependent package cached after \
                           dependency failure"
                    | Riot_planner.Package_graph.Built _ ->
                        Error "package graph left dependent package built after \
                           dependency failure"
                    | Riot_planner.Package_graph.Skipped _ -> Ok ()
                  )))
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_package_export_failure_returns_result = fun _ctx ->
  match Fs.with_tempdir ~prefix:"coordinator_export_failure" (fun tmpdir ->
    write_package ~root:tmpdir ~name:"a" ~lib_body:"let a = 1" ~deps:[];
    write_workspace ~root:tmpdir [ "a" ];
    with_workspace tmpdir (fun workspace ->
      let toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
        |> Result.expect ~msg:"toolchain init failed" in
      let store = Riot_store.Store.create ~workspace in
      let build_ctx = make_build_ctx () in
      let out_dir = Riot_dirs.out_dir_in_workspace ~workspace ~profile:"debug"
        ~target:(Build_ctx.target_triplet build_ctx) in
      Fs.create_dir_all out_dir |> Result.expect ~msg:"create output parent failed";
      let blocked = Path.(out_dir / Path.v "a") in
      Fs.write "preserve this obstruction" blocked |> Result.expect ~msg:"write obstruction failed";
      match Coordinator.build_workspace ~workspace ~toolchain ~store
        ~target:Riot_planner.Workspace_planner.All
        ~scope:Riot_planner.Package_graph.Runtime ~concurrency:2
        ~build_ctx ~session_id:build_ctx.session_id with
      | Error _ -> Error "export failure escaped package result accounting"
      | Ok result ->
          match result.results with
          | [{ status = Package_builder.Failed (ExecutionFailed { message }); _ }]
            when result.failed_count = 1 && String.contains message "Failed to finalize package a" ->
              let key = Riot_planner.Package_graph.package_key ~package_name:"a"
                Riot_planner.Package_graph.Runtime in
              (match Riot_planner.Package_graph.get_node_by_key result.package_graph key with
              | Some { value = Riot_planner.Package_graph.Failed _; _ } ->
                  (match Fs.read blocked with
                  | Ok "preserve this obstruction" -> Ok ()
                  | _ -> Error "failed finalization changed the obstruction")
              | _ -> Error "failed finalization left a successful package graph node")
          | _ -> Error "export failure did not return exactly one failed package")) with
  | Ok result -> result
  | Error error -> Error ("export-failure fixture: " ^ IO.error_message error)

let tests =
  Test.[
    case ~size:Large "build_workspace: two packages succeed" test_build_workspace_two_packages_success;
    case "build_workspace: serial orchestration succeeds" test_build_workspace_respects_serial_package_orchestration;
    case "build_workspace: dependency failure updates package graph" test_failed_dependency_updates_package_graph;
    case ~size:Large "build_workspace: export failure returns failed result" test_package_export_failure_returns_result;
  ]

let name = "Coordinator Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()

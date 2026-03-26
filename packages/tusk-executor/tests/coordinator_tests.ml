open Std

module Test = Std.Test

let make_build_ctx ?(parallelism = 2) () =
  let session_id = Tusk_model.Session_id.make () in
  Tusk_model.Build_ctx.make ~session_id ~profile:Tusk_model.Profile.debug
    ~available_parallelism:parallelism ()

let write_package ~root ~name ~lib_body ~deps =
  let pkg_dir = Path.(root / Path.v name) in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"create src failed" in
  let _ =
    Fs.write lib_body Path.(src_dir / Path.v "lib.ml")
    |> Result.expect ~msg:"write source failed"
  in
  let deps_block =
    match deps with
    | [] -> ""
    | _ ->
        "\n[dependencies]\n"
        ^ String.concat "\n"
            (List.map (fun dep -> dep ^ " = \"*\"") deps)
        ^ "\n"
  in
  let tusk_toml =
    "[package]\nname = \"" ^ name ^ "\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/lib.ml\"\n"
    ^ deps_block
  in
  let _ =
    Fs.write tusk_toml Path.(pkg_dir / Path.v "tusk.toml")
    |> Result.expect ~msg:"write tusk.toml failed"
  in
  ()

let with_scanned_workspace tmpdir f =
  match Tusk_model.Workspace_manager.scan tmpdir with
  | Error _ -> Error "workspace scan failed"
  | Ok (workspace, _load_errors) -> f workspace

let test_build_workspace_two_packages_success () =
  match
    Fs.with_tempdir ~prefix:"coordinator_two_pkg_test" (fun tmpdir ->
        write_package ~root:tmpdir ~name:"a" ~lib_body:"let a = 1" ~deps:[];
        write_package ~root:tmpdir ~name:"b"
          ~lib_body:"let b = A.a + 1" ~deps:[ "a" ];
        with_scanned_workspace tmpdir (fun workspace ->
            let toolchain =
              Tusk_toolchain.init ~config:Tusk_model.Toolchain_config.default
              |> Result.expect ~msg:"toolchain init failed"
            in
            let store = Tusk_store.Store.create ~workspace in
            let build_ctx = make_build_ctx ~parallelism:2 () in
            match
              Tusk_executor.Coordinator.build_workspace ~workspace ~toolchain
                ~store ~target:Tusk_planner.Workspace_planner.All
                ~scope:Tusk_planner.Package_graph.Runtime ~concurrency:2
                ~build_ctx
                ~session_id:build_ctx.Tusk_model.Build_ctx.session_id
            with
            | Error _ -> Error "workspace build failed"
            | Ok result ->
                if List.length result.results = 2 && result.failed_count = 0
                then Ok ()
                else Error "unexpected workspace result accounting"))
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_build_workspace_respects_serial_package_orchestration () =
  match
    Fs.with_tempdir ~prefix:"coordinator_serial_pkg_test" (fun tmpdir ->
        write_package ~root:tmpdir ~name:"left" ~lib_body:"let x = 1" ~deps:[];
        write_package ~root:tmpdir ~name:"right" ~lib_body:"let y = 2" ~deps:[];
        with_scanned_workspace tmpdir (fun workspace ->
            let toolchain =
              Tusk_toolchain.init ~config:Tusk_model.Toolchain_config.default
              |> Result.expect ~msg:"toolchain init failed"
            in
            let store = Tusk_store.Store.create ~workspace in
            let build_ctx = make_build_ctx ~parallelism:1 () in
            match
              Tusk_executor.Coordinator.build_workspace ~workspace ~toolchain
                ~store ~target:Tusk_planner.Workspace_planner.All
                ~scope:Tusk_planner.Package_graph.Runtime ~concurrency:4
                ~build_ctx
                ~session_id:build_ctx.Tusk_model.Build_ctx.session_id
            with
            | Error _ -> Error "workspace build failed"
            | Ok result ->
                if List.length result.results = 2 && result.failed_count = 0
                then Ok ()
                else Error "serial orchestration build should succeed"))
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let tests =
  Test.
    [
      case "build_workspace: two packages succeed"
        test_build_workspace_two_packages_success;
      case "build_workspace: serial orchestration succeeds"
        test_build_workspace_respects_serial_package_orchestration;
    ]

let name = "Coordinator Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()

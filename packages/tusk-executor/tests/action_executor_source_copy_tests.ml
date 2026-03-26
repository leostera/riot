open Std
open Std.Collections

module Test = Std.Test

let test_toolchain () =
  Tusk_toolchain.init ~config:Tusk_model.Toolchain_config.default
  |> Result.expect ~msg:"failed to initialize toolchain"

let make_workspace root =
  Tusk_model.Workspace.
    {
      root;
      target_dir_root = Path.(root / Path.v "target");
      packages = [];
      profile_overrides = [];
    }

let make_package ~root ~name =
  let path = Path.(root / Path.v "packages" / Path.v name) in
  Tusk_model.Package.
    {
      name;
      path;
      relative_path = Path.v ("packages/" ^ name);
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

let make_node ~package ~srcs =
  let graph = Tusk_planner.Action_graph.create () in
  let spec =
    Tusk_planner.Action_node.make
      ~actions:
        [
          Tusk_planner.Action.WriteFile
            { destination = Path.v "out.txt"; content = "ok" };
        ]
      ~outs:[ Path.v "out.txt" ] ~srcs ~package
      ~toolchain:(test_toolchain ())
      ~dependency_hashes:(fun _ -> Crypto.hash_string "") ~deps:[]
  in
  Tusk_planner.Action_graph.add_node graph spec

let with_workspace_dirs tmpdir f =
  let package_src =
    Path.(tmpdir / Path.v "packages" / Path.v "kernel" / Path.v "src")
  in
  match Fs.create_dir_all package_src with
  | Error _ -> Error "create package src failed"
  | Ok () -> (
      match Fs.write "let kernel = 1" Path.(package_src / Path.v "lib.ml") with
      | Error _ -> Error "write package source failed"
      | Ok () -> (
          let sandbox = Path.(tmpdir / Path.v "sandbox") in
          match Fs.create_dir_all sandbox with
          | Error _ -> Error "create sandbox failed"
          | Ok () -> f sandbox))

let test_execute_node_copies_package_relative_sources () =
  match
    Fs.with_tempdir ~prefix:"action_exec_src_copy_rel" (fun tmpdir ->
        with_workspace_dirs tmpdir (fun sandbox ->
            let workspace = make_workspace tmpdir in
            let store = Tusk_store.Store.create ~workspace in
            let package = make_package ~root:tmpdir ~name:"kernel" in
            let node = make_node ~package ~srcs:[ Path.v "src/lib.ml" ] in
            let completed = HashMap.create () in
            let result =
              Tusk_executor.Action_executor.execute_node ~completed ~store
                ~session_id:(Tusk_model.Session_id.make ())
                (test_toolchain ()) sandbox node
            in
            match result.status with
            | Tusk_executor.Action_executor.Executed -> (
                let copied = Path.(sandbox / Path.v "src/lib.ml") in
                match Fs.exists copied with
                | Ok true -> Ok ()
                | _ -> Error "expected package-relative source to be copied")
            | _ -> Error "expected node execution to succeed"))
  with
  | Ok r -> r
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_execute_node_copies_workspace_relative_sources () =
  match
    Fs.with_tempdir ~prefix:"action_exec_src_copy_ws" (fun tmpdir ->
        with_workspace_dirs tmpdir (fun sandbox ->
            let workspace = make_workspace tmpdir in
            let store = Tusk_store.Store.create ~workspace in
            let package = make_package ~root:tmpdir ~name:"kernel" in
            let node =
              make_node ~package ~srcs:[ Path.v "packages/kernel/src/lib.ml" ]
            in
            let completed = HashMap.create () in
            let result =
              Tusk_executor.Action_executor.execute_node ~completed ~store
                ~session_id:(Tusk_model.Session_id.make ())
                (test_toolchain ()) sandbox node
            in
            match result.status with
            | Tusk_executor.Action_executor.Executed -> (
                let copied = Path.(sandbox / Path.v "packages/kernel/src/lib.ml") in
                match Fs.exists copied with
                | Ok true -> Ok ()
                | _ -> Error "expected workspace-relative source to be copied")
            | _ -> Error "expected node execution to succeed"))
  with
  | Ok r -> r
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let tests =
  Test.
    [
      case "execute_node copies package-relative sources"
        test_execute_node_copies_package_relative_sources;
      case "execute_node copies workspace-relative sources"
        test_execute_node_copies_workspace_relative_sources;
    ]

let name = "tusk-executor:action-executor-source-copy"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()

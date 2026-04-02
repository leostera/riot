open Std
module Test = Std.Test
module G = Std.Graph.SimpleGraph

let test_toolchain = Tusk_toolchain.init ~config:Tusk_model.Toolchain_config.default
|> Result.expect ~msg:"Failed to initialize toolchain"

let make_package = fun name ->
  Tusk_model.Package.{
    name;
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

let make_package_with_paths = fun ~name ~path ~relative_path ->
  Tusk_model.Package.{
    name;
    path;
    relative_path;
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

let test_action_graph_json_round_trip_preserves_dependencies = fun _ctx ->
  let package = make_package "pkg" in
  let graph = Tusk_planner.Action_graph.create () in
  let write_a = Tusk_planner.Action.WriteFile { destination = Path.v "a.txt"; content = "a" } in
  let spec_a =
    Tusk_planner.Action_node.make
      ~actions:[ write_a ]
      ~outs:[ Path.v "a.txt" ]
      ~srcs:[]
      ~package
      ~toolchain:test_toolchain
      ~dependency_hashes:(fun _ -> Crypto.hash_string "")
      ~deps:[]
  in
  let node_a = Tusk_planner.Action_graph.add_node graph spec_a in
  let write_b = Tusk_planner.Action.WriteFile { destination = Path.v "b.txt"; content = "b" } in
  let spec_b =
    Tusk_planner.Action_node.make ~actions:[ write_b ] ~outs:[ Path.v "b.txt" ] ~srcs:[] ~package ~toolchain:test_toolchain
      ~dependency_hashes:(fun dep_id ->
        if Graph.SimpleGraph.Node_id.eq dep_id node_a.id then
          Tusk_planner.Action_node.get_hash node_a
        else
          Crypto.hash_string "missing")
      ~deps:[ node_a.id ]
  in
  let node_b = Tusk_planner.Action_graph.add_node graph spec_b in
  Tusk_planner.Action_graph.add_dependency graph node_b ~depends_on:node_a;
  let encoded = Tusk_planner.Action_graph.to_json graph in
  match Tusk_planner.Action_graph.from_json encoded with
  | Error err -> Error ("round-trip decode failed: " ^ err)
  | Ok decoded -> (
      let encoded_decoded = Tusk_planner.Action_graph.to_json decoded in
      match Data.Json.get_field "nodes" encoded_decoded with
      | Some (Data.Json.Array node_jsons) ->
          let edge_count =
            List.fold_left
              (fun acc node_json ->
                match Data.Json.get_field "dependencies" node_json with
                | Some (Data.Json.Array deps) -> acc + List.length deps
                | _ -> acc)
              0
              node_jsons
          in
          if List.length node_jsons = 2 && edge_count = 1 then
            Ok ()
          else
            Error ("expected 2 nodes and 1 edge, got "
            ^ Int.to_string (List.length node_jsons)
            ^ " nodes and "
            ^ Int.to_string edge_count
            ^ " edges")
      | _ -> Error "decoded graph missing nodes array"
    )

let test_action_graph_json_round_trip_preserves_package_paths_and_hashes = fun _ctx ->
  let package = make_package_with_paths
    ~name:"kernel"
    ~path:(Path.v "packages/kernel")
    ~relative_path:(Path.v "packages/kernel") in
  let graph = Tusk_planner.Action_graph.create () in
  let action = Tusk_planner.Action.WriteFile {
    destination = Path.v "build/meta.txt";
    content = "ok"
  } in
  let spec =
    Tusk_planner.Action_node.make
      ~actions:[ action ]
      ~outs:[ Path.v "build/meta.txt" ]
      ~srcs:[ Path.v "packages/kernel/src/lib.ml" ]
      ~package
      ~toolchain:test_toolchain
      ~dependency_hashes:(fun _ -> Crypto.hash_string "")
      ~deps:[]
  in
  let node = Tusk_planner.Action_graph.add_node graph spec in
  let expected_hash = Tusk_planner.Action_node.get_hash node in
  let encoded = Tusk_planner.Action_graph.to_json graph in
  match Tusk_planner.Action_graph.from_json encoded with
  | Error err -> Error ("round-trip decode failed: " ^ err)
  | Ok decoded -> (
      match Tusk_planner.Action_graph.nodes decoded with
      | [ decoded_node ] ->
          let decoded_path = decoded_node.value.package.Tusk_model.Package.path in
          let decoded_rel = decoded_node.value.package.Tusk_model.Package.relative_path in
          let decoded_hash = Tusk_planner.Action_node.get_hash decoded_node in
          if
            Path.equal decoded_path (Path.v "packages/kernel")
            && Path.equal decoded_rel (Path.v "packages/kernel")
            && Crypto.Hash.equal decoded_hash expected_hash
          then
            Ok ()
          else
            Error "package path/relative_path/hash did not round-trip"
      | _ -> Error "expected one decoded node"
    )

let test_action_hash_tracks_package_relative_source_contents = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"action_hash_pkg_src"
      (fun tmpdir ->
        let package_root = Path.(tmpdir / Path.v "packages" / Path.v "demo") in
        let src_dir = Path.(package_root / Path.v "src") in
        let source = Path.(src_dir / Path.v "demo.ml") in
        let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"create src dir failed" in
        let package = make_package_with_paths
          ~name:"demo"
          ~path:package_root
          ~relative_path:(Path.v "packages/demo") in
        let action = Tusk_planner.Action.CompileImplementation {
          source = Path.v "src/demo.ml";
          outputs = [ Path.v "Demo.cmt"; Path.v "Demo.cmi"; Path.v "Demo.cmx" ];
          includes = [ Path.v "." ];
          flags = []
        } in
        let write contents = Fs.write contents source |> Result.expect ~msg:"write source failed" in
        write "let value = 1\n";
        let first =
          Tusk_planner.Action_node.make
            ~actions:[ action ]
            ~outs:[ Path.v "Demo.cmt"; Path.v "Demo.cmi"; Path.v "Demo.cmx" ]
            ~srcs:[ Path.v "src/demo.ml" ]
            ~package
            ~toolchain:test_toolchain
            ~dependency_hashes:(fun _ -> Crypto.hash_string "")
            ~deps:[]
        in
        write "let value = 2\n";
        let second =
          Tusk_planner.Action_node.make
            ~actions:[ action ]
            ~outs:[ Path.v "Demo.cmt"; Path.v "Demo.cmi"; Path.v "Demo.cmx" ]
            ~srcs:[ Path.v "src/demo.ml" ]
            ~package
            ~toolchain:test_toolchain
            ~dependency_hashes:(fun _ -> Crypto.hash_string "")
            ~deps:[]
        in
        if Crypto.Hash.equal first.hash second.hash then
          Error "expected package-relative source edits to change the action hash"
        else
          Ok ())
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_library_builds_do_not_emit_shared_library_actions = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"planner_no_shared"
      (fun tmpdir ->
        let workspace = Tusk_model.Workspace.make ~root:tmpdir ~packages:[] () in
        let store = Tusk_store.Store.create ~workspace in
        let package = {
          (make_package_with_paths
            ~name:"minttea"
            ~path:Path.(tmpdir / Path.v "packages" / Path.v "minttea")
            ~relative_path:(Path.v "packages/minttea"))
          with library = Some { path = Path.v "src/minttea.ml" }
        } in
        let ctx = Tusk_model.Build_ctx.make
          ~session_id:(Tusk_model.Session_id.of_string "test-session")
          ~profile:Tusk_model.Profile.release
          () in
        let module_graph = G.make () in
        let _ = G.add_node
          module_graph
          (Tusk_planner.Module_node.make_library ~name:package.name ~includes:[ Path.v "." ]) in
        let action_graph, _ = Tusk_planner.Action_graph.from_module_graph
          ~package
          ~profile:Tusk_model.Profile.release
          ~ctx
          ~toolchain:test_toolchain
          ~store
          ~depset:[]
          ~needs_unix:false
          ~needs_dynlink:false
          module_graph in
        let shared_actions =
          List.filter
            (
              function
              | Tusk_planner.Action.CreateSharedLibrary _ -> true
              | _ -> false
            )
            (Tusk_planner.Action_graph.to_action_list action_graph)
        in
        if List.is_empty shared_actions then
          Ok ()
        else
          Error "expected library builds to skip CreateSharedLibrary actions")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_library_actions_exclude_ml_object_files = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"planner_library_objects"
      (fun tmpdir ->
        let package_root = Path.(tmpdir / Path.v "packages" / Path.v "demo") in
        let src_dir = Path.(package_root / Path.v "src") in
        let native_dir = Path.(package_root / Path.v "native") in
        let source_path = Path.(src_dir / Path.v "demo.ml") in
        let native_path = Path.(native_dir / Path.v "stub.c") in
        let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"create src dir failed" in
        let _ = Fs.create_dir_all native_dir |> Result.expect ~msg:"create native dir failed" in
        let _ = Fs.write "let value = 1\n" source_path |> Result.expect ~msg:"write ml source failed" in
        let _ = Fs.write "int demo_stub(void) { return 1; }\n" native_path |> Result.expect ~msg:"write native source failed" in
        let workspace = Tusk_model.Workspace.make ~root:tmpdir ~packages:[] () in
        let store = Tusk_store.Store.create ~workspace in
        let package = {
          (make_package_with_paths
            ~name:"demo"
            ~path:package_root
            ~relative_path:(Path.v "packages/demo"))
          with library = Some { path = Path.v "src/demo.ml" }
        } in
        let ctx = Tusk_model.Build_ctx.make
          ~session_id:(Tusk_model.Session_id.of_string "test-session")
          ~profile:Tusk_model.Profile.release
          () in
        let module_graph = G.make () in
        let demo_module = Tusk_model.Module.make
          ~namespace:Tusk_model.Namespace.empty
          ~filename:(Path.v "src/demo.ml") in
        let demo_node = G.add_node
          module_graph
          (Tusk_planner.Module_node.make_ml
            demo_module
            (Tusk_planner.Module_node.Concrete (Path.v "src/demo.ml"))) in
        let native_node = G.add_node
          module_graph
          (Tusk_planner.Module_node.make_native ~files:[ Path.v "native/stub.c" ]) in
        let library_node = G.add_node
          module_graph
          (Tusk_planner.Module_node.make_library ~name:package.name ~includes:[ Path.v "." ]) in
        let _ = G.add_edge library_node ~depends_on:demo_node in
        let _ = G.add_edge library_node ~depends_on:native_node in
        let action_graph, _ = Tusk_planner.Action_graph.from_module_graph
          ~package
          ~profile:Tusk_model.Profile.release
          ~ctx
          ~toolchain:test_toolchain
          ~store
          ~depset:[]
          ~needs_unix:false
          ~needs_dynlink:false
          module_graph in
        match
          List.find_opt
            (
              function
              | Tusk_planner.Action.CreateLibrary _ -> true
              | _ -> false
            )
            (Tusk_planner.Action_graph.to_action_list action_graph)
        with
        | Some (Tusk_planner.Action.CreateLibrary { objects; _ }) ->
            let has_demo_cmx = List.exists (Path.equal (Path.v "Demo.cmx")) objects in
            let has_demo_o = List.exists (Path.equal (Path.v "Demo.o")) objects in
            let has_stub_o = List.exists (Path.equal (Path.v "stub.o")) objects in
            if not has_demo_cmx then
              Error "expected CreateLibrary to include Demo.cmx"
            else if has_demo_o then
              Error "expected CreateLibrary to exclude Demo.o from ML module outputs"
            else if not has_stub_o then
              Error "expected CreateLibrary to keep stub.o from native C sources"
            else
              Ok ()
        | Some _ ->
            Error "expected CreateLibrary action"
        | None ->
            Error "missing CreateLibrary action")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_release_profile_flags_flow_into_compile_actions = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"planner_release_flags"
      (fun tmpdir ->
        let package_root = Path.(tmpdir / Path.v "packages" / Path.v "demo") in
        let src_dir = Path.(package_root / Path.v "src") in
        let source_path = Path.(src_dir / Path.v "demo.ml") in
        let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"create src dir failed" in
        let _ = Fs.write "let value = 1\n" source_path |> Result.expect ~msg:"write ml source failed" in
        let workspace = Tusk_model.Workspace.make ~root:tmpdir ~packages:[] () in
        let store = Tusk_store.Store.create ~workspace in
        let package = make_package_with_paths
          ~name:"demo"
          ~path:package_root
          ~relative_path:(Path.v "packages/demo") in
        let ctx = Tusk_model.Build_ctx.make
          ~session_id:(Tusk_model.Session_id.of_string "test-session")
          ~profile:Tusk_model.Profile.release
          () in
        let module_graph = G.make () in
        let demo_module = Tusk_model.Module.make
          ~namespace:Tusk_model.Namespace.empty
          ~filename:(Path.v "src/demo.ml") in
        let _ = G.add_node
          module_graph
          (Tusk_planner.Module_node.make_ml
            demo_module
            (Tusk_planner.Module_node.Concrete (Path.v "src/demo.ml"))) in
        let action_graph, _ = Tusk_planner.Action_graph.from_module_graph
          ~package
          ~profile:Tusk_model.Profile.release
          ~ctx
          ~toolchain:test_toolchain
          ~store
          ~depset:[]
          ~needs_unix:false
          ~needs_dynlink:false
          module_graph in
        match
          List.find_opt
            (
              function
              | Tusk_planner.Action.CompileImplementation _ -> true
              | _ -> false
            )
            (Tusk_planner.Action_graph.to_action_list action_graph)
        with
        | Some (Tusk_planner.Action.CompileImplementation { flags; _ }) ->
            let has_flag expected = List.mem expected flags in
            if not (has_flag (Tusk_toolchain.Ocamlc.Inline 100)) then
              Error "expected release compile action to include inline threshold"
            else if not (has_flag Tusk_toolchain.Ocamlc.NoAssert) then
              Error "expected release compile action to include -noassert"
            else if not (has_flag Tusk_toolchain.Ocamlc.Compact) then
              Error "expected release compile action to include -compact"
            else if not (has_flag (Tusk_toolchain.Ocamlc.WarnError [ Tusk_toolchain.Ocamlc.All ])) then
              Error "expected release compile action to treat all warnings as errors"
            else
              Ok ()
        | Some _ ->
            Error "expected CompileImplementation action"
        | None ->
            Error "missing CompileImplementation action")
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let tests =
  Test.[
    case "action graph json round-trip preserves edges" test_action_graph_json_round_trip_preserves_dependencies;
    case "action graph json round-trip preserves package paths and hashes" test_action_graph_json_round_trip_preserves_package_paths_and_hashes;
    case "action hash tracks package-relative source contents" test_action_hash_tracks_package_relative_source_contents;
    case "library builds skip shared native plugin artifacts by default" test_library_builds_do_not_emit_shared_library_actions;
    case "library actions exclude ML object files while keeping native stubs" test_library_actions_exclude_ml_object_files;
    case "release profile flags flow into compile actions" test_release_profile_flags_flow_into_compile_actions;
  ]

let name = "Planner Action Graph Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()

open Std
open Riot_model

module Test = Std.Test
module G = Std.Graph.SimpleGraph

let test_toolchain =
  Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
  |> Result.expect ~msg:"Failed to initialize toolchain"

let make_package = fun name ->
  Riot_model.Package.make
    ~name:(
      Package_name.from_string name
      |> Result.expect ~msg:("expected valid package name: " ^ name)
    )
    ~path:(Path.v ".")
    ~relative_path:(Path.v ".")
    ()

let make_package_with_paths = fun ~name ~path ~relative_path ->
  Riot_model.Package.make
    ~name:(
      Package_name.from_string name
      |> Result.expect ~msg:("expected valid package name: " ^ name)
    )
    ~path
    ~relative_path
    ()

let make_write_spec = fun ~package ~path ~content ~deps ~dependency_hashes ->
  Riot_planner.Action_node.make
    ~actions:[ Riot_planner.Action.WriteFile { destination = path; content } ]
    ~outs:[ path ]
    ~srcs:[]
    ~package
    ~toolchain:test_toolchain
    ~dependency_hashes
    ~deps

let find_action_node_by_output = fun graph output_name ->
  Riot_planner.Action_graph.nodes graph
  |> List.find
    ~fn:(fun (node: Riot_planner.Action_node.t) ->
      List.any
        (G.value node).outs
        ~fn:(fun output -> Path.to_string output = output_name))

let dependency_output_names = fun graph (node: Riot_planner.Action_node.t) ->
  List.filter_map
    (G.deps node)
    ~fn:(fun dep_id ->
      match G.get_node (Riot_planner.Action_graph.graph graph) dep_id with
      | Some dep_node ->
          List.head ((G.value dep_node).outs)
          |> Option.map ~fn:Path.to_string
      | None -> None)

let dependency_output_names_flat = fun graph (node: Riot_planner.Action_node.t) ->
  List.filter_map
    (G.deps node)
    ~fn:(fun dep_id -> G.get_node (Riot_planner.Action_graph.graph graph) dep_id)
  |> List.map
    ~fn:(fun (dep_node: Riot_planner.Action_node.t) ->
      List.map
        (G.value dep_node).outs
        ~fn:Path.to_string)
  |> List.concat

let find_compile_action_node_by_source = fun graph source ->
  Riot_planner.Action_graph.nodes graph
  |> List.find
    ~fn:(fun (node: Riot_planner.Action_node.t) ->
      List.any
        (G.value node).actions
        ~fn:(fun action ->
          match action with
          | Riot_planner.Action.CompileImplementation { source = action_source; _ } ->
              Path.equal action_source source
          | _ -> false))

let test_action_graph_json_round_trip_preserves_dependencies = fun _ctx ->
  let package = make_package "pkg" in
  let graph = Riot_planner.Action_graph.create () in
  let write_a = Riot_planner.Action.WriteFile { destination = Path.v "a.txt"; content = "a" } in
  let spec_a =
    Riot_planner.Action_node.make
      ~actions:[ write_a ]
      ~outs:[ Path.v "a.txt" ]
      ~srcs:[]
      ~package
      ~toolchain:test_toolchain
      ~dependency_hashes:(fun _ -> Crypto.hash_string "")
      ~deps:[]
  in
  let node_a = Riot_planner.Action_graph.add_node graph spec_a in
  let write_b = Riot_planner.Action.WriteFile { destination = Path.v "b.txt"; content = "b" } in
  let spec_b =
    Riot_planner.Action_node.make
      ~actions:[ write_b ]
      ~outs:[ Path.v "b.txt" ]
      ~srcs:[]
      ~package
      ~toolchain:test_toolchain
      ~dependency_hashes:(fun dep_id ->
        if Graph.SimpleGraph.Node_id.eq dep_id (G.id node_a) then
          Riot_planner.Action_node.get_hash node_a
        else
          Crypto.hash_string "missing")
      ~deps:[ (G.id node_a) ]
  in
  let node_b = Riot_planner.Action_graph.add_node graph spec_b in
  Riot_planner.Action_graph.add_dependency graph node_b ~depends_on:node_a;
  let encoded = Riot_planner.Action_graph.to_json graph in
  match Riot_planner.Action_graph.from_json encoded with
  | Error err -> Error ("round-trip decode failed: " ^ err)
  | Ok decoded -> (
      let encoded_decoded = Riot_planner.Action_graph.to_json decoded in
      match Data.Json.get_field "nodes" encoded_decoded with
      | Some (Data.Json.Array node_jsons) ->
          let edge_count =
            List.fold_left
              node_jsons
              ~init:0
              ~fn:(fun acc node_json ->
                match Data.Json.get_field "dependencies" node_json with
                | Some (Data.Json.Array deps) -> acc + List.length deps
                | _ -> acc)
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
  let package =
    make_package_with_paths
      ~name:"kernel"
      ~path:(Path.v "packages/kernel")
      ~relative_path:(Path.v "packages/kernel")
  in
  let graph = Riot_planner.Action_graph.create () in
  let action = Riot_planner.Action.WriteFile {
    destination = Path.v "build/meta.txt";
    content = "ok";
  }
  in
  let spec =
    Riot_planner.Action_node.make
      ~actions:[ action ]
      ~outs:[ Path.v "build/meta.txt" ]
      ~srcs:[ Path.v "packages/kernel/src/lib.ml" ]
      ~package
      ~toolchain:test_toolchain
      ~dependency_hashes:(fun _ -> Crypto.hash_string "")
      ~deps:[]
  in
  let node = Riot_planner.Action_graph.add_node graph spec in
  let expected_hash = Riot_planner.Action_node.get_hash node in
  let encoded = Riot_planner.Action_graph.to_json graph in
  match Riot_planner.Action_graph.from_json encoded with
  | Error err -> Error ("round-trip decode failed: " ^ err)
  | Ok decoded -> (
      match Riot_planner.Action_graph.nodes decoded with
      | [ decoded_node ] ->
          let decoded_path = (G.value decoded_node).package.Riot_model.Package.path in
          let decoded_rel = (G.value decoded_node).package.Riot_model.Package.relative_path in
          let decoded_hash = Riot_planner.Action_node.get_hash decoded_node in
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

let test_action_graph_json_round_trip_preserves_dependency_order = fun _ctx ->
  let package = make_package "pkg" in
  let graph = Riot_planner.Action_graph.create () in
  let spec_a =
    make_write_spec
      ~package
      ~path:(Path.v "a.txt")
      ~content:"a"
      ~deps:[]
      ~dependency_hashes:(fun _ -> Crypto.hash_string "")
  in
  let node_a = Riot_planner.Action_graph.add_node graph spec_a in
  let spec_b =
    make_write_spec
      ~package
      ~path:(Path.v "b.txt")
      ~content:"b"
      ~deps:[]
      ~dependency_hashes:(fun _ -> Crypto.hash_string "")
  in
  let node_b = Riot_planner.Action_graph.add_node graph spec_b in
  let spec_c =
    make_write_spec
      ~package
      ~path:(Path.v "c.txt")
      ~content:"c"
      ~deps:[]
      ~dependency_hashes:(fun _ -> Crypto.hash_string "")
  in
  let node_c = Riot_planner.Action_graph.add_node graph spec_c in
  let spec_d =
    make_write_spec
      ~package
      ~path:(Path.v "d.txt")
      ~content:"d"
      ~deps:[ (G.id node_a); (G.id node_b); (G.id node_c) ]
      ~dependency_hashes:(fun dep_id ->
        if Graph.SimpleGraph.Node_id.eq dep_id (G.id node_a) then
          Riot_planner.Action_node.get_hash node_a
        else if Graph.SimpleGraph.Node_id.eq dep_id (G.id node_b) then
          Riot_planner.Action_node.get_hash node_b
        else if Graph.SimpleGraph.Node_id.eq dep_id (G.id node_c) then
          Riot_planner.Action_node.get_hash node_c
        else
          Crypto.hash_string "missing")
  in
  let node_d = Riot_planner.Action_graph.add_node graph spec_d in
  Riot_planner.Action_graph.add_dependency graph node_d ~depends_on:node_c;
  Riot_planner.Action_graph.add_dependency graph node_d ~depends_on:node_b;
  Riot_planner.Action_graph.add_dependency graph node_d ~depends_on:node_a;
  let expected = dependency_output_names graph node_d in
  match Riot_planner.Action_graph.from_json (Riot_planner.Action_graph.to_json graph) with
  | Error err -> Error ("round-trip decode failed: " ^ err)
  | Ok decoded -> (
      match find_action_node_by_output decoded "d.txt" with
      | Some decoded_node ->
          let actual = dependency_output_names decoded decoded_node in
          if actual = expected then
            Ok ()
          else
            Error ("expected dependency order ["
            ^ String.concat ", " expected
            ^ "] but got ["
            ^ String.concat ", " actual
            ^ "]")
      | None -> Error "expected decoded node d.txt"
    )

let test_action_hash_tracks_package_relative_source_contents = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"action_hash_pkg_src"
    (fun tmpdir ->
      let package_root = Path.(tmpdir / Path.v "packages" / Path.v "demo") in
      let src_dir = Path.(package_root / Path.v "src") in
      let source = Path.(src_dir / Path.v "demo.ml") in
      let _ =
        Fs.create_dir_all src_dir
        |> Result.expect ~msg:"create src dir failed"
      in
      let package =
        make_package_with_paths
          ~name:"demo"
          ~path:package_root
          ~relative_path:(Path.v "packages/demo")
      in
      let action = Riot_planner.Action.CompileImplementation {
        source = Path.v "src/demo.ml";
        outputs = [ Path.v "Demo.cmt"; Path.v "Demo.cmi"; Path.v "Demo.cmx" ];
        includes = [ Path.v "." ];
        flags = [];
      }
      in
      let write contents =
        Fs.write contents source
        |> Result.expect ~msg:"write source failed"
      in
      write "let value = 1\n";
      let first =
        Riot_planner.Action_node.make
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
        Riot_planner.Action_node.make
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
        Ok ()) with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_library_builds_do_not_emit_shared_library_actions = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_no_shared"
    (fun tmpdir ->
      let workspace = Riot_model.Workspace.make ~root:tmpdir ~packages:[] () in
      let store = Riot_store.Store.create ~workspace in
      let package =
        Riot_model.Package.make
          ~name:(
            Package_name.from_string "minttea"
            |> Result.expect ~msg:"expected valid package name"
          )
          ~path:Path.(tmpdir / Path.v "packages" / Path.v "minttea")
          ~relative_path:(Path.v "packages/minttea")
          ~library:{ path = Path.v "src/minttea.ml" }
          ()
      in
      let ctx =
        Riot_model.Build_ctx.make
          ~session_id:(Riot_model.Session_id.from_string "test-session")
          ~profile:Riot_model.Profile.release
          ()
      in
      let module_graph = G.make () in
      let _ =
        G.add_node
          module_graph
          (Riot_planner.Module_node.make_library
            ~name:(Package_name.to_string package.name)
            ~includes:[ Path.v "." ])
      in
      let (action_graph, _) =
        Riot_planner.Action_graph.from_module_graph
          ~package
          ~profile:Riot_model.Profile.release
          ~ctx
          ~toolchain:test_toolchain
          ~store
          ~depset:[]
          ~needs_unix:false
          ~needs_dynlink:false
          module_graph
      in
      let shared_actions =
        List.filter
          (Riot_planner.Action_graph.to_action_list action_graph)
          ~fn:(fun __tmp1 ->
            match __tmp1 with
            | Riot_planner.Action.CreateSharedLibrary _ -> true
            | _ -> false)
      in
      if List.is_empty shared_actions then
        Ok ()
      else
        Error "expected library builds to skip CreateSharedLibrary actions") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_library_actions_exclude_ml_object_files = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_library_objects"
    (fun tmpdir ->
      let package_root = Path.(tmpdir / Path.v "packages" / Path.v "demo") in
      let src_dir = Path.(package_root / Path.v "src") in
      let native_dir = Path.(package_root / Path.v "native") in
      let source_path = Path.(src_dir / Path.v "demo.ml") in
      let native_path = Path.(native_dir / Path.v "stub.c") in
      let _ =
        Fs.create_dir_all src_dir
        |> Result.expect ~msg:"create src dir failed"
      in
      let _ =
        Fs.create_dir_all native_dir
        |> Result.expect ~msg:"create native dir failed"
      in
      let _ =
        Fs.write "let value = 1\n" source_path
        |> Result.expect ~msg:"write ml source failed"
      in
      let _ =
        Fs.write "int demo_stub(void) { return 1; }\n" native_path
        |> Result.expect ~msg:"write native source failed"
      in
      let workspace = Riot_model.Workspace.make ~root:tmpdir ~packages:[] () in
      let store = Riot_store.Store.create ~workspace in
      let package =
        Riot_model.Package.make
          ~name:(
            Package_name.from_string "demo"
            |> Result.expect ~msg:"expected valid package name"
          )
          ~path:package_root
          ~relative_path:(Path.v "packages/demo")
          ~library:{ path = Path.v "src/demo.ml" }
          ()
      in
      let ctx =
        Riot_model.Build_ctx.make
          ~session_id:(Riot_model.Session_id.from_string "test-session")
          ~profile:Riot_model.Profile.release
          ()
      in
      let module_graph = G.make () in
      let demo_module =
        Riot_model.Module.make
          ~namespace:Riot_model.Namespace.empty
          ~filename:(Path.v "src/demo.ml")
      in
      let demo_node =
        G.add_node
          module_graph
          (Riot_planner.Module_node.make_ml
            demo_module
            (Riot_planner.Module_node.Concrete (Path.v "src/demo.ml")))
      in
      let native_node =
        G.add_node
          module_graph
          (Riot_planner.Module_node.make_native ~files:[ Path.v "native/stub.c" ])
      in
      let library_node =
        G.add_node
          module_graph
          (Riot_planner.Module_node.make_library
            ~name:(Package_name.to_string package.name)
            ~includes:[ Path.v "." ])
      in
      let _ = G.add_edge library_node ~depends_on:demo_node in
      let _ = G.add_edge library_node ~depends_on:native_node in
      let (action_graph, _) =
        Riot_planner.Action_graph.from_module_graph
          ~package
          ~profile:Riot_model.Profile.release
          ~ctx
          ~toolchain:test_toolchain
          ~store
          ~depset:[]
          ~needs_unix:false
          ~needs_dynlink:false
          module_graph
      in
      match List.find
        (Riot_planner.Action_graph.to_action_list action_graph)
        ~fn:(fun __tmp1 ->
          match __tmp1 with
          | Riot_planner.Action.CreateLibrary _ -> true
          | _ -> false) with
      | Some (Riot_planner.Action.CreateLibrary { objects; _ }) ->
          let has_demo_cmx = List.any objects ~fn:(Path.equal (Path.v "Demo.cmx")) in
          let has_demo_o = List.any objects ~fn:(Path.equal (Path.v "Demo.o")) in
          let has_stub_o = List.any objects ~fn:(Path.equal (Path.v "stub.o")) in
          if not has_demo_cmx then
            Error "expected CreateLibrary to include Demo.cmx"
          else if has_demo_o then
            Error "expected CreateLibrary to exclude Demo.o from ML module outputs"
          else if not has_stub_o then
            Error "expected CreateLibrary to keep stub.o from native C sources"
          else
            Ok ()
      | Some _ -> Error "expected CreateLibrary action"
      | None -> Error "missing CreateLibrary action") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_release_profile_flags_flow_into_compile_actions = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_release_flags"
    (fun tmpdir ->
      let package_root = Path.(tmpdir / Path.v "packages" / Path.v "demo") in
      let src_dir = Path.(package_root / Path.v "src") in
      let source_path = Path.(src_dir / Path.v "demo.ml") in
      let _ =
        Fs.create_dir_all src_dir
        |> Result.expect ~msg:"create src dir failed"
      in
      let _ =
        Fs.write "let value = 1\n" source_path
        |> Result.expect ~msg:"write ml source failed"
      in
      let workspace = Riot_model.Workspace.make ~root:tmpdir ~packages:[] () in
      let store = Riot_store.Store.create ~workspace in
      let package =
        make_package_with_paths
          ~name:"demo"
          ~path:package_root
          ~relative_path:(Path.v "packages/demo")
      in
      let ctx =
        Riot_model.Build_ctx.make
          ~session_id:(Riot_model.Session_id.from_string "test-session")
          ~profile:Riot_model.Profile.release
          ()
      in
      let module_graph = G.make () in
      let demo_module =
        Riot_model.Module.make
          ~namespace:Riot_model.Namespace.empty
          ~filename:(Path.v "src/demo.ml")
      in
      let _ =
        G.add_node
          module_graph
          (Riot_planner.Module_node.make_ml
            demo_module
            (Riot_planner.Module_node.Concrete (Path.v "src/demo.ml")))
      in
      let (action_graph, _) =
        Riot_planner.Action_graph.from_module_graph
          ~package
          ~profile:Riot_model.Profile.release
          ~ctx
          ~toolchain:test_toolchain
          ~store
          ~depset:[]
          ~needs_unix:false
          ~needs_dynlink:false
          module_graph
      in
      match List.find
        (Riot_planner.Action_graph.to_action_list action_graph)
        ~fn:(fun __tmp1 ->
          match __tmp1 with
          | Riot_planner.Action.CompileImplementation _ -> true
          | _ -> false) with
      | Some (Riot_planner.Action.CompileImplementation { flags; _ }) ->
          let has_flag expected = List.any flags ~fn:(fun flag -> flag = expected) in
          if not (has_flag (Riot_toolchain.Ocamlc.Inline 100)) then
            Error "expected release compile action to include inline threshold"
          else if not (has_flag Riot_toolchain.Ocamlc.NoAssert) then
            Error "expected release compile action to include -noassert"
          else if not (has_flag Riot_toolchain.Ocamlc.Compact) then
            Error "expected release compile action to include -compact"
          else if
            not (has_flag (Riot_toolchain.Ocamlc.WarnError [ Riot_toolchain.Ocamlc.All ]))
          then
            Error "expected release compile action to treat all warnings as errors"
          else
            Ok ()
      | Some _ -> Error "expected CompileImplementation action"
      | None -> Error "missing CompileImplementation action") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_create_library_preserves_module_dependency_order = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_library_object_order"
    (fun tmpdir ->
      let package_root = Path.(tmpdir / Path.v "packages" / Path.v "demo") in
      let src_dir = Path.(package_root / Path.v "src") in
      let _ =
        Fs.create_dir_all src_dir
        |> Result.expect ~msg:"create src dir failed"
      in
      let workspace = Riot_model.Workspace.make ~root:tmpdir ~packages:[] () in
      let store = Riot_store.Store.create ~workspace in
      let package =
        Riot_model.Package.make
          ~name:(
            Package_name.from_string "demo"
            |> Result.expect ~msg:"expected valid package name"
          )
          ~path:package_root
          ~relative_path:(Path.v "packages/demo")
          ~library:{ path = Path.v "src/demo.ml" }
          ()
      in
      let ctx =
        Riot_model.Build_ctx.make
          ~session_id:(Riot_model.Session_id.from_string "test-session")
          ~profile:Riot_model.Profile.release
          ()
      in
      let module_graph = G.make () in
      let make_ml_node filename =
        let mod_ =
          Riot_model.Module.make
            ~namespace:Riot_model.Namespace.empty
            ~filename:(Path.v ("src/" ^ filename ^ ".ml"))
        in
        G.add_node
          module_graph
          (Riot_planner.Module_node.make_ml
            mod_
            (Riot_planner.Module_node.Concrete (Path.v ("src/" ^ filename ^ ".ml"))))
      in
      let node_a = make_ml_node "a" in
      let node_b = make_ml_node "b" in
      let node_c = make_ml_node "c" in
      let library_node =
        G.add_node
          module_graph
          (Riot_planner.Module_node.make_library
            ~name:(Package_name.to_string package.name)
            ~includes:[ Path.v "." ])
      in
      let _ = G.add_edge library_node ~depends_on:node_c in
      let _ = G.add_edge library_node ~depends_on:node_b in
      let _ = G.add_edge library_node ~depends_on:node_a in
      let (action_graph, _) =
        Riot_planner.Action_graph.from_module_graph
          ~package
          ~profile:Riot_model.Profile.release
          ~ctx
          ~toolchain:test_toolchain
          ~store
          ~depset:[]
          ~needs_unix:false
          ~needs_dynlink:false
          module_graph
      in
      match List.find
        (Riot_planner.Action_graph.to_action_list action_graph)
        ~fn:(fun action ->
          match action with
          | Riot_planner.Action.CreateLibrary _ -> true
          | _ -> false) with
      | Some (Riot_planner.Action.CreateLibrary { objects; _ }) ->
          let actual = List.map objects ~fn:Path.to_string in
          let expected = [ "C.cmx"; "B.cmx"; "A.cmx" ] in
          if actual = expected then
            Ok ()
          else
            Error ("expected CreateLibrary objects ["
            ^ String.concat ", " expected
            ^ "] but got ["
            ^ String.concat ", " actual
            ^ "]")
      | Some _ -> Error "expected CreateLibrary action"
      | None -> Error "missing CreateLibrary action") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_library_actions_exclude_unreachable_modules = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_library_reachability"
    (fun tmpdir ->
      let package_root = Path.(tmpdir / Path.v "packages" / Path.v "lib_with_unreachable") in
      let src_dir = Path.(package_root / Path.v "src") in
      let _ =
        Fs.create_dir_all src_dir
        |> Result.expect ~msg:"create src dir failed"
      in
      let _ =
        Fs.write "module A = A\n" Path.(src_dir / Path.v "lib_with_unreachable.ml")
        |> Result.expect ~msg:"write root library source failed"
      in
      let _ =
        Fs.write "let used = 42\n" Path.(src_dir / Path.v "a.ml")
        |> Result.expect ~msg:"write a.ml failed"
      in
      let _ =
        Fs.write "let unused = 7\n" Path.(src_dir / Path.v "orphan.ml")
        |> Result.expect ~msg:"write orphan.ml failed"
      in
      let workspace = Riot_model.Workspace.make ~root:tmpdir ~packages:[] () in
      let store = Riot_store.Store.create ~workspace in
      let package =
        Riot_model.Package.make
          ~name:(
            Package_name.from_string "lib_with_unreachable"
            |> Result.expect ~msg:"expected valid package name"
          )
          ~path:package_root
          ~relative_path:(Path.v "packages/lib_with_unreachable")
          ~library:{ path = Path.v "src/lib_with_unreachable.ml" }
          ~sources:{
            src = [
              Path.v "src/lib_with_unreachable.ml";
              Path.v "src/a.ml";
              Path.v "src/orphan.ml";
            ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
      in
      let build_ctx =
        Riot_model.Build_ctx.make
          ~session_id:(Riot_model.Session_id.from_string "test-session")
          ~profile:Riot_model.Profile.release
          ()
      in
      let graph_builder =
        Riot_planner.Module_graph.create
          Riot_planner.Module_graph.{
            root = package_root;
            source_groups =
              [
                Riot_planner.Module_graph.{
                  source_dir = Path.v "src";
                  allowed_source_files = package.sources.src;
                  root_mode = Riot_planner.Module_graph.Library_root {
                    library_name = Package_name.to_string package.name;
                    public_root_name = Package_name.to_string package.name;
                  };
                  namespace = Namespace.empty;
                };
              ];
            package;
            toolchain = test_toolchain;
            workspace;
          }
      in
      match Riot_planner.Module_graph.wire_dependencies graph_builder with
      | Error err ->
          Error ("dependency wiring failed: " ^ Riot_planner.Planning_error.to_string err)
      | Ok () ->
          Riot_planner.Module_graph.add_library_node
            graph_builder
            ~name:(Package_name.to_string package.name)
            ~includes:[];
          let (action_graph, _) =
            Riot_planner.Action_graph.from_module_graph
              ~analyzed_modules:(Riot_planner.Module_graph.analyzed_modules graph_builder)
              ~package
              ~profile:Riot_model.Profile.release
              ~ctx:build_ctx
              ~toolchain:test_toolchain
              ~store
              ~depset:[]
              ~needs_unix:false
              ~needs_dynlink:false
              (Riot_planner.Module_graph.graph graph_builder)
          in
          let actions = Riot_planner.Action_graph.to_action_list action_graph in
          let compiles_orphan =
            List.any
              actions
              ~fn:(fun __tmp1 ->
                match __tmp1 with
                | Riot_planner.Action.CompileImplementation { source; _ } ->
                    Path.equal source (Path.v "src/orphan.ml")
                | _ -> false)
          in
          let compiled_sources =
            List.filter_map
              actions
              ~fn:(fun __tmp1 ->
                match __tmp1 with
                | Riot_planner.Action.CompileImplementation { source; _ } ->
                    Some (Path.to_string source)
                | _ -> None)
          in
          let create_library =
            List.find
              actions
              ~fn:(fun __tmp1 ->
                match __tmp1 with
                | Riot_planner.Action.CreateLibrary _ -> true
                | _ -> false)
          in
          match create_library with
          | Some (Riot_planner.Action.CreateLibrary { objects; _ }) ->
              let has_a =
                List.any objects ~fn:(Path.equal (Path.v "Lib_with_unreachable__A.cmx"))
              in
              let has_orphan =
                List.any objects ~fn:(Path.equal (Path.v "Lib_with_unreachable__Orphan.cmx"))
              in
              let object_names =
                List.map objects ~fn:Path.to_string
                |> String.concat ", "
              in
              let compiled_source_names = String.concat ", " compiled_sources in
              if compiles_orphan then
                Error ("did not expect compile action for unreachable orphan module; compiled sources: "
                ^ compiled_source_names)
              else if not has_a then
                Error ("expected CreateLibrary to include reachable module A; objects: "
                ^ object_names
                ^ "; compiled sources: "
                ^ compiled_source_names)
              else if has_orphan then
                Error ("did not expect CreateLibrary to include unreachable orphan module; objects: "
                ^ object_names)
              else
                Ok ()
          | Some _ -> Error "expected CreateLibrary action"
          | None -> Error "missing CreateLibrary action") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let find_compile_implementation = fun actions source ->
  List.find
    actions
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Riot_planner.Action.CompileImplementation { source = action_source; _ } ->
          Path.equal action_source source
      | _ -> false)

let find_compile_cmx = fun actions source ->
  match find_compile_implementation actions source with
  | Some (Riot_planner.Action.CompileImplementation { outputs; _ }) ->
      List.find outputs ~fn:(fun output -> Path.extension output = Some ".cmx")
  | _ -> None

let compile_sources = fun actions ->
  List.filter_map
    actions
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Riot_planner.Action.CompileImplementation { source; _ } -> Some source
      | _ -> None)

let find_create_library = fun actions ->
  List.find
    actions
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Riot_planner.Action.CreateLibrary _ -> true
      | _ -> false)

let find_create_executable = fun actions ->
  List.find
    actions
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Riot_planner.Action.CreateExecutable _ -> true
      | _ -> false)

let find_create_executable_named = fun actions name ->
  List.find
    actions
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Riot_planner.Action.CreateExecutable { outputs; _ } ->
          List.any outputs ~fn:(Path.equal (Path.v name))
      | _ -> false)

let index_of_path = fun paths target ->
  let rec loop index = fun __tmp1 ->
    match __tmp1 with
    | [] -> None
    | path :: rest ->
        if Path.equal path target then
          Some index
        else
          loop (index + 1) rest
  in
  loop 0 paths

let binary_main = fun expression -> "let main ~args:_ =\n  " ^ expression ^ ";\n  Ok ()\n"

let plan_actions_for_package = fun ~tmpdir ~package_name ~files ?(binaries = []) ?library () ->
  let package_root = Path.(tmpdir / Path.v "packages" / Path.v package_name) in
  let src_dir = Path.(package_root / Path.v "src") in
  let _ =
    Fs.create_dir_all src_dir
    |> Result.expect ~msg:"create src dir failed"
  in
  let () =
    List.for_each
      files
      ~fn:(fun (relpath, content) ->
        let path = Path.(package_root / Path.v relpath) in
        let parent =
          Path.parent path
          |> Option.unwrap_or ~default:package_root
        in
        let _ =
          Fs.create_dir_all parent
          |> Result.expect ~msg:("create parent dir failed for " ^ relpath)
        in
        let _ =
          Fs.write content path
          |> Result.expect ~msg:("write failed for " ^ relpath)
        in
        ())
  in
  let package =
    Riot_model.Package.make
      ~name:(
        Package_name.from_string package_name
        |> Result.expect ~msg:"expected valid package name"
      )
      ~path:package_root
      ~relative_path:Path.(Path.v "packages" / Path.v package_name)
      ?library
      ~binaries:(List.map
        binaries
        ~fn:(fun (name, path) -> Riot_model.Package.{ name; path = Path.v path }))
      ~sources:{
        src = List.map files ~fn:(fun (relpath, _) -> Path.v relpath);
        native = [];
        tests = [];
        examples = [];
        bench = [];
      }
      ()
  in
  let workspace = Riot_model.Workspace.make ~root:tmpdir ~packages:[] () in
  let store = Riot_store.Store.create ~workspace in
  let build_ctx =
    Riot_model.Build_ctx.make
      ~session_id:(Riot_model.Session_id.from_string "test-session")
      ~profile:Riot_model.Profile.release
      ()
  in
  let graph_builder =
    Riot_planner.Module_graph.create
      Riot_planner.Module_graph.{
        root = package_root;
        source_groups =
          [
            Riot_planner.Module_graph.{
              source_dir = Path.v "src";
              allowed_source_files = package.sources.src;
              root_mode =
                (
                  match package.library with
                  | Some _ ->
                      Riot_planner.Module_graph.Library_root {
                        library_name = Package_name.to_string package.name;
                        public_root_name = Package_name.to_string package.name;
                      }
                  | None -> Riot_planner.Module_graph.Loose_sources
                );
              namespace = Namespace.empty;
            };
          ];
        package;
        toolchain = test_toolchain;
        workspace;
      }
  in
  match Riot_planner.Module_graph.wire_dependencies graph_builder with
  | Error err -> Error ("dependency wiring failed: " ^ Riot_planner.Planning_error.to_string err)
  | Ok () ->
      let () =
        match package.library with
        | Some _ ->
            Riot_planner.Module_graph.add_library_node
              graph_builder
              ~name:(Package_name.to_string package.name)
              ~includes:[]
        | None -> ()
      in
      let binary_libraries =
        match package.library with
        | Some _ ->
            [
              Riot_model.Module_name.(from_string (Package_name.to_string package.name)
              |> cmxa);
            ]
        | None -> []
      in
      let () =
        List.for_each
          binaries
          ~fn:(fun (name, path) ->
            Riot_planner.Module_graph.add_binary_node
              graph_builder
              ~name
              ~source:(Path.v path)
              ~libraries:binary_libraries
              ~includes:[ Path.v "." ])
      in
      let (action_graph, _) =
        Riot_planner.Action_graph.from_module_graph
          ~analyzed_modules:(Riot_planner.Module_graph.analyzed_modules graph_builder)
          ~package
          ~profile:Riot_model.Profile.release
          ~ctx:build_ctx
          ~toolchain:test_toolchain
          ~store
          ~depset:[]
          ~needs_unix:false
          ~needs_dynlink:false
          (Riot_planner.Module_graph.graph graph_builder)
      in
      Ok (package, Riot_planner.Action_graph.to_action_list action_graph)

let plan_action_graph_for_package = fun ~tmpdir ~package_name ~files ?(binaries = []) ?library () ->
  let package_root = Path.(tmpdir / Path.v "packages" / Path.v package_name) in
  let src_dir = Path.(package_root / Path.v "src") in
  let _ =
    Fs.create_dir_all src_dir
    |> Result.expect ~msg:"create src dir failed"
  in
  let () =
    List.for_each
      files
      ~fn:(fun (relpath, content) ->
        let path = Path.(package_root / Path.v relpath) in
        let parent =
          Path.parent path
          |> Option.unwrap_or ~default:package_root
        in
        let _ =
          Fs.create_dir_all parent
          |> Result.expect ~msg:("create parent dir failed for " ^ relpath)
        in
        let _ =
          Fs.write content path
          |> Result.expect ~msg:("write failed for " ^ relpath)
        in
        ())
  in
  let package =
    Riot_model.Package.make
      ~name:(
        Package_name.from_string package_name
        |> Result.expect ~msg:"expected valid package name"
      )
      ~path:package_root
      ~relative_path:Path.(Path.v "packages" / Path.v package_name)
      ?library
      ~binaries:(List.map
        binaries
        ~fn:(fun (name, path) -> Riot_model.Package.{ name; path = Path.v path }))
      ~sources:{
        src = List.map files ~fn:(fun (relpath, _) -> Path.v relpath);
        native = [];
        tests = [];
        examples = [];
        bench = [];
      }
      ()
  in
  let workspace = Riot_model.Workspace.make ~root:tmpdir ~packages:[] () in
  let store = Riot_store.Store.create ~workspace in
  let build_ctx =
    Riot_model.Build_ctx.make
      ~session_id:(Riot_model.Session_id.from_string "test-session")
      ~profile:Riot_model.Profile.release
      ()
  in
  let graph_builder =
    Riot_planner.Module_graph.create
      Riot_planner.Module_graph.{
        root = package_root;
        source_groups =
          [
            Riot_planner.Module_graph.{
              source_dir = Path.v "src";
              allowed_source_files = package.sources.src;
              root_mode =
                (
                  match package.library with
                  | Some _ ->
                      Riot_planner.Module_graph.Library_root {
                        library_name = Package_name.to_string package.name;
                        public_root_name = Package_name.to_string package.name;
                      }
                  | None -> Riot_planner.Module_graph.Loose_sources
                );
              namespace = Namespace.empty;
            };
          ];
        package;
        toolchain = test_toolchain;
        workspace;
      }
  in
  match Riot_planner.Module_graph.wire_dependencies graph_builder with
  | Error err -> Error ("dependency wiring failed: " ^ Riot_planner.Planning_error.to_string err)
  | Ok () ->
      let () =
        match package.library with
        | Some _ ->
            Riot_planner.Module_graph.add_library_node
              graph_builder
              ~name:(Package_name.to_string package.name)
              ~includes:[]
        | None -> ()
      in
      let binary_libraries =
        match package.library with
        | Some _ ->
            [
              Riot_model.Module_name.(from_string (Package_name.to_string package.name)
              |> cmxa);
            ]
        | None -> []
      in
      let () =
        List.for_each
          binaries
          ~fn:(fun (name, path) ->
            Riot_planner.Module_graph.add_binary_node
              graph_builder
              ~name
              ~source:(Path.v path)
              ~libraries:binary_libraries
              ~includes:[ Path.v "." ])
      in
      let (action_graph, _) =
        Riot_planner.Action_graph.from_module_graph
          ~analyzed_modules:(Riot_planner.Module_graph.analyzed_modules graph_builder)
          ~package
          ~profile:Riot_model.Profile.release
          ~ctx:build_ctx
          ~toolchain:test_toolchain
          ~store
          ~depset:[]
          ~needs_unix:false
          ~needs_dynlink:false
          (Riot_planner.Module_graph.graph graph_builder)
      in
      Ok (package, action_graph)

let test_generated_library_interface_depends_on_child_module_interfaces = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_generated_library_interface_deps"
    (fun tmpdir ->
      match plan_action_graph_for_package
        ~tmpdir
        ~package_name:"rootexportdemo"
        ~library:{ path = Path.v "src/rootexportdemo.ml" }
        ~files:[ ("src/framing.mli", "val helper: int\n"); ("src/framing.ml", "let helper = 2\n"); ]
        () with
      | Error _ as err -> err
      | Ok (_package, action_graph) ->
          match find_action_node_by_output action_graph "Rootexportdemo.cmti" with
          | None -> Error "expected generated root interface compile action"
          | Some root_intf_node ->
              let dep_outputs = dependency_output_names action_graph root_intf_node in
              if List.any dep_outputs ~fn:(String.equal "Rootexportdemo__Framing.cmti") then
                Ok ()
              else
                Error ("expected generated root interface to depend on child module interface; deps: ["
                ^ String.concat ", " dep_outputs
                ^ "]")) with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_generated_library_interface_with_multiple_children_depends_on_child_module_interfaces = fun
  _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_generated_library_interface_multiple_children"
    (fun tmpdir ->
      match plan_action_graph_for_package
        ~tmpdir
        ~package_name:"riot_doc"
        ~library:{ path = Path.v "src/riot_doc.ml" }
        ~files:[
          ("src/doctree.mli", "val render: int\n");
          ("src/doctree.ml", "let render = 1\n");
          ("src/html.mli", "val emit: int\n");
          ("src/html.ml", "let emit = 2\n");
          ("src/source.mli", "val load: int\n");
          ("src/source.ml", "let load = 3\n");
          ("src/transform.mli", "val run: int\n");
          ("src/transform.ml", "let run = 4\n");
        ]
        () with
      | Error _ as err -> err
      | Ok (_package, action_graph) ->
          match find_action_node_by_output action_graph "Riot_doc.cmti" with
          | None -> Error "expected generated root interface compile action"
          | Some root_intf_node ->
              let dep_outputs = dependency_output_names action_graph root_intf_node in
              let expected_outputs = [
                "Riot_doc__Doctree.cmti";
                "Riot_doc__Html.cmti";
                "Riot_doc__Source.cmti";
                "Riot_doc__Transform.cmti";
              ]
              in
              if
                List.all
                  expected_outputs
                  ~fn:(fun output -> List.any dep_outputs ~fn:(String.equal output))
              then
                Ok ()
              else
                Error ("expected generated root interface to depend on child module interfaces; deps: ["
                ^ String.concat ", " dep_outputs
                ^ "]")) with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_nested_generated_library_interface_depends_on_public_child_modules = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_nested_generated_library_interface_deps"
    (fun tmpdir ->
      match plan_action_graph_for_package
        ~tmpdir
        ~package_name:"stdish"
        ~library:{ path = Path.v "src/stdish.ml" }
        ~files:[
          ("src/stdish.ml", "module Crypto = Crypto\n");
          ("src/crypto/crypto.ml", "module Md5 = Algo.Md5\n");
          ("src/crypto/algo/md5.ml", "let hash = 1\n");
        ]
        () with
      | Error _ as err -> err
      | Ok (_package, action_graph) ->
          match find_action_node_by_output action_graph "Stdish__Crypto__Algo.cmti" with
          | None -> Error "expected generated nested interface compile action"
          | Some algo_intf_node ->
              let dep_outputs = dependency_output_names action_graph algo_intf_node in
              if List.any dep_outputs ~fn:(String.equal "Stdish__Crypto__Algo__Md5.cmt") then
                Ok ()
              else
                Error ("expected generated nested interface to depend on child module implementation; deps: ["
                ^ String.concat ", " dep_outputs
                ^ "]")) with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_nested_concrete_library_implementation_keeps_alias_child_dependency = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_nested_concrete_alias_child_dep"
    (fun tmpdir ->
      match plan_action_graph_for_package
        ~tmpdir
        ~package_name:"kernelish"
        ~library:{ path = Path.v "src/kernelish.ml" }
        ~files:[
          ("src/kernelish.ml", {ocaml|module Prelude = Prelude
module Regex = Regex
|ocaml});
          ("src/prelude.mli", {ocaml|type 'a result =
  | Ok of 'a
  | Error of string
|ocaml});
          ("src/prelude.ml", {ocaml|type 'a result =
  | Ok of 'a
  | Error of string
|ocaml});
          ("src/regex/regex.mli", {ocaml|type t
val value: int
|ocaml});
          (
            "src/regex/regex.ml",
            {ocaml|open Prelude

type t = Regex_stubs.compiled
let value = Regex_stubs.value
|ocaml}
          );
          ("src/regex/regex_stubs.ml", {ocaml|type compiled = int
let value = 1
|ocaml});
        ]
        () with
      | Error _ as err -> err
      | Ok (_package, action_graph) -> (
          match find_compile_action_node_by_source action_graph (Path.v "src/regex/regex.ml") with
          | None -> Error "expected compile action for src/regex/regex.ml"
          | Some regex_node ->
              let dep_outputs = dependency_output_names_flat action_graph regex_node in
              let has output = List.any dep_outputs ~fn:(String.equal output) in
              if not (has "Kernelish__Regex__Regex_stubs.cmi") then
                Error ("expected regex.ml to depend on Regex_stubs.cmi through the implicit alias open; deps: ["
                ^ String.concat ", " dep_outputs
                ^ "]")
              else if not (has "Kernelish__Regex__Regex_stubs.cmt") then
                Error ("expected action graph to compile Regex_stubs before regex.ml; deps: ["
                ^ String.concat ", " dep_outputs
                ^ "]")
              else
                Ok ()
        )) with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_nested_concrete_library_implementation_keeps_generated_child_root_dependency = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_nested_concrete_generated_child_root_dep"
    (fun tmpdir ->
      match plan_action_graph_for_package
        ~tmpdir
        ~package_name:"stdish"
        ~library:{ path = Path.v "src/stdish.ml" }
        ~files:[
          ("src/stdish.ml", {ocaml|module Crypto = Crypto
|ocaml});
          ("src/crypto/crypto.mli", {ocaml|module Sha1: sig
  val value: int
end
|ocaml});
          ("src/crypto/crypto.ml", {ocaml|module Sha1 = Algo.Sha1
|ocaml});
          ("src/crypto/algo/sha1.ml", {ocaml|let value = 1
|ocaml});
        ]
        () with
      | Error _ as err -> err
      | Ok (_package, action_graph) -> (
          match find_compile_action_node_by_source action_graph (Path.v "src/crypto/crypto.ml") with
          | None -> Error "expected compile action for src/crypto/crypto.ml"
          | Some crypto_node ->
              let dep_outputs = dependency_output_names_flat action_graph crypto_node in
              let has output = List.any dep_outputs ~fn:(String.equal output) in
              if not (has "Stdish__Crypto__Algo.cmi") then
                Error ("expected crypto.ml to depend on generated Algo.cmi; deps: ["
                ^ String.concat ", " dep_outputs
                ^ "]")
              else if not (has "Stdish__Crypto__Algo.cmt") then
                Error ("expected crypto.ml to depend on generated Algo.cmt; deps: ["
                ^ String.concat ", " dep_outputs
                ^ "]")
              else
                Ok ()
        )) with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_real_kernel_unix_addr_interface_keeps_sibling_modules = fun _ctx ->
  let package_root = Path.v "packages/kernel" in
  let package =
    Riot_model.Package.make
      ~name:(
        Package_name.from_string "kernel"
        |> Result.expect ~msg:"expected valid package name"
      )
      ~path:package_root
      ~relative_path:(Path.v "packages/kernel")
      ~library:{ path = Path.v "src/kernel.ml" }
      ~sources:{
        src = [
          Path.v "src/kernel.ml";
          Path.v "src/result.mli";
          Path.v "src/result.ml";
          Path.v "src/system_error.mli";
          Path.v "src/system_error.ml";
          Path.v "src/net/net.ml";
          Path.v "src/net/socket_addr/socket_addr.mli";
          Path.v "src/net/socket_addr/socket_addr.ml";
          Path.v "src/net/addr/addr.mli";
          Path.v "src/net/addr/addr.ml";
          Path.v "src/net/addr/unix.mli";
        ];
        native = [];
        tests = [];
        examples = [];
        bench = [];
      }
      ()
  in
  let workspace =
    Riot_model.Workspace.make_realized
      ~root:(Path.v ".")
      ~packages:[ package ]
      ~target_dir:(Path.v "target")
      ()
  in
  let store = Riot_store.Store.create ~workspace in
  let build_ctx =
    Riot_model.Build_ctx.make
      ~session_id:(Riot_model.Session_id.from_string "test-session")
      ~profile:Riot_model.Profile.release
      ()
  in
  let graph_builder =
    Riot_planner.Module_graph.create
      Riot_planner.Module_graph.{
        root = package_root;
        source_groups =
          [
            Riot_planner.Module_graph.{
              source_dir = Path.v "src";
              allowed_source_files = package.sources.src;
              root_mode = Riot_planner.Module_graph.Library_root {
                library_name = Package_name.to_string package.name;
                public_root_name = Package_name.to_string package.name;
              };
              namespace = Namespace.empty;
            };
          ];
        package;
        toolchain = test_toolchain;
        workspace;
      }
  in
  match Riot_planner.Module_graph.wire_dependencies graph_builder with
  | Error err -> Error ("dependency wiring failed: " ^ Riot_planner.Planning_error.to_string err)
  | Ok () ->
      Riot_planner.Module_graph.add_library_node
        graph_builder
        ~name:(Package_name.to_string package.name)
        ~includes:[];
      let (action_graph, _) =
        Riot_planner.Action_graph.from_module_graph
          ~analyzed_modules:(Riot_planner.Module_graph.analyzed_modules graph_builder)
          ~package
          ~profile:Riot_model.Profile.release
          ~ctx:build_ctx
          ~toolchain:test_toolchain
          ~store
          ~depset:[]
          ~needs_unix:false
          ~needs_dynlink:false
          (Riot_planner.Module_graph.graph graph_builder)
      in
      match find_action_node_by_output action_graph "Kernel__Net__Addr__Unix.cmti" with
      | None -> Error "expected compile action for Kernel__Net__Addr__Unix.cmti"
      | Some unix_addr_node ->
          let dep_outputs = dependency_output_names_flat action_graph unix_addr_node in
          let has output = List.any dep_outputs ~fn:(String.equal output) in
          if not (has "Kernel__System_error.cmi") then
            Error ("expected Kernel__Net__Addr__Unix.cmti to depend on Kernel__System_error.cmi; deps: ["
            ^ String.concat ", " dep_outputs
            ^ "]")
          else if not (has "Kernel__Result.cmi") then
            Error ("expected Kernel__Net__Addr__Unix.cmti to depend on Kernel__Result.cmi; deps: ["
            ^ String.concat ", " dep_outputs
            ^ "]")
          else if not (has "Kernel__Net__Socket_addr.cmi") then
            Error ("expected Kernel__Net__Addr__Unix.cmti to depend on Kernel__Net__Socket_addr.cmi; deps: ["
            ^ String.concat ", " dep_outputs
            ^ "]")
          else
            Ok ()

let test_binary_actions_include_target_private_modules = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_binary_private_modules"
    (fun tmpdir ->
      match plan_actions_for_package
        ~tmpdir
        ~package_name:"splitdemo"
        ~library:{ path = Path.v "src/splitdemo.ml" }
        ~binaries:[ ("splitdemo", "src/main.ml"); ]
        ~files:[
          ("src/splitdemo.ml", "module A = A\n");
          ("src/a.ml", "let library_value = 1\n");
          ("src/b.ml", "let private_value = 2\n");
          ("src/main.ml", binary_main "ignore B.private_value");
          ("src/orphan.ml", "let orphan = 3\n");
        ]
        () with
      | Error _ as err -> err
      | Ok (_package, actions) ->
          let b_source = Path.v "src/b.ml" in
          let a_source = Path.v "src/a.ml" in
          let main_source = Path.v "src/main.ml" in
          let orphan_source = Path.v "src/orphan.ml" in
          let b_cmx = find_compile_cmx actions b_source in
          let a_cmx = find_compile_cmx actions a_source in
          let main_cmx = find_compile_cmx actions main_source in
          let compiles_orphan = List.any (compile_sources actions) ~fn:(Path.equal orphan_source) in
          match (
            find_create_library actions,
            find_create_executable actions,
            b_cmx,
            a_cmx,
            main_cmx
          ) with
          | (
              Some (Riot_planner.Action.CreateLibrary { objects = library_objects; _ }),
              Some (
                Riot_planner.Action.CreateExecutable { objects = binary_objects; libraries; _ }
              ),
              Some b_cmx,
              Some a_cmx,
              Some main_cmx
            ) ->
              let has object_ objects = List.any objects ~fn:(Path.equal object_) in
              let object_names objects =
                List.map objects ~fn:Path.to_string
                |> String.concat ", "
              in
              let binary_positions =
                (index_of_path binary_objects b_cmx, index_of_path binary_objects main_cmx)
              in
              if compiles_orphan then
                Error "did not expect unreachable orphan module to be compiled for library or binary"
              else if not (has a_cmx library_objects) then
                Error ("expected library archive to include A; objects: "
                ^ object_names library_objects)
              else if has b_cmx library_objects then
                Error ("did not expect binary-private helper B in library archive; objects: "
                ^ object_names library_objects)
              else if not (has b_cmx binary_objects) then
                Error ("expected executable to link binary-private helper B; objects: "
                ^ object_names binary_objects)
              else if not (has main_cmx binary_objects) then
                Error ("expected executable to link binary root object; objects: "
                ^ object_names binary_objects)
              else if has a_cmx binary_objects then
                Error ("did not expect library-owned module A to be linked privately into executable; objects: "
                ^ object_names binary_objects)
              else if not
                (
                  List.any
                    libraries
                    ~fn:(
                      Path.equal
                        Riot_model.Module_name.(from_string "splitdemo"
                        |> cmxa)
                    )
                ) then
                Error "expected executable to still link the package archive"
              else
                (
                  match binary_positions with
                  | (Some b_index, Some main_index) when b_index < main_index -> Ok ()
                  | (Some _, Some _) ->
                      Error ("expected binary-private helper B to appear before main object in executable link order; objects: "
                      ^ object_names binary_objects)
                  | _ ->
                      Error ("expected executable object list to contain both binary-private helper and main object; objects: "
                      ^ object_names binary_objects)
                )
          | (Some _, Some _, None, _, _) ->
              Error "expected compile action for binary-private helper b.ml"
          | (Some _, Some _, _, None, _) -> Error "expected compile action for library module a.ml"
          | (Some _, Some _, _, _, None) -> Error "expected compile action for binary root main.ml"
          | (Some _, Some _, _, _, _) -> Error "expected CreateLibrary and CreateExecutable actions"
          | _ -> Error "missing CreateLibrary or CreateExecutable action") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_binary_actions_follow_transitive_private_reachability = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_binary_transitive_private_modules"
    (fun tmpdir ->
      match plan_actions_for_package
        ~tmpdir
        ~package_name:"transitivedemo"
        ~library:{ path = Path.v "src/transitivedemo.ml" }
        ~binaries:[ ("transitivedemo", "src/main.ml"); ]
        ~files:[
          ("src/transitivedemo.ml", "let library_only = 0\n");
          ("src/c.ml", "let value = 41\n");
          ("src/b.ml", "let value = C.value + 1\n");
          ("src/main.ml", binary_main "ignore B.value");
          ("src/orphan.ml", "let orphan = 7\n");
        ]
        () with
      | Error _ as err -> err
      | Ok (_package, actions) ->
          let c_source = Path.v "src/c.ml" in
          let b_source = Path.v "src/b.ml" in
          let main_source = Path.v "src/main.ml" in
          let orphan_source = Path.v "src/orphan.ml" in
          let c_cmx = find_compile_cmx actions c_source in
          let b_cmx = find_compile_cmx actions b_source in
          let main_cmx = find_compile_cmx actions main_source in
          let compiles_orphan = List.any (compile_sources actions) ~fn:(Path.equal orphan_source) in
          match (
            find_create_library actions,
            find_create_executable actions,
            c_cmx,
            b_cmx,
            main_cmx
          ) with
          | (
              Some (Riot_planner.Action.CreateLibrary { objects = library_objects; _ }),
              Some (Riot_planner.Action.CreateExecutable { objects = binary_objects; _ }),
              Some c_cmx,
              Some b_cmx,
              Some main_cmx
            ) ->
              let has object_ objects = List.any objects ~fn:(Path.equal object_) in
              let object_names objects =
                List.map objects ~fn:Path.to_string
                |> String.concat ", "
              in
              if compiles_orphan then
                Error "did not expect unreachable orphan module to be compiled"
              else if has c_cmx library_objects || has b_cmx library_objects then
                Error ("did not expect transitive binary-private helper modules in library archive; objects: "
                ^ object_names library_objects)
              else if
                not
                  (has c_cmx binary_objects
                  && has b_cmx binary_objects
                  && has main_cmx binary_objects)
              then
                Error ("expected executable to link the full transitive private closure (c.ml -> b.ml -> main.ml); objects: "
                ^ object_names binary_objects)
              else
                (
                  match (
                    index_of_path binary_objects c_cmx,
                    index_of_path binary_objects b_cmx,
                    index_of_path binary_objects main_cmx
                  ) with
                  | (Some c_index, Some b_index, Some main_index) when c_index < b_index
                  && b_index < main_index -> Ok ()
                  | _ ->
                      Error ("expected transitive private helper objects to preserve dependency order C -> B -> Main; objects: "
                      ^ object_names binary_objects)
                )
          | _ ->
              Error "expected CreateLibrary/CreateExecutable actions and compile outputs for c.ml, b.ml, and main.ml") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_executable_actions_do_not_duplicate_library_owned_modules = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_binary_shared_module"
    (fun tmpdir ->
      match plan_actions_for_package
        ~tmpdir
        ~package_name:"shareddemo"
        ~library:{ path = Path.v "src/shareddemo.ml" }
        ~binaries:[ ("shareddemo", "src/main.ml"); ]
        ~files:[
          ("src/shareddemo.ml", "module Shared = Shared\n");
          ("src/shared.ml", "let value = 1\n");
          ("src/main.ml", binary_main "ignore Shareddemo.Shared.value");
        ]
        () with
      | Error _ as err -> err
      | Ok (_package, actions) ->
          let shared_source = Path.v "src/shared.ml" in
          let main_source = Path.v "src/main.ml" in
          let shared_cmx = find_compile_cmx actions shared_source in
          let main_cmx = find_compile_cmx actions main_source in
          match (find_create_library actions, find_create_executable actions, shared_cmx, main_cmx) with
          | (
              Some (Riot_planner.Action.CreateLibrary { objects = library_objects; _ }),
              Some (Riot_planner.Action.CreateExecutable { objects = binary_objects; _ }),
              Some shared_cmx,
              Some main_cmx
            ) ->
              let has object_ objects = List.any objects ~fn:(Path.equal object_) in
              let object_names objects =
                List.map objects ~fn:Path.to_string
                |> String.concat ", "
              in
              if not (has shared_cmx library_objects) then
                Error ("expected library archive to own Shared; objects: "
                ^ object_names library_objects)
              else if has shared_cmx binary_objects then
                Error ("did not expect library-owned Shared module to be linked privately into executable; objects: "
                ^ object_names binary_objects)
              else if not (has main_cmx binary_objects) then
                Error ("expected executable to link its root object; objects: "
                ^ object_names binary_objects)
              else
                Ok ()
          | _ ->
              Error "expected CreateLibrary/CreateExecutable actions and compile outputs for shared.ml and main.ml") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_executable_actions_allow_private_helpers_without_a_library = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_binary_no_library"
    (fun tmpdir ->
      match plan_actions_for_package
        ~tmpdir
        ~package_name:"standalone"
        ~binaries:[ ("standalone", "src/main.ml"); ]
        ~files:[
          ("src/helper.ml", "let value = 1\n");
          ("src/main.ml", binary_main "ignore Helper.value");
          ("src/orphan.ml", "let orphan = 0\n");
        ]
        () with
      | Error _ as err -> err
      | Ok (_package, actions) ->
          let helper_source = Path.v "src/helper.ml" in
          let main_source = Path.v "src/main.ml" in
          let orphan_source = Path.v "src/orphan.ml" in
          let helper_cmx = find_compile_cmx actions helper_source in
          let main_cmx = find_compile_cmx actions main_source in
          let compiles_orphan = List.any (compile_sources actions) ~fn:(Path.equal orphan_source) in
          match (find_create_library actions, find_create_executable actions, helper_cmx, main_cmx) with
          | (
              None,
              Some (
                Riot_planner.Action.CreateExecutable { objects = binary_objects; libraries; _ }
              ),
              Some helper_cmx,
              Some main_cmx
            ) ->
              let has object_ objects = List.any objects ~fn:(Path.equal object_) in
              let object_names objects =
                List.map objects ~fn:Path.to_string
                |> String.concat ", "
              in
              if compiles_orphan then
                Error "did not expect unreachable orphan module to be compiled in no-library package"
              else if not (has helper_cmx binary_objects && has main_cmx binary_objects) then
                Error ("expected executable to link helper and main objects in no-library package; objects: "
                ^ object_names binary_objects)
              else if not (List.is_empty libraries) then
                Error "did not expect no-library executable to link a package archive"
              else
                Ok ()
          | (Some _, _, _, _) -> Error "did not expect CreateLibrary action for no-library package"
          | _ ->
              Error "expected CreateExecutable action and compile outputs for helper.ml and main.ml") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_binary_actions_without_private_helpers = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_binary_without_private_helpers"
    (fun tmpdir ->
      match plan_actions_for_package
        ~tmpdir
        ~package_name:"nohelperdemo"
        ~library:{ path = Path.v "src/nohelperdemo.ml" }
        ~binaries:[ ("nohelperdemo", "src/main.ml"); ]
        ~files:[
          ("src/nohelperdemo.ml", "module Shared = Shared\n");
          ("src/shared.ml", "let value = 1\n");
          ("src/main.ml", binary_main "ignore Nohelperdemo.Shared.value");
        ]
        () with
      | Error _ as err -> err
      | Ok (_package, actions) ->
          let shared_source = Path.v "src/shared.ml" in
          let main_source = Path.v "src/main.ml" in
          let shared_cmx = find_compile_cmx actions shared_source in
          let main_cmx = find_compile_cmx actions main_source in
          match (
            find_create_library actions,
            find_create_executable_named actions "nohelperdemo",
            shared_cmx,
            main_cmx
          ) with
          | (
              Some (Riot_planner.Action.CreateLibrary { objects = library_objects; _ }),
              Some (
                Riot_planner.Action.CreateExecutable { objects = binary_objects; libraries; _ }
              ),
              Some shared_cmx,
              Some main_cmx
            ) ->
              let has object_ objects = List.any objects ~fn:(Path.equal object_) in
              let object_names objects =
                List.map objects ~fn:Path.to_string
                |> String.concat ", "
              in
              if not (has shared_cmx library_objects) then
                Error ("expected library archive to include Shared; objects: "
                ^ object_names library_objects)
              else if
                not Int.(List.length binary_objects = 1) || not (has main_cmx binary_objects)
              then
                Error ("expected executable without private helpers to link only main.cmx; objects: "
                ^ object_names binary_objects)
              else if not
                (
                  List.any
                    libraries
                    ~fn:(
                      Path.equal
                        Riot_model.Module_name.(from_string "nohelperdemo"
                        |> cmxa)
                    )
                ) then
                Error "expected executable to link the package archive"
              else
                Ok ()
          | _ ->
              Error "expected CreateLibrary/CreateExecutable actions and compile outputs for shared.ml and main.ml") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_binary_actions_include_multiple_private_helpers = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_binary_multiple_private_helpers"
    (fun tmpdir ->
      match plan_actions_for_package
        ~tmpdir
        ~package_name:"multidemo"
        ~library:{ path = Path.v "src/multidemo.ml" }
        ~binaries:[ ("multidemo", "src/main.ml"); ]
        ~files:[
          ("src/multidemo.ml", "let library_value = 1\n");
          ("src/a.ml", "let value = 10\n");
          ("src/b.ml", "let value = 20\n");
          ("src/main.ml", binary_main "ignore (A.value + B.value)");
        ]
        () with
      | Error _ as err -> err
      | Ok (_package, actions) ->
          let a_source = Path.v "src/a.ml" in
          let b_source = Path.v "src/b.ml" in
          let main_source = Path.v "src/main.ml" in
          let a_cmx = find_compile_cmx actions a_source in
          let b_cmx = find_compile_cmx actions b_source in
          let main_cmx = find_compile_cmx actions main_source in
          match (
            find_create_library actions,
            find_create_executable_named actions "multidemo",
            a_cmx,
            b_cmx,
            main_cmx
          ) with
          | (
              Some (Riot_planner.Action.CreateLibrary { objects = library_objects; _ }),
              Some (Riot_planner.Action.CreateExecutable { objects = binary_objects; _ }),
              Some a_cmx,
              Some b_cmx,
              Some main_cmx
            ) ->
              let has object_ objects = List.any objects ~fn:(Path.equal object_) in
              let object_names objects =
                List.map objects ~fn:Path.to_string
                |> String.concat ", "
              in
              if has a_cmx library_objects || has b_cmx library_objects then
                Error ("did not expect private fan-out helpers in library archive; objects: "
                ^ object_names library_objects)
              else if
                not
                  (has a_cmx binary_objects
                  && has b_cmx binary_objects
                  && has main_cmx binary_objects)
              then
                Error ("expected executable to link both private helpers and main object; objects: "
                ^ object_names binary_objects)
              else
                Ok ()
          | _ ->
              Error "expected CreateLibrary/CreateExecutable actions and compile outputs for a.ml, b.ml, and main.ml") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_multiple_binaries_share_private_helper = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_multiple_binaries_shared_private_helper"
    (fun tmpdir ->
      match plan_actions_for_package
        ~tmpdir
        ~package_name:"sharedhelperdemo"
        ~binaries:[ ("main", "src/main.ml"); ("tool", "src/tool.ml"); ]
        ~files:[
          ("src/shared.ml", "let value = 1\n");
          ("src/main.ml", binary_main "ignore Shared.value");
          ("src/tool.ml", binary_main "ignore Shared.value");
        ]
        () with
      | Error _ as err -> err
      | Ok (_package, actions) ->
          let shared_source = Path.v "src/shared.ml" in
          let main_source = Path.v "src/main.ml" in
          let tool_source = Path.v "src/tool.ml" in
          let shared_cmx = find_compile_cmx actions shared_source in
          let main_cmx = find_compile_cmx actions main_source in
          let tool_cmx = find_compile_cmx actions tool_source in
          match (
            find_create_library actions,
            find_create_executable_named actions "main",
            find_create_executable_named actions "tool",
            shared_cmx,
            main_cmx,
            tool_cmx
          ) with
          | (
              None,
              Some (Riot_planner.Action.CreateExecutable { objects = main_objects; _ }),
              Some (Riot_planner.Action.CreateExecutable { objects = tool_objects; _ }),
              Some shared_cmx,
              Some main_cmx,
              Some tool_cmx
            ) ->
              let has object_ objects = List.any objects ~fn:(Path.equal object_) in
              let object_names objects =
                List.map objects ~fn:Path.to_string
                |> String.concat ", "
              in
              if not (has shared_cmx main_objects && has main_cmx main_objects) then
                Error ("expected main executable to link shared helper and main root; objects: "
                ^ object_names main_objects)
              else if not (has shared_cmx tool_objects && has tool_cmx tool_objects) then
                Error ("expected tool executable to link shared helper and tool root; objects: "
                ^ object_names tool_objects)
              else
                Ok ()
          | (Some _, _, _, _, _, _) ->
              Error "did not expect CreateLibrary action for no-library multi-binary package"
          | _ ->
              Error "expected CreateExecutable actions and compile outputs for shared.ml, main.ml, and tool.ml") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_multiple_binaries_keep_private_helpers_separate = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_multiple_binaries_disjoint_private_helpers"
    (fun tmpdir ->
      match plan_actions_for_package
        ~tmpdir
        ~package_name:"disjointdemo"
        ~binaries:[ ("main", "src/main.ml"); ("tool", "src/tool.ml"); ]
        ~files:[
          ("src/a.ml", "let value = 1\n");
          ("src/b.ml", "let value = 2\n");
          ("src/main.ml", binary_main "ignore A.value");
          ("src/tool.ml", binary_main "ignore B.value");
        ]
        () with
      | Error _ as err -> err
      | Ok (_package, actions) ->
          let a_source = Path.v "src/a.ml" in
          let b_source = Path.v "src/b.ml" in
          let main_source = Path.v "src/main.ml" in
          let tool_source = Path.v "src/tool.ml" in
          let a_cmx = find_compile_cmx actions a_source in
          let b_cmx = find_compile_cmx actions b_source in
          let main_cmx = find_compile_cmx actions main_source in
          let tool_cmx = find_compile_cmx actions tool_source in
          match (
            find_create_executable_named actions "main",
            find_create_executable_named actions "tool",
            a_cmx,
            b_cmx,
            main_cmx,
            tool_cmx
          ) with
          | (
              Some (Riot_planner.Action.CreateExecutable { objects = main_objects; _ }),
              Some (Riot_planner.Action.CreateExecutable { objects = tool_objects; _ }),
              Some a_cmx,
              Some b_cmx,
              Some main_cmx,
              Some tool_cmx
            ) ->
              let has object_ objects = List.any objects ~fn:(Path.equal object_) in
              let object_names objects =
                List.map objects ~fn:Path.to_string
                |> String.concat ", "
              in
              if not (has a_cmx main_objects && has main_cmx main_objects) then
                Error ("expected main executable to link A and main root; objects: "
                ^ object_names main_objects)
              else if has b_cmx main_objects then
                Error ("did not expect main executable to link tool-private helper B; objects: "
                ^ object_names main_objects)
              else if not (has b_cmx tool_objects && has tool_cmx tool_objects) then
                Error ("expected tool executable to link B and tool root; objects: "
                ^ object_names tool_objects)
              else if has a_cmx tool_objects then
                Error ("did not expect tool executable to link main-private helper A; objects: "
                ^ object_names tool_objects)
              else
                Ok ()
          | _ ->
              Error "expected CreateExecutable actions and compile outputs for a.ml, b.ml, main.ml, and tool.ml") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_binary_only_package_links_package_named_private_helper = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_binary_only_package_named_helper"
    (fun tmpdir ->
      match plan_actions_for_package
        ~tmpdir
        ~package_name:"hello-world"
        ~binaries:[ ("hello-world", "src/main.ml"); ]
        ~files:[
          ("src/hello_world.ml", "let hello = fun () -> \"Hello from hello-world\"\n");
          ("src/main.ml", binary_main "print_endline (Hello_world.hello ())");
        ]
        () with
      | Error _ as err -> err
      | Ok (_package, actions) ->
          let helper_source = Path.v "src/hello_world.ml" in
          let main_source = Path.v "src/main.ml" in
          let helper_cmx = find_compile_cmx actions helper_source in
          let main_cmx = find_compile_cmx actions main_source in
          match (
            find_create_library actions,
            find_create_executable_named actions "hello-world",
            helper_cmx,
            main_cmx
          ) with
          | (
              None,
              Some (Riot_planner.Action.CreateExecutable { objects; _ }),
              Some helper_cmx,
              Some main_cmx
            ) ->
              let has object_ objects = List.any objects ~fn:(Path.equal object_) in
              let object_names objects =
                List.map objects ~fn:Path.to_string
                |> String.concat ", "
              in
              if not (has helper_cmx objects && has main_cmx objects) then
                Error ("expected executable to link package-named helper and main root; objects: "
                ^ object_names objects)
              else
                Ok ()
          | (Some _, _, _, _) -> Error "did not expect CreateLibrary action for binary-only package"
          | _ -> Error "expected executable and compile outputs for hello_world.ml and main.ml") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_private_helper_can_depend_on_library_owned_module = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_private_helper_depends_on_library_module"
    (fun tmpdir ->
      match plan_actions_for_package
        ~tmpdir
        ~package_name:"librarymixdemo"
        ~library:{ path = Path.v "src/librarymixdemo.ml" }
        ~binaries:[ ("librarymixdemo", "src/main.ml"); ]
        ~files:[
          ("src/librarymixdemo.ml", "module A = A\n");
          ("src/a.ml", "let value = 10\n");
          ("src/b.ml", "let value = Librarymixdemo.A.value + 20\n");
          ("src/main.ml", binary_main "ignore B.value");
        ]
        () with
      | Error _ as err -> err
      | Ok (_package, actions) ->
          let a_source = Path.v "src/a.ml" in
          let b_source = Path.v "src/b.ml" in
          let main_source = Path.v "src/main.ml" in
          let a_cmx = find_compile_cmx actions a_source in
          let b_cmx = find_compile_cmx actions b_source in
          let main_cmx = find_compile_cmx actions main_source in
          match (
            find_create_library actions,
            find_create_executable_named actions "librarymixdemo",
            a_cmx,
            b_cmx,
            main_cmx
          ) with
          | (
              Some (Riot_planner.Action.CreateLibrary { objects = library_objects; _ }),
              Some (
                Riot_planner.Action.CreateExecutable { objects = binary_objects; libraries; _ }
              ),
              Some a_cmx,
              Some b_cmx,
              Some main_cmx
            ) ->
              let has object_ objects = List.any objects ~fn:(Path.equal object_) in
              let object_names objects =
                List.map objects ~fn:Path.to_string
                |> String.concat ", "
              in
              if not (has a_cmx library_objects) then
                Error ("expected library archive to include A; objects: "
                ^ object_names library_objects)
              else if has b_cmx library_objects then
                Error ("did not expect binary-private helper B in library archive; objects: "
                ^ object_names library_objects)
              else if not (has b_cmx binary_objects && has main_cmx binary_objects) then
                Error ("expected executable to link helper B and main object; objects: "
                ^ object_names binary_objects)
              else if has a_cmx binary_objects then
                Error ("did not expect executable to duplicate library-owned A privately; objects: "
                ^ object_names binary_objects)
              else if not
                (
                  List.any
                    libraries
                    ~fn:(
                      Path.equal
                        Riot_model.Module_name.(from_string "librarymixdemo"
                        |> cmxa)
                    )
                ) then
                Error "expected executable to link the package archive"
              else
                Ok ()
          | _ ->
              Error "expected CreateLibrary/CreateExecutable actions and compile outputs for a.ml, b.ml, and main.ml") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_private_helper_links_only_into_reaching_binary = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_private_helper_links_only_into_reaching_binary"
    (fun tmpdir ->
      match plan_actions_for_package
        ~tmpdir
        ~package_name:"selectivedemo"
        ~binaries:[ ("main", "src/main.ml"); ("tool", "src/tool.ml"); ]
        ~files:[
          ("src/shared.ml", "let value = 1\n");
          ("src/main.ml", binary_main "ignore Shared.value");
          ("src/tool.ml", binary_main "()");
        ]
        () with
      | Error _ as err -> err
      | Ok (_package, actions) ->
          let shared_source = Path.v "src/shared.ml" in
          let main_source = Path.v "src/main.ml" in
          let tool_source = Path.v "src/tool.ml" in
          let shared_cmx = find_compile_cmx actions shared_source in
          let main_cmx = find_compile_cmx actions main_source in
          let tool_cmx = find_compile_cmx actions tool_source in
          match (
            find_create_executable_named actions "main",
            find_create_executable_named actions "tool",
            shared_cmx,
            main_cmx,
            tool_cmx
          ) with
          | (
              Some (Riot_planner.Action.CreateExecutable { objects = main_objects; _ }),
              Some (Riot_planner.Action.CreateExecutable { objects = tool_objects; _ }),
              Some shared_cmx,
              Some main_cmx,
              Some tool_cmx
            ) ->
              let has object_ objects = List.any objects ~fn:(Path.equal object_) in
              let object_names objects =
                List.map objects ~fn:Path.to_string
                |> String.concat ", "
              in
              if not (has shared_cmx main_objects && has main_cmx main_objects) then
                Error ("expected reaching binary to link shared helper and main root; objects: "
                ^ object_names main_objects)
              else if has shared_cmx tool_objects then
                Error ("did not expect non-reaching binary to link shared helper privately; objects: "
                ^ object_names tool_objects)
              else if not (has tool_cmx tool_objects) then
                Error ("expected tool executable to link its root object; objects: "
                ^ object_names tool_objects)
              else
                Ok ()
          | _ ->
              Error "expected CreateExecutable actions and compile outputs for shared.ml, main.ml, and tool.ml") with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_binary_compile_depends_on_public_library_root_not_internal_modules = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_binary_public_root_dependency"
    (fun tmpdir ->
      match plan_action_graph_for_package
        ~tmpdir
        ~package_name:"berrybot"
        ~library:{ path = Path.v "src/berrybot.ml" }
        ~binaries:[ ("berrybot", "src/main.ml"); ]
        ~files:[
          ("src/berrybot.ml", "module A = A\n");
          ("src/a.ml", "let value = 10\n");
          ("src/main.ml", binary_main "ignore Berrybot.A.value");
        ]
        () with
      | Error _ as err -> err
      | Ok (_package, action_graph) -> (
          match find_compile_action_node_by_source action_graph (Path.v "src/main.ml") with
          | None -> Error "expected compile action for main.ml"
          | Some main_node ->
              let dep_outputs = dependency_output_names action_graph main_node in
              if List.any dep_outputs ~fn:(String.equal "A.cmt") then
                Error ("did not expect main.ml to depend directly on internal module A; deps: ["
                ^ String.concat ", " dep_outputs
                ^ "]")
              else if List.any dep_outputs ~fn:(String.equal "Berrybot.cmt") then
                Ok ()
              else
                Error ("expected main.ml to depend on the public Berrybot module; deps: ["
                ^ String.concat ", " dep_outputs
                ^ "]")
        )) with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let test_binary_compile_does_not_reach_internal_library_modules_directly = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"planner_binary_internal_library_dependency"
    (fun tmpdir ->
      match plan_action_graph_for_package
        ~tmpdir
        ~package_name:"berrybot"
        ~library:{ path = Path.v "src/berrybot.ml" }
        ~binaries:[ ("berrybot", "src/main.ml"); ]
        ~files:[
          ("src/berrybot.ml", "module A = A\n");
          ("src/a.ml", "let value = 10\n");
          ("src/main.ml", binary_main "ignore A.value");
        ]
        () with
      | Error _ as err -> err
      | Ok (_package, action_graph) -> (
          match find_compile_action_node_by_source action_graph (Path.v "src/main.ml") with
          | None -> Error "expected compile action for main.ml"
          | Some main_node ->
              let dep_outputs = dependency_output_names action_graph main_node in
              if List.any dep_outputs ~fn:(String.equal "A.cmt") then
                Error ("did not expect main.ml to depend directly on internal module A; deps: ["
                ^ String.concat ", " dep_outputs
                ^ "]")
              else
                Ok ()
        )) with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let tests =
  Test.[
    case
      "action graph json round-trip preserves edges"
      test_action_graph_json_round_trip_preserves_dependencies;
    case
      "action graph json round-trip preserves package paths and hashes"
      test_action_graph_json_round_trip_preserves_package_paths_and_hashes;
    case
      "action graph json round-trip preserves dependency order"
      test_action_graph_json_round_trip_preserves_dependency_order;
    case
      "action hash tracks package-relative source contents"
      test_action_hash_tracks_package_relative_source_contents;
    case
      "library builds skip shared native plugin artifacts by default"
      test_library_builds_do_not_emit_shared_library_actions;
    case
      "library actions exclude ML object files while keeping native stubs"
      test_library_actions_exclude_ml_object_files;
    case
      "library actions exclude unreachable modules"
      test_library_actions_exclude_unreachable_modules;
    case
      "generated library interfaces depend on child module interfaces"
      test_generated_library_interface_depends_on_child_module_interfaces;
    case
      "generated library interfaces with multiple children depend on child module interfaces"
      test_generated_library_interface_with_multiple_children_depends_on_child_module_interfaces;
    case
      "nested generated library interfaces depend on public child modules"
      test_nested_generated_library_interface_depends_on_public_child_modules;
    case
      "nested concrete library implementations keep alias child dependencies"
      test_nested_concrete_library_implementation_keeps_alias_child_dependency;
    case
      "nested concrete library implementations keep generated child root dependencies"
      test_nested_concrete_library_implementation_keeps_generated_child_root_dependency;
    case
      ~size:Large
      "real kernel unix addr interface keeps sibling modules"
      test_real_kernel_unix_addr_interface_keeps_sibling_modules;
    case
      "binary actions without private helpers stay thin"
      test_binary_actions_without_private_helpers;
    case
      "binary actions include target-private modules"
      test_binary_actions_include_target_private_modules;
    case
      "binary actions include multiple private helpers"
      test_binary_actions_include_multiple_private_helpers;
    case
      "binary actions follow transitive private reachability"
      test_binary_actions_follow_transitive_private_reachability;
    case
      "binary compile depends on the public library root"
      test_binary_compile_depends_on_public_library_root_not_internal_modules;
    case
      "binary compile does not reach internal library modules directly"
      test_binary_compile_does_not_reach_internal_library_modules_directly;
    case "multiple binaries can share a private helper" test_multiple_binaries_share_private_helper;
    case
      "multiple binaries keep private helpers separate"
      test_multiple_binaries_keep_private_helpers_separate;
    case
      "binary-only package links package-named private helper"
      test_binary_only_package_links_package_named_private_helper;
    case
      "private helper can depend on library-owned module"
      test_private_helper_can_depend_on_library_owned_module;
    case
      "private helper only links into the binary that reaches it"
      test_private_helper_links_only_into_reaching_binary;
    case
      "executable actions do not duplicate library-owned modules"
      test_executable_actions_do_not_duplicate_library_owned_modules;
    case
      "executable actions allow private helpers without a library"
      test_executable_actions_allow_private_helpers_without_a_library;
    case
      "CreateLibrary preserves module dependency order"
      test_create_library_preserves_module_dependency_order;
    case
      "release profile flags flow into compile actions"
      test_release_profile_flags_flow_into_compile_actions;
  ]

let name = "Planner Action Graph Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()

open Std

module Test = Std.Test

let test_toolchain =
  Tusk_toolchain.init ~config:Tusk_model.Toolchain_config.default
  |> Result.expect ~msg:"Failed to initialize toolchain"

let make_package name =
  Tusk_model.Package.
    {
      name;
      path = Path.v ".";
      relative_path = Path.v ".";
      dependencies = [];
      dev_dependencies = [];
      build_dependencies = [];
      foreign_dependencies = [];
      binaries = [];
      library = None;
      sources = { src = []; native = []; tests = []; examples = []; bench = [] };
      compiler = { profile_overrides = []; target_overrides = [] };
      commands = [];
      fix_providers = [];
    }

let make_package_with_paths ~name ~path ~relative_path =
  Tusk_model.Package.
    {
      name;
      path;
      relative_path;
      dependencies = [];
      dev_dependencies = [];
      build_dependencies = [];
      foreign_dependencies = [];
      binaries = [];
      library = None;
      sources = { src = []; native = []; tests = []; examples = []; bench = [] };
      compiler = { profile_overrides = []; target_overrides = [] };
      commands = [];
      fix_providers = [];
    }

let test_action_graph_json_round_trip_preserves_dependencies () =
  let package = make_package "pkg" in
  let graph = Tusk_planner.Action_graph.create () in

  let write_a =
    Tusk_planner.Action.WriteFile
      { destination = Path.v "a.txt"; content = "a" }
  in
  let spec_a =
    Tusk_planner.Action_node.make ~actions:[ write_a ] ~outs:[ Path.v "a.txt" ]
      ~srcs:[] ~package ~toolchain:test_toolchain
      ~dependency_hashes:(fun _ -> Crypto.hash_string "") ~deps:[]
  in
  let node_a = Tusk_planner.Action_graph.add_node graph spec_a in

  let write_b =
    Tusk_planner.Action.WriteFile
      { destination = Path.v "b.txt"; content = "b" }
  in
  let spec_b =
    Tusk_planner.Action_node.make ~actions:[ write_b ] ~outs:[ Path.v "b.txt" ]
      ~srcs:[] ~package ~toolchain:test_toolchain
      ~dependency_hashes:(fun dep_id ->
        if Graph.SimpleGraph.Node_id.eq dep_id node_a.id then
          Tusk_planner.Action_node.get_hash node_a
        else Crypto.hash_string "missing")
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
              0 node_jsons
          in
          if List.length node_jsons = 2 && edge_count = 1 then Ok ()
          else
            Error
              ("expected 2 nodes and 1 edge, got "
             ^ Int.to_string (List.length node_jsons)
             ^ " nodes and " ^ Int.to_string edge_count ^ " edges")
      | _ -> Error "decoded graph missing nodes array")

let test_action_graph_json_round_trip_preserves_package_paths_and_hashes () =
  let package =
    make_package_with_paths ~name:"kernel" ~path:(Path.v "packages/kernel")
      ~relative_path:(Path.v "packages/kernel")
  in
  let graph = Tusk_planner.Action_graph.create () in
  let action =
    Tusk_planner.Action.WriteFile
      { destination = Path.v "build/meta.txt"; content = "ok" }
  in
  let spec =
    Tusk_planner.Action_node.make ~actions:[ action ]
      ~outs:[ Path.v "build/meta.txt" ]
      ~srcs:[ Path.v "packages/kernel/src/lib.ml" ] ~package
      ~toolchain:test_toolchain
      ~dependency_hashes:(fun _ -> Crypto.hash_string "") ~deps:[]
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
          let decoded_rel =
            decoded_node.value.package.Tusk_model.Package.relative_path
          in
          let decoded_hash = Tusk_planner.Action_node.get_hash decoded_node in
          if
            Path.equal decoded_path (Path.v "packages/kernel")
            && Path.equal decoded_rel (Path.v "packages/kernel")
            && Crypto.Hash.equal decoded_hash expected_hash
          then Ok ()
          else Error "package path/relative_path/hash did not round-trip"
      | _ -> Error "expected one decoded node")

let tests =
  Test.
    [
      case "action graph json round-trip preserves edges"
        test_action_graph_json_round_trip_preserves_dependencies;
      case "action graph json round-trip preserves package paths and hashes"
        test_action_graph_json_round_trip_preserves_package_paths_and_hashes;
    ]

let name = "Planner Action Graph Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()

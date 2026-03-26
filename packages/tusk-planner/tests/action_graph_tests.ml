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
  | Ok decoded ->
      let nodes = Tusk_planner.Action_graph.nodes decoded in
      let edge_count =
        List.fold_left (fun acc node -> acc + List.length node.deps) 0 nodes
      in
      if List.length nodes = 2 && edge_count = 1 then Ok ()
      else
        Error
          ("expected 2 nodes and 1 edge, got " ^ Int.to_string (List.length nodes)
         ^ " nodes and " ^ Int.to_string edge_count ^ " edges")

let tests =
  Test.
    [
      case "action graph json round-trip preserves edges"
        test_action_graph_json_round_trip_preserves_dependencies;
    ]

let name = "Planner Action Graph Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()

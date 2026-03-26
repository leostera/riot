open Std

module Test = Std.Test
module Package_graph = Tusk_planner.Package_graph
module Package = Tusk_model.Package
module Workspace = Tusk_model.Workspace

let dependency name = Package.{ name; source = Workspace }

let make_package ?(dependencies = []) ?(dev_dependencies = [])
    ?(build_dependencies = []) name =
  Package.
    {
      name;
      path = Path.v ("packages/" ^ name);
      relative_path = Path.v ("packages/" ^ name);
      dependencies = List.map dependency dependencies;
      dev_dependencies = List.map dependency dev_dependencies;
      build_dependencies = List.map dependency build_dependencies;
      foreign_dependencies = [];
      binaries = [];
      library = Some { path = Path.v "src/lib.ml" };
      sources = { src = []; native = []; tests = []; examples = []; bench = [] };
      compiler = { profile_overrides = []; target_overrides = [] };
      commands = [];
      fix_providers = [];
    }

let make_workspace packages =
  Workspace.
    {
      root = Path.v "/tmp/workspace_like_graph_tests";
      target_dir_root = Path.v "/tmp/workspace_like_graph_tests/_build";
      packages;
      profile_overrides = [];
    }

let node_for graph package_name scope =
  Package_graph.package_key ~package_name scope
  |> Package_graph.get_node_by_key graph
  |> Option.expect ~msg:("missing node: " ^ package_name)

let dependency_keys_for_node graph node =
  Package_graph.get_dependencies_for_node graph node
  |> List.map Package_graph.get_key
  |> List.sort String.compare

let package_keys_for_scope scope names =
  names
  |> List.map (fun name -> Package_graph.package_key ~package_name:name scope)
  |> List.sort String.compare

let assert_same_keys ~expected ~actual =
  Test.assert_equal ~expected:(List.sort String.compare expected)
    ~actual:(List.sort String.compare actual)

let runtime_scope_wires_workspace_like_graph () =
  let packages =
    [
      make_package "std";
      make_package ~dependencies:[ "std" ] "kernel";
      make_package ~dependencies:[ "std"; "kernel" ] "miniriot";
      make_package ~dependencies:[ "std"; "miniriot" ] "tusk-model";
      make_package ~dependencies:[ "std"; "tusk-model" ] "tusk-planner";
      make_package ~dependencies:[ "std"; "tusk-model"; "tusk-planner" ]
        "tusk-executor";
      make_package ~dependencies:[ "std"; "tusk-model"; "tusk-planner" ]
        "tusk-store";
      make_package ~dependencies:[ "std"; "tusk-model"; "tusk-planner"; "tusk-executor"; "tusk-store" ]
        "tusk-server";
    ]
  in
  let workspace = make_workspace packages in
  let graph =
    Package_graph.create ~scope:Runtime workspace
    |> Result.expect ~msg:"expected runtime graph"
  in
  Test.assert_equal ~expected:(List.length packages * 2)
    ~actual:(Package_graph.size graph);
  let server_runtime = node_for graph "tusk-server" Runtime in
  assert_same_keys
    ~expected:
      (package_keys_for_scope Runtime
         [ "std"; "tusk-model"; "tusk-planner"; "tusk-executor"; "tusk-store" ]
      @ package_keys_for_scope Build [ "tusk-server" ])
    ~actual:(dependency_keys_for_node graph server_runtime);
  Ok ()

let dev_scope_does_not_inherit_build_only_dependencies () =
  let packages =
    [
      make_package "std";
      make_package "codegen";
      make_package "runtime-lib";
      make_package "propane";
      make_package ~dependencies:[ "runtime-lib" ]
        ~dev_dependencies:[ "propane" ]
        ~build_dependencies:[ "codegen" ] "app";
    ]
  in
  let workspace = make_workspace packages in
  let graph =
    Package_graph.create ~scope:Dev workspace
    |> Result.expect ~msg:"expected dev graph"
  in
  let app_dev = node_for graph "app" Dev in
  assert_same_keys
    ~expected:(package_keys_for_scope Runtime [ "propane"; "app" ])
    ~actual:(dependency_keys_for_node graph app_dev);
  Ok ()

let missing_workspace_dependencies_are_reported () =
  let packages =
    [
      make_package ~dependencies:[ "missing_a" ] "left";
      make_package ~dependencies:[ "missing_b" ] "right";
    ]
  in
  let workspace = make_workspace packages in
  match Package_graph.create ~scope:Runtime workspace with
  | Ok _ -> Error "expected missing dependency error"
  | Error (Package_graph.MissingPackages { missing }) ->
      let missing_pairs =
        List.map
          (fun (item : Package_graph.missing_dependency) ->
            item.package ^ "->" ^ item.dependency)
          missing
        |> List.sort String.compare
      in
      Test.assert_equal ~expected:[ "left->missing_a"; "right->missing_b" ]
        ~actual:missing_pairs;
      Ok ()

let filter_for_package_keeps_only_transitive_dependencies () =
  let packages =
    [
      make_package "std";
      make_package ~dependencies:[ "std" ] "kernel";
      make_package ~dependencies:[ "kernel" ] "a";
      make_package ~dependencies:[ "a" ] "app";
      make_package ~dependencies:[ "std" ] "unrelated";
    ]
  in
  let workspace = make_workspace packages in
  let graph =
    Package_graph.create ~scope:Runtime workspace
    |> Result.expect ~msg:"expected runtime graph"
  in
  let filtered = Package_graph.filter_for_package graph "app" in
  let package_names =
    Package_graph.packages filtered
    |> List.map (fun (pkg : Package.t) -> pkg.name)
    |> List.sort_uniq String.compare
  in
  Test.assert_equal ~expected:[ "a"; "app"; "kernel"; "std" ]
    ~actual:package_names;
  Ok ()

let topological_sort_places_dependencies_before_dependents () =
  let packages =
    [
      make_package "std";
      make_package ~dependencies:[ "std" ] "kernel";
      make_package ~dependencies:[ "kernel" ] "miniriot";
      make_package ~dependencies:[ "miniriot" ] "app";
    ]
  in
  let workspace = make_workspace packages in
  let graph =
    Package_graph.create ~scope:Runtime workspace
    |> Result.expect ~msg:"expected runtime graph"
  in
  let sorted =
    Package_graph.topological_sort graph
    |> List.map Package_graph.get_key
  in
  let position_of key =
    List.find_index (String.equal key) sorted
    |> Option.expect ~msg:("missing key in topo sort: " ^ key)
  in
  let std_runtime = Package_graph.package_key ~package_name:"std" Runtime in
  let kernel_runtime =
    Package_graph.package_key ~package_name:"kernel" Runtime
  in
  let miniriot_runtime =
    Package_graph.package_key ~package_name:"miniriot" Runtime
  in
  let app_runtime = Package_graph.package_key ~package_name:"app" Runtime in
  Test.assert_true (position_of std_runtime < position_of kernel_runtime);
  Test.assert_true (position_of kernel_runtime < position_of miniriot_runtime);
  Test.assert_true (position_of miniriot_runtime < position_of app_runtime);
  Ok ()

let runtime_nodes_depend_on_their_own_build_nodes () =
  let packages =
    [ make_package "std"; make_package ~dependencies:[ "std" ] "app" ]
  in
  let workspace = make_workspace packages in
  let graph =
    Package_graph.create ~scope:Runtime workspace
    |> Result.expect ~msg:"expected runtime graph"
  in
  let app_runtime = node_for graph "app" Runtime in
  let dependency_keys = dependency_keys_for_node graph app_runtime in
  Test.assert_true
    (List.exists
       (String.equal
          (Package_graph.package_key ~package_name:"app" Build))
       dependency_keys);
  Ok ()

let scope_node_counts_match_expected_projection () =
  let packages =
    [
      make_package "std";
      make_package ~dependencies:[ "std" ] "kernel";
      make_package ~dependencies:[ "kernel" ] "app";
    ]
  in
  let workspace = make_workspace packages in
  let build_graph =
    Package_graph.create ~scope:Build workspace
    |> Result.expect ~msg:"expected build graph"
  in
  let runtime_graph =
    Package_graph.create ~scope:Runtime workspace
    |> Result.expect ~msg:"expected runtime graph"
  in
  let dev_graph =
    Package_graph.create ~scope:Dev workspace
    |> Result.expect ~msg:"expected dev graph"
  in
  Test.assert_equal ~expected:3 ~actual:(Package_graph.size build_graph);
  Test.assert_equal ~expected:6 ~actual:(Package_graph.size runtime_graph);
  Test.assert_equal ~expected:9 ~actual:(Package_graph.size dev_graph);
  Ok ()

let filter_for_unknown_package_returns_empty_graph () =
  let packages = [ make_package "std"; make_package ~dependencies:[ "std" ] "app" ] in
  let workspace = make_workspace packages in
  let graph =
    Package_graph.create ~scope:Runtime workspace
    |> Result.expect ~msg:"expected runtime graph"
  in
  let filtered = Package_graph.filter_for_package graph "does-not-exist" in
  Test.assert_equal ~expected:0 ~actual:(Package_graph.size filtered);
  Ok ()

let get_unplanned_dependencies_only_reports_unplanned_runtime_dependencies () =
  let std = make_package "std" in
  let kernel = make_package ~dependencies:[ "std" ] "kernel" in
  let app = make_package ~dependencies:[ "kernel" ] "app" in
  let workspace = make_workspace [ std; kernel; app ] in
  let graph =
    Package_graph.create ~scope:Runtime workspace
    |> Result.expect ~msg:"expected runtime graph"
  in
  let std_runtime_key = Package_graph.package_key ~package_name:"std" Runtime in
  Package_graph.mark_planned graph std_runtime_key
    ~module_graph:(Graph.SimpleGraph.make ()) ~action_graph:(Tusk_planner.Action_graph.create ())
    ~hash:(Crypto.hash_string "std-runtime");
  let unplanned = Package_graph.get_unplanned_dependencies graph app in
  Test.assert_equal ~expected:[ "kernel" ]
    ~actual:(List.map (fun (pkg : Package.t) -> pkg.name) unplanned);
  Ok ()

let build_scope_wires_declared_build_dependencies () =
  let packages = [ make_package "codegen"; make_package ~build_dependencies:[ "codegen" ] "app" ] in
  let workspace = make_workspace packages in
  let graph =
    Package_graph.create ~scope:Build workspace
    |> Result.expect ~msg:"expected build graph"
  in
  let app_build = node_for graph "app" Build in
  let deps = dependency_keys_for_node graph app_build in
  Test.assert_true
    (List.exists
       (String.equal
          (Package_graph.package_key ~package_name:"codegen" Runtime))
       deps);
  Ok ()

let tests =
  Test.
    [
      case "runtime scope wires workspace-like graph"
        runtime_scope_wires_workspace_like_graph;
      case "dev scope does not inherit build-only dependencies"
        dev_scope_does_not_inherit_build_only_dependencies;
      case "missing workspace dependencies are reported"
        missing_workspace_dependencies_are_reported;
      case "filter_for_package keeps only transitive dependencies"
        filter_for_package_keeps_only_transitive_dependencies;
      case "topological sort places dependencies before dependents"
        topological_sort_places_dependencies_before_dependents;
      case "runtime nodes depend on their own build nodes"
        runtime_nodes_depend_on_their_own_build_nodes;
      case "scope node counts match expected projection"
        scope_node_counts_match_expected_projection;
      case "filter_for_unknown_package returns empty graph"
        filter_for_unknown_package_returns_empty_graph;
      case "get_unplanned_dependencies only reports unplanned runtime dependencies"
        get_unplanned_dependencies_only_reports_unplanned_runtime_dependencies;
      case "build scope wires declared build dependencies"
        build_scope_wires_declared_build_dependencies;
    ]

let name = "tusk-planner:workspace-like-graph"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()

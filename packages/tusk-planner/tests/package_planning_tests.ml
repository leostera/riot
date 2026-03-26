open Std

module Test = Std.Test
module G = Graph.SimpleGraph

let test_toolchain =
  Tusk_toolchain.init ~config:Tusk_model.Toolchain_config.default
  |> Result.expect ~msg:"Failed to initialize toolchain"

let make_test_workspace tmpdir packages =
  Tusk_model.Workspace.
    {
      root = tmpdir;
      target_dir_root = Path.(tmpdir / Path.v "target");
      packages;
      profile_overrides = [];
    }

let make_package tmpdir name =
  let pkg_dir = Path.(tmpdir / Path.v name) in
  let _ = Fs.create_dir_all pkg_dir in
  Tusk_model.Package.
    {
      name;
      path = pkg_dir;
      relative_path = Path.v name;
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

let compute_input_hash ~package ~workspace ~profile ~build_ctx =
  let module H = Std.Crypto.Sha256 in
  let state = H.create () in
  H.write_string state "planner-artifacts:v2";
  Tusk_model.Build_ctx.hash state build_ctx;
  Tusk_model.Package.hash state package;
  let sorted_deps =
    List.sort
      (fun (a : Tusk_model.Package.dependency) (b : Tusk_model.Package.dependency) ->
        String.compare a.name b.name)
      (Tusk_model.Package.build_graph_dependencies package)
  in
  List.iter
    (fun (dep : Tusk_model.Package.dependency) ->
      match dep.source with
      | Tusk_model.Package.Workspace -> (
          match
            List.find_opt
              (fun (p : Tusk_model.Package.t) -> p.name = dep.name)
              workspace.Tusk_model.Workspace.packages
          with
          | Some dep_pkg ->
              H.write_string state (Path.to_string dep_pkg.path);
              H.write_string state
                (if Option.is_some dep_pkg.library then "true" else "false")
          | None -> ())
      | Tusk_model.Package.Path _ -> ())
    sorted_deps;
  H.finish state

let test_plan_bundle_cache_hit_restores_module_and_action_graphs () =
  match
    Fs.with_tempdir ~prefix:"planner_bundle_hit_test" (fun tmpdir ->
        let package = make_package tmpdir "pkg" in
        let workspace = make_test_workspace tmpdir [ package ] in
        let store = Tusk_store.Store.create ~workspace in
        let session_id = Tusk_model.Session_id.make () in
        let profile = Tusk_model.Profile.debug in
        let build_ctx = Tusk_model.Build_ctx.make ~session_id ~profile () in
        let input_hash =
          compute_input_hash ~package ~workspace ~profile ~build_ctx
        in

        let action_graph_json =
          let ag = Tusk_planner.Action_graph.create () in
          let action =
            Tusk_planner.Action.WriteFile
              { destination = Path.v "out.txt"; content = "cached" }
          in
          let spec =
            Tusk_planner.Action_node.make ~actions:[ action ]
              ~outs:[ Path.v "out.txt" ] ~srcs:[] ~package
              ~toolchain:test_toolchain
              ~dependency_hashes:(fun _ -> Crypto.hash_string "")
              ~deps:[]
          in
          let _ = Tusk_planner.Action_graph.add_node ag spec in
          Tusk_planner.Action_graph.to_json ag
        in

        let module_graph_json =
          Std.Data.Json.Object
            [
              ( "nodes",
                Std.Data.Json.Array
                  [
                    Std.Data.Json.Object
                      [
                        ("id", Std.Data.Json.Int 1);
                        ( "file",
                          Std.Data.Json.Object
                            [
                              ("kind", Std.Data.Json.String "concrete");
                              ("path", Std.Data.Json.String "");
                            ] );
                        ( "kind",
                          Std.Data.Json.Object
                            [ ("kind", Std.Data.Json.String "root") ] );
                        ("deps", Std.Data.Json.Array []);
                        ("opens", Std.Data.Json.Array []);
                      ];
                  ] );
            ]
        in

        let bundle =
          Std.Data.Json.Object
            [
              ("version", Std.Data.Json.Int 1);
              ("package", Std.Data.Json.String package.name);
              ("module_graph", module_graph_json);
              ("action_graph", action_graph_json);
            ]
        in
        let _ =
          Tusk_store.Store.save_plan_bundle store ~hash:input_hash ~plan:bundle
          |> Result.expect ~msg:"save_plan_bundle should succeed"
        in

        let package_graph =
          Tusk_planner.Package_graph.create
            ~scope:Tusk_planner.Package_graph.Runtime workspace
          |> Result.expect ~msg:"package graph should build"
        in
        let package_key =
          Tusk_planner.Package_graph.package_key ~package_name:package.name
            Tusk_planner.Package_graph.Runtime
        in

        match
          Tusk_planner.Package_planner.plan_package ~workspace
            ~toolchain:test_toolchain ~store ~package_graph ~package_key
            ~package ~build_ctx
        with
        | Error err ->
            Error
              ("expected cache-hit plan result, got planner error: "
             ^ Tusk_planner.Planning_error.to_string err)
        | Ok (Tusk_planner.Package_planner.Planned { module_graph; action_graph; _ })
          ->
            let module_nodes =
              match G.topo_sort module_graph with
              | Ok nodes -> nodes
              | Error _ -> []
            in
            let action_nodes = Tusk_planner.Action_graph.nodes action_graph in
            if List.length module_nodes = 1 && List.length action_nodes = 1 then
              Ok ()
            else
              Error
                ("expected restored module/action graphs with one node each, got "
               ^ Int.to_string (List.length module_nodes)
               ^ " module nodes and "
               ^ Int.to_string (List.length action_nodes)
               ^ " action nodes")
        | Ok _ -> Error "expected Planned result")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let tests =
  Test.
    [
      case "plan bundle cache hit restores module and action graphs"
        test_plan_bundle_cache_hit_restores_module_and_action_graphs;
    ]

let name = "Planner Package Planning Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()

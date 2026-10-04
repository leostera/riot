open Std
open Riot_model
module Test = Std.Test
module G = Graph.SimpleGraph

let test_toolchain = Riot_toolchain.init ~config:Riot_model.Toolchain_config.default
|> Result.expect ~msg:"Failed to initialize toolchain"

let planner_artifacts_version = "planner-artifacts:v15"
let legacy_planner_artifacts_version = "planner-artifacts:v12"
let explicit_root_library_path_fix_planner_artifacts_version = "planner-artifacts:v12"
let nested_sibling_dependency_fix_planner_artifacts_version = "planner-artifacts:v13"

let make_test_workspace = fun tmpdir packages ->
  Riot_model.Workspace.make_realized
    ~root:tmpdir
    ~packages
    ~target_dir:"target"
    ()

let make_package = fun tmpdir name ->
  let pkg_dir = Path.(tmpdir / Path.v name) in
  let _ = Fs.create_dir_all pkg_dir in
  Riot_model.Package.make
    ~name:(Package_name.from_string name |> Result.expect ~msg:("expected valid package name: " ^ name))
    ~path:pkg_dir
    ~relative_path:(Path.v name)
    ()

let clone_workspace_with_target = fun (workspace: Riot_model.Workspace.t) ~target_dir ->
  Riot_model.Workspace.make
    ?name:workspace.name
    ~root:workspace.root
    ~packages:workspace.packages
    ~dependencies:workspace.dependencies
    ~dev_dependencies:workspace.dev_dependencies
    ~build_dependencies:workspace.build_dependencies
    ~profile_overrides:workspace.profile_overrides
    ~target_dir:(Path.to_string target_dir)
    ()

let find_package_by_name = fun (workspace: Riot_model.Workspace.t) name ->
  Riot_model.Workspace.realize_packages ~intent:Riot_model.Package.Dev workspace
  |> List.find
    ~fn:(fun (pkg: Riot_model.Package.t) ->
      Package_name.equal
        pkg.name
        (Package_name.from_string name |> Result.expect ~msg:("expected valid package name: " ^ name)))

let plan_graph_package = fun ~workspace ~store ~package_graph ~package_key ~build_ctx ->
  match Riot_planner.Package_graph.get_node_by_key package_graph package_key with
  | None ->
      Error ("package graph node not found: " ^ Riot_model.Package.key_to_string package_key)
  | Some node ->
      let package = Riot_planner.Package_graph.get_package node.value in
      Riot_planner.Package_planner.plan_package
        ~workspace
        ~toolchain:test_toolchain
        ~store
        ~package_graph
        ~package_key
        ~package
        ~build_ctx
      |> Result.map_err ~fn:Riot_planner.Planning_error.to_string

let module_node_label = fun (node: Riot_planner.Module_node.t G.node) ->
  match node.value.kind with
  | Riot_planner.Module_node.ML mod_ ->
      "ML(" ^ Riot_model.Module.namespaced_name mod_ ^ ")"
  | Riot_planner.Module_node.MLI mod_ ->
      "MLI(" ^ Riot_model.Module.namespaced_name mod_ ^ ")"
  | Riot_planner.Module_node.Library { name; _ } ->
      "Library(" ^ name ^ ")"
  | Riot_planner.Module_node.Binary { name; _ } ->
      "Binary(" ^ name ^ ")"
  | Riot_planner.Module_node.Native { files } ->
      "Native(" ^ String.concat ", " (List.map files ~fn:Path.to_string) ^ ")"
  | Riot_planner.Module_node.C ->
      "C(" ^ Riot_planner.Module_node.file_to_string node.value.file ^ ")"
  | Riot_planner.Module_node.H ->
      "H(" ^ Riot_planner.Module_node.file_to_string node.value.file ^ ")"
  | Riot_planner.Module_node.Root ->
      "Root"
  | Riot_planner.Module_node.Other value ->
      "Other(" ^ value ^ ")"

let module_dependency_labels = fun graph (node: Riot_planner.Module_node.t G.node) ->
  List.filter_map node.deps ~fn:(fun dep_id ->
    G.get_node graph dep_id |> Option.map ~fn:module_node_label)

let find_module_node_by_label = fun graph label ->
  match G.topo_sort graph with
  | Ok nodes ->
      List.find
        nodes
        ~fn:(fun (node: Riot_planner.Module_node.t G.node) ->
          String.equal (module_node_label node) label)
  | Error _ ->
      None

let find_library_node = fun graph ->
  match G.topo_sort graph with
  | Ok nodes ->
      List.find nodes ~fn:(fun (node: Riot_planner.Module_node.t G.node) ->
        match node.value.kind with
        | Riot_planner.Module_node.Library _ -> true
        | _ -> false)
  | Error _ ->
      None

let find_create_library_objects = fun action_graph ->
  match
    List.find
      (Riot_planner.Action_graph.to_action_list action_graph)
      ~fn:(fun action ->
        match action with
        | Riot_planner.Action.CreateLibrary _ -> true
        | _ -> false)
  with
  | Some (Riot_planner.Action.CreateLibrary { objects; _ }) ->
      Ok (List.map objects ~fn:Path.to_string)
  | Some _ ->
      Error "expected CreateLibrary action"
  | None ->
      Error "missing CreateLibrary action"

let find_create_library_node = fun action_graph ->
  List.find
    (Riot_planner.Action_graph.nodes action_graph)
    ~fn:(fun (node: Riot_planner.Action_node.t) ->
      List.any node.value.actions
        ~fn:(function
          | Riot_planner.Action.CreateLibrary _ -> true
          | _ -> false))

let require_order = fun items ~before ~after ->
  let rec find_index needle index items =
    match items with
    | [] -> None
    | item :: rest ->
        if String.equal item needle then
          Some index
        else
          find_index needle (index + 1) rest
  in
  match (find_index before 0 items, find_index after 0 items) with
  | Some before_index, Some after_index ->
      if before_index < after_index then
        Ok ()
      else
        Error ("expected " ^ before ^ " before " ^ after ^ " in ["
        ^ String.concat ", " items
        ^ "]")
  | None, _ ->
      Error ("missing " ^ before ^ " in [" ^ String.concat ", " items ^ "]")
  | _, None ->
      Error ("missing " ^ after ^ " in [" ^ String.concat ", " items ^ "]")

let object_name_of_module_node = fun (node: Riot_planner.Module_node.t G.node) ->
  match node.value.kind with
  | Riot_planner.Module_node.ML mod_ ->
      Some (Path.to_string (Riot_model.Module.cmx mod_))
  | Riot_planner.Module_node.MLI _
  | Riot_planner.Module_node.Library _
  | Riot_planner.Module_node.Binary _
  | Riot_planner.Module_node.Native _
  | Riot_planner.Module_node.C
  | Riot_planner.Module_node.H
  | Riot_planner.Module_node.Root
  | Riot_planner.Module_node.Other _ ->
      None

let validate_create_library_topological_order = fun graph objects ->
  let object_position = fun needle ->
    List.find (List.enumerate objects) ~fn:(fun (_, object_) -> String.equal object_ needle)
    |> Option.map ~fn:(fun (index, _) -> index)
  in
  match G.topo_sort graph with
  | Error cycle_ids ->
      Error ("module graph unexpectedly contains cycle: "
      ^ String.concat ", " (List.map cycle_ids ~fn:G.Node_id.to_string))
  | Ok nodes ->
      let violations =
        List.filter_map nodes ~fn:(fun (node: Riot_planner.Module_node.t G.node) ->
          match object_name_of_module_node node with
          | None ->
              None
          | Some node_object -> (
              match object_position node_object with
              | None ->
                  None
              | Some node_index ->
                  let bad_dependencies =
                    List.filter_map node.deps ~fn:(fun dep_id ->
                      match G.get_node graph dep_id with
                      | None ->
                          None
                      | Some dep_node -> (
                          match object_name_of_module_node dep_node with
                          | None ->
                              None
                          | Some dep_object -> (
                              match object_position dep_object with
                              | Some dep_index when dep_index < node_index ->
                                  None
                              | Some dep_index ->
                                  Some
                                    (dep_object
                                    ^ "@" ^ Int.to_string dep_index
                                    ^ " after "
                                    ^ node_object
                                    ^ "@"
                                    ^ Int.to_string node_index)
                              | None ->
                                  Some (dep_object ^ " missing for " ^ node_object)
                            )
                        ))
                  in
                  if List.is_empty bad_dependencies then
                    None
                  else
                    Some (node_object, bad_dependencies)
            ))
      in
      match violations with
      | [] ->
          Ok ()
      | (node_object, bad_dependencies) :: _ ->
          Error ("CreateLibrary object order violates module dependency order for "
          ^ node_object
          ^ ": "
          ^ String.concat "; " bad_dependencies)

let move_item_to_front = fun needle items ->
  let matches, rest =
    List.fold_right
      items
      ~acc:([], [])
      ~fn:(fun item (matches, rest) ->
        if String.equal item needle then
          (item :: matches, rest)
        else
          (matches, item :: rest))
  in
  matches @ rest

let rewrite_create_library_objects_json = fun json ~rewrite ->
  let open Std.Data.Json in
  let rewrite_action = fun action_json ->
    match action_json with
    | Object _ -> (
        match (
          get_field "type" action_json,
          get_field "outputs" action_json,
          get_field "objects" action_json,
          get_field "includes" action_json
        ) with
        | Some (String "CreateLibrary"), Some outputs, Some (Array object_jsons), Some includes ->
            let objects =
              List.filter_map object_jsons ~fn:(function
                | String path -> Some path
                | _ -> None)
            in
            Object [
              ("type", String "CreateLibrary");
              ("outputs", outputs);
              ("objects", Array (List.map (rewrite objects) ~fn:(fun path -> String path)));
              ("includes", includes);
            ]
        | _ ->
            action_json
      )
    | _ ->
        action_json
  in
  let rewrite_node = fun node_json ->
    match node_json with
    | Object _ -> (
        match (
          get_field "id" node_json,
          get_field "actions" node_json,
          get_field "outputs" node_json,
          get_field "sources" node_json,
          get_field "package" node_json,
          get_field "package_path" node_json,
          get_field "package_relative_path" node_json,
          get_field "hash" node_json,
          get_field "dependencies" node_json
        ) with
        | Some id, Some (Array actions), Some outputs, Some sources, Some package, Some package_path, Some package_relative_path, Some hash, Some dependencies ->
            Object [
              ("id", id);
              ("actions", Array (List.map actions ~fn:rewrite_action));
              ("outputs", outputs);
              ("sources", sources);
              ("package", package);
              ("package_path", package_path);
              ("package_relative_path", package_relative_path);
              ("hash", hash);
              ("dependencies", dependencies);
            ]
        | _ ->
            node_json
      )
    | _ ->
        node_json
  in
  match json with
  | Object _ -> (
      match get_field "nodes" json with
      | Some (Array nodes) ->
          Object [ ("nodes", Array (List.map nodes ~fn:rewrite_node)) ]
      | _ ->
          json
    )
  | _ ->
      json

let rewrite_plan_bundle_action_graph = fun bundle ~rewrite ->
  let open Std.Data.Json in
  match bundle with
  | Object _ -> (
      match (
        get_field "version" bundle,
        get_field "package" bundle,
        get_field "module_graph" bundle,
        get_field "action_graph" bundle
      ) with
      | Some version, Some package, Some module_graph, Some action_graph ->
          Object [
            ("version", version);
            ("package", package);
            ("module_graph", module_graph);
            ("action_graph", rewrite_create_library_objects_json action_graph ~rewrite);
          ]
      | _ ->
          bundle
    )
  | _ ->
      bundle

let render_module_graph_dependency_walk = fun graph ->
  let render_dependencies = fun deps ->
    match deps with
    | [] -> "  deps: []"
    | _ -> "  deps:\n" ^ String.concat "\n" (List.map deps ~fn:(fun dep -> "    - " ^ dep))
  in
  match G.topo_sort graph with
  | Error cycle_ids ->
      "cycle: " ^ String.concat ", " (List.map cycle_ids ~fn:G.Node_id.to_string) ^ "\n"
  | Ok nodes ->
      nodes
      |> List.map ~fn:(fun (node: Riot_planner.Module_node.t G.node) ->
        module_node_label node ^ "\n" ^ render_dependencies (module_dependency_labels graph node))
      |> String.concat "\n\n"

let load_repo_workspace = fun () ->
  let manager = Riot_model.Workspace_manager.create () in
  match Riot_model.Workspace_manager.scan manager (Path.v ".") with
  | Error err -> Error ("workspace scan failed: " ^ err)
  | Ok (workspace_manifest, errors) ->
      if List.is_empty errors then
        Ok (Riot_model.Workspace.make_realized
          ?name:workspace_manifest.name
          ~root:workspace_manifest.root
          ~packages:(Riot_model.Workspace_manifest.realize_packages ~intent:Riot_model.Package.Dev workspace_manifest)
          ~dependencies:workspace_manifest.dependencies
          ~dev_dependencies:workspace_manifest.dev_dependencies
          ~build_dependencies:workspace_manifest.build_dependencies
          ~profile_overrides:workspace_manifest.profile_overrides
          ~target_dir:(Path.to_string workspace_manifest.target_dir_root)
          ())
      else
        Error ("workspace scan produced load errors: "
        ^ String.concat "; " (List.map errors ~fn:Riot_model.Workspace_manager.load_error_to_string))

let plan_kernel_package_with_fresh_store = fun () ->
  match
    Fs.with_tempdir ~prefix:"planner_kernel_order"
      (fun tempdir ->
        match load_repo_workspace () with
        | Error _ as err -> err
        | Ok repo_workspace -> (
            match find_package_by_name repo_workspace "kernel" with
            | None -> Error "kernel package not found in workspace"
            | Some package ->
                let workspace = clone_workspace_with_target repo_workspace ~target_dir:Path.(tempdir / Path.v "target") in
                let store = Riot_store.Store.create ~workspace in
                let package_graph =
                  Riot_planner.Package_graph.create
                    ~scope:Riot_planner.Package_graph.Runtime
                    workspace
                  |> Result.expect ~msg:"package graph should build"
                in
                let build_key =
                  Riot_planner.Package_graph.package_key
                    ~package_name:(Package_name.to_string package.name)
                    Riot_planner.Package_graph.Build
                in
                let runtime_key =
                  Riot_planner.Package_graph.package_key
                    ~package_name:(Package_name.to_string package.name)
                    Riot_planner.Package_graph.Runtime
                in
                let session_id = Riot_model.Session_id.make () in
                let profile = Riot_model.Profile.debug in
                let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
                let runtime_result =
                  match plan_graph_package ~workspace ~store ~package_graph ~package_key:build_key ~build_ctx with
                  | Error err ->
                      Error ("kernel build-scope plan failed: " ^ err)
                  | Ok (Riot_planner.Package_planner.Planned { module_graph; action_graph; hash; _ }) ->
                      let _ =
                        Riot_planner.Package_graph.mark_planned
                          package_graph
                          build_key
                          ~module_graph
                          ~action_graph
                          ~hash
                      in
                      plan_graph_package
                        ~workspace
                        ~store
                        ~package_graph
                        ~package_key:runtime_key
                        ~build_ctx
                  | Ok _ ->
                      Error "expected kernel build-scope plan to return Planned"
                in
                match runtime_result with
                | Error err ->
                    Error ("kernel live plan failed: " ^ err)
                | Ok (Riot_planner.Package_planner.Planned { module_graph; action_graph; _ }) -> (
                    match find_create_library_objects action_graph with
                    | Error _ as err -> err
                    | Ok live_objects -> (
                        match
                          plan_graph_package
                            ~workspace
                            ~store
                            ~package_graph
                            ~package_key:runtime_key
                            ~build_ctx
                        with
                        | Error err ->
                            Error ("kernel cached plan failed: " ^ err)
                        | Ok (Riot_planner.Package_planner.Planned { action_graph; _ }) -> (
                            match find_create_library_objects action_graph with
                            | Error _ as err -> err
                            | Ok cached_objects -> Ok (module_graph, live_objects, cached_objects)
                          )
                        | Ok _ ->
                            Error "expected cached kernel plan to return Planned"
                      )
                  )
                | Ok _ ->
                    Error "expected live kernel plan to return Planned"
          ))
  with
  | Ok result -> result
  | Error err -> Error ("tempdir creation failed: " ^ IO.error_message err)

let plan_kernel_runtime_graphs = fun ~workspace ~store ~build_ctx ->
  match find_package_by_name workspace "kernel" with
  | None ->
      Error "kernel package not found in workspace"
  | Some package ->
      let package_graph =
        Riot_planner.Package_graph.create
          ~scope:Riot_planner.Package_graph.Runtime
          workspace
        |> Result.expect ~msg:"package graph should build"
      in
      let build_key =
        Riot_planner.Package_graph.package_key
          ~package_name:(Package_name.to_string package.name)
          Riot_planner.Package_graph.Build
      in
      let runtime_key =
        Riot_planner.Package_graph.package_key
          ~package_name:(Package_name.to_string package.name)
          Riot_planner.Package_graph.Runtime
      in
      match plan_graph_package ~workspace ~store ~package_graph ~package_key:build_key ~build_ctx with
      | Error err ->
          Error ("kernel build-scope plan failed: " ^ err)
      | Ok (Riot_planner.Package_planner.Planned { module_graph; action_graph; hash; _ }) ->
          let _ =
            Riot_planner.Package_graph.mark_planned
              package_graph
              build_key
              ~module_graph
              ~action_graph
              ~hash
          in
          (
            match plan_graph_package ~workspace ~store ~package_graph ~package_key:runtime_key ~build_ctx with
            | Error err ->
                Error ("kernel runtime plan failed: " ^ err)
            | Ok (Riot_planner.Package_planner.Planned { module_graph; action_graph; hash; depset; _ }) ->
                Ok (package, module_graph, action_graph, hash, depset)
            | Ok _ ->
                Error "expected kernel runtime plan to return Planned"
          )
      | Ok _ ->
          Error "expected kernel build-scope plan to return Planned"

let compute_input_hash = fun ?(planner_version = planner_artifacts_version) ?(depset = []) ~package ~workspace ~profile ~build_ctx () ->
  let module H = Std.Crypto.Sha256 in
  let state = H.create () in
  H.write state planner_version;
  Riot_model.Build_ctx.hash state build_ctx;
  H.write_hash state (Riot_toolchain.hash test_toolchain);
  Riot_model.Package.hash state package;
  let sorted_deps =
    List.sort
      (Riot_model.Package.build_graph_dependencies package)
      ~compare:(fun (a: Riot_model.Package.dependency) (b: Riot_model.Package.dependency) ->
        Package_name.compare a.name b.name)
  in
  List.for_each
    sorted_deps
    ~fn:(fun (dep: Riot_model.Package.dependency) ->
      match dep.source with
      | { workspace=true; _ } -> (
          match List.find
            workspace.Riot_model.Workspace.packages
            ~fn:(fun (p: Riot_model.Package_manifest.t) -> Package_name.equal p.name dep.name)
          with
          | Some dep_pkg ->
              H.write state (Path.to_string dep_pkg.path);
              H.write state
                (
                  if Option.is_some dep_pkg.library then
                    "true"
                  else
                    "false"
                )
          | None -> ()
        )
      | { builtin=true; _ } ->
          ()
      | _ ->
          ())
  ;
  let dep_hashes = depset
  |> List.map ~fn:(fun (dep: Riot_planner.Dependency.t) -> dep.hash)
  |> List.sort ~compare:Std.Crypto.Hash.compare in
  List.for_each dep_hashes ~fn:(fun hash -> H.write_hash state hash);
  H.finish state

let test_plan_bundle_cache_hit_restores_module_and_action_graphs = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"planner_bundle_hit_test"
      (fun tmpdir ->
        let package = make_package tmpdir "pkg" in
        let workspace = make_test_workspace tmpdir [ package ] in
        let store = Riot_store.Store.create ~workspace in
        let session_id = Riot_model.Session_id.make () in
        let profile = Riot_model.Profile.debug in
        let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
        let input_hash = compute_input_hash ~package ~workspace ~profile ~build_ctx () in
        let action_graph_json =
          let ag = Riot_planner.Action_graph.create () in
          let action = Riot_planner.Action.WriteFile {
            destination = Path.v "out.txt";
            content = "cached"
          } in
          let spec =
            Riot_planner.Action_node.make
              ~actions:[ action ]
              ~outs:[ Path.v "out.txt" ]
              ~srcs:[]
              ~package
              ~toolchain:test_toolchain
              ~dependency_hashes:(fun _ -> Crypto.hash_string "")
              ~deps:[]
          in
          let _ = Riot_planner.Action_graph.add_node ag spec in
          Riot_planner.Action_graph.to_json ag
        in
        let module_graph_json = Std.Data.Json.Object [
          (
            "nodes",
            Std.Data.Json.Array [
              Std.Data.Json.Object [
                ("id", Std.Data.Json.Int 1);
                (
                  "file",
                  Std.Data.Json.Object [
                    ("kind", Std.Data.Json.String "concrete");
                    ("path", Std.Data.Json.String "");
                  ]
                );
                ("kind", Std.Data.Json.Object [ ("kind", Std.Data.Json.String "root") ]);
                ("deps", Std.Data.Json.Array []);
                ("opens", Std.Data.Json.Array []);
              ];
            ]
          );
        ] in
        let bundle = Std.Data.Json.Object [
          ("version", Std.Data.Json.Int 1);
          ("package", Std.Data.Json.String (Package_name.to_string package.name));
          ("module_graph", module_graph_json);
          ("action_graph", action_graph_json);
        ] in
        let _ = Riot_store.Store.save_plan_bundle store ~hash:input_hash ~plan:bundle
        |> Result.expect ~msg:"save_plan_bundle should succeed" in
        let package_graph = Riot_planner.Package_graph.create
          ~scope:Riot_planner.Package_graph.Runtime workspace
        |> Result.expect ~msg:"package graph should build" in
        let package_key = Riot_planner.Package_graph.package_key
          ~package_name:(Package_name.to_string package.name)
          Riot_planner.Package_graph.Runtime in
        match Riot_planner.Package_planner.plan_package
          ~workspace
          ~toolchain:test_toolchain
          ~store
          ~package_graph
          ~package_key
          ~package
          ~build_ctx with
        | Error err ->
            Error ("expected cache-hit plan result, got planner error: "
            ^ Riot_planner.Planning_error.to_string err)
        | Ok (Riot_planner.Package_planner.Planned { module_graph; action_graph; _ }) ->
            let module_nodes =
              match G.topo_sort module_graph with
              | Ok nodes -> nodes
              | Error _ -> []
            in
            let action_nodes = Riot_planner.Action_graph.nodes action_graph in
            if List.length module_nodes = 1 && List.length action_nodes = 1 then
              Ok ()
            else
              Error ("expected restored module/action graphs with one node each, got "
              ^ Int.to_string (List.length module_nodes)
              ^ " module nodes and "
              ^ Int.to_string (List.length action_nodes)
              ^ " action nodes")
        | Ok _ ->
            Error "expected Planned result")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_cached_artifact_and_exports_short_circuit_without_plan_bundle = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"planner_cached_artifact_hit_test"
      (fun tmpdir ->
        let package = make_package tmpdir "pkg" in
        let workspace = make_test_workspace tmpdir [ package ] in
        let store = Riot_store.Store.create ~workspace in
        let session_id = Riot_model.Session_id.make () in
        let profile = Riot_model.Profile.debug in
        let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
        let input_hash = compute_input_hash ~package ~workspace ~profile ~build_ctx () in
        let sandbox_dir = Path.(tmpdir / Path.v "sandbox") in
        let output = Path.(sandbox_dir / Path.v "pkg.cma") in
        let _ = Fs.create_dir_all sandbox_dir |> Result.expect ~msg:"sandbox dir creation should succeed" in
        let _ = Fs.write "cached" output |> Result.expect ~msg:"artifact output write should succeed" in
        let exports = [
          Riot_store.Store.{
            name = "pkg.cma";
            path = Path.v "pkg.cma";
            action_hash = Std.Crypto.Digest.hex input_hash
          };
        ] in
        let _artifact = Riot_store.Store.save
          store
          ~package:(Package_name.to_string package.name)
          ~exports
          ~hash:input_hash
          ~sandbox_dir
          ~outs:[ output ]
        |> Result.expect ~msg:"artifact save should succeed" in
        if Option.is_some (Riot_store.Store.load_plan_bundle store ~hash:input_hash) then
          Error "expected no plan bundle before cached planner lookup"
        else
          let package_graph = Riot_planner.Package_graph.create
            ~scope:Riot_planner.Package_graph.Runtime workspace
          |> Result.expect ~msg:"package graph should build" in
          let package_key = Riot_planner.Package_graph.package_key
            ~package_name:(Package_name.to_string package.name)
            Riot_planner.Package_graph.Runtime in
          match Riot_planner.Package_planner.plan_package
            ~workspace
            ~toolchain:test_toolchain
            ~store
            ~package_graph
            ~package_key
            ~package
            ~build_ctx with
          | Error err -> Error ("expected cached plan result, got planner error: "
          ^ Riot_planner.Planning_error.to_string err)
          | Ok (Riot_planner.Package_planner.Cached {
            hash;
            artifact=cached_artifact;
            exports=cached_exports;
            _
          }) ->
              if not (Std.Crypto.Hash.compare hash input_hash = 0) then
                Error "expected cached plan hash to match input hash"
              else if not (List.length cached_artifact.Riot_store.Artifact.files = 1) then
                Error "expected cached artifact to expose one file"
              else if not (List.length cached_exports = 1) then
                Error "expected cached export manifest to expose one export"
              else
                Ok ()
          | Ok _ -> Error "expected Cached result")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_stale_plan_bundle_version_rebuilds_plan_graphs = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"planner_bundle_stale_version_test"
      (fun tmpdir ->
        let package =
          Riot_model.Package.make
            ~name:(Package_name.from_string "pkg" |> Result.expect ~msg:"expected valid package name")
            ~path:Path.(tmpdir / Path.v "pkg")
            ~relative_path:(Path.v "pkg")
            ~library:{ path = Path.v "src/pkg.ml" }
          ~sources:{
            src = [ Path.v "src/pkg.ml" ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
        in
        let src_dir = Path.(package.path / Path.v "src") in
        let source = Path.(src_dir / Path.v "pkg.ml") in
        let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"expected src dir creation to succeed" in
        let _ = Fs.write "let value = 1\n" source |> Result.expect ~msg:"expected source write to succeed" in
        let workspace = make_test_workspace tmpdir [ package ] in
        let store = Riot_store.Store.create ~workspace in
        let session_id = Riot_model.Session_id.make () in
        let profile = Riot_model.Profile.release in
        let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
        let stale_input_hash = compute_input_hash
          ~planner_version:"planner-artifacts:v14"
          ~package
          ~workspace
          ~profile
          ~build_ctx
          () in
        let stale_action_graph_json =
          let ag = Riot_planner.Action_graph.create () in
          let action = Riot_planner.Action.WriteFile {
            destination = Path.v "out.txt";
            content = "stale"
          } in
          let spec =
            Riot_planner.Action_node.make
              ~actions:[ action ]
              ~outs:[ Path.v "out.txt" ]
              ~srcs:[]
              ~package
              ~toolchain:test_toolchain
              ~dependency_hashes:(fun _ -> Crypto.hash_string "")
              ~deps:[]
          in
          let _ = Riot_planner.Action_graph.add_node ag spec in
          Riot_planner.Action_graph.to_json ag
        in
        let stale_module_graph_json = Std.Data.Json.Object [
          (
            "nodes",
            Std.Data.Json.Array [
              Std.Data.Json.Object [
                ("id", Std.Data.Json.Int 1);
                (
                  "file",
                  Std.Data.Json.Object [
                    ("kind", Std.Data.Json.String "concrete");
                    ("path", Std.Data.Json.String "");
                  ]
                );
                ("kind", Std.Data.Json.Object [ ("kind", Std.Data.Json.String "root") ]);
                ("deps", Std.Data.Json.Array []);
                ("opens", Std.Data.Json.Array []);
              ];
            ]
          );
        ] in
        let stale_bundle = Std.Data.Json.Object [
          ("version", Std.Data.Json.Int 1);
          ("package", Std.Data.Json.String (Package_name.to_string package.name));
          ("module_graph", stale_module_graph_json);
          ("action_graph", stale_action_graph_json);
        ] in
        let _ = Riot_store.Store.save_plan_bundle store ~hash:stale_input_hash ~plan:stale_bundle
        |> Result.expect ~msg:"expected stale plan bundle save to succeed" in
        let package_graph = Riot_planner.Package_graph.create
          ~scope:Riot_planner.Package_graph.Runtime workspace
        |> Result.expect ~msg:"package graph should build" in
        let package_key = Riot_planner.Package_graph.package_key
          ~package_name:(Package_name.to_string package.name)
          Riot_planner.Package_graph.Runtime in
        match Riot_planner.Package_planner.plan_package
          ~workspace
          ~toolchain:test_toolchain
          ~store
          ~package_graph
          ~package_key
          ~package
          ~build_ctx with
        | Error err ->
            Error ("expected stale bundle miss to replan package, got planner error: "
            ^ Riot_planner.Planning_error.to_string err)
        | Ok (Riot_planner.Package_planner.Planned { action_graph; _ }) ->
            let actions = Riot_planner.Action_graph.to_action_list action_graph in
            if List.any
                actions
                ~fn:(
                  function
                  | Riot_planner.Action.CreateLibrary _ -> true
                  | _ -> false
                )
            then
              Ok ()
            else
              Error "expected stale plan bundle to be ignored and rebuilt"
        | Ok _ ->
            Error "expected Planned result")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_plan_bundle_cache_hit_preserves_module_dependency_order = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"planner_bundle_order_test"
      (fun tmpdir ->
        let package = Riot_model.Package.make
          ~name:(Package_name.from_string "pkg" |> Result.expect ~msg:"expected valid package name")
          ~path:Path.(tmpdir / Path.v "pkg")
          ~relative_path:(Path.v "pkg")
          ~library:{ path = Path.v "src/pkg.ml" }
          ~sources:{
            src = [ Path.v "src/a.ml"; Path.v "src/b.ml"; Path.v "src/c.ml"; Path.v "src/pkg.ml" ];
            native = [];
            tests = [];
            examples = [];
            bench = [];
          }
          ()
        in
        let workspace = make_test_workspace tmpdir [ package ] in
        let store = Riot_store.Store.create ~workspace in
        let session_id = Riot_model.Session_id.make () in
        let profile = Riot_model.Profile.debug in
        let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
        let input_hash = compute_input_hash ~package ~workspace ~profile ~build_ctx () in
        let module_graph_json =
          Std.Data.Json.Object [
            (
              "nodes",
              Std.Data.Json.Array [
                Std.Data.Json.Object [
                  ("id", Std.Data.Json.Int 1);
                  ("file", Std.Data.Json.Object [
                    ("kind", Std.Data.Json.String "concrete");
                    ("path", Std.Data.Json.String "src/a.ml");
                  ]);
                  ("kind", Std.Data.Json.Object [
                    ("kind", Std.Data.Json.String "ml");
                    ("filename", Std.Data.Json.String "src/a.ml");
                    ("namespace", Std.Data.Json.Array []);
                  ]);
                  ("deps", Std.Data.Json.Array []);
                  ("opens", Std.Data.Json.Array []);
                ];
                Std.Data.Json.Object [
                  ("id", Std.Data.Json.Int 2);
                  ("file", Std.Data.Json.Object [
                    ("kind", Std.Data.Json.String "concrete");
                    ("path", Std.Data.Json.String "src/b.ml");
                  ]);
                  ("kind", Std.Data.Json.Object [
                    ("kind", Std.Data.Json.String "ml");
                    ("filename", Std.Data.Json.String "src/b.ml");
                    ("namespace", Std.Data.Json.Array []);
                  ]);
                  ("deps", Std.Data.Json.Array []);
                  ("opens", Std.Data.Json.Array []);
                ];
                Std.Data.Json.Object [
                  ("id", Std.Data.Json.Int 3);
                  ("file", Std.Data.Json.Object [
                    ("kind", Std.Data.Json.String "concrete");
                    ("path", Std.Data.Json.String "src/c.ml");
                  ]);
                  ("kind", Std.Data.Json.Object [
                    ("kind", Std.Data.Json.String "ml");
                    ("filename", Std.Data.Json.String "src/c.ml");
                    ("namespace", Std.Data.Json.Array []);
                  ]);
                  ("deps", Std.Data.Json.Array []);
                  ("opens", Std.Data.Json.Array []);
                ];
                Std.Data.Json.Object [
                  ("id", Std.Data.Json.Int 4);
                  ("file", Std.Data.Json.Object [
                    ("kind", Std.Data.Json.String "concrete");
                    ("path", Std.Data.Json.String "");
                  ]);
                  ("kind", Std.Data.Json.Object [
                    ("kind", Std.Data.Json.String "library");
                    ("name", Std.Data.Json.String (Package_name.to_string package.name));
                    ("includes", Std.Data.Json.Array []);
                  ]);
                  ("deps", Std.Data.Json.Array [
                    Std.Data.Json.Int 1;
                    Std.Data.Json.Int 2;
                    Std.Data.Json.Int 3;
                  ]);
                  ("opens", Std.Data.Json.Array []);
                ];
              ]
            );
          ]
        in
        let action_graph_json =
          let graph = Riot_planner.Action_graph.create () in
          let spec =
            Riot_planner.Action_node.make
              ~actions:[ Riot_planner.Action.WriteFile {
                destination = Path.v "out.txt";
                content = "cached";
              } ]
              ~outs:[ Path.v "out.txt" ]
              ~srcs:[]
              ~package
              ~toolchain:test_toolchain
              ~dependency_hashes:(fun _ -> Crypto.hash_string "")
              ~deps:[]
          in
          let _ = Riot_planner.Action_graph.add_node graph spec in
          Riot_planner.Action_graph.to_json graph
        in
        let bundle = Std.Data.Json.Object [
          ("version", Std.Data.Json.Int 1);
          ("package", Std.Data.Json.String (Package_name.to_string package.name));
          ("module_graph", module_graph_json);
          ("action_graph", action_graph_json);
        ] in
        let _ = Riot_store.Store.save_plan_bundle store ~hash:input_hash ~plan:bundle
        |> Result.expect ~msg:"save_plan_bundle should succeed" in
        let package_graph = Riot_planner.Package_graph.create
          ~scope:Riot_planner.Package_graph.Runtime workspace
          |> Result.expect ~msg:"package graph should build" in
        let package_key = Riot_planner.Package_graph.package_key
          ~package_name:(Package_name.to_string package.name)
          Riot_planner.Package_graph.Runtime in
        match Riot_planner.Package_planner.plan_package
          ~workspace
          ~toolchain:test_toolchain
          ~store
          ~package_graph
          ~package_key
          ~package
          ~build_ctx with
        | Error err ->
            Error ("expected cache-hit plan result, got planner error: "
            ^ Riot_planner.Planning_error.to_string err)
        | Ok (Riot_planner.Package_planner.Planned { module_graph; _ }) -> (
            match find_library_node module_graph with
            | None -> Error "expected restored library node"
            | Some library_node ->
                let actual = module_dependency_labels module_graph library_node in
                let expected = [ "ML(A)"; "ML(B)"; "ML(C)" ] in
                if actual = expected then
                  Ok ()
                else
                  Error ("expected library dependency order ["
                  ^ String.concat ", " expected
                  ^ "] but got ["
                  ^ String.concat ", " actual
                  ^ "]")
          )
        | Ok _ ->
            Error "expected Planned result")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_underscore_sibling_module_dependency_is_planned = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"planner_underscore_sibling_dep"
      (fun tmpdir ->
        let package_root = Path.(tmpdir / Path.v "pkg") in
        let src_dir = Path.(package_root / Path.v "src") in
        let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"expected src dir creation to succeed" in
        let _ =
          Fs.write "module Udp_socket = Udp_socket\nmodule Udp_server = Udp_server\n" Path.(src_dir / Path.v "pkg.ml")
          |> Result.expect ~msg:"expected pkg.ml write to succeed"
        in
        let _ =
          Fs.write "type t\nval create : unit -> t\n" Path.(src_dir / Path.v "udp_socket.mli")
          |> Result.expect ~msg:"expected udp_socket.mli write to succeed"
        in
        let _ =
          Fs.write "type t = unit\nlet create () = ()\n" Path.(src_dir / Path.v "udp_socket.ml")
          |> Result.expect ~msg:"expected udp_socket.ml write to succeed"
        in
        let _ =
          Fs.write
            "type handler = socket:Udp_socket.t -> bytes -> unit\nval run : handler -> unit\n"
            Path.(src_dir / Path.v "udp_server.mli")
          |> Result.expect ~msg:"expected udp_server.mli write to succeed"
        in
        let _ =
          Fs.write
            "type handler = socket:Udp_socket.t -> bytes -> unit\nlet run _ = ()\n"
            Path.(src_dir / Path.v "udp_server.ml")
          |> Result.expect ~msg:"expected udp_server.ml write to succeed"
        in
        let package =
          Riot_model.Package.make
            ~name:(Package_name.from_string "pkg" |> Result.expect ~msg:"expected valid package name")
            ~path:package_root
            ~relative_path:(Path.v "pkg")
            ~library:{ path = Path.v "src/pkg.ml" }
            ~sources:{
              src = [
                Path.v "src/pkg.ml";
                Path.v "src/udp_server.mli";
                Path.v "src/udp_socket.mli";
                Path.v "src/udp_socket.ml";
                Path.v "src/udp_server.ml";
              ];
              native = [];
              tests = [];
              examples = [];
              bench = [];
            }
            ()
        in
        let workspace = make_test_workspace tmpdir [ package ] in
        let store = Riot_store.Store.create ~workspace in
        let package_graph =
          Riot_planner.Package_graph.create
            ~scope:Riot_planner.Package_graph.Runtime
            workspace
          |> Result.expect ~msg:"package graph should build"
        in
        let package_key =
          Riot_planner.Package_graph.package_key
            ~package_name:(Package_name.to_string package.name)
            Riot_planner.Package_graph.Runtime
        in
        let session_id = Riot_model.Session_id.make () in
        let profile = Riot_model.Profile.debug in
        let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
        match plan_graph_package ~workspace ~store ~package_graph ~package_key ~build_ctx with
        | Error err ->
            Error ("expected package plan to succeed, got planner error: " ^ err)
        | Ok (Riot_planner.Package_planner.Planned { module_graph; _ }) -> (
            match find_module_node_by_label module_graph "MLI(Pkg__Udp_server)" with
            | None ->
                Error "missing MLI(Pkg__Udp_server) in module graph"
            | Some node ->
                let deps = module_dependency_labels module_graph node in
                if
                  List.any
                    deps
                    ~fn:(fun label ->
                      String.equal label "ML(Pkg__Udp_socket)"
                      || String.equal label "MLI(Pkg__Udp_socket)")
                then
                  Ok ()
                else
                  Error ("expected MLI(Pkg__Udp_server) to depend on Udp_socket, got ["
                  ^ String.concat ", " deps
                  ^ "]")
          )
        | Ok _ ->
            Error "expected Planned result")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_legacy_nested_sibling_plan_bundle_is_ignored_after_version_bump = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"planner_nested_sibling_legacy_bundle"
      (fun tmpdir ->
        let package_root = Path.(tmpdir / Path.v "demo") in
        let src_dir = Path.(package_root / Path.v "src") in
        let net_dir = Path.(src_dir / Path.v "net") in
        let _ = Fs.create_dir_all net_dir |> Result.expect ~msg:"expected nested src dir creation to succeed" in
        let _ =
          Fs.write "module Net = Net\n" Path.(src_dir / Path.v "demo.ml")
          |> Result.expect ~msg:"expected demo.ml write to succeed"
        in
        let _ =
          Fs.write "module Udp_socket = Udp_socket\nmodule Udp_server = Udp_server\n" Path.(net_dir / Path.v "net.ml")
          |> Result.expect ~msg:"expected net.ml write to succeed"
        in
        let _ =
          Fs.write "type t\n" Path.(net_dir / Path.v "udp_socket.mli")
          |> Result.expect ~msg:"expected udp_socket.mli write to succeed"
        in
        let _ =
          Fs.write "type t = unit\n" Path.(net_dir / Path.v "udp_socket.ml")
          |> Result.expect ~msg:"expected udp_socket.ml write to succeed"
        in
        let _ =
          Fs.write
            "type handler = socket:Udp_socket.t -> bytes -> unit\nval run : handler -> unit\n"
            Path.(net_dir / Path.v "udp_server.mli")
          |> Result.expect ~msg:"expected udp_server.mli write to succeed"
        in
        let _ =
          Fs.write
            "type handler = socket:Udp_socket.t -> bytes -> unit\nlet run _ = ()\n"
            Path.(net_dir / Path.v "udp_server.ml")
          |> Result.expect ~msg:"expected udp_server.ml write to succeed"
        in
        let package =
          Riot_model.Package.make
            ~name:(Package_name.from_string "demo" |> Result.expect ~msg:"expected valid package name")
            ~path:package_root
            ~relative_path:(Path.v "demo")
            ~library:{ path = Path.v "src/demo.ml" }
            ~sources:{
              src = [
                Path.v "src/demo.ml";
                Path.v "src/net/net.ml";
                Path.v "src/net/udp_socket.mli";
                Path.v "src/net/udp_socket.ml";
                Path.v "src/net/udp_server.mli";
                Path.v "src/net/udp_server.ml";
              ];
              native = [];
              tests = [];
              examples = [];
              bench = [];
            }
            ()
        in
        let workspace = make_test_workspace tmpdir [ package ] in
        let store = Riot_store.Store.create ~workspace in
        let session_id = Riot_model.Session_id.make () in
        let profile = Riot_model.Profile.debug in
        let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
        let stale_input_hash =
          compute_input_hash
            ~planner_version:nested_sibling_dependency_fix_planner_artifacts_version
            ~package
            ~workspace
            ~profile
            ~build_ctx
            ()
        in
        let stale_action_graph_json =
          let ag = Riot_planner.Action_graph.create () in
          let action = Riot_planner.Action.WriteFile {
            destination = Path.v "out.txt";
            content = "stale"
          } in
          let spec =
            Riot_planner.Action_node.make
              ~actions:[ action ]
              ~outs:[ Path.v "out.txt" ]
              ~srcs:[]
              ~package
              ~toolchain:test_toolchain
              ~dependency_hashes:(fun _ -> Crypto.hash_string "")
              ~deps:[]
          in
          let _ = Riot_planner.Action_graph.add_node ag spec in
          Riot_planner.Action_graph.to_json ag
        in
        let stale_module_graph_json = Std.Data.Json.Object [
          (
            "nodes",
            Std.Data.Json.Array [
              Std.Data.Json.Object [
                ("id", Std.Data.Json.Int 1);
                (
                  "file",
                  Std.Data.Json.Object [
                    ("kind", Std.Data.Json.String "concrete");
                    ("path", Std.Data.Json.String "");
                  ]
                );
                ("kind", Std.Data.Json.Object [ ("kind", Std.Data.Json.String "root") ]);
                ("deps", Std.Data.Json.Array []);
                ("opens", Std.Data.Json.Array []);
              ];
            ]
          );
        ] in
        let stale_bundle = Std.Data.Json.Object [
          ("version", Std.Data.Json.Int 1);
          ("package", Std.Data.Json.String (Package_name.to_string package.name));
          ("module_graph", stale_module_graph_json);
          ("action_graph", stale_action_graph_json);
        ] in
        let _ =
          Riot_store.Store.save_plan_bundle store ~hash:stale_input_hash ~plan:stale_bundle
          |> Result.expect ~msg:"expected stale nested plan bundle save to succeed"
        in
        let package_graph =
          Riot_planner.Package_graph.create
            ~scope:Riot_planner.Package_graph.Runtime
            workspace
          |> Result.expect ~msg:"package graph should build"
        in
        let package_key =
          Riot_planner.Package_graph.package_key
            ~package_name:(Package_name.to_string package.name)
            Riot_planner.Package_graph.Runtime
        in
        match plan_graph_package ~workspace ~store ~package_graph ~package_key ~build_ctx with
        | Error err ->
            Error ("expected nested sibling plan bundle to be ignored, got planner error: " ^ err)
        | Ok (Riot_planner.Package_planner.Planned { module_graph; _ }) -> (
            match find_module_node_by_label module_graph "MLI(Demo__Net__Udp_server)" with
            | None ->
                Error "expected stale nested plan bundle to be ignored and rebuilt, but udp_server node was missing"
            | Some node ->
                let deps = module_dependency_labels module_graph node in
                if
                  List.any
                    deps
                    ~fn:(fun label ->
                      String.equal label "ML(Demo__Net__Udp_socket)"
                      || String.equal label "MLI(Demo__Net__Udp_socket)")
                then
                  Ok ()
                else
                  Error ("expected rebuilt nested plan graph to restore Udp_socket dependency, got ["
                  ^ String.concat ", " deps
                  ^ "]")
          )
        | Ok _ ->
            Error "expected Planned result")
  with
  | Ok x ->
      x
  | Error _ ->
      Error "tempdir creation failed"

let test_kernel_live_create_library_orders_dependencies_before_error = fun _ctx ->
  match plan_kernel_package_with_fresh_store () with
  | Error _ as err -> err
  | Ok (_module_graph, live_objects, _cached_objects) -> (
      match
        require_order live_objects ~before:"Kernel__Net__Tcp_listener.cmx" ~after:"Kernel__Error.cmx"
      with
      | Error _ as err -> err
      | Ok () -> (
          match require_order live_objects ~before:"Kernel__Net__Udp_socket.cmx" ~after:"Kernel__Error.cmx" with
          | Error _ as err -> err
          | Ok () ->
              require_order live_objects ~before:"Kernel__Process.cmx" ~after:"Kernel__Error.cmx"
        )
    )

let test_kernel_plan_bundle_cache_hit_preserves_live_create_library_order = fun _ctx ->
  match plan_kernel_package_with_fresh_store () with
  | Error _ as err -> err
  | Ok (_module_graph, live_objects, cached_objects) ->
      if live_objects = cached_objects then
        Ok ()
      else
        Error ("expected cached CreateLibrary object order to match live plan\nlive: ["
        ^ String.concat ", " live_objects
        ^ "]\ncached: ["
        ^ String.concat ", " cached_objects
        ^ "]")

let test_kernel_create_library_is_topological = fun _ctx ->
  match plan_kernel_package_with_fresh_store () with
  | Error err ->
      Error err
  | Ok (module_graph, live_objects, _cached_objects) ->
      validate_create_library_topological_order module_graph live_objects

let test_kernel_create_library_dependencies_are_unique = fun _ctx ->
  let check = fun tempdir ->
    match load_repo_workspace () with
    | Error _ as err ->
        err
    | Ok repo_workspace ->
        let workspace =
          clone_workspace_with_target
            repo_workspace
            ~target_dir:Path.(tempdir / Path.v "target")
        in
        let store = Riot_store.Store.create ~workspace in
        let session_id = Riot_model.Session_id.make () in
        let profile = Riot_model.Profile.debug in
        let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
        match plan_kernel_runtime_graphs ~workspace ~store ~build_ctx with
        | Error _ as err ->
            err
        | Ok (_, _, action_graph, _, _) ->
            match find_create_library_node action_graph with
            | None ->
                Error "missing kernel CreateLibrary action node"
            | Some node ->
                let seen = Collections.HashSet.create () in
                let duplicates =
                  List.filter_map node.deps ~fn:(fun dep_id ->
                    if Collections.HashSet.insert seen ~value:dep_id then
                      None
                    else
                      Some (G.Node_id.to_string dep_id))
                in
                if List.is_empty duplicates then
                  Ok ()
                else
                  Error
                    ("expected unique CreateLibrary deps, found duplicates: "
                    ^ String.concat ", " duplicates)
  in
  match Fs.with_tempdir ~prefix:"planner_kernel_unique_deps" check with
  | Ok result ->
      result
  | Error err ->
      Error ("tempdir creation failed: " ^ IO.error_message err)

let test_kernel_dependency_walk_snapshot = fun ctx ->
  match plan_kernel_package_with_fresh_store () with
  | Error err -> Error err
  | Ok (module_graph, live_objects, _cached_objects) ->
      let actual =
        String.concat
          "\n\n"
          [
            "MODULE GRAPH";
            render_module_graph_dependency_walk module_graph;
            "CREATE LIBRARY OBJECTS";
            String.concat "\n" (List.map live_objects ~fn:(fun object_ -> "- " ^ object_));
          ]
        ^ "\n"
      in
      Test.Snapshot.assert_text ~ctx ~actual

let test_legacy_kernel_plan_bundle_is_ignored_after_version_bump = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"planner_kernel_legacy_bundle"
      (fun tempdir ->
        match load_repo_workspace () with
        | Error _ as err ->
            err
        | Ok repo_workspace ->
            let analysis_workspace =
              clone_workspace_with_target
                repo_workspace
                ~target_dir:Path.(tempdir / Path.v "analysis-target")
            in
            let test_workspace =
              clone_workspace_with_target
                repo_workspace
                ~target_dir:Path.(tempdir / Path.v "test-target")
            in
            let analysis_store = Riot_store.Store.create ~workspace:analysis_workspace in
            let test_store = Riot_store.Store.create ~workspace:test_workspace in
            let session_id = Riot_model.Session_id.make () in
            let profile = Riot_model.Profile.debug in
            let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
            match plan_kernel_runtime_graphs ~workspace:analysis_workspace ~store:analysis_store ~build_ctx with
            | Error _ as err ->
                err
            | Ok (package, _module_graph, action_graph, current_input_hash, depset) -> (
                match Riot_store.Store.load_plan_bundle analysis_store ~hash:current_input_hash with
                | None ->
                    Error "expected current kernel plan bundle to be persisted"
                | Some bundle ->
                    let stale_bundle =
                      rewrite_plan_bundle_action_graph
                        bundle
                        ~rewrite:(move_item_to_front "Kernel__Error.cmx")
                    in
                    let stale_input_hash = compute_input_hash
                      ~planner_version:legacy_planner_artifacts_version
                      ~depset
                      ~package
                      ~workspace:test_workspace
                      ~profile
                      ~build_ctx
                      () in
                    let _ = Riot_store.Store.save_plan_bundle test_store ~hash:stale_input_hash ~plan:stale_bundle
                    |> Result.expect ~msg:"expected legacy plan bundle save to succeed" in
                    match plan_kernel_runtime_graphs ~workspace:test_workspace ~store:test_store ~build_ctx with
                    | Error _ as err ->
                        err
                    | Ok (_, _, replanned_action_graph, _, _) -> (
                        match find_create_library_objects replanned_action_graph with
                        | Error _ as err ->
                            err
                        | Ok objects -> (
                            match require_order objects ~before:"Kernel__Net__Tcp_listener.cmx" ~after:"Kernel__Error.cmx" with
                            | Error _ as err ->
                                err
                            | Ok () -> (
                                match require_order objects ~before:"Kernel__Net__Udp_socket.cmx" ~after:"Kernel__Error.cmx" with
                                | Error _ as err ->
                                    err
                                | Ok () ->
                                    require_order objects ~before:"Kernel__Process.cmx" ~after:"Kernel__Error.cmx"
                              )
                          )
                      )
              )
      )
  with
  | Ok x ->
      x
  | Error _ ->
      Error "tempdir creation failed"

let test_legacy_krasny_plan_bundle_with_bad_root_module_is_ignored_after_version_bump = fun _ctx ->
  let rewrite_bad_krasny_root = fun objects ->
    List.map
      objects
      ~fn:(fun object_ ->
        if String.equal object_ "Krasny.cmx" then
          "Krasny__Krasny.cmx"
        else
          object_)
  in
  match
    Fs.with_tempdir ~prefix:"planner_krasny_legacy_bundle"
      (fun tempdir ->
        let package_root = Path.(tempdir / Path.v "krasny") in
        let src_dir = Path.(package_root / Path.v "src") in
        let _ = Fs.create_dir_all src_dir |> Result.expect ~msg:"expected krasny src dir creation to succeed" in
        let _ =
          Fs.write "let format value = value\n" Path.(src_dir / Path.v "Krasny.ml")
          |> Result.expect ~msg:"expected Krasny.ml write to succeed"
        in
        let _ =
          Fs.write "let () = ignore (Krasny.format \"ok\")\n" Path.(src_dir / Path.v "main.ml")
          |> Result.expect ~msg:"expected main.ml write to succeed"
        in
        let package =
          Riot_model.Package.make
            ~name:(Package_name.from_string "krasny" |> Result.expect ~msg:"expected valid package name")
            ~path:package_root
            ~relative_path:(Path.v "krasny")
            ~library:{ path = Path.v "src/krasny.ml" }
            ~binaries:[ Riot_model.Package.{ name = "krasny"; path = Path.v "src/main.ml" } ]
            ~sources:{
              src = [ Path.v "src/Krasny.ml"; Path.v "src/main.ml" ];
              native = [];
              tests = [];
              examples = [];
              bench = [];
            }
            ()
        in
        let analysis_workspace = make_test_workspace Path.(tempdir / Path.v "analysis") [ package ] in
        let test_workspace = make_test_workspace Path.(tempdir / Path.v "test") [ package ] in
        let analysis_store = Riot_store.Store.create ~workspace:analysis_workspace in
        let test_store = Riot_store.Store.create ~workspace:test_workspace in
        let session_id = Riot_model.Session_id.make () in
        let profile = Riot_model.Profile.debug in
        let build_ctx = Riot_model.Build_ctx.make ~session_id ~profile () in
        let package_key =
          Riot_planner.Package_graph.package_key
            ~package_name:(Package_name.to_string package.name)
            Riot_planner.Package_graph.Runtime
        in
        let analysis_package_graph =
          Riot_planner.Package_graph.create
            ~scope:Riot_planner.Package_graph.Runtime
            analysis_workspace
          |> Result.expect ~msg:"analysis package graph should build"
        in
        match
          plan_graph_package
            ~workspace:analysis_workspace
            ~store:analysis_store
            ~package_graph:analysis_package_graph
            ~package_key
            ~build_ctx
        with
        | Error _ as err ->
            err
        | Ok (Riot_planner.Package_planner.Planned {
          hash=current_input_hash;
          depset;
          _;
        }) -> (
            match Riot_store.Store.load_plan_bundle analysis_store ~hash:current_input_hash with
            | None ->
                Error "expected current krasny plan bundle to be persisted"
            | Some bundle ->
                let stale_bundle =
                  rewrite_plan_bundle_action_graph
                    bundle
                    ~rewrite:rewrite_bad_krasny_root
                in
                let stale_input_hash =
                  compute_input_hash
                    ~planner_version:explicit_root_library_path_fix_planner_artifacts_version
                    ~depset
                    ~package
                    ~workspace:test_workspace
                    ~profile
                    ~build_ctx
                    ()
                in
                let _ =
                  Riot_store.Store.save_plan_bundle test_store ~hash:stale_input_hash ~plan:stale_bundle
                  |> Result.expect ~msg:"expected legacy krasny plan bundle save to succeed"
                in
                let test_package_graph =
                  Riot_planner.Package_graph.create
                    ~scope:Riot_planner.Package_graph.Runtime
                    test_workspace
                  |> Result.expect ~msg:"test package graph should build"
                in
                match
                  plan_graph_package
                    ~workspace:test_workspace
                    ~store:test_store
                    ~package_graph:test_package_graph
                    ~package_key
                    ~build_ctx
                with
                | Error _ as err ->
                    err
                | Ok (Riot_planner.Package_planner.Planned { action_graph; _ }) -> (
                    match find_create_library_objects action_graph with
                    | Error _ as err ->
                        err
                    | Ok objects ->
                        if List.contains objects ~value:"Krasny__Krasny.cmx" then
                          Error ("expected stale krasny plan bundle to be ignored, got ["
                          ^ String.concat ", " objects
                          ^ "]")
                        else if List.contains objects ~value:"Krasny.cmx" then
                          Ok ()
                        else
                          Error ("expected replanned krasny bundle to include Krasny.cmx, got ["
                          ^ String.concat ", " objects
                          ^ "]")
                  )
                | Ok _ ->
                    Error "expected cached krasny plan to return Planned"
          )
        | Ok _ ->
            Error "expected analysis krasny plan to return Planned"
      )
  with
  | Ok x ->
      x
  | Error _ ->
      Error "tempdir creation failed"

let tests =
  Test.[
    case "plan bundle cache hit restores module and action graphs" test_plan_bundle_cache_hit_restores_module_and_action_graphs;
    case "cached artifact and exports short-circuit without plan bundle" test_cached_artifact_and_exports_short_circuit_without_plan_bundle;
    case "stale plan bundle version rebuilds plan graphs" test_stale_plan_bundle_version_rebuilds_plan_graphs;
    case "plan bundle cache hit preserves module dependency order" test_plan_bundle_cache_hit_preserves_module_dependency_order;
    case "underscore sibling module dependency is planned" test_underscore_sibling_module_dependency_is_planned;
    case
      "legacy nested sibling plan bundle is ignored after version bump"
      test_legacy_nested_sibling_plan_bundle_is_ignored_after_version_bump;
    case ~size:Large "kernel live CreateLibrary orders dependencies before Error" test_kernel_live_create_library_orders_dependencies_before_error;
    case ~size:Large "kernel plan bundle cache hit preserves live CreateLibrary order" test_kernel_plan_bundle_cache_hit_preserves_live_create_library_order;
    case ~size:Large "kernel CreateLibrary objects are topological" test_kernel_create_library_is_topological;
    case ~size:Large "kernel CreateLibrary dependencies are unique" test_kernel_create_library_dependencies_are_unique;
    case ~size:Large "kernel dependency walk snapshot" test_kernel_dependency_walk_snapshot;
    case ~size:Large "legacy kernel plan bundle is ignored after version bump" test_legacy_kernel_plan_bundle_is_ignored_after_version_bump;
    case
      ~size:Large
      "legacy krasny plan bundle with bad root module is ignored after version bump"
      test_legacy_krasny_plan_bundle_with_bad_root_module_is_ignored_after_version_bump;
  ]

let name = "Planner Package Planning Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()

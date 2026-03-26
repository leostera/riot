(** Package Planner - Plans individual packages with dependency-aware hashing *)

open Std
open Std.Collections
open Std.Iter
open Tusk_model
module G = Std.Graph.SimpleGraph

type plan_result =
  | Planned of {
      package_key : Package.key;
      package : Package.t;
      module_graph : Module_node.t G.t;
      action_graph : Action_graph.t;
      hash : Std.Crypto.hash;
      depset : Dependency.t list;
    }
  | MissingDependencies of { package : Package.t; missing : Package.t list }
  | FailedDependencies of { package : Package.t; failed : Package.t list }

type check_deps_error = Missing of Package.t list | Failed of Package.t list

let file_to_json (file : Module_node.file) =
  let open Std.Data.Json in
  match file with
  | Module_node.Concrete path ->
      Object
        [
          ("kind", String "concrete");
          ("path", String (Path.to_string path));
        ]
  | Module_node.Generated { path; contents } ->
      Object
        [
          ("kind", String "generated");
          ("path", String (Path.to_string path));
          ("contents", String contents);
        ]

let file_of_json json =
  let open Std.Data.Json in
  match json with
  | Object fields -> (
      match
        ( List.assoc_opt "kind" fields,
          List.assoc_opt "path" fields,
          List.assoc_opt "contents" fields )
      with
      | Some (String "concrete"), Some (String path), _ ->
          Ok (Module_node.Concrete (Path.v path))
      | Some (String "generated"), Some (String path), Some (String contents) ->
          Ok (Module_node.Generated { path = Path.v path; contents })
      | _ -> Error "invalid module file payload")
  | _ -> Error "module file must be an object"

let module_kind_to_json (kind : Module_node.kind) =
  let open Std.Data.Json in
  match kind with
  | Module_node.ML mod_ ->
      let ns =
        Module.module_name mod_ |> Module_name.namespace |> Namespace.to_list
      in
      Object
        [
          ("kind", String "ml");
          ("filename", String (Path.to_string (Module.filename mod_)));
          ("namespace", Array (List.map (fun s -> String s) ns));
        ]
  | Module_node.MLI mod_ ->
      let ns =
        Module.module_name mod_ |> Module_name.namespace |> Namespace.to_list
      in
      Object
        [
          ("kind", String "mli");
          ("filename", String (Path.to_string (Module.filename mod_)));
          ("namespace", Array (List.map (fun s -> String s) ns));
        ]
  | Module_node.C -> Object [ ("kind", String "c") ]
  | Module_node.H -> Object [ ("kind", String "h") ]
  | Module_node.Other s -> Object [ ("kind", String "other"); ("value", String s) ]
  | Module_node.Root -> Object [ ("kind", String "root") ]
  | Module_node.Native { files } ->
      Object
        [
          ("kind", String "native");
          ("files", Array (List.map (fun p -> String (Path.to_string p)) files));
        ]
  | Module_node.Library { name; includes } ->
      Object
        [
          ("kind", String "library");
          ("name", String name);
          ( "includes",
            Array (List.map (fun p -> String (Path.to_string p)) includes) );
        ]
  | Module_node.Binary { name; source; libraries; includes } ->
      Object
        [
          ("kind", String "binary");
          ("name", String name);
          ("source", String (Path.to_string source));
          ( "libraries",
            Array (List.map (fun p -> String (Path.to_string p)) libraries) );
          ( "includes",
            Array (List.map (fun p -> String (Path.to_string p)) includes) );
        ]

let parse_string_array = function
  | Std.Data.Json.Array xs ->
      List.fold_left
        (fun acc item ->
          match (acc, item) with
          | Error e, _ -> Error e
          | Ok items, Std.Data.Json.String s -> Ok (s :: items)
          | Ok _, _ -> Error "expected string array")
        (Ok []) xs
      |> Result.map List.rev
  | _ -> Error "expected array"

let module_kind_of_json json =
  let open Std.Data.Json in
  match json with
  | Object fields -> (
      match List.assoc_opt "kind" fields with
      | Some (String "ml") -> (
          match
            ( List.assoc_opt "filename" fields,
              List.assoc_opt "namespace" fields )
          with
          | Some (String filename), Some namespace_json -> (
              match parse_string_array namespace_json with
              | Ok ns ->
                  let mod_ =
                    Module.make ~namespace:(Namespace.of_list ns)
                      ~filename:(Path.v filename)
                  in
                  Ok (Module_node.ML mod_)
              | Error e -> Error e)
          | _ -> Error "invalid ml kind payload")
      | Some (String "mli") -> (
          match
            ( List.assoc_opt "filename" fields,
              List.assoc_opt "namespace" fields )
          with
          | Some (String filename), Some namespace_json -> (
              match parse_string_array namespace_json with
              | Ok ns ->
                  let mod_ =
                    Module.make ~namespace:(Namespace.of_list ns)
                      ~filename:(Path.v filename)
                  in
                  Ok (Module_node.MLI mod_)
              | Error e -> Error e)
          | _ -> Error "invalid mli kind payload")
      | Some (String "c") -> Ok Module_node.C
      | Some (String "h") -> Ok Module_node.H
      | Some (String "other") -> (
          match List.assoc_opt "value" fields with
          | Some (String v) -> Ok (Module_node.Other v)
          | _ -> Error "invalid other kind payload")
      | Some (String "root") -> Ok Module_node.Root
      | Some (String "native") -> (
          match List.assoc_opt "files" fields with
          | Some files_json -> (
              match parse_string_array files_json with
              | Ok files ->
                  Ok (Module_node.Native { files = List.map Path.v files })
              | Error e -> Error e)
          | None -> Error "invalid native kind payload")
      | Some (String "library") -> (
          match
            ( List.assoc_opt "name" fields,
              List.assoc_opt "includes" fields )
          with
          | Some (String name), Some includes_json -> (
              match parse_string_array includes_json with
              | Ok includes ->
                  Ok
                    (Module_node.Library
                       { name; includes = List.map Path.v includes })
              | Error e -> Error e)
          | _ -> Error "invalid library kind payload")
      | Some (String "binary") -> (
          match
            ( List.assoc_opt "name" fields,
              List.assoc_opt "source" fields,
              List.assoc_opt "libraries" fields,
              List.assoc_opt "includes" fields )
          with
          | ( Some (String name),
              Some (String source),
              Some libraries_json,
              Some includes_json ) -> (
              match
                (parse_string_array libraries_json, parse_string_array includes_json)
              with
              | Ok libraries, Ok includes ->
                  Ok
                    (Module_node.Binary
                       {
                         name;
                         source = Path.v source;
                         libraries = List.map Path.v libraries;
                         includes = List.map Path.v includes;
                       })
              | Error e, _ | _, Error e -> Error e)
          | _ -> Error "invalid binary kind payload")
      | _ -> Error "unknown module kind")
  | _ -> Error "module kind must be an object"

let module_graph_to_json (module_graph : Module_node.t G.t) =
  let open Std.Data.Json in
  let nodes =
    match G.topo_sort module_graph with
    | Ok nodes -> nodes
    | Error _ -> []
  in
  let node_to_json (node : Module_node.t G.node) =
    Object
      [
        ("id", Int (G.Node_id.to_int node.id));
        ("file", file_to_json node.value.file);
        ("kind", module_kind_to_json node.value.kind);
        ( "deps",
          Array (List.map (fun dep -> Int (G.Node_id.to_int dep)) node.deps) );
        ("opens", Array []);
      ]
  in
  Object [ ("nodes", Array (List.map node_to_json nodes)) ]

let module_graph_of_json json =
  let open Std.Data.Json in
  match json with
  | Object fields -> (
      match List.assoc_opt "nodes" fields with
      | Some (Array node_jsons) ->
          let graph = G.make () in
          let id_to_node : (int, Module_node.t G.node) HashMap.t = HashMap.create () in
          let pending_deps : (Module_node.t G.node * int list) vec = vec [] in
          let parse_int_array = function
            | Array xs ->
                List.fold_left
                  (fun acc item ->
                    match (acc, item) with
                    | Error e, _ -> Error e
                    | Ok items, Int i -> Ok (i :: items)
                    | Ok _, _ -> Error "expected int array")
                  (Ok []) xs
                |> Result.map List.rev
            | _ -> Error "expected int array"
          in
          let result =
            List.fold_left
              (fun acc node_json ->
                match acc with
                | Error _ -> acc
                | Ok () -> (
                    match node_json with
                    | Object node_fields -> (
                        match
                          ( List.assoc_opt "id" node_fields,
                            List.assoc_opt "file" node_fields,
                            List.assoc_opt "kind" node_fields,
                            List.assoc_opt "deps" node_fields )
                        with
                        | ( Some (Int legacy_id),
                            Some file_json,
                            Some kind_json,
                            Some deps_json ) -> (
                            match
                              ( file_of_json file_json,
                                module_kind_of_json kind_json,
                                parse_int_array deps_json )
                            with
                            | Ok file, Ok kind, Ok deps ->
                                let node_value : Module_node.t =
                                  { file; open_modules = []; kind }
                                in
                                let node = G.add_node graph node_value in
                                let _ = HashMap.insert id_to_node legacy_id node in
                                Vector.push pending_deps (node, deps);
                                Ok ()
                            | Error e, _, _ | _, Error e, _ | _, _, Error e ->
                                Error e)
                        | _ -> Error "invalid module node payload")
                    | _ -> Error "module node must be an object"))
              (Ok ()) node_jsons
          in
          (match result with
          | Error e -> Error e
          | Ok () ->
              Vector.into_iter pending_deps
              |> Iterator.to_list
              |> List.iter (fun (node, dep_ids) ->
                     List.iter
                       (fun dep_id ->
                         match HashMap.get id_to_node dep_id with
                         | Some dep_node -> G.add_edge node ~depends_on:dep_node
                         | None -> ())
                       dep_ids);
              Ok graph)
      | _ -> Error "missing module graph nodes")
  | _ -> Error "module graph payload must be an object"

let plan_bundle_to_json ~(package : Package.t) ~(module_graph : Module_node.t G.t)
    ~(action_graph : Action_graph.t) =
  Std.Data.Json.Object
    [
      ("version", Std.Data.Json.Int 1);
      ("package", Std.Data.Json.String package.name);
      ("module_graph", module_graph_to_json module_graph);
      ("action_graph", Action_graph.to_json action_graph);
    ]

let plan_bundle_of_json ~(package : Package.t) json =
  let open Std.Data.Json in
  match json with
  | Object fields -> (
      match
        ( List.assoc_opt "version" fields,
          List.assoc_opt "package" fields,
          List.assoc_opt "module_graph" fields,
          List.assoc_opt "action_graph" fields )
      with
      | Some (Int 1), Some (String pkg_name), Some module_graph_json, Some action_graph_json
        when String.equal pkg_name package.name ->
          (match
             (module_graph_of_json module_graph_json, Action_graph.from_json action_graph_json)
           with
          | Ok module_graph, Ok action_graph -> Ok (module_graph, action_graph)
          | Error e, _ | _, Error e -> Error e)
      | _ -> Error "invalid plan bundle shape")
  | _ -> Error "plan bundle must be a JSON object"

(** Compute input hash - fast path that doesn't require ocamldep.

    This hash includes:
    - Build context (host/target platform, session ID, resolved profile)
    - Package metadata (via Package.hash: name, deps, binaries, library, compiler config, 
      source files, foreign dependencies)
    - Workspace-specific dependency details (paths, library presence)
    - Dependency hashes (for transitive invalidation)

    It does NOT depend on:
    - Module graph (requires ocamldep)
    - Action graph (derived from module graph)

    If input_hash hasn't changed, we know the full hash is the same! *)
let compute_input_hash ~package ~depset ~workspace ~profile ~build_ctx =
  let module H = Std.Crypto.Sha256 in
  let state = H.create () in

  (* Planner artifact contract version.
     Bump this when planned output shapes or link-time artifact requirements
     change in ways that must invalidate cached package artifacts. *)
  H.write_string state "planner-artifacts:v2";

  (* Build context (includes resolved profile) *)
  Build_ctx.hash state build_ctx;
  
  (* Package metadata (includes compiler config overrides) *)
  Package.hash state package;
  
  (* Add workspace-specific dependency info not captured in package metadata *)
  let sorted_deps =
    List.sort
      (fun (a : Package.dependency) (b : Package.dependency) ->
        String.compare a.name b.name)
      (Package.build_graph_dependencies package)
  in
  List.iter
    (fun (dep : Package.dependency) ->
      (* Package.hash already includes dep name and source, we just add workspace-specific details *)
      match dep.source with
      | Package.Workspace -> (
          match
            List.find_opt
              (fun (p : Package.t) -> p.name = dep.name)
              workspace.Workspace.packages
          with
          | Some dep_pkg -> (
              H.write_string state (Path.to_string dep_pkg.path);
              match dep_pkg.library with
              | Some _ -> H.write_string state "true"
              | None -> H.write_string state "false")
          | None -> ())
      | Package.Path _ -> ())
    sorted_deps;

  (* Dependency hashes *)
  let dep_hashes =
    depset
    |> List.map (fun (dep : Dependency.t) -> dep.hash)
    |> List.sort Std.Crypto.Hash.compare
  in
  List.iter
    (fun hash -> H.write state (Kernel.Crypto.Hash.to_bytes hash))
    dep_hashes;

  H.finish state

let check_dependencies_built ~store ~package_graph ~package_key =
  let current_package_name, current_scope =
    match Package_graph.get_node_by_key package_graph package_key with
    | Some node ->
        ( (Package_graph.get_package node.value).Package.name,
          Package_graph.get_scope node.value )
    | None -> ("", Package_graph.Runtime)
  in
  let deps =
    match Package_graph.get_node_by_key package_graph package_key with
    | Some node -> Package_graph.get_dependencies_for_node package_graph node
    | None -> []
  in

  let unplanned = ref [] in
  let failed = ref [] in

  let rec summarize_dependency (node_value : Package_graph.package_node) :
      Dependency.t option =
    let pkg = Package_graph.get_package node_value in
    let is_ordering_only_self_dependency =
      String.equal pkg.Package.name current_package_name
      &&
      match (current_scope, Package_graph.get_scope node_value) with
      | Package_graph.Runtime, Package_graph.Build -> true
      | _ -> false
    in
    match node_value with
    | Package_graph.Unplanned _ ->
        (* Not yet planned - unplanned dependency *)
        unplanned := pkg :: !unplanned
        ;
        None
    | Package_graph.Planned { package; hash; _ }
    | Package_graph.Built { package; hash; _ } ->
        if is_ordering_only_self_dependency then None
        else
          let dep_nodes =
            match
              Package_graph.get_node_by_key package_graph
                (Package_graph.get_key node_value)
            with
            | Some node ->
                Package_graph.get_dependencies_for_node package_graph node
            | None -> []
          in
          let dep_depset = List.filter_map summarize_dependency dep_nodes in
          Some
            Dependency.
              {
                package;
                artifact_dir = Tusk_store.Store.hash_dir_of store hash;
                depset = dep_depset;
                hash;
              }
    | Package_graph.Failed _ ->
        (* Dependency failed to build *)
        failed := pkg :: !failed
        ;
        None
    | Package_graph.Skipped _ ->
        (* Dependency was skipped - treat as failed *)
        failed := pkg :: !failed
        ;
        None
  in

  let depset = List.filter_map summarize_dependency deps in

  (* Check the sets in order: failed takes precedence *)
  if !failed != [] then Error (Failed !failed)
  else if !unplanned != [] then Error (Missing !unplanned)
  else Ok depset

let plan_package ~workspace ~toolchain ~store ~package_graph ~package_key
    ~package ~build_ctx =
  match check_dependencies_built ~store ~package_graph ~package_key with
  | Error (Failed failed) -> Ok (FailedDependencies { package; failed })
  | Error (Missing missing) -> Ok (MissingDependencies { package; missing })
  | Ok depset ->
      (* Resolve profile for this package *)
      let base_profile = build_ctx.Build_ctx.profile in
      
      (* Apply package-level profile overrides based on current profile name *)
      (* Then apply target-specific overrides *)
      let profile = 
        let profile = Profile.apply_overrides base_profile package.compiler.profile_overrides in
        let target_platform = Build_ctx.target_platform_name build_ctx in
        Log.info ("Package " ^ package.name ^ ": looking for target." ^ target_platform ^ " overrides");
        Log.info ("Available targets: [" ^ (String.concat ", " (List.map fst package.compiler.target_overrides)) ^ "]");
        match List.assoc_opt target_platform package.compiler.target_overrides with
        | Some target_override -> (
            Log.info ("Found target." ^ target_platform ^ " override, applying...");
            match target_override.profile_override with
            | Some override -> 
                let result = Profile.apply_override profile override in
                Log.info ("After applying target override: cc_flags=[" ^ (String.concat ", " result.cc_flags) ^ "], ld_flags=[" ^ (String.concat ", " result.ld_flags) ^ "]");
                result
            | None -> profile)
        | None -> 
            Log.warn ("No target." ^ target_platform ^ " override found for package " ^ package.name);
            profile
      in
      
      let input_hash = compute_input_hash ~package ~depset ~workspace ~profile ~build_ctx in

      (match Tusk_store.Store.load_plan_bundle store ~hash:input_hash with
      | Some json -> (
          let parsed_bundle =
            try plan_bundle_of_json ~package json
            with exn ->
              Log.warn
                ("Package " ^ package.name
               ^ ": plan bundle decode raised exception, rebuilding plan graph ("
               ^ Exception.to_string exn ^ ")");
              Error "plan bundle decode exception"
          in
          match parsed_bundle with
          | Ok (module_graph, action_graph) ->
              Log.info ("Package " ^ package.name ^ ": plan bundle cache hit");
              Ok
                (Planned
                   {
                     package_key;
                     package;
                     module_graph;
                     action_graph;
                     hash = input_hash;
                     depset;
                   })
          | Error _ ->
              Log.warn
                ("Package " ^ package.name
               ^ ": plan bundle parse failed, rebuilding plan graph");
              let plan_input =
                Module_planner.
                  {
                    package;
                    profile;
                    ctx = build_ctx;
                    toolchain;
                    workspace;
                    planning_root = Path.v "src";
                    depset;
                    store;
                  }
              in

              match Module_planner.plan_node plan_input with
              | Error err -> Error err
              | Ok { sources; module_graph; action_graph } ->
                  (* Add foreign dependency build actions and make all other nodes depend on them *)
                  let foreign_nodes = List.map (fun (fdep : Package.foreign_dependency) ->
                    Log.info ("[PACKAGE_PLANNER] Adding foreign dependency: " ^ fdep.name ^ " with " ^ Int.to_string (List.length fdep.inputs) ^ " input files");
                    let foreign_action = Action.BuildForeignDependency {
                      name = fdep.name;
                      path = fdep.path;
                      build_cmd = fdep.build_cmd;
                      outputs = fdep.outputs;
                      env = fdep.env;
                    } in
                    let foreign_node = Action_node.make
                      ~actions:[foreign_action]
                      ~outs:fdep.outputs
                      ~srcs:[]  (* Foreign inputs are NOT copied to sandbox - they stay in their directory *)
                      ~package
                      ~toolchain
                      ~dependency_hashes:(fun _ -> Crypto.hash_string "")
                      ~deps:[]
                    in
                    Action_graph.add_node action_graph foreign_node
                  ) package.foreign_dependencies in

                  (* Make all existing nodes depend on foreign dependency nodes *)
                  if List.length foreign_nodes > 0 then (
                    let foreign_node_ids = List.map (fun (node : Action_node.t) -> node.id) foreign_nodes in
                    Log.info ("[PACKAGE_PLANNER] Making all action nodes depend on " ^ Int.to_string (List.length foreign_nodes) ^ " foreign dependencies");
                    let all_nodes = Action_graph.nodes action_graph in
                    Log.info ("[PACKAGE_PLANNER] Total action nodes (including foreign): " ^ Int.to_string (List.length all_nodes));

                    let dep_count = ref 0 in
                    List.iter (fun (node : Action_node.t) ->
                      (* Skip foreign dependency nodes themselves *)
                      let is_foreign_node = List.mem node.id foreign_node_ids in
                      if not is_foreign_node then (
                        (* Make this node depend on all foreign nodes *)
                        List.iter (fun foreign_node ->
                          Action_graph.add_dependency action_graph node ~depends_on:foreign_node;
                          dep_count := !dep_count + 1
                        ) foreign_nodes
                      )
                    ) all_nodes;
                    Log.info ("[PACKAGE_PLANNER] Added " ^ Int.to_string !dep_count ^ " dependency edges to foreign nodes")
                  );

                  let _ =
                    Tusk_store.Store.save_plan_bundle store ~hash:input_hash
                      ~plan:(plan_bundle_to_json ~package ~module_graph ~action_graph)
                  in
                  Ok
                    (Planned
                       {
                         package_key;
                         package;
                         module_graph;
                         action_graph;
                         hash = input_hash;
                         depset;
                       }))
      | None ->
      (* Always produce a concrete plan graph. The old fast path returned dummy
         empty graphs keyed off package-level artifact existence, which made
         planning correctness depend on execution-time cache state. *)
      Log.info ("Package " ^ package.name ^ ": computing plan graph");

        let plan_input =
          Module_planner.
            {
              package;
              profile;
              ctx = build_ctx;
              toolchain;
              workspace;
              planning_root = Path.v "src";
              depset;
              store;
            }
        in

        match Module_planner.plan_node plan_input with
        | Error err -> Error err
        | Ok { sources; module_graph; action_graph } ->
            (* Add foreign dependency build actions and make all other nodes depend on them *)
            let foreign_nodes = List.map (fun (fdep : Package.foreign_dependency) ->
              Log.info ("[PACKAGE_PLANNER] Adding foreign dependency: " ^ fdep.name ^ " with " ^ Int.to_string (List.length fdep.inputs) ^ " input files");
              let foreign_action = Action.BuildForeignDependency {
                name = fdep.name;
                path = fdep.path;
                build_cmd = fdep.build_cmd;
                outputs = fdep.outputs;
                env = fdep.env;
              } in
              let foreign_node = Action_node.make
                ~actions:[foreign_action]
                ~outs:fdep.outputs
                ~srcs:[]  (* Foreign inputs are NOT copied to sandbox - they stay in their directory *)
                ~package
                ~toolchain
                ~dependency_hashes:(fun _ -> Crypto.hash_string "")
                ~deps:[]
              in
              Action_graph.add_node action_graph foreign_node
            ) package.foreign_dependencies in
            
            (* Make all existing nodes depend on foreign dependency nodes *)
            if List.length foreign_nodes > 0 then (
              let foreign_node_ids = List.map (fun (node : Action_node.t) -> node.id) foreign_nodes in
              Log.info ("[PACKAGE_PLANNER] Making all action nodes depend on " ^ Int.to_string (List.length foreign_nodes) ^ " foreign dependencies");
              let all_nodes = Action_graph.nodes action_graph in
              Log.info ("[PACKAGE_PLANNER] Total action nodes (including foreign): " ^ Int.to_string (List.length all_nodes));
              
              let dep_count = ref 0 in
              List.iter (fun (node : Action_node.t) ->
                (* Skip foreign dependency nodes themselves *)
                let is_foreign_node = List.mem node.id foreign_node_ids in
                if not is_foreign_node then (
                  (* Make this node depend on all foreign nodes *)
                  List.iter (fun foreign_node ->
                    Action_graph.add_dependency action_graph node ~depends_on:foreign_node;
                    dep_count := !dep_count + 1
                  ) foreign_nodes
                )
              ) all_nodes;
              Log.info ("[PACKAGE_PLANNER] Added " ^ Int.to_string !dep_count ^ " dependency edges to foreign nodes")
            );

            (* Use input_hash as the package hash - it's deterministic and sufficient *)
            let _ =
              Tusk_store.Store.save_plan_bundle store ~hash:input_hash
                ~plan:(plan_bundle_to_json ~package ~module_graph ~action_graph)
            in
            Ok
              (Planned
                 {
                   package_key;
                   package;
                   module_graph;
                   action_graph;
                   hash = input_hash;
                   depset;
                 }))

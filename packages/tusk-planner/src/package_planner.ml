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

  let depset = List.filter_map (fun node -> summarize_dependency node.value) deps in

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

      if Tusk_store.Store.exists store input_hash then (
        (* Cache hit! Skip expensive planning - use dummy graphs *)
        Log.info
          ("Package " ^ package.name ^ ": fast path (input hash exists in cache, skipping \
           ocamldep)");
        Ok
          (Planned
             {
               package_key;
               package;
               module_graph = G.make ();
               action_graph = Action_graph.create ();
               hash = input_hash;
               depset;
             }))
      else (
        (* Cache miss - do full planning *)
        Log.info ("Package " ^ package.name ^ ": slow path (computing full hash with ocamldep)");

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

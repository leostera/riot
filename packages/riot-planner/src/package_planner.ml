(** Package Planner - Plans individual packages with dependency-aware hashing *)
open Std
open Std.Collections
open Std.Iter
open Riot_model
module G = Std.Graph.SimpleGraph

type plan_result =
  | Cached of {
      package_key: Package.key;
      package: Package.t;
      hash: Std.Crypto.hash;
      artifact: Riot_store.Artifact.t;
      depset: Dependency.t list;
      exports: Riot_store.Store.export_entry list;
      breakdown: planning_breakdown
    }
  | Planned of {
      package_key: Package.key;
      package: Package.t;
      module_graph: Module_node.t G.t;
      action_graph: Action_graph.t;
      hash: Std.Crypto.hash;
      depset: Dependency.t list;
      breakdown: planning_breakdown
    }
  | MissingDependencies of { package: Package.t; missing: Package.t list; breakdown: planning_breakdown }
  | FailedDependencies of { package: Package.t; failed: Package.t list; breakdown: planning_breakdown }

and planning_breakdown = {
  dependency_count: int;
  dependency_check_duration: Time.Duration.t;
  input_hash_duration: Time.Duration.t;
  artifact_lookup_duration: Time.Duration.t;
  artifact_cache_hit: bool;
  plan_bundle_lookup_duration: Time.Duration.t;
  plan_bundle_decode_duration: Time.Duration.t;
  plan_bundle_cache_hit: bool;
  module_plan_duration: Time.Duration.t;
}

let empty_breakdown = {
  dependency_count = 0;
  dependency_check_duration = Time.Duration.zero;
  input_hash_duration = Time.Duration.zero;
  artifact_lookup_duration = Time.Duration.zero;
  artifact_cache_hit = false;
  plan_bundle_lookup_duration = Time.Duration.zero;
  plan_bundle_decode_duration = Time.Duration.zero;
  plan_bundle_cache_hit = false;
  module_plan_duration = Time.Duration.zero;
}

type check_deps_error =
  Missing of Package.t list
  | Failed of Package.t list

let file_to_json = fun (file: Module_node.file) ->
  let open Std.Data.Json in
    match file with
    | Module_node.Concrete path -> Object [
      ("kind", String "concrete");
      ("path", String (Path.to_string path));
    ]
    | Module_node.Generated { path; contents } -> Object [
      ("kind", String "generated");
      ("path", String (Path.to_string path));
      ("contents", String contents);
    ]

let file_of_json = fun json ->
  let open Std.Data.Json in
    match json with
    | Object _ -> (
        match (
          get_field "kind" json,
          get_field "path" json,
          get_field "contents" json
        ) with
        | Some (String "concrete"), Some (String path), _ -> Ok (Module_node.Concrete (Path.v path))
        | Some (String "generated"), Some (String path), Some (String contents) -> Ok (Module_node.Generated {
          path = Path.v path;
          contents
        })
        | _ -> Error "invalid module file payload"
      )
    | _ -> Error "module file must be an object"

let module_kind_to_json = fun (kind: Module_node.kind) ->
  let open Std.Data.Json in
    match kind with
    | Module_node.ML mod_ ->
        let ns = Module.module_name mod_ |> Module_name.namespace |> Namespace.to_list in
        Object [
          ("kind", String "ml");
          ("filename", String (Path.to_string (Module.filename mod_)));
          ("namespace", Array (List.map ns ~fn:(fun s -> String s)));
        ]
    | Module_node.MLI mod_ ->
        let ns = Module.module_name mod_ |> Module_name.namespace |> Namespace.to_list in
        Object [
          ("kind", String "mli");
          ("filename", String (Path.to_string (Module.filename mod_)));
          ("namespace", Array (List.map ns ~fn:(fun s -> String s)));
        ]
    | Module_node.C ->
        Object [ ("kind", String "c") ]
    | Module_node.H ->
        Object [ ("kind", String "h") ]
    | Module_node.Other s ->
        Object [ ("kind", String "other"); ("value", String s) ]
    | Module_node.Root ->
        Object [ ("kind", String "root") ]
    | Module_node.Native { files } ->
        Object [
          ("kind", String "native");
          ("files", Array (List.map files ~fn:(fun p -> String (Path.to_string p))));
        ]
    | Module_node.Library { name; includes } ->
        Object [
          ("kind", String "library");
          ("name", String name);
          ("includes", Array (List.map includes ~fn:(fun p -> String (Path.to_string p))));
        ]
    | Module_node.Binary { name; source; libraries; includes } ->
        Object [
          ("kind", String "binary");
          ("name", String name);
          ("source", String (Path.to_string source));
          ("libraries", Array (List.map libraries ~fn:(fun p -> String (Path.to_string p))));
          ("includes", Array (List.map includes ~fn:(fun p -> String (Path.to_string p))));
        ]

let parse_string_array = function
  | Std.Data.Json.Array xs ->
      List.fold_left
        xs
        ~acc:(Ok [])
        ~fn:(fun acc item ->
          match (acc, item) with
          | Error e, _ -> Error e
          | Ok items, Std.Data.Json.String s -> Ok (s :: items)
          | Ok _, _ -> Error "expected string array")
      |> Result.map ~fn:List.reverse
  | _ -> Error "expected array"

let module_kind_of_json = fun json ->
  let open Std.Data.Json in
    match json with
    | Object _ -> (
        match get_field "kind" json with
        | Some (String "ml") -> (
            match (get_field "filename" json, get_field "namespace" json) with
            | Some (String filename), Some namespace_json -> (
                match parse_string_array namespace_json with
                | Ok ns ->
                    let mod_ = Module.make
                      ~namespace:(Namespace.of_list ns)
                      ~filename:(Path.v filename) in
                    Ok (Module_node.ML mod_)
                | Error e -> Error e
              )
            | _ -> Error "invalid ml kind payload"
          )
        | Some (String "mli") -> (
            match (get_field "filename" json, get_field "namespace" json) with
            | Some (String filename), Some namespace_json -> (
                match parse_string_array namespace_json with
                | Ok ns ->
                    let mod_ = Module.make
                      ~namespace:(Namespace.of_list ns)
                      ~filename:(Path.v filename) in
                    Ok (Module_node.MLI mod_)
                | Error e -> Error e
              )
            | _ -> Error "invalid mli kind payload"
          )
        | Some (String "c") ->
            Ok Module_node.C
        | Some (String "h") ->
            Ok Module_node.H
        | Some (String "other") -> (
            match get_field "value" json with
            | Some (String v) -> Ok (Module_node.Other v)
            | _ -> Error "invalid other kind payload"
          )
        | Some (String "root") ->
            Ok Module_node.Root
        | Some (String "native") -> (
            match get_field "files" json with
            | Some files_json -> (
                match parse_string_array files_json with
                | Ok files -> Ok (Module_node.Native { files = List.map files ~fn:Path.v })
                | Error e -> Error e
              )
            | None -> Error "invalid native kind payload"
          )
        | Some (String "library") -> (
            match (get_field "name" json, get_field "includes" json) with
            | Some (String name), Some includes_json -> (
                match parse_string_array includes_json with
                | Ok includes -> Ok (Module_node.Library {
                  name;
                  includes = List.map includes ~fn:Path.v
                })
                | Error e -> Error e
              )
            | _ -> Error "invalid library kind payload"
          )
        | Some (String "binary") -> (
            match (
              get_field "name" json,
              get_field "source" json,
              get_field "libraries" json,
              get_field "includes" json
            ) with
            | (Some (String name), Some (String source), Some libraries_json, Some includes_json) -> (
                match (parse_string_array libraries_json, parse_string_array includes_json) with
                | Ok libraries, Ok includes -> Ok (Module_node.Binary {
                  name;
                  source = Path.v source;
                  libraries = List.map libraries ~fn:Path.v;
                  includes = List.map includes ~fn:Path.v
                })
                | (Error e, _)
                | (_, Error e) -> Error e
              )
            | _ -> Error "invalid binary kind payload"
          )
        | _ ->
            Error "unknown module kind"
      )
    | _ -> Error "module kind must be an object"

let module_graph_to_json = fun (module_graph: Module_node.t G.t) ->
  let open Std.Data.Json in
    let nodes =
      match G.topo_sort module_graph with
      | Ok nodes -> nodes
      | Error _ -> []
    in
    let node_to_json (node: Module_node.t G.node) = Object [
      ("id", Int (G.Node_id.to_int node.id));
      ("file", file_to_json node.value.file);
      ("kind", module_kind_to_json node.value.kind);
      ("deps", Array (List.map node.deps ~fn:(fun dep -> Int (G.Node_id.to_int dep))));
      ("opens", Array []);
    ] in
    Object [ ("nodes", Array (List.map nodes ~fn:node_to_json)) ]

let module_graph_of_json = fun json ->
  let open Std.Data.Json in
    match json with
    | Object _ -> (
        match get_field "nodes" json with
        | Some (Array node_jsons) ->
            let graph = G.make () in
            let id_to_node: (int, Module_node.t G.node) HashMap.t = HashMap.create () in
            let pending_deps: (Module_node.t G.node * int list) vec = vec [] in
            let parse_int_array = function
              | Array xs ->
                  List.fold_left
                    xs
                    ~acc:(Ok [])
                    ~fn:(fun acc item ->
                      match (acc, item) with
                      | Error e, _ -> Error e
                      | Ok items, Int i -> Ok (i :: items)
                      | Ok _, _ -> Error "expected int array")
                  |> Result.map ~fn:List.reverse
              | _ -> Error "expected int array"
            in
            let result =
              List.fold_left
                node_jsons
                ~acc:(Ok ())
                ~fn:(fun acc node_json ->
                  match acc with
                  | Error _ -> acc
                  | Ok () -> (
                      match node_json with
                      | Object _ -> (
                          match (
                            get_field "id" node_json,
                            get_field "file" node_json,
                            get_field "kind" node_json,
                            get_field "deps" node_json
                          ) with
                          | (Some (Int legacy_id), Some file_json, Some kind_json, Some deps_json) -> (
                              match (
                                file_of_json file_json,
                                module_kind_of_json kind_json,
                                parse_int_array deps_json
                              ) with
                              | Ok file, Ok kind, Ok deps ->
                                  let node_value: Module_node.t = { file; open_modules = []; kind } in
                                  let node = G.add_node graph node_value in
                                  let _ = HashMap.insert id_to_node ~key:legacy_id ~value:node in
                                  Vector.push pending_deps ~value:(node, deps);
                                  Ok ()
                              | (Error e, _, _)
                              | (_, Error e, _)
                              | (_, _, Error e) -> Error e
                            )
                          | _ -> Error "invalid module node payload"
                        )
                      | _ -> Error "module node must be an object"
                    ))
            in
            (
              match result with
              | Error e -> Error e
              | Ok () ->
                  Vector.iter pending_deps |> Iterator.to_list |> List.for_each ~fn:(fun (node, dep_ids) ->
                      List.for_each (List.reverse dep_ids) ~fn:(fun dep_id ->
                          match HashMap.get id_to_node ~key:dep_id with
                          | Some dep_node -> G.add_edge node ~depends_on:dep_node
                          | None -> ()));
                  Ok graph
            )
        | _ -> Error "missing module graph nodes"
      )
    | _ -> Error "module graph payload must be an object"

let plan_bundle_to_json = fun ~(package:Package.t) ~(module_graph:Module_node.t G.t) ~(action_graph:Action_graph.t) ->
  Std.Data.Json.Object [
    ("version", Std.Data.Json.Int 1);
    ("package", Std.Data.Json.String (Package_name.to_string package.name));
    ("module_graph", module_graph_to_json module_graph);
    ("action_graph", Action_graph.to_json action_graph);
  ]

let plan_bundle_of_json = fun ~(package:Package.t) json ->
  let open Std.Data.Json in
    match json with
    | Object _ -> (
        match (
          get_field "version" json,
          get_field "package" json,
          get_field "module_graph" json,
          get_field "action_graph" json
        ) with
        | Some (Int 1), Some (String pkg_name), Some module_graph_json, Some action_graph_json when
          String.equal pkg_name (Package_name.to_string package.name) -> (
            match (module_graph_of_json module_graph_json, Action_graph.from_json action_graph_json) with
            | Ok module_graph, Ok action_graph -> Ok (module_graph, action_graph)
            | (Error e, _)
            | (_, Error e) -> Error e
          )
        | _ -> Error "invalid plan bundle shape"
      )
    | _ -> Error "plan bundle must be a JSON object"

(** Compute input hash - fast path that doesn't require dependency analysis.

    This hash includes:
    - Build context (host/target platform, session ID, resolved profile)
    - Package metadata (via Package.hash: name, deps, binaries, library, compiler config, 
      source files, foreign dependencies)
    - Workspace-specific dependency details (paths, library presence)
    - Dependency hashes (for transitive invalidation)

    It does NOT depend on:
    - Module graph (requires dependency analysis)
    - Action graph (derived from module graph)

    If input_hash hasn't changed, we know the full hash is the same! *)
let compute_input_hash = fun ~package ~depset ~workspace ~profile ~build_ctx ~toolchain ->
  let module H = Std.Crypto.Sha256 in
  let state = H.create () in
  (* Planner artifact contract version.
     Bump this when planned output shapes, action hashing, or link-time artifact
     requirements change. v15 invalidates stored foreign-action hashes that
     sorted argv and omitted field boundaries. *)
  H.write state "planner-artifacts:v15";
  (* Build context (includes resolved profile) *)
  Build_ctx.hash state build_ctx;
  (* Toolchain identity must participate in package cache invalidation so
     cross-compiled artifacts are rebuilt when the installed compiler/sysroot
     changes underneath the same target triple. *)
  H.write_hash state (Riot_toolchain.hash toolchain);
  (* Package metadata (includes compiler config overrides) *)
  Package.hash state package;
  (* Add workspace-specific dependency info not captured in package metadata *)
  let sorted_deps =
    List.sort
      (Package.build_graph_dependencies package)
      ~compare:(fun (a: Package.dependency) (b: Package.dependency) ->
        Package_name.compare a.name b.name)
  in
  List.for_each
    sorted_deps
    ~fn:(fun (dep: Package.dependency) ->
      (* Package.hash already includes dep name and source, we just add workspace-specific details *)
      match dep.source with
      | { Package.workspace=true; _ } -> (
          match
            List.find
              workspace.Workspace.packages
              ~fn:(fun (p: Package_manifest.t) ->
                Package_name.equal p.name dep.name)
          with
          | Some dep_pkg -> (
              H.write state (Path.to_string dep_pkg.path);
              match dep_pkg.library with
              | Some _ -> H.write_bool state true
              | None -> H.write_bool state false
            )
          | None -> ()
        )
      | { Package.builtin=true; _ } ->
          ()
      | _ ->
          ());
  (* Dependency hashes *)
  let dep_hashes = depset
  |> List.map ~fn:(fun (dep: Dependency.t) -> dep.hash)
  |> List.sort ~compare:Std.Crypto.Hash.compare in
  List.for_each dep_hashes ~fn:(fun hash -> H.write_hash state hash);
  H.finish state

let check_dependencies_built = fun ~store ~package_graph ~package_key ->
  let current_package_name, current_scope =
    match Package_graph.get_node_by_key package_graph package_key with
    | Some node -> (
      Package_name.to_string (Package_graph.get_package node.value).Package.name,
      Package_graph.get_scope node.value
    )
    | None -> ("", Package_graph.Runtime)
  in
  let deps =
    match Package_graph.get_node_by_key package_graph package_key with
    | Some node -> Package_graph.get_dependencies_for_node package_graph node
    | None -> []
  in
  let unplanned = ref [] in
  let failed = ref [] in
  let rec summarize_dependency: Package_graph.package_node -> Dependency.t option = fun node_value ->
    let pkg = Package_graph.get_package node_value in
    let is_ordering_only_self_dependency =
      String.equal (Package_name.to_string pkg.Package.name) current_package_name
      && match (current_scope, Package_graph.get_scope node_value) with
      | Package_graph.Runtime, Package_graph.Build -> true
      | _ -> false
    in
    match node_value with
    | Package_graph.Unplanned _ ->
        (* Not yet planned - unplanned dependency *)
        unplanned := pkg :: !unplanned;
        None
    | Package_graph.Planned { package; hash; _ } ->
        if is_ordering_only_self_dependency then
          None
        else if Riot_store.Store.exists store hash then
          let dep_nodes =
            match Package_graph.get_node_by_key package_graph (Package_graph.get_key node_value) with
            | Some node -> Package_graph.get_dependencies_for_node package_graph node
            | None -> []
          in
          let dep_depset = List.filter_map dep_nodes ~fn:summarize_dependency in
          Some Dependency.{
            package;
            artifact_dir = Riot_store.Store.hash_dir_of store hash;
            depset = dep_depset;
            hash
          }
        else (
          unplanned := pkg :: !unplanned;
          None
        )
    | Package_graph.Cached { package; hash; depset; _ } ->
        if is_ordering_only_self_dependency then
          None
        else
          Some Dependency.{
            package;
            artifact_dir = Riot_store.Store.hash_dir_of store hash;
            depset;
            hash
          }
    | Package_graph.Built { package; hash; _ } ->
        if is_ordering_only_self_dependency then
          None
        else
          let dep_nodes =
            match Package_graph.get_node_by_key package_graph (Package_graph.get_key node_value) with
            | Some node -> Package_graph.get_dependencies_for_node package_graph node
            | None -> []
          in
          let dep_depset = List.filter_map dep_nodes ~fn:summarize_dependency in
          Some Dependency.{
            package;
            artifact_dir = Riot_store.Store.hash_dir_of store hash;
            depset = dep_depset;
            hash
          }
    | Package_graph.Failed _ ->
        (* Dependency failed to build *)
        failed := pkg :: !failed;
        None
    | Package_graph.Skipped _ ->
        (* Dependency was skipped - treat as failed *)
        failed := pkg :: !failed;
        None
  in
  let depset = List.filter_map deps ~fn:summarize_dependency in
  (* Check the sets in order: failed takes precedence *)
  if !failed != [] then
    Error (Failed !failed)
  else if !unplanned != [] then
    Error (Missing !unplanned)
  else
    Ok depset

let plan_package = fun ~workspace ~toolchain ~store ~package_graph ~package_key ~package ~build_ctx ->
  let dependency_check_started_at = Time.Instant.now () in
  match check_dependencies_built ~store ~package_graph ~package_key with
  | Error (Failed failed) ->
      let dependency_check_duration =
        Time.Instant.duration_since ~earlier:dependency_check_started_at (Time.Instant.now ())
      in
      Ok (
        FailedDependencies {
          package;
          failed;
          breakdown = {
            empty_breakdown with
            dependency_count = List.length failed;
            dependency_check_duration;
          };
        }
      )
  | Error (Missing missing) ->
      let dependency_check_duration =
        Time.Instant.duration_since ~earlier:dependency_check_started_at (Time.Instant.now ())
      in
      Ok (
        MissingDependencies {
          package;
          missing;
          breakdown = {
            empty_breakdown with
            dependency_count = List.length missing;
            dependency_check_duration;
          };
        }
      )
  | Ok depset ->
      let dependency_check_duration =
        Time.Instant.duration_since ~earlier:dependency_check_started_at (Time.Instant.now ())
      in
      (* Resolve profile for this package *)
      let base_profile = build_ctx.Build_ctx.profile in
      (* Apply package-level profile overrides based on current profile name *)
      (* Then apply target-specific overrides *)
        let profile =
          let profile = Profile.apply_overrides base_profile package.compiler.profile_overrides in
          let target_platform = Build_ctx.target_platform_name build_ctx in
          Log.info
          ("Package "
          ^ Package_name.to_string package.name
          ^ ": looking for target."
          ^ target_platform
          ^ " overrides");
        Log.info
          ("Available targets: ["
          ^ (String.concat
            ", "
            (List.map package.compiler.target_overrides ~fn:(fun (target, _) -> target)))
          ^ "]");
        match
          List.find package.compiler.target_overrides
            ~fn:(fun (target, _) -> String.equal target target_platform)
        with
        | Some (_, target_override) -> (
            Log.info ("Found target." ^ target_platform ^ " override, applying...");
            match target_override.profile_override with
            | Some override ->
                let result = Profile.apply_override profile override in
                Log.info
                  ("After applying target override: cc_flags=["
                  ^ (String.concat ", " result.cc_flags)
                  ^ "], ld_flags=["
                  ^ (String.concat ", " result.ld_flags)
                  ^ "]");
                result
            | None -> profile
          )
        | None ->
            Log.warn
              ("No target."
              ^ target_platform
              ^ " override found for package "
              ^ Package_name.to_string package.name);
            profile
      in
      let input_hash_started_at = Time.Instant.now () in
      let input_hash = compute_input_hash ~package ~depset ~workspace ~profile ~build_ctx ~toolchain in
      let input_hash_duration =
        Time.Instant.duration_since ~earlier:input_hash_started_at (Time.Instant.now ())
      in
      let artifact_lookup_started_at = Time.Instant.now () in
      let cached_artifact =
        match Riot_store.Store.get store input_hash with
        | Some artifact -> Some (artifact, artifact.exports)
        | _ -> None
      in
      let artifact_lookup_duration =
        Time.Instant.duration_since ~earlier:artifact_lookup_started_at (Time.Instant.now ())
      in
      match cached_artifact with
      | Some (artifact, exports) ->
          Log.info
            ("Package "
            ^ Package_name.to_string package.name
            ^ ": cache hit via artifact + export metadata");
          Ok (
            Cached {
              package_key;
              package;
              hash = input_hash;
              artifact;
              depset;
              exports;
              breakdown = {
                empty_breakdown with
                dependency_count = List.length depset;
                dependency_check_duration;
                input_hash_duration;
                artifact_lookup_duration;
                artifact_cache_hit = true;
              };
            }
          )
      | None -> (
          let plan_bundle_lookup_started_at = Time.Instant.now () in
          match Riot_store.Store.load_plan_bundle store ~hash:input_hash with
          | Some json ->
              let plan_bundle_lookup_duration =
                Time.Instant.duration_since ~earlier:plan_bundle_lookup_started_at (Time.Instant.now ())
              in
              let plan_bundle_decode_started_at = Time.Instant.now () in
              let parsed_bundle =
                try plan_bundle_of_json ~package json with
                | exn ->
                    Log.warn
                      ("Package "
                      ^ Package_name.to_string package.name
                      ^ ": plan bundle decode raised exception, rebuilding plan graph ("
                      ^ Kernel.Exception.to_string exn
                      ^ ")");
                    Error "plan bundle decode exception"
              in
              let plan_bundle_decode_duration =
                Time.Instant.duration_since ~earlier:plan_bundle_decode_started_at (Time.Instant.now ())
              in
              (
                match parsed_bundle with
                | Ok (module_graph, action_graph) ->
                    Log.info
                      ("Package "
                      ^ Package_name.to_string package.name
                      ^ ": plan bundle cache hit");
                    Ok (
                      Planned {
                        package_key;
                        package;
                        module_graph;
                        action_graph;
                        hash = input_hash;
                        depset;
                        breakdown = {
                          empty_breakdown with
                          dependency_count = List.length depset;
                          dependency_check_duration;
                          input_hash_duration;
                          artifact_lookup_duration;
                          plan_bundle_lookup_duration;
                          plan_bundle_decode_duration;
                          plan_bundle_cache_hit = true;
                        };
                      }
                    )
                | Error _ ->
                    Log.warn
                      ("Package "
                      ^ Package_name.to_string package.name
                      ^ ": plan bundle parse failed, rebuilding plan graph");
                    let module_plan_started_at = Time.Instant.now () in
                    let plan_input =
                      Module_planner.{
                        package;
                        profile;
                        ctx = build_ctx;
                        toolchain;
                        workspace;
                        planning_root = Path.v "src";
                        allowed_source_files = package.sources.src;
                        root_mode =
                          Module_graph.Library_root {
                            library_name = Package_name.to_string package.name;
                          };
                        depset;
                        store;
                      }
                    in
                    match Module_planner.plan_node plan_input with
                    | Error err -> Error err
                    | Ok { sources; module_graph; analyzed_modules=_; action_graph } ->
                        (* Add foreign dependency build actions and make all other nodes depend on them *)
                        let foreign_nodes =
                          List.map
                            package.foreign_dependencies
                            ~fn:(fun (fdep: Package.foreign_dependency) ->
                              Log.info
                                ("[PACKAGE_PLANNER] Adding foreign dependency: "
                                ^ fdep.name
                                ^ " with "
                                ^ Int.to_string (List.length fdep.inputs)
                                ^ " input files");
                              let foreign_action = Action.BuildForeignDependency {
                                name = fdep.name;
                                path = fdep.path;
                                build_cmd = fdep.build_cmd;
                                outputs = fdep.outputs;
                                env = fdep.env;
                              }
                              in
                              let foreign_node =
                                Action_node.make
                                  ~actions:[ foreign_action ]
                                  ~outs:fdep.outputs
                                  ~srcs:[]
                                  ~package
                                  ~toolchain
                                  ~dependency_hashes:(fun _ -> Crypto.hash_string "")
                                  ~deps:[]
                              in
                              Action_graph.add_node action_graph foreign_node)
                        in
                        (* Make all existing nodes depend on foreign dependency nodes *)
                        if List.length foreign_nodes > 0 then
                          (
                            let foreign_node_ids =
                              List.map foreign_nodes ~fn:(fun (node: Action_node.t) -> node.id)
                            in
                            Log.info
                              ("[PACKAGE_PLANNER] Making all action nodes depend on "
                              ^ Int.to_string (List.length foreign_nodes)
                              ^ " foreign dependencies");
                            let all_nodes = Action_graph.nodes action_graph in
                            Log.info
                              ("[PACKAGE_PLANNER] Total action nodes (including foreign): "
                              ^ Int.to_string (List.length all_nodes));
                            let dep_count = ref 0 in
                            List.for_each
                              all_nodes
                              ~fn:(fun (node: Action_node.t) ->
                                let is_foreign_node = List.contains foreign_node_ids ~value:node.id in
                                if not is_foreign_node then
                                  List.for_each
                                    foreign_nodes
                                    ~fn:(fun foreign_node ->
                                      Action_graph.add_dependency action_graph node ~depends_on:foreign_node;
                                      dep_count := !dep_count + 1)
                              );
                            Log.info
                              ("[PACKAGE_PLANNER] Added " ^ Int.to_string !dep_count ^ " dependency edges to foreign nodes")
                          );
                        let _ = Riot_store.Store.save_plan_bundle
                          store
                          ~hash:input_hash
                          ~plan:(plan_bundle_to_json ~package ~module_graph ~action_graph) in
                        let module_plan_duration =
                          Time.Instant.duration_since ~earlier:module_plan_started_at (Time.Instant.now ())
                        in
                        Ok (
                          Planned {
                            package_key;
                            package;
                            module_graph;
                            action_graph;
                            hash = input_hash;
                            depset;
                            breakdown = {
                              empty_breakdown with
                              dependency_count = List.length depset;
                              dependency_check_duration;
                              input_hash_duration;
                              artifact_lookup_duration;
                              plan_bundle_lookup_duration;
                              plan_bundle_decode_duration;
                              module_plan_duration;
                            };
                          }
                        )
              )
          | None ->
              let plan_bundle_lookup_duration =
                Time.Instant.duration_since ~earlier:plan_bundle_lookup_started_at (Time.Instant.now ())
              in
              (* Always produce a concrete plan graph. The old fast path returned dummy
         empty graphs keyed off package-level artifact existence, which made
         planning correctness depend on execution-time cache state. *)
              Log.info
                ("Package "
                ^ Package_name.to_string package.name
                ^ ": computing plan graph");
              let module_plan_started_at = Time.Instant.now () in
              let plan_input =
                Module_planner.{
                  package;
                  profile;
                  ctx = build_ctx;
                  toolchain;
                  workspace;
                  planning_root = Path.v "src";
                  allowed_source_files = package.sources.src;
                  root_mode =
                    Module_graph.Library_root {
                      library_name = Package_name.to_string package.name;
                    };
                  depset;
                  store;
                }
              in
              match Module_planner.plan_node plan_input with
              | Error err -> Error err
              | Ok { sources; module_graph; analyzed_modules=_; action_graph } ->
                  (* Add foreign dependency build actions and make all other nodes depend on them *)
                  let foreign_nodes =
                    List.map
                      package.foreign_dependencies
                      ~fn:(fun (fdep: Package.foreign_dependency) ->
                        Log.info
                          ("[PACKAGE_PLANNER] Adding foreign dependency: "
                          ^ fdep.name
                          ^ " with "
                          ^ Int.to_string (List.length fdep.inputs)
                          ^ " input files");
                        let foreign_action = Action.BuildForeignDependency {
                          name = fdep.name;
                          path = fdep.path;
                          build_cmd = fdep.build_cmd;
                          outputs = fdep.outputs;
                          env = fdep.env;
                        }
                        in
                        let foreign_node =
                          Action_node.make
                            ~actions:[ foreign_action ]
                            ~outs:fdep.outputs
                            ~srcs:[]
                            ~package
                            ~toolchain
                            ~dependency_hashes:(fun _ -> Crypto.hash_string "")
                            ~deps:[]
                        in
                        Action_graph.add_node action_graph foreign_node)
                  in
                  (* Make all existing nodes depend on foreign dependency nodes *)
                  if List.length foreign_nodes > 0 then
                    (
                      let foreign_node_ids =
                        List.map foreign_nodes ~fn:(fun (node: Action_node.t) -> node.id)
                      in
                      Log.info
                        ("[PACKAGE_PLANNER] Making all action nodes depend on "
                        ^ Int.to_string (List.length foreign_nodes)
                        ^ " foreign dependencies");
                      let all_nodes = Action_graph.nodes action_graph in
                      Log.info
                        ("[PACKAGE_PLANNER] Total action nodes (including foreign): "
                        ^ Int.to_string (List.length all_nodes));
                      let dep_count = ref 0 in
                      List.for_each
                        all_nodes
                        ~fn:(fun (node: Action_node.t) ->
                          (* Skip foreign dependency nodes themselves *)
                          let is_foreign_node = List.contains foreign_node_ids ~value:node.id in
                          if not is_foreign_node then
                            (
                              (* Make this node depend on all foreign nodes *)
                              List.for_each
                                foreign_nodes
                                ~fn:(fun foreign_node ->
                                  Action_graph.add_dependency action_graph node ~depends_on:foreign_node;
                                  dep_count := !dep_count + 1)
                            )
                        );
                      Log.info
                        ("[PACKAGE_PLANNER] Added " ^ Int.to_string !dep_count ^ " dependency edges to foreign nodes")
                    );
                  let _ = Riot_store.Store.save_plan_bundle
                    store
                    ~hash:input_hash
                    ~plan:(plan_bundle_to_json ~package ~module_graph ~action_graph) in
                  let module_plan_duration =
                    Time.Instant.duration_since ~earlier:module_plan_started_at (Time.Instant.now ())
                  in
                  Ok (
                    Planned {
                      package_key;
                      package;
                      module_graph;
                      action_graph;
                      hash = input_hash;
                      depset;
                      breakdown = {
                        empty_breakdown with
                        dependency_count = List.length depset;
                        dependency_check_duration;
                        input_hash_duration;
                        artifact_lookup_duration;
                        plan_bundle_lookup_duration;
                        module_plan_duration;
                      };
                    }
                  )
            )

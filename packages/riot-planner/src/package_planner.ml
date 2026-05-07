(** Package Planner - Plans individual packages with dependency-aware hashing *)
open Std
open Std.Collections
open Std.Iter
open Riot_model

module G = Std.Graph.SimpleGraph

type plan_result =
  | Cached of {
      unit_key: Build_unit.key;
      package: Package.t;
      hash: Std.Crypto.hash;
      artifact: Riot_store.Artifact.t;
      depset: Dependency.t list;
      exports: Riot_store.Store.export_entry list;
      breakdown: planning_breakdown;
    }
  | Planned of {
      unit_key: Build_unit.key;
      package: Package.t;
      module_graph: Module_node.t G.t;
      action_graph: Action_graph.t;
      hash: Std.Crypto.hash;
      depset: Dependency.t list;
      breakdown: planning_breakdown;
    }

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

type input_hash_cache = {
  package_hashes_lock: Sync.Mutex.t;
  package_hashes: (string, package_hash_entry) HashMap.t;
  toolchain_hashes_lock: Sync.Mutex.t;
  toolchain_hashes: (string, Std.Crypto.hash) HashMap.t;
}

and package_hash_entry =
  | PackageHashReady of Std.Crypto.hash
  | PackageHashComputing of Sync.Condition.t

type cached_artifact_lookup =
  | Full_cached_artifact
  | Metadata_cached_artifact

let create_input_hash_cache = fun () -> {
  package_hashes_lock = Sync.Mutex.create ();
  package_hashes = HashMap.create ();
  toolchain_hashes_lock = Sync.Mutex.create ();
  toolchain_hashes = HashMap.create ();
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

let planner_trace_enabled = fun () ->
  match Env.get Env.String ~var:"RIOT_PLANNER_TRACE" with
  | Some ("1" | "true" | "yes") -> true
  | _ -> false

let duration_us = fun duration -> Time.Duration.to_micros duration

let trace_breakdown = fun ~package ~status breakdown ->
  if planner_trace_enabled () then
    eprintln
      ("riot-planner package="
      ^ Package_name.to_string package.Package.name
      ^ " status="
      ^ status
      ^ " deps="
      ^ Int.to_string breakdown.dependency_count
      ^ " dep_check_us="
      ^ Int.to_string (duration_us breakdown.dependency_check_duration)
      ^ " input_hash_us="
      ^ Int.to_string (duration_us breakdown.input_hash_duration)
      ^ " artifact_lookup_us="
      ^ Int.to_string (duration_us breakdown.artifact_lookup_duration)
      ^ " artifact_hit="
      ^ Bool.to_string breakdown.artifact_cache_hit
      ^ " plan_bundle_lookup_us="
      ^ Int.to_string (duration_us breakdown.plan_bundle_lookup_duration)
      ^ " plan_bundle_decode_us="
      ^ Int.to_string (duration_us breakdown.plan_bundle_decode_duration)
      ^ " plan_bundle_hit="
      ^ Bool.to_string breakdown.plan_bundle_cache_hit
      ^ " module_plan_us="
      ^ Int.to_string (duration_us breakdown.module_plan_duration))

let group_namespace = fun root ->
  if Path.equal root (Path.v "src") then
    Namespace.empty
  else
    Path.to_string root
    |> String.split ~by:"/"
    |> List.filter ~fn:(fun part -> not (String.is_empty part))
    |> List.map ~fn:String.capitalize_ascii
    |> Namespace.from_list

let planning_groups_for_package = fun (package: Package.t) ->
  let groups = [
    (Path.v "src", package.sources.src);
    (Path.v "tests", package.sources.tests);
    (Path.v "examples", package.sources.examples);
    (Path.v "bench", package.sources.bench);
  ]
  in
  List.filter_map
    groups
    ~fn:(fun (source_dir, allowed_source_files) ->
      if List.is_empty allowed_source_files then
        None
      else
        let root_mode =
          if Path.equal source_dir (Path.v "src") then
            match package.library with
            | Some _ ->
                Module_graph.Library_root {
                  library_name = Package_namespace.planning_library_name package;
                  public_root_name = Package_namespace.public_root package;
                }
            | None -> Module_graph.Loose_sources
          else
            Module_graph.Loose_sources
        in
        Some Module_graph.{
          source_dir;
          allowed_source_files;
          root_mode;
          namespace = group_namespace source_dir;
        })

let file_to_json = fun (file: Module_node.file) ->
  let open Std.Data.Json in
  match file with
  | Module_node.Concrete path ->
      Object [ ("kind", String "concrete"); ("path", String (Path.to_string path)); ]
  | Module_node.Generated { path; contents } ->
      Object [
        ("kind", String "generated");
        ("path", String (Path.to_string path));
        ("contents", String contents);
      ]

let file_of_json = fun json ->
  let open Std.Data.Json in
  match json with
  | Object _ -> (
      match (get_field "kind" json, get_field "path" json, get_field "contents" json) with
      | (Some (String "concrete"), Some (String path), _) -> Ok (Module_node.Concrete (Path.v path))
      | (Some (String "generated"), Some (String path), Some (String contents)) ->
          Ok (Module_node.Generated { path = Path.v path; contents })
      | _ -> Error "invalid module file payload"
    )
  | _ -> Error "module file must be an object"

let module_kind_to_json = fun (kind: Module_node.kind) ->
  let open Std.Data.Json in
  match kind with
  | Module_node.ML mod_ ->
      let ns =
        Module.module_name mod_
        |> Module_name.namespace
        |> Namespace.to_list
      in
      Object [
        ("kind", String "ml");
        ("filename", String (Path.to_string (Module.filename mod_)));
        ("namespace", Array (List.map ns ~fn:(fun s -> String s)));
      ]
  | Module_node.MLI mod_ ->
      let ns =
        Module.module_name mod_
        |> Module_name.namespace
        |> Namespace.to_list
      in
      Object [
        ("kind", String "mli");
        ("filename", String (Path.to_string (Module.filename mod_)));
        ("namespace", Array (List.map ns ~fn:(fun s -> String s)));
      ]
  | Module_node.C -> Object [ ("kind", String "c"); ]
  | Module_node.H -> Object [ ("kind", String "h"); ]
  | Module_node.Other s -> Object [ ("kind", String "other"); ("value", String s); ]
  | Module_node.Root -> Object [ ("kind", String "root"); ]
  | Module_node.Native { files } ->
      Object [
        ("kind", String "native");
        ("files", Array (List.map files ~fn:(fun p -> String (Path.to_string p))));
      ]
  | Module_node.PackageDependency { package_name; root_module } ->
      Object [
        ("kind", String "package_dependency");
        ("package_name", String (Package_name.to_string package_name));
        ("root_module", String root_module);
      ]
  | Module_node.Library { name; includes } ->
      Object [
        ("kind", String "library");
        ("name", String name);
        ("includes", Array (List.map includes ~fn:(fun p -> String (Path.to_string p))));
      ]
  | Module_node.Binary {
      name;
      source;
      libraries;
      includes;
    } ->
      Object [
        ("kind", String "binary");
        ("name", String name);
        ("source", String (Path.to_string source));
        ("libraries", Array (List.map libraries ~fn:(fun p -> String (Path.to_string p))));
        ("includes", Array (List.map includes ~fn:(fun p -> String (Path.to_string p))));
      ]

let parse_string_array = fun __tmp1 ->
  match __tmp1 with
  | Std.Data.Json.Array xs ->
      List.fold_left
        xs
        ~init:(Ok [])
        ~fn:(fun acc item ->
          match (acc, item) with
          | (Error e, _) -> Error e
          | (Ok items, Std.Data.Json.String s) -> Ok (s :: items)
          | (Ok _, _) -> Error "expected string array")
      |> Result.map ~fn:List.reverse
  | _ -> Error "expected array"

let module_kind_of_json = fun json ->
  let open Std.Data.Json in
  match json with
  | Object _ -> (
      match get_field "kind" json with
      | Some (String "ml") -> (
          match (get_field "filename" json, get_field "namespace" json) with
          | (Some (String filename), Some namespace_json) -> (
              match parse_string_array namespace_json with
              | Ok ns ->
                  let mod_ =
                    Module.make ~namespace:(Namespace.from_list ns) ~filename:(Path.v filename)
                  in
                  Ok (Module_node.ML mod_)
              | Error e -> Error e
            )
          | _ -> Error "invalid ml kind payload"
        )
      | Some (String "mli") -> (
          match (get_field "filename" json, get_field "namespace" json) with
          | (Some (String filename), Some namespace_json) -> (
              match parse_string_array namespace_json with
              | Ok ns ->
                  let mod_ =
                    Module.make ~namespace:(Namespace.from_list ns) ~filename:(Path.v filename)
                  in
                  Ok (Module_node.MLI mod_)
              | Error e -> Error e
            )
          | _ -> Error "invalid mli kind payload"
        )
      | Some (String "c") -> Ok Module_node.C
      | Some (String "h") -> Ok Module_node.H
      | Some (String "other") -> (
          match get_field "value" json with
          | Some (String v) -> Ok (Module_node.Other v)
          | _ -> Error "invalid other kind payload"
        )
      | Some (String "root") -> Ok Module_node.Root
      | Some (String "native") -> (
          match get_field "files" json with
          | Some files_json -> (
              match parse_string_array files_json with
              | Ok files -> Ok (Module_node.Native { files = List.map files ~fn:Path.v })
              | Error e -> Error e
            )
          | None -> Error "invalid native kind payload"
        )
      | Some (String "package_dependency") -> (
          match (get_field "package_name" json, get_field "root_module" json) with
          | (Some (String package_name), Some (String root_module)) -> (
              match Package_name.from_string package_name with
              | Ok package_name -> Ok (Module_node.PackageDependency { package_name; root_module })
              | Error err -> Error (Package_name.error_message err)
            )
          | _ -> Error "invalid package_dependency kind payload"
        )
      | Some (String "library") -> (
          match (get_field "name" json, get_field "includes" json) with
          | (Some (String name), Some includes_json) -> (
              match parse_string_array includes_json with
              | Ok includes ->
                  Ok (Module_node.Library { name; includes = List.map includes ~fn:Path.v })
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
              | (Ok libraries, Ok includes) ->
                  Ok (
                    Module_node.Binary {
                      name;
                      source = Path.v source;
                      libraries = List.map libraries ~fn:Path.v;
                      includes = List.map includes ~fn:Path.v;
                    }
                  )
              | (Error e, _)
              | (_, Error e) -> Error e
            )
          | _ -> Error "invalid binary kind payload"
        )
      | _ -> Error "unknown module kind"
    )
  | _ -> Error "module kind must be an object"

let module_graph_to_json = fun (module_graph: Module_node.t G.t) ->
  let open Std.Data.Json in
  let nodes =
    match G.topo_sort module_graph with
    | Ok nodes -> nodes
    | Error _ -> []
  in
  let node_to_json (node: Module_node.t G.node) =
    Object [
      ("id", Int (G.Node_id.to_int (G.id node)));
      ("file", file_to_json (G.value node).file);
      ("kind", module_kind_to_json (G.value node).kind);
      ("deps", Array (List.map (G.deps node) ~fn:(fun dep -> Int (G.Node_id.to_int dep))));
      ("opens", Array []);
    ]
  in
  Object [ ("nodes", Array (List.map nodes ~fn:node_to_json)); ]

let module_graph_of_json = fun json ->
  let open Std.Data.Json in
  match json with
  | Object _ -> (
      match get_field "nodes" json with
      | Some (Array node_jsons) ->
          let graph = G.make () in
          let id_to_node: (int, Module_node.t G.node) HashMap.t = HashMap.create () in
          let pending_deps: (Module_node.t G.node * int list) vec = vec [] in
          let parse_int_array = fun __tmp1 ->
            match __tmp1 with
            | Array xs ->
                List.fold_left
                  xs
                  ~init:(Ok [])
                  ~fn:(fun acc item ->
                    match (acc, item) with
                    | (Error e, _) -> Error e
                    | (Ok items, Int i) -> Ok (i :: items)
                    | (Ok _, _) -> Error "expected int array")
                |> Result.map ~fn:List.reverse
            | _ -> Error "expected int array"
          in
          let result =
            List.fold_left
              node_jsons
              ~init:(Ok ())
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
                            | (Ok file, Ok kind, Ok deps) ->
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
                Vector.iter pending_deps
                |> Iterator.to_list
                |> List.for_each
                  ~fn:(fun (node, dep_ids) ->
                    List.for_each
                      dep_ids
                      ~fn:(fun dep_id ->
                        match HashMap.get id_to_node ~key:dep_id with
                        | Some dep_node -> G.add_edge node ~depends_on:dep_node
                        | None -> ()));
                Ok graph
          )
      | _ -> Error "missing module graph nodes"
    )
  | _ -> Error "module graph payload must be an object"

let plan_bundle_to_json = fun
  ~(package:Package.t) ~(module_graph:Module_node.t G.t) ~(action_graph:Action_graph.t) ->
  Std.Data.Json.Object [
    ("version", Std.Data.Json.Int 1);
    ("package", Std.Data.Json.String (Package_name.to_string package.name));
    ("module_graph", module_graph_to_json module_graph);
    ("action_graph", Action_graph.to_json action_graph);
  ]

let validate_plan_bundle = fun ~(package:Package.t) action_graph ->
  let package_has_library =
    match package.library with
    | Some _ -> true
    | None -> false
  in
  let has_empty_create_library =
    Action_graph.to_action_list action_graph
    |> List.any
      ~fn:(fun action ->
        match action with
        | Action.CreateLibrary { objects = []; _ } -> true
        | _ -> false)
  in
  if package_has_library && has_empty_create_library then
    Error "plan bundle has a CreateLibrary action with no object inputs"
  else
    Ok ()

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
      | (Some (Int 1), Some (String pkg_name), Some module_graph_json, Some action_graph_json) when String.equal
        pkg_name
        (Package_name.to_string package.name) -> (
          match (module_graph_of_json module_graph_json, Action_graph.from_json action_graph_json) with
          | (Ok module_graph, Ok action_graph) -> (
              match validate_plan_bundle ~package action_graph with
              | Ok () -> Ok (module_graph, action_graph)
              | Error _ as err -> err
            )
          | (Error e, _)
          | (_, Error e) -> Error e
        )
      | _ -> Error "invalid plan bundle shape"
    )
  | _ -> Error "plan bundle must be a JSON object"

let compute_package_hash = fun package ->
  let state = Std.Crypto.Sha256.create () in
  Package.hash state package;
  Std.Crypto.Sha256.finish state

let compute_package_fingerprint = fun package ->
  let state = Std.Crypto.Sha256.create () in
  Package.hash_fingerprint state package;
  Std.Crypto.Sha256.finish state
  |> Std.Crypto.Digest.hex

let package_hash_cache_dir = fun (workspace: Workspace.t) ->
  Path.(workspace.target_dir_root / Path.v "planner" / Path.v "package-hashes")

let package_hash_cache_path = fun workspace fingerprint ->
  Path.(package_hash_cache_dir workspace / Path.v (fingerprint ^ ".hash"))

let read_persistent_package_hash = fun workspace fingerprint ->
  match Fs.read (package_hash_cache_path workspace fingerprint) with
  | Ok hash -> Some (String.trim hash)
  | Error _ -> None

let write_persistent_package_hash = fun workspace fingerprint hash ->
  let dir = package_hash_cache_dir workspace in
  match Fs.create_dir_all dir with
  | Error _ -> ()
  | Ok () -> ignore (Fs.write (hash ^ "\n") (package_hash_cache_path workspace fingerprint))

let hash_of_hex = fun hex ->
  let hex_nibble ch =
    match ch with
    | '0' .. '9' -> Some (Char.code ch - Char.code '0')
    | 'a' .. 'f' -> Some (10 + Char.code ch - Char.code 'a')
    | 'A' .. 'F' -> Some (10 + Char.code ch - Char.code 'A')
    | _ -> None
  in
  let len = String.length hex in
  if len = 0 || len mod 2 != 0 then
    None
  else
    let bytes = IO.Bytes.create ~size:(len / 2) in
    let rec loop index =
      if index >= len then
        Some (Std.Crypto.Hash.from_bytes bytes)
      else
        match (
          hex_nibble (String.get_unchecked hex ~at:index),
          hex_nibble (String.get_unchecked hex ~at:(index + 1))
        ) with
        | (Some hi, Some lo) ->
            IO.Bytes.set_unchecked
              bytes
              ~at:(index / 2)
              ~char:(Char.from_int_unchecked ((hi lsl 4) lor lo));
            loop (index + 2)
        | _ -> None
    in
    loop 0

let compute_or_load_package_hash = fun workspace package ->
  let fingerprint = compute_package_fingerprint package in
  match read_persistent_package_hash workspace fingerprint with
  | Some hash when not (String.is_empty hash) -> (
      match hash_of_hex hash with
      | Some hash -> hash
      | None -> compute_package_hash package
    )
  | _ ->
      let hash = compute_package_hash package in
      write_persistent_package_hash workspace fingerprint (Std.Crypto.Digest.hex hash);
      hash

let rec cached_package_hash = fun cache ~workspace ~key package ->
  Sync.Mutex.lock cache.package_hashes_lock;
  match HashMap.get cache.package_hashes ~key with
  | Some (PackageHashReady hash) ->
      Sync.Mutex.unlock cache.package_hashes_lock;
      hash
  | Some (PackageHashComputing condition) ->
      Sync.Condition.wait condition cache.package_hashes_lock;
      Sync.Mutex.unlock cache.package_hashes_lock;
      cached_package_hash cache ~workspace ~key package
  | None ->
      let condition = Sync.Condition.create () in
      ignore (HashMap.insert cache.package_hashes ~key ~value:(PackageHashComputing condition));
      Sync.Mutex.unlock cache.package_hashes_lock;
      let hash = compute_or_load_package_hash workspace package in
      Sync.Mutex.lock cache.package_hashes_lock;
      ignore (HashMap.insert cache.package_hashes ~key ~value:(PackageHashReady hash));
      Sync.Condition.broadcast condition;
      Sync.Mutex.unlock cache.package_hashes_lock;
      hash

let package_hash = fun input_hash_cache ~workspace ~key package ->
  match (input_hash_cache, key) with
  | (Some cache, Some key) -> cached_package_hash cache ~workspace ~key package
  | _ -> compute_or_load_package_hash workspace package

let cached_toolchain_hash = fun cache toolchain ->
  let key = Path.to_string (Riot_toolchain.path toolchain) in
  Sync.Mutex.lock cache.toolchain_hashes_lock;
  match HashMap.get cache.toolchain_hashes ~key with
  | Some hash ->
      Sync.Mutex.unlock cache.toolchain_hashes_lock;
      hash
  | None ->
      Sync.Mutex.unlock cache.toolchain_hashes_lock;
      let hash = Riot_toolchain.hash toolchain in
      Sync.Mutex.lock cache.toolchain_hashes_lock;
      ignore (HashMap.insert cache.toolchain_hashes ~key ~value:hash);
      Sync.Mutex.unlock cache.toolchain_hashes_lock;
      hash

let toolchain_hash = fun input_hash_cache toolchain ->
  match input_hash_cache with
  | Some cache -> cached_toolchain_hash cache toolchain
  | None -> Riot_toolchain.hash toolchain

let package_hash_key = fun (unit_key: Build_unit.key) ->
  String.concat
    ":"
    [
      Package_name.to_string unit_key.package;
      Build_unit.artifact_kind_to_string unit_key.artifact;
    ]

(**
   Compute input hash - fast path that doesn't require dependency analysis.

   This hash includes:
   - Build context (host/target platform, session ID, resolved profile)
   - Package metadata (via Package.hash: name, deps, binaries, library, compiler config,
     source files, foreign dependencies)
   - Workspace-specific dependency details (paths, library presence)
   - Dependency hashes (for transitive invalidation)

   It does NOT depend on:
   - Module graph (requires dependency analysis)
   - Action graph (derived from module graph)

   If input_hash hasn't changed, we know the full hash is the same!
*)
let compute_input_hash_with_cache = fun
  ?(planner_version = "planner-artifacts:v39")
  ?package_hash_key
  ~input_hash_cache
  ~package
  ~depset
  ~workspace
  ~profile
  ~build_ctx
  ~toolchain
  () ->
  let module H = Std.Crypto.Sha256 in
  let state = H.create () in
  (* Planner artifact contract version.
     Bump this when planned output shapes or link-time artifact requirements
     change in ways that must invalidate cached package artifacts.
  *)
  H.write state planner_version;
  H.write state (Package_namespace.mode_key ());
  (* Build context (includes resolved profile) *)
  Build_ctx.hash state build_ctx;
  (* Toolchain identity must participate in package cache invalidation so
     cross-compiled artifacts are rebuilt when the installed compiler/sysroot
     changes underneath the same target triple.
  *)
  H.write_hash state (toolchain_hash input_hash_cache toolchain);
  (* Package metadata (includes compiler config overrides) *)
  H.write_hash state (package_hash input_hash_cache ~workspace ~key:package_hash_key package);
  (* Add workspace-specific dependency info not captured in package metadata *)
  let sorted_deps =
    List.sort
      (Package.build_graph_dependencies package)
      ~compare:(fun (a: Package.dependency) (b: Package.dependency) ->
        Package_name.compare
          a.name
          b.name)
  in
  List.for_each
    sorted_deps
    ~fn:(fun (dep: Package.dependency) ->
      (* Package.hash already includes dep name and source, we just add workspace-specific details *)
      match dep.source with
      | { Package.workspace = true; _ } -> (
          match List.find
            workspace.Workspace.packages
            ~fn:(fun (p: Package_manifest.t) -> Package_name.equal p.name dep.name) with
          | Some dep_pkg -> (
              H.write state (Path.to_string dep_pkg.path);
              match dep_pkg.library with
              | Some _ -> H.write_bool state true
              | None -> H.write_bool state false
            )
          | None -> ()
        )
      | { Package.builtin = true; _ } -> ()
      | _ -> ());
  (* Dependency hashes *)
  let dep_output_hashes =
    depset
    |> List.map ~fn:(fun (dep: Dependency.t) -> dep.output_hash)
    |> List.sort ~compare:Std.Crypto.Hash.compare
  in
  List.for_each dep_output_hashes ~fn:(fun hash -> H.write_hash state hash);
  H.finish state

let compute_input_hash = fun ?planner_version ~package ~depset ~workspace ~profile ~build_ctx ~toolchain () ->
    compute_input_hash_with_cache
    ?planner_version
    ~input_hash_cache:None
    ~package
    ~depset
    ~workspace
    ~profile
    ~build_ctx
    ~toolchain
    ()

let native_object_output = fun path ->
  let path_string = Path.to_string path in
  if String.ends_with ~suffix:".c" path_string then
    Some (
      Path.remove_extension path
      |> Path.add_extension ~ext:"o"
      |> Path.basename
      |> Path.v
    )
  else
    None

let artifact_contains_file = fun (artifact: Riot_store.Artifact.t) path ->
  List.any
    artifact.files
    ~fn:(fun (entry: Riot_store.Manifest.file_entry) -> Path.equal entry.path path)

let native_object_outputs = fun package ->
  package.Package.sources.native
  |> List.filter_map ~fn:native_object_output

let missing_native_object_outputs_in_artifact = fun artifact outputs ->
  outputs
  |> List.filter ~fn:(fun output -> not (artifact_contains_file artifact output))

let artifact_path_exists = fun store input_hash path ->
  Fs.exists Path.(Riot_store.Store.hash_dir_of store input_hash / path)
  |> Result.unwrap_or ~default:false

let missing_native_object_outputs_in_store = fun store input_hash outputs ->
  outputs
  |> List.filter ~fn:(fun output -> not (artifact_path_exists store input_hash output))

let cached_artifact_is_complete = fun store package input_hash artifact ->
  let expected_outputs = native_object_outputs package in
  let missing =
    if List.is_empty expected_outputs then
      []
    else if List.is_empty artifact.Riot_store.Artifact.files then
      missing_native_object_outputs_in_store store input_hash expected_outputs
    else
      missing_native_object_outputs_in_artifact artifact expected_outputs
  in
  match missing with
  | [] -> true
  | missing ->
      Log.warn
        ("Package "
        ^ Package_name.to_string package.Package.name
        ^ ": ignoring cached artifact missing native object outputs: "
        ^ String.concat ", " (List.map missing ~fn:Path.to_string));
      false

let load_cached_artifact = fun lookup store package input_hash ->
  match lookup with
  | Full_cached_artifact -> Riot_store.Store.get_package store input_hash
  | Metadata_cached_artifact -> Riot_store.Store.get_package_metadata store input_hash

let plan_package_after_dependencies = fun
  ~on_source_analyzed
  ~input_hash_cache
  ~cached_artifact_lookup
  ~workspace
  ~toolchain
  ~store
  ~unit_key
  ~(package:Package.t)
  ~depset
  ~dependency_check_duration
  ~build_ctx ->
  (* Resolve profile for this package *)
  let base_profile = build_ctx.Build_ctx.profile in
  (* Apply package-level profile overrides based on current profile name *)
  (* Then apply target-specific overrides *)
  let profile =
    let profile = Profile.apply_overrides base_profile package.compiler.profile_overrides in
    let target_platform = Build_ctx.target_platform_name build_ctx in
    match List.find
      package.compiler.target_overrides
      ~fn:(fun (target, _) -> String.equal target target_platform) with
    | Some (_, target_override) -> (
        match target_override.profile_override with
        | Some override ->
            Profile.apply_override profile override
        | None -> profile
      )
    | None -> profile
  in
  let input_hash_started_at = Time.Instant.now () in
  let input_hash =
    compute_input_hash_with_cache
      ~input_hash_cache
      ~package_hash_key:(package_hash_key unit_key)
      ~package
      ~depset
      ~workspace
      ~profile
      ~build_ctx
      ~toolchain
      ()
  in
  let input_hash_duration =
    Time.Instant.duration_since ~earlier:input_hash_started_at (Time.Instant.now ())
  in
  let artifact_lookup_started_at = Time.Instant.now () in
  let cached_artifact =
    let artifact = load_cached_artifact cached_artifact_lookup store package input_hash in
    match artifact with
    | Some artifact when cached_artifact_is_complete store package input_hash artifact ->
        Some (artifact, artifact.exports)
    | Some _ -> None
    | _ -> None
  in
  let artifact_lookup_duration =
    Time.Instant.duration_since ~earlier:artifact_lookup_started_at (Time.Instant.now ())
  in
  match cached_artifact with
  | Some (artifact, exports) ->
      let breakdown = {
        empty_breakdown with
        dependency_count = List.length depset;
        dependency_check_duration;
        input_hash_duration;
        artifact_lookup_duration;
        artifact_cache_hit = true;
      }
      in
      trace_breakdown ~package ~status:"cached" breakdown;
      Ok (
        Cached {
          unit_key;
          package;
          hash = input_hash;
          artifact;
          depset;
          exports;
          breakdown;
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
                  ^ Exception.to_string exn
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
                  ("Package " ^ Package_name.to_string package.name ^ ": plan bundle cache hit");
                Ok (
                  Planned {
                    unit_key;
                    package;
                    module_graph;
                    action_graph;
                    hash = input_hash;
                    depset;
                    breakdown =
                      {
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
                    source_groups = planning_groups_for_package package;
                    depset;
                    dependency_packages = List.map
                      (Dependency.transitive_closure depset)
                      ~fn:(fun (dep: Dependency.t) -> dep.package);
                    store;
                    on_source_analyzed;
                  }
                in
                match Module_planner.plan_node plan_input with
                | Error err -> Error err
                | Ok {
                    sources;
                    module_graph;
                    analyzed_modules = _;
                    action_graph;
                  } ->
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
                    if List.length foreign_nodes > 0 then (
                      let foreign_node_ids =
                        List.map foreign_nodes ~fn:(fun (node: Action_node.t) -> G.id node)
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
                          let is_foreign_node = List.contains foreign_node_ids ~value:(G.id node) in
                          if not is_foreign_node then
                            List.for_each
                              foreign_nodes
                              ~fn:(fun foreign_node ->
                                Action_graph.add_dependency
                                  action_graph
                                  node
                                  ~depends_on:foreign_node;
                                dep_count := !dep_count + 1));
                      Log.info
                        ("[PACKAGE_PLANNER] Added "
                        ^ Int.to_string !dep_count
                        ^ " dependency edges to foreign nodes")
                    );
                    let _ =
                      Riot_store.Store.save_plan_bundle
                        store
                        ~hash:input_hash
                        ~plan:(plan_bundle_to_json ~package ~module_graph ~action_graph)
                    in
                    let module_plan_duration =
                      Time.Instant.duration_since
                        ~earlier:module_plan_started_at
                        (Time.Instant.now ())
                    in
                    Ok (
                      Planned {
                        unit_key;
                        package;
                        module_graph;
                        action_graph;
                        hash = input_hash;
                        depset;
                        breakdown =
                          {
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
             planning correctness depend on execution-time cache state.
          *)
          Log.info ("Package " ^ Package_name.to_string package.name ^ ": computing plan graph");
          let module_plan_started_at = Time.Instant.now () in
          let plan_input =
            Module_planner.{
              package;
              profile;
              ctx = build_ctx;
              toolchain;
              workspace;
              source_groups = planning_groups_for_package package;
              depset;
              dependency_packages = List.map
                (Dependency.transitive_closure depset)
                ~fn:(fun (dep: Dependency.t) -> dep.package);
              store;
              on_source_analyzed;
            }
          in
          match Module_planner.plan_node plan_input with
          | Error err -> Error err
          | Ok {
              sources;
              module_graph;
              analyzed_modules = _;
              action_graph;
            } ->
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
              if List.length foreign_nodes > 0 then (
                let foreign_node_ids =
                  List.map foreign_nodes ~fn:(fun (node: Action_node.t) -> G.id node)
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
                    let is_foreign_node = List.contains foreign_node_ids ~value:(G.id node) in
                    if not is_foreign_node then (
                      (* Make this node depend on all foreign nodes *)
                      List.for_each
                        foreign_nodes
                        ~fn:(fun foreign_node ->
                          Action_graph.add_dependency action_graph node ~depends_on:foreign_node;
                          dep_count := !dep_count + 1)
                    ));
                Log.info
                  ("[PACKAGE_PLANNER] Added "
                  ^ Int.to_string !dep_count
                  ^ " dependency edges to foreign nodes")
              );
              let _ =
                Riot_store.Store.save_plan_bundle
                  store
                  ~hash:input_hash
                  ~plan:(plan_bundle_to_json ~package ~module_graph ~action_graph)
              in
              let module_plan_duration =
                Time.Instant.duration_since ~earlier:module_plan_started_at (Time.Instant.now ())
              in
              Ok (
                Planned {
                  unit_key;
                  package;
                  module_graph;
                  action_graph;
                  hash = input_hash;
                  depset;
                  breakdown =
                    {
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

let plan_build_unit_with_cache = fun
  ~on_source_analyzed
  ~input_hash_cache
  ~workspace
  ~toolchain
  ~store
  ~(unit:Build_unit.t)
  ~depset
  ~build_ctx ->
  plan_package_after_dependencies
    ~on_source_analyzed
    ~input_hash_cache:(Some input_hash_cache)
    ~cached_artifact_lookup:Metadata_cached_artifact
    ~workspace
    ~toolchain
    ~store
    ~unit_key:(Build_unit.key unit)
    ~package:(Build_unit.package unit)
    ~depset
    ~dependency_check_duration:Time.Duration.zero
    ~build_ctx

let plan_build_unit = fun
  ~on_source_analyzed ~workspace ~toolchain ~store ~(unit:Build_unit.t) ~depset ~build_ctx ->
  plan_package_after_dependencies
    ~on_source_analyzed
    ~input_hash_cache:None
    ~cached_artifact_lookup:Full_cached_artifact
    ~workspace
    ~toolchain
    ~store
    ~unit_key:(Build_unit.key unit)
    ~package:(Build_unit.package unit)
    ~depset
    ~dependency_check_duration:Time.Duration.zero
    ~build_ctx

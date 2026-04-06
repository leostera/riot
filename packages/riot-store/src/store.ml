open Std
open Std.Iter
open Std.Collections
open Riot_model
module ContentStore = Contentstore.Store

(** Store - Content-addressable storage for build artifacts **)
module Manifest = Manifest

type t = {
  content_store: ContentStore.t;
}

type error = string

type export_entry = Manifest.export_entry = {
  name: string;
  path: Path.t;
  action_hash: string;
}

(** Create a store rooted at a specific build lane *)
let create_for_lane = fun ~(workspace:Workspace.t) ~profile ~target ->
  let store_dir = Path.(workspace.target_dir_root / Path.v profile / Path.v target / Path.v "cache") in
  { content_store = ContentStore.create ~root_dir:store_dir }

(** Create a new store for the given workspace *)
let create = fun ~(workspace:Workspace.t) ->
  create_for_lane ~workspace ~profile:"debug" ~target:(Riot_dirs.host_target ())

(** Get the path for a given hash in the store *)
let get_hash_dir = fun store hash -> ContentStore.hash_dir_of store.content_store hash

let manifest_path = fun hash_dir -> Path.(hash_dir / Path.v "manifest.json")

let manifest_cache_key = fun hash -> Std.Crypto.Digest.hex hash

(** Check if artifacts for a given hash exist in the store *)
let exists = fun store hash ->
  let hash_dir = get_hash_dir store hash in
  match Fs.exists hash_dir with
  | Ok true -> (
      match Fs.exists (manifest_path hash_dir) with
      | Ok true -> true
      | Ok false
      | Error _ -> false
    )
  | Ok false
  | Error _ -> false

(** Promote artifacts from store to target directory *)
let promote = fun store hash ~target_dir ->
  let hash_dir = get_hash_dir store hash in
  match Manifest.load ~path:(manifest_path hash_dir) with
  | Ok manifest ->
      Fs.create_dir_all target_dir
      |> Result.expect ~msg:(("Failed to create target directory: " ^ Path.to_string target_dir));
      List.iter
        (fun (entry: Manifest.file_entry) ->
          let src = Path.(hash_dir / entry.path) in
          let dst = Path.(target_dir / entry.path) in
          let dst_parent = Path.dirname dst in
          Fs.create_dir_all dst_parent
          |> Result.expect ~msg:(("Failed to create parent directory: " ^ Path.to_string dst_parent));
          Fs.copy ~src ~dst
          |> Result.expect
            ~msg:(("Failed to copy file: " ^ Path.to_string src ^ " -> " ^ Path.to_string dst)))
        manifest.files;
      Ok ()
  | Error _ -> Error "Hash not found in store"

(** Store artifacts from sandbox to content-addressable store *)
let store_artifacts = fun store ~package ?(ocamlc_warnings = []) ?(exports = []) hash sandbox_dir declared_outputs ->
  let hash_dir = get_hash_dir store hash in
  let temp_dir =
    let nanos = Time.SystemTime.duration_since_epoch () |> Time.Duration.to_nanos in
    let temp_name = Std.Crypto.Digest.hex hash ^ ".tmp." ^ Int64.to_string nanos in
    Path.(ContentStore.root_dir store.content_store / Path.v temp_name)
  in
  Fs.create_dir_all temp_dir
  |> Result.expect ~msg:(("Failed to create temp directory: " ^ Path.to_string temp_dir));
  (* Copy declared outputs to store and track what was actually stored *)
  let stored_files_with_sizes =
    List.fold_left
      (fun acc output_file ->
        let src = Path.(sandbox_dir / Path.v output_file) in
        match Fs.exists src with
        | Ok true ->
            let dst = Path.(temp_dir / Path.v output_file) in
            let dst_parent = Path.dirname dst in
            Fs.create_dir_all dst_parent
            |> Result.expect
              ~msg:(("Failed to create parent directory: " ^ Path.to_string dst_parent));
            Fs.copy ~src ~dst
            |> Result.expect
              ~msg:(("Failed to store artifact: " ^ Path.to_string src ^ " -> " ^ Path.to_string dst));
            let size = Fs.metadata dst
            |> Result.expect ~msg:(("Failed to get metadata for " ^ Path.to_string dst))
            |> Fs.Metadata.len in
            (Path.v output_file, size) :: acc
        | _ -> acc)
      []
      declared_outputs
  in
  (* Create and save manifest *)
  let manifest = Manifest.create
    ~base_dir:temp_dir
    ~ocamlc_warnings
    ~exports
    ()
    ~package
    ~build_hash:(Std.Crypto.Digest.hex hash)
    ~files:(List.rev stored_files_with_sizes) in
  Manifest.save manifest ~path:(manifest_path temp_dir) |> Result.expect ~msg:"Failed to save manifest";
  let commit_result =
    ContentStore.commit_dir store.content_store ~hash ~source_dir:temp_dir
  in
  (
    match commit_result with
    | Ok () -> ()
    | Error _ ->
        if exists store hash then
          ()
        else
          panic
            ("Failed to move temp artifact dir into place: "
            ^ Path.to_string temp_dir
            ^ " -> "
            ^ Path.to_string hash_dir)
  );
  (
    match Fs.exists temp_dir with
    | Ok true ->
        let _ = Fs.remove_dir_all temp_dir in
        ()
    | Ok false
    | Error _ -> ()
  );
  (* Return artifact witness with just the filenames *)
  let stored_files =
    List.map (fun ((path, _)) -> path) stored_files_with_sizes
  in
  Artifact.{ hash; files = List.rev stored_files; ocamlc_warnings; exports }

let export_source_path = fun store (entry: export_entry) ->
  if Path.is_absolute entry.path then
    None
  else
    Some Path.(ContentStore.root_dir store.content_store / Path.v entry.action_hash / entry.path)

let load_manifest = fun store ~hash ->
  match Manifest.load ~path:(manifest_path (get_hash_dir store hash)) with
  | Ok manifest -> Some manifest
  | Error _ -> None

let path_exists = fun path -> Fs.exists path |> Result.unwrap_or ~default:false

let manifest_files_exist = fun store ~hash (manifest: Manifest.t) ->
  let hash_dir = get_hash_dir store hash in
  List.for_all
    (fun (entry: Manifest.file_entry) -> path_exists Path.(hash_dir / entry.path))
    manifest.files

let manifest_exports_exist = fun store (manifest: Manifest.t) ->
  List.for_all
    (fun (entry: Manifest.export_entry) ->
      match export_source_path store entry with
      | Some path -> path_exists path
      | None -> false)
    manifest.exports

(** Simple interface - check if we have cached artifacts for a hash *)
let get = fun store hash ->
  match load_manifest store ~hash with
  | Some manifest ->
      if manifest_files_exist store ~hash manifest && manifest_exports_exist store manifest then
        let files =
          List.map (fun (entry: Manifest.file_entry) -> entry.path) manifest.files
        in
        Some Artifact.{
          hash;
          files;
          ocamlc_warnings = manifest.ocamlc_warnings;
          exports = manifest.exports
        }
      else
        None
  | None -> None

(** Save build outputs to the store *)
let save = fun ?(ocamlc_warnings = []) ?(exports = []) store ~package ~hash ~sandbox_dir ~outs ->
  let sandbox_str = Path.to_string sandbox_dir in
  let sandbox_len = String.length sandbox_str in
  let outs_str =
    List.map
      (fun out_path ->
        let out_str = Path.to_string out_path in
        if String.starts_with ~prefix:sandbox_str out_str then
          let relative_start = sandbox_len + 1 in
          String.sub out_str relative_start (String.length out_str - relative_start)
        else
          Path.to_string out_path)
      outs
  in
  let artifact = store_artifacts store ~package ~ocamlc_warnings ~exports hash sandbox_dir outs_str in
  Ok artifact

(** Promote cached artifacts to target directory *)
let promote_artifact = fun store artifact ~target_dir -> promote store Artifact.(artifact.hash) ~target_dir

(** Get absolute paths to artifact files in immutable cache *)
let get_artifact_paths = fun store artifact ->
  let hash_dir = get_hash_dir store Artifact.(artifact.hash) in
  List.map (fun rel_path -> Path.(hash_dir / rel_path)) Artifact.(artifact.files)

(** Get the cache directory containing an artifact's files *)
let get_artifact_dir = fun store artifact -> get_hash_dir store Artifact.(artifact.hash)

let hash_dir_of = fun store hash -> get_hash_dir store hash

let save_plan_bundle = fun store ~hash ~plan ->
  ContentStore.save_json_bundle
    store.content_store
    ~namespace:"plans"
    ~hash
    ~json:plan

let load_plan_bundle = fun store ~hash ->
  ContentStore.load_json_bundle store.content_store ~namespace:"plans" ~hash

let materialize_package_exports = fun store ~exports ~target_dir ->
  Fs.create_dir_all target_dir
  |> Result.expect ~msg:(("Failed to create package output directory: " ^ Path.to_string target_dir));
  let copy_one (entry: export_entry) =
    match export_source_path store entry with
    | None -> Error ("Export path must be relative: " ^ Path.to_string entry.path)
    | Some src ->
        let dst = Path.(target_dir / Path.v entry.name) in
        match Fs.exists src with
        | Ok true -> Fs.copy ~src ~dst
        |> Result.map_error
          (fun _ -> "Failed to copy export: " ^ Path.to_string src ^ " -> " ^ Path.to_string dst)
        | Ok false
        | Error _ -> Error ("Export source is missing from the store: " ^ Path.to_string src ^ " (cache is corrupted; try `riot clean`)")
  in
  List.fold_left
    (fun acc entry ->
      match acc with
      | Error _ -> acc
      | Ok () -> copy_one entry)
    (Ok ())
    exports

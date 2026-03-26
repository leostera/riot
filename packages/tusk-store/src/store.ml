open Std
open Std.Iter
open Std.Collections

open Tusk_model
(** Store - Content-addressable storage for build artifacts **)

module Manifest = Manifest

type t = { root_dir : Path.t (* Root directory for the store *) }
type error = string

(** Create a new store for the given workspace *)
let create ~(workspace : Workspace.t) =
  let store_dir = Tusk_dirs.cache_dir ~workspace_root:workspace.root in
  Fs.create_dir_all store_dir
  |> Result.expect
       ~msg:
         ("Failed to create store directory: " ^ Path.to_string store_dir);
  { root_dir = store_dir }

(** Get the path for a given hash in the store *)
let get_hash_dir store hash =
  Path.(store.root_dir / Path.v (Std.Crypto.Digest.hex hash))

let manifest_path hash_dir = Path.(hash_dir / Path.v "manifest.json")

(** Check if artifacts for a given hash exist in the store *)
let exists store hash =
  let hash_dir = get_hash_dir store hash in
  match Fs.exists hash_dir with
  | Ok true -> (
      match Fs.exists (manifest_path hash_dir) with
      | Ok true -> true
      | Ok false | Error _ -> false)
  | Ok false | Error _ -> false

(** Promote artifacts from store to target directory *)
let promote store hash ~target_dir =
  let hash_dir = get_hash_dir store hash in
  match Manifest.load ~path:(manifest_path hash_dir) with
  | Ok manifest ->
      Fs.create_dir_all target_dir
      |> Result.expect
           ~msg:
             ("Failed to create target directory: " ^ Path.to_string target_dir);
      List.iter
        (fun (entry : Manifest.file_entry) ->
          let src = Path.(hash_dir / entry.path) in
          let dst = Path.(target_dir / entry.path) in
          let dst_parent = Path.dirname dst in
          Fs.create_dir_all dst_parent
          |> Result.expect
               ~msg:
                 ("Failed to create parent directory: " ^ Path.to_string dst_parent);
          Fs.copy ~src ~dst
          |> Result.expect
               ~msg:
                 ("Failed to copy file: " ^ Path.to_string src ^ " -> "
                ^ Path.to_string dst))
        manifest.files;
      Ok ()
  | Error _ -> Error "Hash not found in store"

(** Store artifacts from sandbox to content-addressable store *)
let store_artifacts store ~package hash sandbox_dir declared_outputs =
  let hash_dir = get_hash_dir store hash in
  let temp_dir =
    let nanos = Time.SystemTime.duration_since_epoch () |> Time.Duration.to_nanos in
    let temp_name =
      Std.Crypto.Digest.hex hash ^ ".tmp." ^ Int64.to_string nanos
    in
    Path.(store.root_dir / Path.v temp_name)
  in
  Fs.create_dir_all temp_dir
  |> Result.expect
       ~msg:
         ("Failed to create temp directory: " ^ Path.to_string temp_dir);

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
                 ~msg:
                   ("Failed to create parent directory: " ^ Path.to_string dst_parent);
            Fs.copy ~src ~dst
            |> Result.expect
                 ~msg:
                   ("Failed to store artifact: " ^ Path.to_string src ^ " -> " ^ Path.to_string dst);
            let size =
              Fs.metadata dst
              |> Result.expect
                   ~msg:
                     ("Failed to get metadata for " ^ Path.to_string dst)
              |> Fs.Metadata.len
            in
            (Path.v output_file, size) :: acc
        | _ -> acc)
      [] declared_outputs
  in

  (* Create and save manifest *)
  let manifest =
    Manifest.create ~package
      ~build_hash:(Std.Crypto.Digest.hex hash)
      ~files:(List.rev stored_files_with_sizes)
  in
  Manifest.save manifest ~path:(manifest_path temp_dir)
  |> Result.expect ~msg:"Failed to save manifest";

  let commit_result =
    if exists store hash then
      Ok ()
    else Fs.rename ~src:temp_dir ~dst:hash_dir
  in

  (match commit_result with
  | Ok () -> ()
  | Error _ ->
      if exists store hash then ()
      else
        panic
          ("Failed to move temp artifact dir into place: " ^ Path.to_string temp_dir
         ^ " -> " ^ Path.to_string hash_dir));

  (match Fs.exists temp_dir with
  | Ok true ->
      let _ = Fs.remove_dir_all temp_dir in
      ()
  | Ok false | Error _ -> ());

  (* Return artifact witness with just the filenames *)
  let stored_files = List.map (fun (path, _) -> path) stored_files_with_sizes in
  Artifact.{ hash; files = List.rev stored_files }

(** Simple interface - check if we have cached artifacts for a hash *)
let get store hash =
  if exists store hash then
    match Manifest.load ~path:(manifest_path (get_hash_dir store hash)) with
    | Ok manifest ->
        let files =
          List.map (fun entry -> entry.Manifest.path) manifest.files
        in
        Some Artifact.{ hash; files }
    | Error _ -> None
  else None

(** Save build outputs to the store *)
let save store ~package ~hash ~sandbox_dir ~outs =
  let sandbox_str = Path.to_string sandbox_dir in
  let sandbox_len = String.length sandbox_str in
  let outs_str =
    List.map
      (fun out_path ->
        let out_str = Path.to_string out_path in
        if String.starts_with ~prefix:sandbox_str out_str then
          let relative_start = sandbox_len + 1 in
          String.sub out_str relative_start
            (String.length out_str - relative_start)
        else Path.to_string out_path)
      outs
  in
  let artifact = store_artifacts store ~package hash sandbox_dir outs_str in
  Ok artifact

(** Promote cached artifacts to target directory *)
let promote_artifact store artifact ~target_dir =
  promote store Artifact.(artifact.hash) target_dir

(** Get absolute paths to artifact files in immutable cache *)
let get_artifact_paths store artifact =
  let hash_dir = get_hash_dir store Artifact.(artifact.hash) in
  List.map
    (fun rel_path -> Path.(hash_dir / rel_path))
    Artifact.(artifact.files)

(** Get the cache directory containing an artifact's files *)
let get_artifact_dir store artifact =
  get_hash_dir store Artifact.(artifact.hash)

let hash_dir_of store hash = get_hash_dir store hash

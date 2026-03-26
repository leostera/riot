open Std
open Std.Iter
open Std.Collections

open Tusk_model
(** Store - Content-addressable storage for build artifacts **)

module Manifest = Manifest

type t = { root_dir : Path.t (* Root directory for the store *) }
type error = string
type export_entry = { name : string; path : Path.t; action_hash : string }

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
let plans_dir store = Path.(store.root_dir / Path.v "plans")

let plan_path store hash =
  Path.(plans_dir store / Path.v (Std.Crypto.Digest.hex hash ^ ".json"))

let exports_dir store = Path.(store.root_dir / Path.v "exports")

let package_exports_path store ~package ~profile ~target =
  Path.(
    exports_dir store / Path.v profile / Path.v target
    / Path.v (package ^ ".json"))

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
  promote store Artifact.(artifact.hash) ~target_dir

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

let save_plan_bundle store ~hash ~plan =
  let plans_root = plans_dir store in
  Fs.create_dir_all plans_root
  |> Result.expect
       ~msg:
         ("Failed to create plan cache directory: " ^ Path.to_string plans_root);
  let destination = plan_path store hash in
  let temp_path =
    let nanos = Time.SystemTime.duration_since_epoch () |> Time.Duration.to_nanos in
    Path.(
      plans_root
      / Path.v
          (Std.Crypto.Digest.hex hash ^ ".tmp." ^ Int64.to_string nanos ^ ".json"))
  in
  let content = Std.Data.Json.to_string plan in
  match Fs.write content temp_path with
  | Error _ -> Error "Failed to write temporary plan bundle"
  | Ok () -> (
      match Fs.rename ~src:temp_path ~dst:destination with
      | Ok () -> Ok ()
      | Error _ ->
          let _ = Fs.remove_file temp_path in
          Error "Failed to commit plan bundle")

let load_plan_bundle store ~hash =
  let path = plan_path store hash in
  match Fs.read path with
  | Ok content -> (
      match Std.Data.Json.of_string content with
      | Ok json -> Some json
      | Error _ -> None)
  | Error _ -> None

let export_entry_to_json (entry : export_entry) =
  Std.Data.Json.Object
    [
      ("name", Std.Data.Json.String entry.name);
      ("path", Std.Data.Json.String (Path.to_string entry.path));
      ("action_hash", Std.Data.Json.String entry.action_hash);
    ]

let export_entry_of_json json =
  match json with
  | Std.Data.Json.Object fields -> (
      match
        ( List.assoc_opt "name" fields,
          List.assoc_opt "path" fields,
          List.assoc_opt "action_hash" fields )
      with
      | Some (Std.Data.Json.String name), Some (Std.Data.Json.String path), Some (Std.Data.Json.String action_hash) ->
          Some { name; path = Path.v path; action_hash }
      | _ -> None)
  | _ -> None

let save_package_exports store ~package ~profile ~target ~exports =
  let path = package_exports_path store ~package ~profile ~target in
  let parent = Path.dirname path in
  Fs.create_dir_all parent
  |> Result.expect
       ~msg:
         ("Failed to create package export directory: " ^ Path.to_string parent);
  let payload =
    Std.Data.Json.Object
      [
        ("version", Std.Data.Json.Int 1);
        ("package", Std.Data.Json.String package);
        ("profile", Std.Data.Json.String profile);
        ("target", Std.Data.Json.String target);
        ("exports", Std.Data.Json.Array (List.map export_entry_to_json exports));
      ]
  in
  match Fs.write (Std.Data.Json.to_string payload) path with
  | Ok () -> Ok ()
  | Error _ -> Error "Failed to write package export manifest"

let load_package_exports store ~package ~profile ~target =
  let path = package_exports_path store ~package ~profile ~target in
  match Fs.read path with
  | Error _ -> None
  | Ok content -> (
      match Std.Data.Json.of_string content with
      | Error _ -> None
      | Ok (Std.Data.Json.Object fields) -> (
          match List.assoc_opt "exports" fields with
          | Some (Std.Data.Json.Array entries) ->
              Some (List.filter_map export_entry_of_json entries)
          | _ -> None)
      | Ok _ -> None)

let find_package_export_path store ~package ~profile ~target ~name =
  match load_package_exports store ~package ~profile ~target with
  | None -> None
  | Some exports -> (
      match List.find_opt (fun entry -> String.equal entry.name name) exports with
      | None -> None
      | Some entry ->
          if Path.is_absolute entry.path then None
          else Some Path.(store.root_dir / Path.v entry.action_hash / entry.path))

let materialize_package_exports store ~exports ~target_dir =
  Fs.create_dir_all target_dir
  |> Result.expect
       ~msg:
         ("Failed to create package output directory: "
        ^ Path.to_string target_dir);
  let copy_one (entry : export_entry) =
    if Path.is_absolute entry.path then
      Error ("Export path must be relative: " ^ Path.to_string entry.path)
    else
      let src = Path.(store.root_dir / Path.v entry.action_hash / entry.path) in
      let dst = Path.(target_dir / Path.v entry.name) in
      match Fs.exists src with
      | Ok true ->
          Fs.copy ~src ~dst
          |> Result.map_error (fun _ ->
                 "Failed to copy export: " ^ Path.to_string src ^ " -> "
                 ^ Path.to_string dst)
      | Ok false | Error _ ->
          Error ("Export source not found in store: " ^ Path.to_string src)
  in
  List.fold_left
    (fun acc entry ->
      match acc with
      | Error _ -> acc
      | Ok () -> copy_one entry)
    (Ok ()) exports

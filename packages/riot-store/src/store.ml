open Std
open Std.Iter
open Std.Collections
open Riot_model
module ContentStore = Contentstore.Store

(** Store - Content-addressable storage for build artifacts **)
module Manifest = Manifest

let ( let* ) result fn = Result.and_then result ~fn

let artifacts_namespace =
  Contentstore.Namespace.from_parts [ "artifacts" ]
  |> Result.expect ~msg:"riot-store artifacts namespace should be valid"

let plans_namespace =
  Contentstore.Namespace.from_parts [ "plans" ]
  |> Result.expect ~msg:"riot-store plans namespace should be valid"

type t = {
  content_store: ContentStore.t;
  plan_store: ContentStore.t;
}

type error =
  | HashNotFound of { hash: Crypto.hash }
  | LoadManifestFailed of { path: Path.t; cause: string }
  | CorruptArtifact of { path: Path.t; reason: string }
  | CreateTargetDirFailed of { path: Path.t; cause: Fs.error }
  | CreateParentDirFailed of { path: Path.t; cause: Fs.error }
  | ReadSourceMetadataFailed of { path: Path.t; cause: Fs.error }
  | CopyArtifactFailed of { src: Path.t; dst: Path.t; cause: Fs.error }
  | SetCopiedArtifactPermissionsFailed of { src: Path.t; dst: Path.t; cause: Fs.error }
  | CreateTempDirFailed of { path: Path.t; cause: Fs.error }
  | CheckSourceExistsFailed of { path: Path.t; cause: Fs.error }
  | MetadataReadFailed of { path: Path.t; cause: Fs.error }
  | SaveManifestFailed of { path: Path.t; cause: string }
  | CommitArtifactsFailed of { source_dir: Path.t; destination_dir: Path.t; cause: string }
  | SavePlanBundleFailed of { hash: Crypto.hash; cause: string }
  | ExportPathMustBeRelative of { path: Path.t }
  | CreatePackageOutputDirFailed of { path: Path.t; cause: Fs.error }
  | CopyExportFailed of { src: Path.t; dst: Path.t; cause: Fs.error }
  | ExportSourceMissing of { path: Path.t }

type export_entry = Manifest.export_entry = {
  name: string;
  path: Path.t;
  action_hash: string;
}

let error_message = function
  | HashNotFound { hash } -> "Hash not found in store: " ^ Crypto.Digest.hex hash
  | LoadManifestFailed { path; cause } -> "Failed to load manifest: "
  ^ Path.to_string path
  ^ " ("
  ^ cause
  ^ ")"
  | CorruptArtifact { path; reason } -> "Cached artifact is damaged: "
  ^ Path.to_string path
  ^ " ("
  ^ reason
  ^ "); use `riot clean --force` to rebuild"
  | CreateTargetDirFailed { path; cause } -> "Failed to create target directory: "
  ^ Path.to_string path
  ^ " ("
  ^ IO.error_message cause
  ^ ")"
  | CreateParentDirFailed { path; cause } -> "Failed to create parent directory: "
  ^ Path.to_string path
  ^ " ("
  ^ IO.error_message cause
  ^ ")"
  | ReadSourceMetadataFailed { path; cause } -> "Failed to read source metadata: "
  ^ Path.to_string path
  ^ " ("
  ^ IO.error_message cause
  ^ ")"
  | CopyArtifactFailed { src; dst; cause } -> "Failed to copy file: "
  ^ Path.to_string src
  ^ " -> "
  ^ Path.to_string dst
  ^ " ("
  ^ IO.error_message cause
  ^ ")"
  | SetCopiedArtifactPermissionsFailed { src; dst; cause } -> "Failed to preserve copied file permissions: "
  ^ Path.to_string src
  ^ " -> "
  ^ Path.to_string dst
  ^ " ("
  ^ IO.error_message cause
  ^ ")"
  | CreateTempDirFailed { path; cause } -> "Failed to create temp directory: "
  ^ Path.to_string path
  ^ " ("
  ^ IO.error_message cause
  ^ ")"
  | CheckSourceExistsFailed { path; cause } -> "Failed to check source path: "
  ^ Path.to_string path
  ^ " ("
  ^ IO.error_message cause
  ^ ")"
  | MetadataReadFailed { path; cause } -> "Failed to get metadata for "
  ^ Path.to_string path
  ^ " ("
  ^ IO.error_message cause
  ^ ")"
  | SaveManifestFailed { path; cause } -> "Failed to save manifest: "
  ^ Path.to_string path
  ^ " ("
  ^ cause
  ^ ")"
  | CommitArtifactsFailed { source_dir; destination_dir; cause } -> "Failed to commit artifact directory: "
  ^ Path.to_string source_dir
  ^ " -> "
  ^ Path.to_string destination_dir
  ^ " ("
  ^ cause
  ^ ")"
  | SavePlanBundleFailed { hash; cause } -> "Failed to save plan bundle for "
  ^ Crypto.Digest.hex hash
  ^ " ("
  ^ cause
  ^ ")"
  | ExportPathMustBeRelative { path } -> "Export path must be relative: " ^ Path.to_string path
  | CreatePackageOutputDirFailed { path; cause } -> "Failed to create package output directory: "
  ^ Path.to_string path
  ^ " ("
  ^ IO.error_message cause
  ^ ")"
  | CopyExportFailed { src; dst; cause } -> "Failed to copy export: "
  ^ Path.to_string src
  ^ " -> "
  ^ Path.to_string dst
  ^ " ("
  ^ IO.error_message cause
  ^ ")"
  | ExportSourceMissing { path } -> "Export source is missing from the store: "
  ^ Path.to_string path
  ^ " (cache is corrupted; try `riot clean`)"

(** Create a store rooted at a specific build lane *)
let create_for_lane = fun ~(workspace:Workspace.t) ~profile ~target ->
  let store_dir =
    Path.(
      workspace.target_dir_root
      / Path.v profile
      / Path.v (Riot_model.Target.to_string target)
      / Path.v "cache")
  in
  {
    content_store = ContentStore.create ~root:store_dir ~ns:artifacts_namespace ~policy:Contentstore.Policy.default;
    plan_store = ContentStore.create ~root:store_dir ~ns:plans_namespace ~policy:Contentstore.Policy.default;
  }

(** Create a new store for the given workspace *)
let create = fun ~(workspace:Workspace.t) ->
  create_for_lane
    ~workspace
    ~profile:"debug"
    ~target:(Riot_dirs.host_target ())

(** Get the path for a given hash in the store *)
let get_hash_dir = fun store hash ->
  ContentStore.hash_dir_of store.content_store hash

let manifest_path = fun hash_dir -> Path.(hash_dir / Path.v "manifest.json")

let manifest_cache_key = fun hash -> Std.Crypto.Digest.hex hash

let copy_with_permissions = fun ~src ~dst ~copy_error ->
  let* metadata = Fs.metadata src
  |> Result.map_err ~fn:(fun cause -> ReadSourceMetadataFailed { path = src; cause }) in
  let* () = Fs.copy ~src ~dst |> Result.map_err ~fn:copy_error in
  Fs.set_permissions dst (Fs.Metadata.permissions metadata)
  |> Result.map_err ~fn:(fun cause -> SetCopiedArtifactPermissionsFailed { src; dst; cause })

let cleanup_temp_dir = fun temp_dir ->
  match Fs.exists temp_dir with
  | Ok true -> Fs.remove_dir_all temp_dir
  | Ok false
  | Error _ -> Ok ()

let artifact_temp_counter = Sync.Atomic.make 0

let next_artifact_temp_nonce = fun () ->
  (* A separate get/set can issue the same nonce to concurrent writers. *)
  Sync.Atomic.fetch_and_add artifact_temp_counter 1

let artifact_temp_dir = fun store hash ->
  let nanos = Time.SystemTime.duration_since_epoch () |> Time.Duration.to_nanos in
  let pid = Process.id () |> Int32.to_string in
  let nonce = next_artifact_temp_nonce () |> Int.to_string in
  let temp_name =
    Std.Crypto.Digest.hex hash
    ^ ".tmp."
    ^ pid
    ^ "."
    ^ Int64.to_string nanos
    ^ "."
    ^ nonce
  in
  Path.(ContentStore.root store.content_store / Path.v temp_name)

let hash_of_hex = fun hex ->
  let hex_nibble = fun ch ->
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
        Some (Crypto.Hash.of_bytes bytes)
      else
        match (
          hex_nibble (String.get_unchecked hex ~at:index),
          hex_nibble (String.get_unchecked hex ~at:(index + 1))
        ) with
        | Some hi, Some lo ->
            IO.Bytes.set_unchecked
              bytes
              ~at:(index / 2)
              ~char:(Char.from_int_unchecked ((hi lsl 4) lor lo));
            loop (index + 2)
        | _ -> None
    in
    loop 0

let read_opened_file = fun file ->
  let content = Fs.File.read_to_end file in
  let _ = Fs.File.close file in
  content

let validate_manifest_files = fun ~hash ~hash_dir (manifest: Manifest.t) ->
  let corrupt path reason = Error (CorruptArtifact { path; reason }) in
  let validate_file (entry: Manifest.file_entry) =
    let relative = Path.to_string entry.path in
    if Path.is_absolute entry.path || String.equal relative ""
      || List.any (String.split ~by:"/" relative) ~fn:(fun part -> String.equal part "..") then
      corrupt hash_dir "manifest file path must stay relative to the artifact"
    else
      let path = Path.(hash_dir / entry.path) in
      match Fs.symlink_metadata path with
      | Error cause -> corrupt path (IO.error_message cause)
      | Ok metadata when not (Fs.Metadata.is_file metadata) ->
          corrupt path "payload is not a regular file"
      | Ok metadata when Fs.Metadata.len metadata != entry.size ->
          corrupt path "payload size does not match manifest"
      | Ok _ ->
          match Fs.read_to_string path with
          | Error cause -> corrupt path (IO.error_message cause)
          | Ok content ->
              let actual = Crypto.(Sha512.hash_string content |> Digest.hex) in
              if String.equal actual entry.hash then Ok ()
              else corrupt path "payload checksum does not match manifest"
  in
  if not (String.equal manifest.build_hash (Crypto.Digest.hex hash)) then
    corrupt (manifest_path hash_dir) "manifest build hash does not match its cache key"
  else
    List.fold_left manifest.files ~acc:(Ok ()) ~fn:(fun acc entry ->
      let* () = acc in
      validate_file entry)

let validate_existing_artifact = fun store hash ->
  let hash_dir = get_hash_dir store hash in
  match Fs.exists hash_dir with
  | Ok false -> Ok ()
  | Error cause -> Error (CorruptArtifact { path = hash_dir; reason = IO.error_message cause })
  | Ok true ->
      match Manifest.load ~path:(manifest_path hash_dir) with
      | Error reason -> Error (CorruptArtifact { path = manifest_path hash_dir; reason })
      | Ok manifest -> validate_manifest_files ~hash ~hash_dir manifest

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
  let manifest_path = manifest_path hash_dir in
  let* manifest =
    match Manifest.load ~path:manifest_path with
    | Ok manifest -> Ok manifest
    | Error cause -> (
        match Fs.exists manifest_path with
        | Ok true -> Error (LoadManifestFailed { path = manifest_path; cause })
        | Ok false
        | Error _ -> Error (HashNotFound { hash })
      )
  in
  (* Verify the entire entry before modifying any working output. A cache hit
     must never mean that damaged bytes or an incomplete manifest were trusted. *)
  let* () = validate_manifest_files ~hash ~hash_dir manifest in
  let* () = Fs.create_dir_all target_dir
  |> Result.map_err ~fn:(fun cause -> CreateTargetDirFailed { path = target_dir; cause }) in
  let promote_one (entry: Manifest.file_entry) =
    let src = Path.(hash_dir / entry.path) in
    let dst = Path.(target_dir / entry.path) in
    let dst_parent = Path.dirname dst in
    let* () = Fs.create_dir_all dst_parent
    |> Result.map_err ~fn:(fun cause -> CreateParentDirFailed { path = dst_parent; cause }) in
    copy_with_permissions ~src ~dst ~copy_error:(fun cause -> CopyArtifactFailed { src; dst; cause })
  in
  List.fold_left manifest.files ~acc:(Ok ())
    ~fn:(fun acc entry ->
      let* () = acc in
      promote_one entry)

(** Store artifacts from sandbox to content-addressable store *)
let store_artifacts = fun store ~package ?(ocamlc_warnings = []) ?(exports = []) hash sandbox_dir declared_outputs ->
  let hash_dir = get_hash_dir store hash in
  (* Immutable same-hash publication reuses an existing directory. Refuse a
     damaged destination rather than falsely reporting that new outputs won. *)
  let* () = validate_existing_artifact store hash in
  let temp_dir = artifact_temp_dir store hash in
  let* () = Fs.create_dir_all temp_dir
  |> Result.map_err ~fn:(fun cause -> CreateTempDirFailed { path = temp_dir; cause }) in
  (* Copy declared outputs to store and track what was actually stored *)
  let copy_output output_file =
    let src = Path.(sandbox_dir / Path.v output_file) in
    match Fs.exists src with
    | Ok false ->
        Ok None
    | Error cause ->
        Error (CheckSourceExistsFailed { path = src; cause })
    | Ok true ->
        let dst = Path.(temp_dir / Path.v output_file) in
        let dst_parent = Path.dirname dst in
        let* () = Fs.create_dir_all dst_parent
        |> Result.map_err ~fn:(fun cause -> CreateParentDirFailed { path = dst_parent; cause }) in
        let* () =
          copy_with_permissions
            ~src
            ~dst
            ~copy_error:(fun cause -> CopyArtifactFailed { src; dst; cause })
        in
        let* metadata = Fs.metadata dst
        |> Result.map_err ~fn:(fun cause -> MetadataReadFailed { path = dst; cause }) in
        Ok (Some (Path.v output_file, Fs.Metadata.len metadata))
  in
  let rec collect_outputs = fun acc ->
    function
    | [] -> Ok (List.reverse acc)
    | output_file :: rest -> (
        match copy_output output_file with
        | Error _ as err -> err
        | Ok None -> collect_outputs acc rest
        | Ok (Some entry) -> collect_outputs (entry :: acc) rest
      )
  in
  let result =
    let* stored_files_with_sizes = collect_outputs [] declared_outputs in
    let manifest = Manifest.create
      ~base_dir:temp_dir
      ~ocamlc_warnings
      ~exports
      ()
      ~package
      ~build_hash:(Std.Crypto.Digest.hex hash)
      ~files:(List.reverse stored_files_with_sizes) in
    let manifest_path = manifest_path temp_dir in
    let* () = Manifest.save manifest ~path:manifest_path
    |> Result.map_err ~fn:(fun cause -> SaveManifestFailed { path = manifest_path; cause }) in
    let* () = ContentStore.commit_dir store.content_store ~hash ~source_dir:temp_dir
    |> Result.map_err
      ~fn:(fun cause ->
        CommitArtifactsFailed {
          source_dir = temp_dir;
          destination_dir = hash_dir;
          cause = ContentStore.error_message cause;
        }) in
    let stored_files =
      List.map stored_files_with_sizes ~fn:(fun (path, _) -> path)
    in
    Ok Artifact.{ hash; files = List.reverse stored_files; ocamlc_warnings; exports }
  in
  let _ = cleanup_temp_dir temp_dir in
  result

let export_source_path = fun store (entry: export_entry) ->
  if Path.is_absolute entry.path
    || List.any (String.split ~by:"/" (Path.to_string entry.path)) ~fn:(fun part -> String.equal part "..") then
    None
  else
    match hash_of_hex entry.action_hash with
    | None -> None
    | Some action_hash -> Some Path.(get_hash_dir store action_hash / entry.path)

let load_manifest = fun store ~hash ->
  match Manifest.load ~path:(manifest_path (get_hash_dir store hash)) with
  | Ok manifest -> Some manifest
  | Error _ -> None

let path_exists = fun path -> Fs.exists path |> Result.unwrap_or ~default:false

let validate_export_source = fun store (entry: export_entry) ->
  match export_source_path store entry with
  | None -> Error (ExportPathMustBeRelative { path = entry.path })
  | Some path when not (path_exists path) -> Error (ExportSourceMissing { path })
  | Some path ->
      match hash_of_hex entry.action_hash with
      | None -> Error (CorruptArtifact { path; reason = "invalid export action hash" })
      | Some hash ->
          match load_manifest store ~hash with
          | None -> Error (CorruptArtifact { path; reason = "export action manifest is missing or invalid" })
          | Some manifest ->
              match List.find manifest.files ~fn:(fun (file: Manifest.file_entry) ->
                String.equal (Path.to_string file.path) (Path.to_string entry.path)) with
              | None -> Error (CorruptArtifact { path; reason = "export is not declared in its action manifest" })
              | Some file ->
                  validate_manifest_files ~hash ~hash_dir:(get_hash_dir store hash)
                    { manifest with files = [file] }

let manifest_exports_valid = fun store (manifest: Manifest.t) ->
  List.all manifest.exports ~fn:(fun entry -> Result.is_ok (validate_export_source store entry))

(** Simple interface - check if we have cached artifacts for a hash *)
let get = fun store hash ->
  match load_manifest store ~hash with
  | Some manifest ->
      if manifest_exports_valid store manifest
        && Result.is_ok (validate_manifest_files ~hash ~hash_dir:(get_hash_dir store hash) manifest) then
        let files =
          List.map manifest.files ~fn:(fun (entry: Manifest.file_entry) -> entry.path)
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
    List.map outs
      ~fn:(fun out_path ->
        let out_str = Path.to_string out_path in
        if String.starts_with ~prefix:sandbox_str out_str then
          let relative_start = sandbox_len + 1 in
          String.sub out_str ~offset:relative_start ~len:(String.length out_str - relative_start)
        else
          Path.to_string out_path)
  in
  store_artifacts store ~package ~ocamlc_warnings ~exports hash sandbox_dir outs_str

(** Promote cached artifacts to target directory *)
let promote_artifact = fun store artifact ~target_dir -> promote store Artifact.(artifact.hash) ~target_dir

(** Get absolute paths to artifact files in immutable cache *)
let get_artifact_paths = fun store artifact ->
  let hash_dir = get_hash_dir store Artifact.(artifact.hash) in
  List.map Artifact.(artifact.files) ~fn:(fun rel_path -> Path.(hash_dir / rel_path))

(** Get the cache directory containing an artifact's files *)
let get_artifact_dir = fun store artifact -> get_hash_dir store Artifact.(artifact.hash)

let hash_dir_of = fun store hash -> get_hash_dir store hash

let save_plan_bundle = fun store ~hash ~plan ->
  ContentStore.save_object store.plan_store ~hash ~content:(Std.Data.Json.to_string plan)
  |> Result.map_err
    ~fn:(fun cause ->
      SavePlanBundleFailed {
        hash;
        cause = ContentStore.error_message cause;
      })

let load_plan_bundle = fun store ~hash ->
  match ContentStore.open_object store.plan_store ~hash with
  | Error _ -> None
  | Ok file -> (
      match read_opened_file file with
      | Error _ -> None
      | Ok content -> Data.Json.of_string content |> Result.to_option
    )

let materialize_package_exports = fun store ~exports ~target_dir ->
  let* () = List.fold_left exports ~acc:(Ok ()) ~fn:(fun acc entry ->
    let* () = acc in
    validate_export_source store entry) in
  let* () = Fs.create_dir_all target_dir
  |> Result.map_err ~fn:(fun cause -> CreatePackageOutputDirFailed { path = target_dir; cause }) in
  let copy_one (entry: export_entry) =
    match export_source_path store entry with
    | None -> Error (ExportPathMustBeRelative { path = entry.path })
    | Some src ->
        let dst = Path.(target_dir / Path.v entry.name) in
        match Fs.exists src with
        | Ok true -> copy_with_permissions
          ~src
          ~dst
          ~copy_error:(fun cause -> CopyExportFailed { src; dst; cause })
        | Ok false
        | Error _ -> Error (ExportSourceMissing { path = src })
  in
  List.fold_left exports ~acc:(Ok ())
    ~fn:(fun acc entry ->
      let* () = acc in
      copy_one entry)

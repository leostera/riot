open Std

(* The inode must outlive force-clean: putting this lock inside the build root
   would let deletion create two independently locked inodes for one workspace. *)
type t = { file: Fs.File.t }

type error =
  | InvalidRoot of Path.t
  | Busy of Path.t
  | IoFailed of { path: Path.t; cause: Fs.error }
  | FileFailed of { path: Path.t; cause: Fs.File.error }

let error_message = function
  | InvalidRoot root -> "invalid build root for locking: " ^ Path.to_string root
  | Busy root -> "build root is in use by another build or cleanup: " ^ Path.to_string root
  | IoFailed { path; cause } -> "build root lock failed at " ^ Path.to_string path ^ ": " ^ IO.error_message cause
  | FileFailed { path; cause } -> "build root lock failed at " ^ Path.to_string path ^ ": " ^ Fs.File.error_to_string cause

let path = fun ~target_dir_root ->
  let name = Path.basename target_dir_root in
  if String.equal name "" || String.equal name "/" || String.equal name "." || String.equal name ".." then
    Error (InvalidRoot target_dir_root)
  else
    Ok Path.(Path.dirname target_dir_root / Path.v ("." ^ name ^ ".riot-build.lock"))

let release = fun lock ->
  let _ = Fs.File.close lock.file in
  ()

let acquire = fun ~target_dir_root ~try_lock ->
  match path ~target_dir_root with
  | Error error -> Error error
  | Ok path ->
      match Fs.create_dir_all (Path.dirname path) with
      | Error cause -> Error (IoFailed { path; cause })
      | Ok () ->
          match Fs.File.open_write path with
          | Error cause -> Error (FileFailed { path; cause })
          | Ok file ->
              match try_lock file with
              | Ok true -> Ok { file }
              | Ok false ->
                  let _ = Fs.File.close file in
                  Error (Busy target_dir_root)
              | Error cause ->
                  let _ = Fs.File.close file in
                  Error (FileFailed { path; cause })

let acquire_shared = fun ~target_dir_root -> acquire ~target_dir_root ~try_lock:Fs.File.try_lock_shared

let acquire_exclusive = fun ~target_dir_root -> acquire ~target_dir_root ~try_lock:Fs.File.try_lock_exclusive

(** Filesystem utilities *)
open Global
open Iter
open IO
open Collections

include Common

module Event = Event
module Permissions = Permissions
module Metadata = Metadata
module ReadDir = ReadDir
module File = File
module Walker = Walker
module FileWatcher = File_watcher

let kernel_path = fun path -> Kernel.Path.from_string (Path.to_string path)

let path_of_kernel = fun path -> Path.v (Kernel.Path.to_string path)

(**
   Basic filesystem operations - defined first as they're used by other
   functions
*)
let is_directory = fun path ->
  Kernel.Fs.File.is_directory (kernel_path path)
  |> convert_kernel_result

let rmdir = fun path ->
  Kernel.Fs.File.remove_dir (kernel_path path)
  |> convert_kernel_result

(** Clean API implementations following the FIXME guidelines *)
let canonicalize = fun path ->
  match Kernel.Fs.File.canonicalize (kernel_path path) with
  | Ok abs_path -> Ok (path_of_kernel abs_path)
  | Error error -> Error (from_file_error error)

let copy = fun ~src ~dst ->
  Kernel.Fs.File.copy ~src:(kernel_path src) ~dst:(kernel_path dst)
  |> convert_kernel_result

let clone = fun ~src ~dst ->
  Kernel.Fs.File.clone ~src:(kernel_path src) ~dst:(kernel_path dst)
  |> convert_kernel_result

let create_dir_all = fun path ->
  let rec create_parents path =
    match Path.parent path with
    | None -> Ok ()
    | Some parent ->
        match Kernel.Fs.File.exists (kernel_path parent) with
        | Error error -> Error (from_file_error error)
        | Ok true -> Ok ()
        | Ok false -> (
            match create_parents parent with
            | Error error -> Error error
            | Ok () -> (
                match Kernel.Fs.File.create_dir (kernel_path parent) ~perm:0o755 with
                | Ok () -> Ok ()
                | Error (Kernel.Fs.File.System Kernel.SystemError.AlreadyExists) -> Ok ()
                | Error error -> Error (from_file_error error)
              )
          )
  in
  match create_parents path with
  | Error error -> Error error
  | Ok () -> (
      match Kernel.Fs.File.create_dir (kernel_path path) ~perm:0o755 with
      | Ok () -> Ok ()
      | Error (Kernel.Fs.File.System Kernel.SystemError.AlreadyExists) -> Ok ()
      | Error error -> Error (from_file_error error)
    )

let exists = fun path ->
  Kernel.Fs.File.exists (kernel_path path)
  |> convert_kernel_result

let hard_link = fun ~src ~dst ->
  Kernel.Fs.File.hard_link ~src:(kernel_path src) ~dst:(kernel_path dst)
  |> convert_kernel_result

let metadata = fun path ->
  Kernel.Fs.File.metadata (kernel_path path)
  |> convert_kernel_result

let symlink_metadata = fun path ->
  Kernel.Fs.File.symlink_metadata (kernel_path path)
  |> convert_kernel_result

let read_to_string = fun path ->
  match File.open_read path with
  | Error error -> Error (from_file_error error)
  | Ok file -> (
      match File.read_to_end file with
      | Error error ->
          let _ = File.close file in
          Error (from_file_error error)
      | Ok content ->
          match File.close file with
          | Ok () -> Ok content
          | Error error -> Error (from_file_error error)
    )

let read_dir = fun path ->
  match ReadDir.open_dir path with
  | Error e -> Error e
  | Ok state ->
      Ok (
        MutIterator.make (module ReadDir) state
        |> MutIterator.map ~fn:(fun (entry: ReadDir.entry) -> entry.path)
      )

let remove_file = fun path ->
  Kernel.Fs.File.remove_file (kernel_path path)
  |> convert_kernel_result

let remove_dir_all = fun path ->
  (* Inspect entries with [symlink_metadata] so a symlink to a directory is
     unlinked instead of followed. Following it would delete the target's
     contents, possibly outside [path], and then fail to [rmdir] the link.
  *)
  let rec remove_recursive path =
    match symlink_metadata path with
    | Error e -> Error e
    | Ok meta when not (Metadata.is_dir meta) -> remove_file path
    | Ok _ -> (
        (* It's a directory *)
        match ReadDir.open_dir path with
        | Error e -> Error e
        | Ok dir ->
            let rec collect_entries acc =
              match ReadDir.next_result dir with
              | Error error -> Error error
              | Ok None -> Ok (List.reverse acc)
              | Ok (Some entry) -> collect_entries (entry.path :: acc)
            in
            match collect_entries [] with
            | Error _ as err -> err
            | Ok entries ->
                let rec remove_entries = fun __tmp1 ->
                  match __tmp1 with
                  | [] -> rmdir path
                  | entry_path :: rest -> (
                      let full_path = Path.join path entry_path in
                      match remove_recursive full_path with
                      | Error e -> Error e
                      | Ok () -> remove_entries rest
                    )
                in
                remove_entries entries
      )
  in
  remove_recursive path

let rename = fun ~src ~dst ->
  Kernel.Fs.File.rename ~src:(kernel_path src) ~dst:(kernel_path dst)
  |> convert_kernel_result

let set_permissions = fun path perm ->
  Kernel.Fs.File.set_permissions (kernel_path path) ~perm:(Permissions.to_mode perm)
  |> convert_kernel_result

let write = fun content path ->
  match File.create path with
  | Error error -> Error (from_file_error error)
  | Ok file -> (
      match File.write_all file content with
      | Error error ->
          let _ = File.close file in
          Error (from_file_error error)
      | Ok () -> (
          match File.close file with
          | Ok () -> Ok ()
          | Error error -> Error (from_file_error error)
        )
    )

let read_link = fun path ->
  match Kernel.Fs.File.read_link (kernel_path path) with
  | Ok target -> Ok (path_of_kernel target)
  | Error error -> Error (from_file_error error)

let create_dir = fun path ->
  Kernel.Fs.File.create_dir (kernel_path path) ~perm:0o755
  |> convert_kernel_result

let file_exists = fun path -> exists path

let dir_exists = fun path ->
  Kernel.Fs.File.is_directory (kernel_path path)
  |> convert_kernel_result

let stat = fun path -> metadata path

let chmod = fun path perm ->
  Kernel.Fs.File.set_permissions (kernel_path path) ~perm:(Permissions.to_mode perm)
  |> convert_kernel_result

let symlink = fun ~src ~dst ->
  Kernel.Fs.File.symlink ~src:(kernel_path src) ~dst:(kernel_path dst)
  |> convert_kernel_result

let mkdir = fun path perm ->
  Kernel.Fs.File.create_dir (kernel_path path) ~perm
  |> convert_kernel_result

let mkdir_safe = fun path perm ->
  match Kernel.Fs.File.create_dir (kernel_path path) ~perm with
  | Ok () -> Ok ()
  | Error (Kernel.Fs.File.System Kernel.SystemError.AlreadyExists) -> Ok ()
  | Error error -> Error (from_file_error error)

let rec mkdirp = fun path -> create_dir_all path

let rec remove_dir = fun path ->
  match ReadDir.open_dir path with
  | Error error -> Error error
  | Ok dir ->
      let rec process_entries () =
        match ReadDir.next_result dir with
        | Error error -> Error error
        | Ok None -> Ok ()
        | Ok (Some entry) -> (
            let file_path = entry.path in
            let full_path = Path.join path file_path in
            match dir_exists full_path with
            | Error error -> Error error
            | Ok true -> (
                match remove_dir full_path with
                | Error error -> Error error
                | Ok () -> process_entries ()
              )
            | Ok false -> (
                match remove_file full_path with
                | Error error -> Error error
                | Ok () -> process_entries ()
              )
          )
      in
      match process_entries () with
      | Error error ->
          let _ = ReadDir.close dir in
          Error error
      | Ok () -> (
          match ReadDir.close dir with
          | Error error -> Error error
          | Ok () -> rmdir path
        )

let file_size = fun path ->
  match Kernel.Fs.File.metadata (kernel_path path) with
  | Ok stats -> Ok (Kernel.Int64.to_int (Kernel.Fs.File.Metadata.len stats))
  | Error error -> Error (from_file_error error)

let path_separator = fun () ->
  if Kernel.System.unix then
    "/"
  else
    "\\"

let current_executable = fun () ->
  match Kernel.Env.executable_name with
  | Some path -> Ok (Path.v path)
  | None -> Error (IO.Unknown_error "Current executable name is unavailable")

let is_absolute = fun path -> Path.is_absolute path

let is_relative = fun path -> Path.is_relative path

let join = fun paths ->
  match paths with
  | [] -> Path.v ""
  | path :: rest -> List.fold_left rest ~init:path ~fn:Path.join

let read = fun path ->
  match File.open_read path with
  | Error error -> Error (from_file_error error)
  | Ok file -> (
      match File.read_to_end file with
      | Error error ->
          let _ = File.close file in
          Error (from_file_error error)
      | Ok content ->
          let _ = File.close file in
          Ok content
    )

let read_file = read

let write_file = fun path content -> write content path

(** Get system temp directory *)
let get_temp_dir = fun () ->
  (* Try TMPDIR, TEMP, TMP environment variables, fallback to /tmp *)
  match Env.get Env.String ~var:"TMPDIR" with
  | Some dir when dir != "" -> dir
  | _ -> (
      match Env.get Env.String ~var:"TEMP" with
      | Some dir when dir != "" -> dir
      | _ -> (
          match Env.get Env.String ~var:"TMP" with
          | Some dir when dir != "" -> dir
          | _ -> "/tmp"
        )
    )

(** Create a unique temporary directory name *)
let make_temp_dir_name = fun temp_base prefix ->
  let pid = Kernel.Process.current_pid () in
  let random_suffix =
    match Kernel.Time.Monotonic.now () with
    | Ok time ->
        let (secs, nanos) = Kernel.Time.Monotonic.to_parts time in
        (secs lxor nanos lxor pid) land 0xff_ffff
    | Error _ -> pid land 0xff_ffff
  in
  (* Convert to 6-digit hex string with leading zeros *)
  let hex_suffix =
    let hex_chars = "0123456789abcdef" in
    let s = Bytes.create ~size:6 in
    let n = ref random_suffix in
    for i = 5 downto 0 do
      Bytes.set_unchecked
        s
        ~at:i
        ~char:(String.get_unchecked hex_chars ~at:(!n land 0xf));
      n := !n lsr 4
    done;
    Bytes.to_string s
  in
  let dir_name = prefix ^ Int.to_string pid ^ "_" ^ hex_suffix in
  temp_base ^ "/" ^ dir_name

(** Create a temporary directory, run a function with it, then clean it up *)
let with_tempdir = fun ?(prefix = "tmp") fn ->
  let max_attempts = 32 in
  let rec create_unique_tempdir attempt =
    if attempt >= max_attempts then
      Error (IO.Unknown_error "Failed to create temp directory after retries")
    else
      let temp_base = get_temp_dir () in
      let temp_name = make_temp_dir_name temp_base prefix in
      match Path.from_string temp_name with
      | Error _ -> Error (IO.Unknown_error "Failed to create temp directory")
      | Ok temp_path -> (
          match create_dir temp_path with
          | Ok () -> Ok temp_path
          | Error IO.File_exists -> create_unique_tempdir (attempt + 1)
          | Error error -> Error error
        )
  in
  try
    match create_unique_tempdir 0 with
    | Error error -> Error error
    | Ok temp_path ->
        let result =
          try Ok (fn temp_path) with
          | e -> Error (IO.Unknown_error (Kernel.Exception.to_string e))
        in
        let cleanup = remove_dir_all temp_path in
        (
          match (result, cleanup) with
          | (Ok value, Ok ()) -> Ok value
          | (Ok _, Error error) -> Error error
          | (Error error, Ok ()) -> Error error
          | (Error error, Error cleanup_error) ->
              Error (IO.Unknown_error (IO.error_message error
              ^ "; tempdir cleanup failed: "
              ^ IO.error_message cleanup_error))
        )
  with
  | e -> Error (IO.Unknown_error (Kernel.Exception.to_string e))

(** Walk directory tree and call function on each path *)
let rec walk = fun path fn ->
  match is_directory path with
  | Error e -> Error e
  | Ok false ->
      fn path;
      Ok ()
  | Ok true -> (
      fn path;
      match ReadDir.open_dir path with
      | Error e -> Error e
      | Ok dir ->
          let rec walk_entries () =
            match ReadDir.next_result dir with
            | Error e -> Error e
            | Ok None -> Ok ()
            | Ok (Some entry) -> (
                let entry_path = entry.path in
                let full_path = Path.join path entry_path in
                match walk full_path fn with
                | Error e -> Error e
                | Ok () -> walk_entries ()
              )
          in
          walk_entries ()
    )

let is_file = fun path ->
  match metadata path with
  | Error e -> Error e
  | Ok m -> Ok (Metadata.is_file m)

let is_dir = fun path ->
  match metadata path with
  | Error e -> Error e
  | Ok m -> Ok (Metadata.is_dir m)

let current_dir = fun () ->
  match Env.current_dir () with
  | Ok cwd -> Ok cwd
  | Error _ -> Error (IO.Unknown_error "Failed to get current directory")

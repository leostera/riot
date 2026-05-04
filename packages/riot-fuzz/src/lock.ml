open Std
open Std.Result.Syntax

type t = {
  path: Path.t;
  file: Fs.File.t;
}

let retry_interval = Time.Duration.from_millis 500

let path = fun ~(workspace:Riot_model.Workspace.t) ->
  Path.(workspace.target_dir_root / Path.v "fuzz.lock")

let release = fun t ->
  let _ = Fs.File.unlock t.file in
  let _ = Fs.File.close t.file in
  ()

let wait = fun ~on_waiting path ->
  let* () =
    match Path.parent path with
    | Some parent ->
        Fs.create_dir_all parent
        |> Result.map_err ~fn:(fun err -> Error.Io_error (IO.error_message err))
    | None -> Ok ()
  in
  let* file =
    Fs.File.open_write path
    |> Result.map_err ~fn:(fun err -> Error.Io_error (Fs.File.error_to_string err))
  in
  let t = { path; file } in
  let rec loop announced =
    match Fs.File.try_lock_exclusive file with
    | Ok true -> Ok t
    | Ok false ->
        if not announced then
          on_waiting path;
        sleep retry_interval;
        loop true
    | Error err ->
        release t;
        Error (Error.Io_error (Fs.File.error_to_string err))
  in
  loop false

let with_lock = fun ~workspace ~on_waiting fn ->
  let path = path ~workspace in
  let* lock = wait ~on_waiting path in
  try
    let result = fn () in
    release lock;
    result
  with
  | exn ->
      release lock;
      raise exn

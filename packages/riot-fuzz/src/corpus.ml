open Std
open Std.Result.Syntax

type entry = {
  path: Path.t;
  content: string;
}

let path_in_dir = fun dir path ->
  if Path.is_absolute path then
    path
  else
    Path.(dir / path)

let compare_path = fun left right -> String.compare (Path.to_string left) (Path.to_string right)

let ensure_case_dirs = fun dir ->
  let corpus_dir = Path.(dir / Path.v "corpus") in
  let crashes_dir = Path.(dir / Path.v "crashes") in
  let* () =
    Fs.create_dir_all corpus_dir
    |> Result.map_err ~fn:(fun err -> Error.Io_error (IO.error_message err))
  in
  let* () =
    Fs.create_dir_all crashes_dir
    |> Result.map_err ~fn:(fun err -> Error.Io_error (IO.error_message err))
  in
  Ok (corpus_dir, crashes_dir)

let load_entries = fun corpus_dir ->
  match Fs.read_dir corpus_dir with
  | Error _ -> []
  | Ok reader ->
      Std.Iter.MutIterator.to_list reader
      |> List.sort ~compare:compare_path
      |> List.filter_map
        ~fn:(fun path ->
          let path = path_in_dir corpus_dir path in
          match Fs.is_file path with
          | Ok true -> (
              match Fs.read path with
              | Ok content -> Some { path; content }
              | Error _ -> None
            )
          | Ok false
          | Error _ -> None)

let load = fun corpus_dir ->
  load_entries corpus_dir
  |> List.map ~fn:(fun entry -> entry.content)

let file_name = fun input ->
  let digest =
    Crypto.Sha256.hash_string input
    |> Crypto.Digest.hex
  in
  "id-" ^ digest

let save_input = fun dir prefix input ->
  let path = Path.(dir / Path.v (prefix ^ file_name input)) in
  match Fs.exists path with
  | Ok true -> Ok path
  | Ok false
  | Error _ ->
      Fs.write input path
      |> Result.map_err ~fn:(fun err -> Error.Io_error (IO.error_message err))
      |> Result.map ~fn:(fun () -> path)

let seed_empty = fun corpus_dir ->
  let seed_path = Path.(corpus_dir / Path.v "seed-empty") in
  match Fs.exists seed_path with
  | Ok true -> Ok ()
  | Ok false
  | Error _ ->
      Fs.write "" seed_path
      |> Result.map_err ~fn:(fun err -> Error.Io_error (IO.error_message err))

let delete_input = fun path ->
  Fs.remove_file path
  |> Result.map_err ~fn:(fun err -> Error.Io_error (IO.error_message err))

type crash_artifacts = {
  stdout_path: Path.t;
  stderr_path: Path.t;
  status_path: Path.t;
}

let save_crash_artifacts = fun ~case_dir ~crash_path ~status ~stdout ~stderr ->
  let dir = Path.(case_dir / Path.v "crash-artifacts" / Path.v (Path.basename crash_path)) in
  let* () =
    Fs.create_dir_all dir
    |> Result.map_err ~fn:(fun err -> Error.Io_error (IO.error_message err))
  in
  let stdout_path = Path.(dir / Path.v "stdout") in
  let stderr_path = Path.(dir / Path.v "stderr") in
  let status_path = Path.(dir / Path.v "status") in
  let* () =
    Fs.write stdout stdout_path
    |> Result.map_err ~fn:(fun err -> Error.Io_error (IO.error_message err))
  in
  let* () =
    Fs.write stderr stderr_path
    |> Result.map_err ~fn:(fun err -> Error.Io_error (IO.error_message err))
  in
  let* () =
    Fs.write (Afl.status_to_string status ^ "\n") status_path
    |> Result.map_err ~fn:(fun err -> Error.Io_error (IO.error_message err))
  in
  Ok { stdout_path; stderr_path; status_path }

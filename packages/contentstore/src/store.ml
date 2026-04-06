open Std

type t = {
  root_dir: Path.t;
}

type error = string

let create = fun ~root_dir ->
  let _ =
    Fs.create_dir_all root_dir
    |> Result.expect ~msg:("Failed to create content store root: " ^ Path.to_string root_dir)
  in
  { root_dir }

let root_dir = fun store -> store.root_dir

let hash_dir_of = fun store hash ->
  Path.(store.root_dir / Path.v (Crypto.Digest.hex hash))

let exists = fun store hash ->
  Fs.exists (hash_dir_of store hash) |> Result.unwrap_or ~default:false

let commit_dir = fun store ~hash ~source_dir ->
  let destination = hash_dir_of store hash in
  if exists store hash then
    Ok ()
  else
    match Fs.rename ~src:source_dir ~dst:destination with
    | Ok () -> Ok ()
    | Error _ ->
        if exists store hash then
          Ok ()
        else
          Error
            ("Failed to commit content-addressed directory: "
            ^ Path.to_string source_dir
            ^ " -> "
            ^ Path.to_string destination)

let namespace_dir = fun store namespace ->
  Path.(store.root_dir / Path.v namespace)

let blob_path = fun store ~namespace ~hash ->
  Path.(namespace_dir store namespace / Path.v (Crypto.Digest.hex hash))

let save_blob = fun store ~namespace ~hash ~content ->
  let root = namespace_dir store namespace in
  let _ =
    Fs.create_dir_all root
    |> Result.expect ~msg:("Failed to create content namespace root: " ^ Path.to_string root)
  in
  let destination = blob_path store ~namespace ~hash in
  let temp_path =
    let nanos = Time.SystemTime.duration_since_epoch () |> Time.Duration.to_nanos in
    Path.(root / Path.v (Crypto.Digest.hex hash ^ ".tmp." ^ Int64.to_string nanos))
  in
  match Fs.write content temp_path with
  | Error _ -> Error ("Failed to write temporary blob: " ^ Path.to_string temp_path)
  | Ok () -> (
      if Fs.exists destination |> Result.unwrap_or ~default:false then (
        let _ = Fs.remove_file temp_path in
        Ok ()
      ) else
        match Fs.rename ~src:temp_path ~dst:destination with
        | Ok () -> Ok ()
        | Error _ ->
            let _ = Fs.remove_file temp_path in
            if Fs.exists destination |> Result.unwrap_or ~default:false then
              Ok ()
            else
              Error ("Failed to commit blob: " ^ Path.to_string destination)
    )

let load_blob = fun store ~namespace ~hash ->
  Fs.read (blob_path store ~namespace ~hash) |> Result.to_option

let save_json_bundle = fun store ~namespace ~hash ~json ->
  save_blob store ~namespace ~hash ~content:(Data.Json.to_string json)

let load_json_bundle = fun store ~namespace ~hash ->
  match load_blob store ~namespace ~hash with
  | None -> None
  | Some content -> Data.Json.of_string content |> Result.to_option

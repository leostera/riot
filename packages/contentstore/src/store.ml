open Std

type policy = Policy.t

type t = {
  root: Path.t;
  policy: policy;
}

type error = string

let create = fun ~root ~policy () ->
  let _ =
    Fs.create_dir_all root
    |> Result.expect ~msg:("Failed to create content store root: " ^ Path.to_string root)
  in
  { root; policy }

let root = fun store -> store.root

let hash_dir_of = fun store hash ->
  Path.(store.root / Path.v (Crypto.Digest.hex hash))

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
  Path.(store.root / Path.v namespace)

let blob_path = fun store ~namespace ~hash ->
  Path.(namespace_dir store namespace / Path.v (Crypto.Digest.hex hash))

let named_namespace_dir = fun store namespace ->
  Path.(store.root / Path.v "__named" / Path.v namespace)

let named_blob_path = fun store ~namespace ~key ->
  let key_hash = Crypto.hash_string key |> Crypto.Digest.hex in
  Path.(named_namespace_dir store namespace / Path.v key_hash)

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

let save_named_blob = fun store ~namespace ~key ~content ->
  let root = named_namespace_dir store namespace in
  let _ =
    Fs.create_dir_all root
    |> Result.expect ~msg:("Failed to create named namespace root: " ^ Path.to_string root)
  in
  let destination = named_blob_path store ~namespace ~key in
  let temp_path =
    let nanos = Time.SystemTime.duration_since_epoch () |> Time.Duration.to_nanos in
    Path.(root / Path.v (Path.basename destination ^ ".tmp." ^ Int64.to_string nanos))
  in
  match Fs.write content temp_path with
  | Error _ -> Error ("Failed to write temporary named blob: " ^ Path.to_string temp_path)
  | Ok () -> (
      let _ =
        match Fs.exists destination with
        | Ok true -> Fs.remove_file destination
        | Ok false
        | Error _ -> Ok ()
      in
      match Fs.rename ~src:temp_path ~dst:destination with
      | Ok () -> Ok ()
      | Error _ ->
          let _ = Fs.remove_file temp_path in
          Error ("Failed to commit named blob: " ^ Path.to_string destination)
    )

let load_named_blob = fun store ~namespace ~key ->
  Fs.read (named_blob_path store ~namespace ~key) |> Result.to_option

let save_named_json_bundle = fun store ~namespace ~key ~json ->
  save_named_blob store ~namespace ~key ~content:(Data.Json.to_string json)

let load_named_json_bundle = fun store ~namespace ~key ->
  match load_named_blob store ~namespace ~key with
  | None -> None
  | Some content -> Data.Json.of_string content |> Result.to_option

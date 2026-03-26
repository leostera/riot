(** Manifest for tracking stored build artifacts *)

open Std
open Std.Data
open Std.Collections

type version = V0

type file_entry = {
  path : Path.t;
  hash : string; (* SHA512 hash of the file *)
  size : int;
}

type t = {
  version : version;
  package : string;
  build_hash : string; (* The build hash *)
  timestamp : Time.SystemTime.t;
  files : file_entry list;
}

(** Convert manifest to JSON *)
let to_json manifest : Data.Json.t =
  let version_to_string = function V0 -> "v0" in

  let file_entry_to_json entry =
    Data.Json.Object
      [
        ("path", Data.Json.String (Path.to_string entry.path));
        ("hash", Data.Json.String entry.hash);
        ("size", Data.Json.Int entry.size);
      ]
  in

  Data.Json.Object
    [
      ("version", Data.Json.String (version_to_string manifest.version));
      ("package", Data.Json.String manifest.package);
      ("build_hash", Data.Json.String manifest.build_hash);
      ( "timestamp",
        Data.Json.Int (Time.SystemTime.to_unix_timestamp manifest.timestamp) );
      ("files", Data.Json.Array (List.map file_entry_to_json manifest.files));
    ]

(** Parse manifest from JSON *)
let of_json json =
  let open Result in
  let open Data.Json in
  try
    match json with
    | Object fields -> (
        let get_field name = List.assoc_opt name fields in

        let version =
          match get_field "version" with
          | Some (String "v0") -> Ok V0
          | _ -> Error "Invalid or missing version"
        in

        let package =
          match get_field "package" with
          | Some (String p) -> Ok p
          | _ -> Error "Invalid or missing package"
        in

        let build_hash =
          match get_field "build_hash" with
          | Some (String h) -> Ok h
          | _ -> Error "Invalid or missing build_hash"
        in

        let timestamp =
          match get_field "timestamp" with
          | Some (Int t) -> Ok (Time.SystemTime.from_unix_timestamp t)
          | _ -> Error "Invalid or missing timestamp"
        in

        let files =
          match get_field "files" with
          | Some (Array entries) ->
              let parse_entry = function
                | Object entry_fields -> (
                    let get_entry_field name =
                      List.assoc_opt name entry_fields
                    in
                    match
                      ( get_entry_field "path",
                        get_entry_field "hash",
                        get_entry_field "size" )
                    with
                    | Some (String path), Some (String hash), Some (Int size) ->
                        Ok { path = Path.v path; hash; size }
                    | _ -> Error "Invalid file entry")
                | _ -> Error "File entry must be an object"
              in
              List.fold_left
                (fun acc entry ->
                  match (acc, parse_entry entry) with
                  | Ok entries, Ok e -> Ok (e :: entries)
                  | Error e, _ | _, Error e -> Error e)
                (Ok []) entries
              |> Result.map List.rev
          | _ -> Error "Invalid or missing files"
        in

        match (version, package, build_hash, timestamp, files) with
        | Ok v, Ok p, Ok h, Ok t, Ok f ->
            Ok
              {
                version = v;
                package = p;
                build_hash = h;
                timestamp = t;
                files = f;
              }
        | Error e, _, _, _, _
        | _, Error e, _, _, _
        | _, _, Error e, _, _
        | _, _, _, Error e, _
        | _, _, _, _, Error e ->
            Error e)
    | _ -> Error "Manifest must be a JSON object"
  with
  | Not_found -> Error "Missing required field"
  | _ -> Error "Failed to parse manifest"

(** Write manifest to file *)
let save manifest ~path =
  let json = to_json manifest in
  let content = Data.Json.to_string json in
  match Std.Fs.write content path with
  | Ok () -> Ok ()
  | Error _ -> Error "Failed to write manifest"

(** Read manifest from file *)
let load ~path =
  match Std.Fs.read path with
  | Ok content -> (
      match Data.Json.of_string content with
      | Ok json -> of_json json
      | Error _ -> Error "Failed to parse JSON")
  | Error _ -> Error "Failed to read manifest file"

(** Create a manifest for stored files *)
let create ~package ~build_hash ~files =
  let timestamp = Time.SystemTime.now () in
  let file_entries =
    List.filter_map
      (fun (path, size) ->
        (* Calculate hash of the file *)
        match Std.Fs.read_to_string path with
        | Ok file ->
            let hash = Std.Crypto.(Sha512.hash_string file |> Digest.hex) in
            Some { path; hash; size }
        | Error _ -> None)
      files
  in
  { version = V0; package; build_hash; timestamp; files = file_entries }

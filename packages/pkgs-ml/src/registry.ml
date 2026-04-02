open Std

let ( let* ) = Result.and_then

type fetch_response = {
  status_code: int;
  body: string;
}

type fetch = {
  get: Net.Uri.t -> (fetch_response, string) result;
  post: Net.Uri.t -> headers:(string * string) list -> body:string -> (fetch_response, string) result;
}

type published_artifact_location = {
  key: string;
  cdn_url: string;
}

type published_record = {
  key: string;
  created: bool;
}

type published_materialization = {
  manifest: bool;
  source: bool;
}

type published_release = {
  artifact_sha256: string;
  package_name: string;
  package_version: string;
  manifest: published_artifact_location;
  source_archive: published_artifact_location;
  claim: published_record;
  release: published_record;
  materialization: published_materialization;
}

type release_file = {
  path: Path.t;
  contents: string;
}

type release_source = {
  package_name: string;
  version: string;
  manifest_toml: string;
  files: release_file list;
}

type materialize_result =
[
  `Materialized
  | `Already_present
]

type source =
  | Filesystem
  | In_memory of {
      config: Sparse_index.config option;
      packages: (string * Sparse_index.package_document) list;
      releases: ((string * string) * release_source) list
    }

type t = {
  cache: Registry_cache.t;
  fetch: fetch;
  source: source;
}

let make_fetch = fun ~get ?post () ->
  let post =
    match post with
    | Some post -> post
    | None -> fun _ ~headers:_ ~body:_ -> Error "POST fetch is not configured"
  in
  { get; post }

let tcp_stream_error_message = function
  | Net.TcpStream.Connection_refused -> "connection refused"
  | Net.TcpStream.Closed -> "connection closed"
  | Net.TcpStream.System_error io_err -> IO.error_message io_err

let blink_error_message = function
  | Blink.Error.Net_error Net.Connection_refused -> "connection refused"
  | Blink.Error.Net_error Net.Closed -> "connection closed"
  | Blink.Error.Net_error (Net.System_error io_err) -> IO.error_message io_err
  | Blink.Error.Tls_error Net.TlsStream.Closed -> "tls connection closed"
  | Blink.Error.Tls_error (Net.TlsStream.Handshake_failed msg) -> "tls handshake failed: " ^ msg
  | Blink.Error.Tls_error (Net.TlsStream.System_error io_err) -> IO.error_message io_err
  | Blink.Error.Tls_error (Net.TlsStream.Network_read_failed tcp_err) -> "tls read failed: "
  ^ tcp_stream_error_message tcp_err
  | Blink.Error.Tls_error (Net.TlsStream.Network_write_failed tcp_err) -> "tls write failed: "
  ^ tcp_stream_error_message tcp_err
  | Blink.Error.Tls_error Net.TlsStream.Tls_not_available -> "tls not available"
  | Blink.Error.Tls_error Net.TlsStream.Unsupported_vectored_operation -> "unsupported vectored tls operation"
  | Blink.Error.Parse_error msg -> "parse error: " ^ msg
  | Blink.Error.Protocol_error msg -> "protocol error: " ^ msg
  | Blink.Error.Handshake_failed msg -> "handshake failed: " ^ msg
  | Blink.Error.Invalid_frame -> "invalid frame"
  | Blink.Error.Eof -> "unexpected eof"
  | Blink.Error.Closed -> "connection closed"

let exn_message = function
  | Failure message -> message
  | exn -> Exception.to_string exn

let default_fetch =
  let run method_ uri ~headers ?body () =
    try
      match Blink.connect uri with
      | Error err -> Error (blink_error_message err)
      | Ok conn ->
          let request =
            List.fold_left
              (fun request (name, value) ->
                Net.Http.Request.add_header request name value)
              (Net.Http.Request.create method_ uri)
              headers
          in
          let finish result =
            try
              Blink.close conn;
              result
            with
            | exn -> (
                match result with
                | Error _ -> result
                | Ok _ -> Error (exn_message exn)
              )
          in
          let response =
            try
              match Blink.request conn request ?body () with
              | Error err -> Error (blink_error_message err)
              | Ok () -> (
                  match Blink.await conn with
                  | Error err -> Error (blink_error_message err)
                  | Ok (response, body) -> Ok {
                    status_code = Net.Http.Status.to_int (Net.Http.Response.status response);
                    body
                  }
                )
            with
            | exn -> Error (exn_message exn)
          in
          finish response
    with
    | exn -> Error (exn_message exn)
  in
  make_fetch
    ~get:(fun uri -> run Net.Http.Method.Get uri ~headers:[] ())
    ~post:(fun uri ~headers ~body -> run Net.Http.Method.Post uri ~headers ~body ())
    ()

let create_filesystem = fun ?(fetch = default_fetch) ~registry_name ?tusk_home () ->
  match Registry_cache.create ?tusk_home ~registry_name () with
  | Error _ as err -> err
  | Ok cache -> Ok { cache; fetch; source = Filesystem }

let filesystem = fun ?(fetch = default_fetch) cache -> { cache; fetch; source = Filesystem }

let cache = fun registry -> registry.cache

let name = fun registry -> Registry_cache.registry_name registry.cache

let release_key = fun ~package_name ~version -> (Sparse_index.normalized_name package_name, version)

let in_memory = fun ?config ~cache ?(releases = []) ~packages () ->
  let packages =
    List.map
      (fun (document: Sparse_index.package_document) ->
        (Sparse_index.normalized_name document.name, document))
      packages
  in
  let releases =
    List.map
      (fun (release: release_source) ->
        (release_key ~package_name:release.package_name ~version:release.version, release))
      releases
  in
  { cache; fetch = default_fetch; source = In_memory { config; packages; releases } }

let http_status_message = fun status_code ->
  let status = Net.Http.Status.of_int status_code in
  Int.to_string status_code ^ " " ^ Net.Http.Status.reason_phrase status

let fetch_required = fun registry uri ->
  match registry.fetch.get uri with
  | Ok { status_code=200; body } -> Ok body
  | Ok { status_code; _ } -> Error ("request to '"
  ^ Net.Uri.to_string uri
  ^ "' failed with "
  ^ http_status_message status_code)
  | Error err -> Error ("request to '" ^ Net.Uri.to_string uri ^ "' failed: " ^ err)

let fetch_optional = fun registry uri ->
  match registry.fetch.get uri with
  | Ok { status_code=200; body } -> Ok (Some body)
  | Ok { status_code=404; body=_ } -> Ok None
  | Ok { status_code; _ } -> Error ("request to '"
  ^ Net.Uri.to_string uri
  ^ "' failed with "
  ^ http_status_message status_code)
  | Error err -> Error ("request to '" ^ Net.Uri.to_string uri ^ "' failed: " ^ err)

let post_required = fun registry uri ~headers ~body ->
  match registry.fetch.post uri ~headers ~body with
  | Ok { status_code=200; body } ->
      Ok body
  | Ok { status_code; body } -> (
      match Data.Json.of_string body with
      | Ok (Data.Json.Object fields) -> (
          match List.assoc_opt "message" fields with
          | Some (Data.Json.String message) -> Error message
          | _ -> Error ("request to '"
          ^ Net.Uri.to_string uri
          ^ "' failed with "
          ^ http_status_message status_code)
        )
      | Ok _
      | Error _ -> Error ("request to '"
      ^ Net.Uri.to_string uri
      ^ "' failed with "
      ^ http_status_message status_code)
    )
  | Error err ->
      Error ("request to '" ^ Net.Uri.to_string uri ^ "' failed: " ^ err)

let object_field = fun ~context ~field fields ->
  match List.assoc_opt field fields with
  | Some value -> Ok value
  | None -> Error (context ^ " is missing required field '" ^ field ^ "'")

let string_field = fun ~context ~field fields ->
  match object_field ~context ~field fields with
  | Error _ as err -> err
  | Ok (Data.Json.String value) -> Ok value
  | Ok _ -> Error (context ^ "." ^ field ^ " must be a string")

let optional_string_field = fun ~context ~field fields ->
  match List.assoc_opt field fields with
  | None -> Ok None
  | Some (Data.Json.String value) -> Ok (Some value)
  | Some _ -> Error (context ^ "." ^ field ^ " must be a string")

let bool_field = fun ~context ~field fields ->
  match object_field ~context ~field fields with
  | Error _ as err -> err
  | Ok (Data.Json.Bool value) -> Ok value
  | Ok _ -> Error (context ^ "." ^ field ^ " must be a boolean")

let published_artifact_location_of_json = fun ~context json ->
  match json with
  | Data.Json.Object fields ->
      let* key = string_field ~context ~field:"key" fields in
      let* cdn_url = string_field ~context ~field:"cdn_url" fields in
      Ok { key; cdn_url }
  | _ -> Error (context ^ " must be an object")

let published_record_of_json = fun ~context json ->
  match json with
  | Data.Json.Object fields -> (
      match string_field ~context ~field:"key" fields, bool_field ~context ~field:"created" fields with
      | Ok key, Ok created -> Ok { key; created }
      | (Error err, _)
      | (_, Error err) -> Error err
    )
  | _ -> Error (context ^ " must be an object")

let published_materialization_of_json = fun ~context json ->
  match json with
  | Data.Json.Object fields -> (
      match bool_field ~context ~field:"manifest" fields, bool_field ~context ~field:"source" fields with
      | Ok manifest, Ok source -> Ok { manifest; source }
      | (Error err, _)
      | (_, Error err) -> Error err
    )
  | _ -> Error (context ^ " must be an object")

let published_release_of_json = fun json ->
  match json with
  | Data.Json.Object fields ->
      let* artifact_sha256 = string_field ~context:"publish response" ~field:"artifact_sha256" fields in
      let* package_name = string_field ~context:"publish response" ~field:"package_name" fields in
      let* package_version = string_field ~context:"publish response" ~field:"package_version" fields in
      let* manifest_json = object_field ~context:"publish response" ~field:"manifest" fields in
      let* source_archive_json = object_field ~context:"publish response" ~field:"source_archive" fields in
      let* claim_json = object_field ~context:"publish response" ~field:"claim" fields in
      let* release_json = object_field ~context:"publish response" ~field:"release" fields in
      let* materialization_json = object_field ~context:"publish response" ~field:"materialization" fields in
      let* manifest = published_artifact_location_of_json ~context:"publish response.manifest" manifest_json in
      let* source_archive = published_artifact_location_of_json
        ~context:"publish response.source_archive"
        source_archive_json in
      let* claim = published_record_of_json ~context:"publish response.claim" claim_json in
      let* release = published_record_of_json ~context:"publish response.release" release_json in
      let* materialization = published_materialization_of_json
        ~context:"publish response.materialization"
        materialization_json in
      Ok {
        artifact_sha256;
        package_name;
        package_version;
        manifest;
        source_archive;
        claim;
        release;
        materialization;
      }
  | _ -> Error "publish response must be an object"

let publish_artifact_url = fun ~registry_name ->
  let url = "https://api." ^ registry_name ^ "/v1/publish" in
  match Net.Uri.of_string url with
  | Ok uri -> Ok uri
  | Error _ -> Error ("failed to build publish url '" ^ url ^ "'")

let read_config = fun registry ->
  match registry.source with
  | Filesystem -> (
      match Sparse_index.read_cached_config registry.cache with
      | Error _ as err ->
          err
      | Ok (Some _ as cached) ->
          Ok cached
      | Ok None -> (
          match Sparse_index.bootstrap_config_url ~registry_name:(name registry) with
          | Error _ as err -> err
          | Ok uri -> (
              match fetch_required registry uri with
              | Error _ as err -> err
              | Ok source -> (
                  match Sparse_index.config_of_string source with
                  | Error err -> Error ("failed to decode sparse index config from '"
                  ^ Net.Uri.to_string uri
                  ^ "': "
                  ^ err)
                  | Ok config -> (
                      match Sparse_index.write_cached_config registry.cache ~source with
                      | Error _ as err -> err
                      | Ok () -> Ok (Some config)
                    )
                )
            )
        )
    )
  | In_memory { config; packages=_; releases=_ } -> Ok config

let read_package_document = fun registry ~package_name ->
  let fetch_package_document config =
    match Sparse_index.package_document_url config ~package_name with
    | Error _ as err -> err
    | Ok uri -> (
        match fetch_optional registry uri with
        | Error _ as err ->
            err
        | Ok None ->
            Ok None
        | Ok (Some source) -> (
            match Sparse_index.package_document_of_string source with
            | Error err -> Error ("failed to decode sparse index package document from '"
            ^ Net.Uri.to_string uri
            ^ "': "
            ^ err)
            | Ok document -> (
                match Sparse_index.write_cached_package_document
                  registry.cache
                  ~package_name
                  ~source with
                | Error _ as err -> err
                | Ok () -> Ok (Some document)
              )
          )
      )
  in
  match registry.source with
  | Filesystem -> (
      match Sparse_index.read_cached_package_document registry.cache ~package_name with
      | Error _ -> (
          match read_config registry with
          | Error _ as err ->
              err
          | Ok None ->
              Error ("filesystem registry '" ^ name registry ^ "' is missing sparse index config")
          | Ok (Some config) -> fetch_package_document config
        )
      | Ok (Some _ as cached) ->
          Ok cached
      | Ok None -> (
          match read_config registry with
          | Error _ as err ->
              err
          | Ok None ->
              Error ("filesystem registry '" ^ name registry ^ "' is missing sparse index config")
          | Ok (Some config) -> fetch_package_document config
        )
    )
  | In_memory { packages; config=_; releases=_ } -> Ok (List.assoc_opt
    (Sparse_index.normalized_name package_name)
    packages)

let refresh_package_document = fun registry ~package_name ->
  match registry.source with
  | Filesystem -> (
      match read_config registry with
      | Error _ as err ->
          err
      | Ok None ->
          Error ("filesystem registry '" ^ name registry ^ "' is missing sparse index config")
      | Ok (Some config) ->
          match Sparse_index.package_document_url config ~package_name with
          | Error _ as err -> err
          | Ok uri -> (
              match fetch_optional registry uri with
              | Error _ as err ->
                  err
              | Ok None ->
                  Ok None
              | Ok (Some source) -> (
                  match Sparse_index.package_document_of_string source with
                  | Error err -> Error ("failed to decode sparse index package document from '"
                  ^ Net.Uri.to_string uri
                  ^ "': "
                  ^ err)
                  | Ok document -> (
                      match Sparse_index.write_cached_package_document
                        registry.cache
                        ~package_name
                        ~source with
                      | Error _ as err -> err
                      | Ok () -> Ok (Some document)
                    )
                )
            )
    )
  | In_memory { packages; config=_; releases=_ } -> Ok (List.assoc_opt
    (Sparse_index.normalized_name package_name)
    packages)

let write_release_files = fun ~root (release: release_source) ->
  let manifest_path = Path.(root / Path.v "tusk.toml") in
  match Fs.create_dir_all root with
  | Error err -> Error ("failed to create package source directory '"
  ^ Path.to_string root
  ^ "': "
  ^ IO.error_message err)
  | Ok () -> (
      match Fs.write release.manifest_toml manifest_path with
      | Error err -> Error ("failed to write package manifest '"
      ^ Path.to_string manifest_path
      ^ "': "
      ^ IO.error_message err)
      | Ok () ->
          let rec loop = function
            | [] -> Ok ()
            | (file: release_file) :: rest ->
                let file_path = Path.(root / file.path) in
                let ensure_parent =
                  match Path.parent file_path with
                  | Some parent -> Fs.create_dir_all parent
                  | None -> Ok ()
                in
                match ensure_parent with
                | Error err -> Error ("failed to create package source parent '"
                ^ Path.to_string file_path
                ^ "': "
                ^ IO.error_message err)
                | Ok () -> (
                    match Fs.write file.contents file_path with
                    | Error err -> Error ("failed to write package source file '"
                    ^ Path.to_string file_path
                    ^ "': "
                    ^ IO.error_message err)
                    | Ok () -> loop rest
                  )
          in
          loop release.files
    )

let gzip_error_message = function
  | Compress.Gzip.Kernel_error Kernel.Compress.Gzip.Invalid_data -> "invalid gzip data"
  | Compress.Gzip.Kernel_error Kernel.Compress.Gzip.Need_dictionary -> "gzip stream requires a preset dictionary"
  | Compress.Gzip.Kernel_error Kernel.Compress.Gzip.Buffer_error -> "gzip decoder buffer error"
  | Compress.Gzip.Kernel_error Kernel.Compress.Gzip.Out_of_memory -> "gzip decoder out of memory"
  | Compress.Gzip.Kernel_error (Kernel.Compress.Gzip.Unknown_error msg) -> msg
  | Compress.Gzip.Truncated_input -> "truncated gzip input"

let tar_entry_kind_to_string = function
  | Archive.Tar.File -> "file"
  | Archive.Tar.Directory -> "directory"
  | Archive.Tar.Symlink -> "symlink"
  | Archive.Tar.Hardlink -> "hardlink"
  | Archive.Tar.Other kind -> kind

let tar_error_message = function
  | Archive.Tar.Kernel_error (Kernel.Archive.Tar.Invalid_header msg) -> "invalid tar header: " ^ msg
  | Archive.Tar.Kernel_error Kernel.Archive.Tar.Entry_in_progress -> "tar entry is already being read"
  | Archive.Tar.Kernel_error (Kernel.Archive.Tar.Invalid_state msg) -> "invalid tar reader state: "
  ^ msg
  | Archive.Tar.Kernel_error Kernel.Archive.Tar.Unexpected_eof -> "unexpected end of tar archive"
  | Archive.Tar.Kernel_error Kernel.Archive.Tar.Out_of_memory -> "tar reader out of memory"
  | Archive.Tar.Kernel_error (Kernel.Archive.Tar.Unknown_error msg) -> msg
  | Archive.Tar.Invalid_path path -> "invalid archive path '" ^ path ^ "'"
  | Archive.Tar.Unsafe_path path -> "unsafe archive path '" ^ path ^ "'"
  | Archive.Tar.Unsupported_entry_kind kind -> "unsupported archive entry kind '"
  ^ tar_entry_kind_to_string kind
  ^ "'"
  | Archive.Tar.Duplicate_entry path -> "duplicate archive entry '" ^ Path.to_string path ^ "'"

let tar_extract_file_error_message = function
  | Archive.Tar.Extract_source_error err -> IO.error_message err
  | Archive.Tar.Extract_fs_error err -> IO.error_message err
  | Archive.Tar.Extract_error err -> tar_error_message err

let gzip_tar_extract_error_message = function
  | Archive.Tar.Extract_source_error (Compress.Gzip.Source_error err) -> IO.error_message err
  | Archive.Tar.Extract_source_error (Compress.Gzip.Gzip_error err) -> gzip_error_message err
  | Archive.Tar.Extract_fs_error err -> IO.error_message err
  | Archive.Tar.Extract_error err -> tar_error_message err

let cached_archive_is_gzip = fun archive_path ->
  match Fs.File.open_read archive_path with
  | Error err -> Error ("failed to open cached package archive '"
  ^ Path.to_string archive_path
  ^ "': "
  ^ IO.error_message err)
  | Ok file ->
      Kernel.Fun.protect ~finally:(fun () -> ignore (Fs.File.close file))
        (fun () ->
          let magic = IO.Bytes.make 2 '\000' in
          match Fs.File.read file magic ~offset:0 ~len:2 with
          | Error err -> Error ("failed to read cached package archive '"
          ^ Path.to_string archive_path
          ^ "': "
          ^ IO.error_message err)
          | Ok bytes_read -> Ok (bytes_read = 2
          && Char.equal (IO.Bytes.get magic 0) '\x1f'
          && Char.equal (IO.Bytes.get magic 1) '\x8b'))

let extract_cached_archive = fun ~archive_path ~root ->
  match Fs.create_dir_all root with
  | Error err -> Error ("failed to create package source directory '"
  ^ Path.to_string root
  ^ "': "
  ^ IO.error_message err)
  | Ok () -> (
      let fail detail =
        let _ = Fs.remove_dir_all root in
        Error ("failed to extract cached package archive '" ^ Path.to_string archive_path ^ "': " ^ detail)
      in
      match cached_archive_is_gzip archive_path with
      | Error _ as err ->
          let _ = Fs.remove_dir_all root in
          err
      | Ok true -> (
          match Fs.File.open_read archive_path with
          | Error err -> fail (IO.error_message err)
          | Ok file ->
              Kernel.Fun.protect ~finally:(fun () -> ignore (Fs.File.close file))
                (fun () ->
                  let reader = Fs.File.to_reader file |> Compress.Gzip.to_reader in
                  match Archive.Tar.extract reader ~into:root with
                  | Ok () -> Ok ()
                  | Error err -> fail (gzip_tar_extract_error_message err))
        )
      | Ok false -> (
          match Archive.Tar.extract_file ~archive:archive_path ~into:root with
          | Ok () -> Ok ()
          | Error err -> fail (tar_extract_file_error_message err)
        )
    )

let find_release = fun registry ~package_name ~version ->
  match read_package_document registry ~package_name with
  | Error _ as err ->
      err
  | Ok None ->
      Error ("package '" ^ package_name ^ "' was not found in registry '" ^ name registry ^ "'")
  | Ok (Some document) -> (
      match
        List.find_opt
          (fun (release: Sparse_index.release) ->
            String.equal release.version version)
          document.releases
      with
      | Some release -> Ok release
      | None -> Error ("package '"
      ^ package_name
      ^ "' does not have release '"
      ^ version
      ^ "' in registry '"
      ^ name registry
      ^ "'")
    )

let write_cached_archive = fun ~archive_path ~contents ->
  let archive_parent =
    match Path.parent archive_path with
    | Some parent -> parent
    | None -> Path.v "."
  in
  match Fs.create_dir_all archive_parent with
  | Error err -> Error ("failed to create package archive directory '"
  ^ Path.to_string archive_parent
  ^ "': "
  ^ IO.error_message err)
  | Ok () -> (
      match Fs.write contents archive_path with
      | Ok () -> Ok ()
      | Error err -> Error ("failed to write cached package archive '"
      ^ Path.to_string archive_path
      ^ "': "
      ^ IO.error_message err)
    )

let fetch_release_archive = fun registry ~package_name ~version ~archive_path ->
  match find_release registry ~package_name ~version with
  | Error _ as err -> err
  | Ok release -> (
      match read_config registry with
      | Error _ as err ->
          err
      | Ok None ->
          Error ("filesystem registry '" ^ name registry ^ "' is missing sparse index config")
      | Ok (Some config) -> (
          match Sparse_index.release_source_url config release with
          | Error _ as err -> err
          | Ok uri -> (
              match fetch_required registry uri with
              | Error _ as err -> err
              | Ok archive -> write_cached_archive ~archive_path ~contents:archive
            )
        )
    )

let materialize_release = fun registry ~package_name ~version ->
  let root = Registry_cache.package_src_dir registry.cache ~package_name ~version in
  let manifest_path = Path.(root / Path.v "tusk.toml") in
  let archive_path = Registry_cache.archive_path registry.cache ~package_name ~version in
  match Fs.exists manifest_path with
  | Error err ->
      Error ("failed to check package manifest '"
      ^ Path.to_string manifest_path
      ^ "': "
      ^ IO.error_message err)
  | Ok true ->
      Ok `Already_present
  | Ok false -> (
      match registry.source with
      | Filesystem -> (
          match Fs.exists archive_path with
          | Error err ->
              Error ("failed to check cached package archive '"
              ^ Path.to_string archive_path
              ^ "': "
              ^ IO.error_message err)
          | Ok false -> (
              match fetch_release_archive registry ~package_name ~version ~archive_path with
              | Error _ as err -> err
              | Ok () -> (
                  match extract_cached_archive ~archive_path ~root with
                  | Ok () -> Ok `Materialized
                  | Error _ as err -> err
                )
            )
          | Ok true -> (
              match extract_cached_archive ~archive_path ~root with
              | Ok () -> Ok `Materialized
              | Error _ as err -> err
            )
        )
      | In_memory { releases; config=_; packages=_ } -> (
          match List.assoc_opt (release_key ~package_name ~version) releases with
          | None -> Error ("in-memory registry is missing source contents for '"
          ^ package_name
          ^ "@"
          ^ version
          ^ "'")
          | Some release ->
              match write_release_files ~root release with
              | Ok () -> Ok `Materialized
              | Error _ as err -> err
        )
    )

let publish_response = fun registry uri ~api_token ~artifact ->
  match post_required
    registry
    uri
    ~headers:[ ("authorization", "Bearer " ^ api_token); ("content-type", "application/gzip"); ]
    ~body:artifact with
  | Error _ as err -> err
  | Ok body -> (
      match Data.Json.of_string body with
      | Error err -> Error ("failed to parse publish response JSON: " ^ Data.Json.error_to_string err)
      | Ok json -> published_release_of_json json
    )

let publish_artifact = fun registry ~api_token ~artifact ->
  match publish_artifact_url ~registry_name:(name registry) with
  | Error _ as err -> err
  | Ok uri -> publish_response registry uri ~api_token ~artifact

let publish_from_locator = fun registry ~locator:_ ~selector:_ ~api_token ~artifact ->
  publish_artifact registry ~api_token ~artifact

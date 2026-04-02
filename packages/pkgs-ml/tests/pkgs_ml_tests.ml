open Std
module Test = Std.Test

let test_registry_split_layout = fun () ->
  let cache = Pkgs_ml.Registry_cache.create
    ~tusk_home:(Path.v "/tmp/.tusk")
    ~registry_name:"pkgs.ml"
    ()
  |> Result.expect ~msg:"expected registry cache to be created" in
  let index = Pkgs_ml.Registry_cache.index_dir cache |> Path.to_string in
  let archive = Pkgs_ml.Registry_cache.archive_path cache ~package_name:"std" ~version:"0.1.0"
  |> Path.to_string in
  let src = Pkgs_ml.Registry_cache.package_src_dir cache ~package_name:"std" ~version:"0.1.0"
  |> Path.to_string in
  if
    String.equal index "/tmp/.tusk/registry/pkgs.ml/index"
    && String.equal archive "/tmp/.tusk/registry/pkgs.ml/archive/std/0.1.0.tar"
    && String.equal src "/tmp/.tusk/registry/pkgs.ml/src/std/0.1.0"
  then
    Ok ()
  else
    Error ("unexpected registry layout:\nindex=" ^ index ^ "\narchive=" ^ archive ^ "\nsrc=" ^ src)

let test_sparse_index_layout = fun () ->
  let cache = Pkgs_ml.Registry_cache.create
    ~tusk_home:(Path.v "/tmp/.tusk")
    ~registry_name:"pkgs.ml"
    ()
  |> Result.expect ~msg:"expected registry cache to be created" in
  let actual = Pkgs_ml.Sparse_index.package_cache_path cache ~package_name:"AbCd" |> Path.to_string in
  if String.equal actual "/tmp/.tusk/registry/pkgs.ml/index/ab/cd/abcd.json" then
    Ok ()
  else
    Error ("unexpected sparse index cache path: " ^ actual)

let test_sparse_index_document_parsing = fun () ->
  let source = {|{
  "schema_version": 1,
  "name": "kernel",
  "latest": "0.0.1",
  "updated_at": "2026-03-27T15:27:35Z",
  "releases": [
    {
      "version": "0.0.1",
      "published_at": "2026-03-27T15:27:35Z",
      "canonical_locator": "github.com/leostera/riot-new/packages/kernel",
      "repo_url": "https://github.com/leostera/riot-new",
      "subdir": "packages/kernel",
      "artifact_sha256": "2aef0372bf5b6687db05bda80cde55f960cbfd9d",
      "manifest_key": "packages/github.com/leostera/riot-new/packages/kernel/2aef0372bf5b6687db05bda80cde55f960cbfd9d.manifest.json",
      "source_key": "sources/github.com/leostera/riot-new/2aef0372bf5b6687db05bda80cde55f960cbfd9d.tar.gz",
      "dependencies": [{ "name": "std", "path": "../std" }]
    }
  ]
}|}
  in
  match Pkgs_ml.Sparse_index.package_document_of_string source with
  | Ok document ->
      if
        String.equal document.name "kernel"
        && String.equal document.latest "0.0.1"
        && List.length document.releases = 1
      then
        Ok ()
      else
        Error "unexpected sparse index document contents"
  | Error err -> Error err

let test_sparse_index_cached_reads = fun () ->
  let config =
    Pkgs_ml.Sparse_index.config_of_string
      {|{
  "schema_version": 1,
  "kind": "sparse",
  "package_path_strategy": "cargo-lowercase-v1",
  "index_base_url": "https://cdn.pkgs.ml/index/v1",
  "artifact_base_url": "https://cdn.pkgs.ml"
}|}
    |> Result.expect ~msg:"expected sparse index config to parse"
  in
  let package =
    Pkgs_ml.Sparse_index.package_document_of_string
      {|{
  "schema_version": 1,
  "name": "kernel",
  "latest": "0.0.1",
  "updated_at": "2026-03-27T15:27:35Z",
  "releases": []
}|}
    |> Result.expect ~msg:"expected sparse index package to parse"
  in
  let cache = Pkgs_ml.Registry_cache.create
    ~tusk_home:(Path.v "/tmp/.tusk")
    ~registry_name:"pkgs.ml"
    ()
  |> Result.expect ~msg:"expected registry cache to be created" in
  let registry = Pkgs_ml.Registry.in_memory ~config ~cache ~packages:[ package ] () in
  match Pkgs_ml.Registry.read_config registry, Pkgs_ml.Registry.read_package_document
    registry
    ~package_name:"Kernel" with
  | Ok (Some actual_config), Ok (Some actual_package) when String.equal actual_config.kind "sparse"
  && String.equal actual_package.name "kernel" -> Ok ()
  | Ok _, Ok _ -> Error "expected in-memory registry to return config and normalized package lookup"
  | (Error err, _)
  | (_, Error err) -> Error err

let sparse_index_config_json = {|{
  "schema_version": 1,
  "kind": "sparse",
  "package_path_strategy": "cargo-lowercase-v1",
  "index_base_url": "https://cdn.pkgs.ml/index/v1",
  "artifact_base_url": "https://cdn.pkgs.ml"
}|}

let sparse_index_kernel_json = {|{
  "schema_version": 1,
  "name": "kernel",
  "latest": "0.0.1",
  "updated_at": "2026-03-27T15:27:35Z",
  "releases": []
}|}

let sparse_index_std_release_json = {|{
  "schema_version": 1,
  "name": "std",
  "latest": "0.1.0",
  "updated_at": "2026-03-27T15:27:35Z",
  "releases": [
    {
      "version": "0.1.0",
      "published_at": "2026-03-27T15:27:35Z",
      "canonical_locator": "github.com/leostera/riot/packages/std",
      "repo_url": "https://github.com/leostera/riot",
      "subdir": "packages/std",
      "artifact_sha256": "deadbeef",
      "manifest_key": "packages/std/0.1.0.manifest.json",
      "source_key": "sources/std/0.1.0.tar",
      "dependencies": []
    }
  ]
}|}

type recorded_request = {
  method_: string;
  url: string;
  headers: (string * string) list;
  body: string option;
}

let make_fetch_recorder = fun ?post_handler get_handler ->
  let requests = ref [] in
  let fetch =
    Pkgs_ml.Registry.make_fetch
      ~get:(fun uri ->
        requests := { method_ = "GET"; url = Net.Uri.to_string uri; headers = []; body = None } :: !requests;
        get_handler uri)
      ?post:(Option.map
        (fun post_handler ->
          fun uri ~headers ~body ->
            requests := { method_ = "POST"; url = Net.Uri.to_string uri; headers; body = Some body }
            :: !requests;
            post_handler uri ~headers ~body)
        post_handler)
      ()
  in
  (fetch, requests)

let test_filesystem_registry_fetches_config_on_cache_miss = fun () ->
  match
    Fs.with_tempdir ~prefix:"pkgs_ml_fetch_config"
      (fun tempdir ->
        let cache = Pkgs_ml.Registry_cache.create
          ~tusk_home:Path.(tempdir / Path.v ".tusk")
          ~registry_name:"pkgs.ml"
          ()
        |> Result.expect ~msg:"expected registry cache to be created" in
        let fetch, requests =
          make_fetch_recorder
            (fun uri ->
              if String.equal (Net.Uri.to_string uri) "https://cdn.pkgs.ml/index/v1/config.json" then
                Ok { Pkgs_ml.Registry.status_code = 200; body = sparse_index_config_json }
              else
                Error ("unexpected fetch url " ^ Net.Uri.to_string uri))
        in
        let registry = Pkgs_ml.Registry.filesystem ~fetch cache in
        match Pkgs_ml.Registry.read_config registry with
        | Error err ->
            Error err
        | Ok None ->
            Error "expected filesystem registry to fetch sparse index config"
        | Ok (Some config) -> (
            match Pkgs_ml.Sparse_index.read_cached_config cache with
            | Error err ->
                Error err
            | Ok None ->
                Error "expected fetched config to be cached"
            | Ok (Some cached) ->
                let requested = List.rev !requests |> List.map (fun request -> request.url) in
                if
                  String.equal config.kind "sparse"
                  && String.equal cached.index_base_url "https://cdn.pkgs.ml/index/v1"
                  && requested = [ "https://cdn.pkgs.ml/index/v1/config.json" ]
                then
                  Ok ()
                else
                  Error "unexpected fetched sparse index config state"
          ))
  with
  | Error err -> Error (IO.error_message err)
  | Ok result -> result

let test_filesystem_registry_fetches_package_document_on_cache_miss = fun () ->
  match
    Fs.with_tempdir ~prefix:"pkgs_ml_fetch_package"
      (fun tempdir ->
        let cache = Pkgs_ml.Registry_cache.create
          ~tusk_home:Path.(tempdir / Path.v ".tusk")
          ~registry_name:"pkgs.ml"
          ()
        |> Result.expect ~msg:"expected registry cache to be created" in
        let fetch, requests =
          make_fetch_recorder
            (fun uri ->
              match Net.Uri.to_string uri with
              | "https://cdn.pkgs.ml/index/v1/config.json" -> Ok {
                Pkgs_ml.Registry.status_code = 200;
                body = sparse_index_config_json
              }
              | "https://cdn.pkgs.ml/index/v1/ke/rn/kernel.json" -> Ok {
                Pkgs_ml.Registry.status_code = 200;
                body = sparse_index_kernel_json
              }
              | url -> Error ("unexpected fetch url " ^ url))
        in
        let registry = Pkgs_ml.Registry.filesystem ~fetch cache in
        match Pkgs_ml.Registry.read_package_document registry ~package_name:"Kernel" with
        | Error err ->
            Error err
        | Ok None ->
            Error "expected filesystem registry to fetch package document"
        | Ok (Some document) -> (
            match Pkgs_ml.Sparse_index.read_cached_config cache, Pkgs_ml.Sparse_index.read_cached_package_document
              cache
              ~package_name:"Kernel" with
            | (Error err, _)
            | (_, Error err) ->
                Error err
            | (Ok None, _)
            | (_, Ok None) ->
                Error "expected fetched sparse index files to be cached"
            | Ok (Some _), Ok (Some cached) ->
                let requested = List.rev !requests |> List.map (fun request -> request.url) in
                if
                  String.equal document.name "kernel"
                  && String.equal cached.name "kernel"
                  && requested
                  = [
                    "https://cdn.pkgs.ml/index/v1/config.json";
                    "https://cdn.pkgs.ml/index/v1/ke/rn/kernel.json";
                  ]
                then
                  Ok ()
                else
                  Error "unexpected fetched sparse index package document state"
          ))
  with
  | Error err -> Error (IO.error_message err)
  | Ok result -> result

let test_filesystem_registry_returns_none_for_missing_package_document = fun () ->
  match
    Fs.with_tempdir ~prefix:"pkgs_ml_fetch_missing_package"
      (fun tempdir ->
        let cache = Pkgs_ml.Registry_cache.create
          ~tusk_home:Path.(tempdir / Path.v ".tusk")
          ~registry_name:"pkgs.ml"
          ()
        |> Result.expect ~msg:"expected registry cache to be created" in
        let fetch, requests =
          make_fetch_recorder
            (fun uri ->
              match Net.Uri.to_string uri with
              | "https://cdn.pkgs.ml/index/v1/config.json" -> Ok {
                Pkgs_ml.Registry.status_code = 200;
                body = sparse_index_config_json
              }
              | "https://cdn.pkgs.ml/index/v1/mi/ss/missing.json" -> Ok {
                Pkgs_ml.Registry.status_code = 404;
                body = ""
              }
              | url -> Error ("unexpected fetch url " ^ url))
        in
        let registry = Pkgs_ml.Registry.filesystem ~fetch cache in
        match Pkgs_ml.Registry.read_package_document registry ~package_name:"Missing" with
        | Error err ->
            Error err
        | Ok (Some _) ->
            Error "expected missing package document lookup to return none"
        | Ok None -> (
            match Pkgs_ml.Sparse_index.read_cached_package_document cache ~package_name:"Missing" with
            | Error err ->
                Error err
            | Ok (Some _) ->
                Error "expected missing package document lookup to leave cache empty"
            | Ok None ->
                let requested = List.rev !requests |> List.map (fun request -> request.url) in
                if
                  requested
                  = [
                    "https://cdn.pkgs.ml/index/v1/config.json";
                    "https://cdn.pkgs.ml/index/v1/mi/ss/missing.json";
                  ]
                then
                  Ok ()
                else
                  Error "unexpected sparse index fetch sequence for missing package document"
          ))
  with
  | Error err -> Error (IO.error_message err)
  | Ok result -> result

let test_registry_materializes_in_memory_release = fun () ->
  match
    Fs.with_tempdir ~prefix:"pkgs_ml_materialize"
      (fun tempdir ->
        let cache = Pkgs_ml.Registry_cache.create
          ~tusk_home:Path.(tempdir / Path.v ".tusk")
          ~registry_name:"pkgs.ml"
          ()
        |> Result.expect ~msg:"expected registry cache to be created" in
        let package =
          Pkgs_ml.Sparse_index.{
            schema_version = 1;
            name = "std";
            latest = "0.1.0";
            updated_at = "2026-04-01T00:00:00Z";
            releases = [];
          }
        in
        let registry = Pkgs_ml.Registry.in_memory
          ~cache
          ~packages:[ package ]
          ~releases:[
            {
              package_name = "std";
              version = "0.1.0";
              manifest_toml = "[package]\nname = \"std\"\n";
              files = [
                { path = Path.v "src/std.ml"; contents = "let answer = 42\n" };
                { path = Path.v "README.md"; contents = "# std\n" };
              ]
            };
          ]
          () in
        match Pkgs_ml.Registry.materialize_release registry ~package_name:"std" ~version:"0.1.0" with
        | Error err ->
            Error err
        | Ok `Already_present ->
            Error "expected in-memory release to be materialized on first attempt"
        | Ok `Materialized ->
            let manifest_path = Pkgs_ml.Registry_cache.package_src_dir
              cache
              ~package_name:"std"
              ~version:"0.1.0"
            |> fun root -> Path.(root / Path.v "tusk.toml") in
            let source_path = Pkgs_ml.Registry_cache.package_src_dir
              cache
              ~package_name:"std"
              ~version:"0.1.0"
            |> fun root -> Path.(root / Path.v "src/std.ml") in
            match Fs.read manifest_path, Fs.read source_path with
            | Ok manifest, Ok source when String.equal manifest "[package]\nname = \"std\"\n"
            && String.equal source "let answer = 42\n" -> Ok ()
            | Ok _, Ok _ -> Error "expected materialized release contents to roundtrip from the in-memory registry"
            | (Error err, _)
            | (_, Error err) -> Error (IO.error_message err))
  with
  | Error err -> Error (IO.error_message err)
  | Ok result -> result

let test_registry_materialize_skips_existing_release = fun () ->
  match
    Fs.with_tempdir ~prefix:"pkgs_ml_materialize_skip"
      (fun tempdir ->
        let cache = Pkgs_ml.Registry_cache.create
          ~tusk_home:Path.(tempdir / Path.v ".tusk")
          ~registry_name:"pkgs.ml"
          ()
        |> Result.expect ~msg:"expected registry cache to be created" in
        let registry = Pkgs_ml.Registry.in_memory
          ~cache
          ~packages:[]
          ~releases:[
            {
              package_name = "std";
              version = "0.1.0";
              manifest_toml = "[package]\nname = \"std\"\n";
              files = []
            };
          ]
          () in
        match Pkgs_ml.Registry.materialize_release registry ~package_name:"std" ~version:"0.1.0" with
        | Error err -> Error err
        | Ok _ -> (
            match Pkgs_ml.Registry.materialize_release registry ~package_name:"std" ~version:"0.1.0" with
            | Ok `Already_present -> Ok ()
            | Ok `Materialized -> Error "expected second materialization to detect existing package sources"
            | Error err -> Error err
          ))
  with
  | Error err -> Error (IO.error_message err)
  | Ok result -> result

let tar_block_size = 512

let tar_bytes_set_string = fun dst ~offset ~width value ->
  let bytes = IO.Bytes.of_string value in
  let copy_len = min width (IO.Bytes.length bytes) in
  IO.Bytes.blit bytes 0 dst offset copy_len

let tar_octal_string = fun value ->
  let rec loop acc remaining =
    if Int64.equal remaining 0L then
      acc
    else
      let digit = Int64.to_int (Int64.rem remaining 8L) in
      let ch = Char.chr (Char.code '0' + digit) in
      loop (String.make 1 ch ^ acc) (Int64.div remaining 8L)
  in
  if Int64.equal value 0L then
    "0"
  else
    loop "" value

let tar_zero_pad_left = fun width value ->
  if String.length value >= width then
    String.sub value (String.length value - width) width
  else
    String.make (width - String.length value) '0' ^ value

let tar_bytes_set_octal = fun dst ~offset ~width value ->
  let digits_width = max 1 (width - 1) in
  let trimmed = tar_zero_pad_left digits_width (tar_octal_string value) in
  tar_bytes_set_string dst ~offset ~width:((width - 1)) trimmed;
  IO.Bytes.set dst (offset + width - 1) '\000'

let tar_compute_checksum = fun header ->
  let sum = ref 0 in
  for index = 0 to tar_block_size - 1 do
    sum := !sum + Char.code (IO.Bytes.get header index)
  done;
  !sum

let tar_make_header = fun ~name ~kind ~mode ~size ->
  let header = IO.Bytes.make tar_block_size '\000' in
  tar_bytes_set_string header ~offset:0 ~width:100 name;
  tar_bytes_set_octal header ~offset:100 ~width:8 mode;
  tar_bytes_set_octal header ~offset:108 ~width:8 0L;
  tar_bytes_set_octal header ~offset:116 ~width:8 0L;
  tar_bytes_set_octal header ~offset:124 ~width:12 size;
  tar_bytes_set_octal header ~offset:136 ~width:12 0L;
  tar_bytes_set_string header ~offset:148 ~width:8 "        ";
  IO.Bytes.set header 156 kind;
  tar_bytes_set_string header ~offset:257 ~width:6 "ustar";
  tar_bytes_set_string header ~offset:263 ~width:2 "00";
  let checksum = tar_compute_checksum header in
  let checksum_field = tar_zero_pad_left 6 (tar_octal_string (Int64.of_int checksum)) ^ "\000 " in
  tar_bytes_set_string header ~offset:148 ~width:8 checksum_field;
  header

let tar_pad_data = fun data ->
  let len = String.length data in
  let remainder = Int.rem len tar_block_size in
  if remainder = 0 then
    ""
  else
    String.make (tar_block_size - remainder) '\000'

let create_test_archive = fun ~source_root ~archive_path ->
  let archive_parent =
    match Path.parent archive_path with
    | Some parent -> parent
    | None -> Path.v "."
  in
  let manifest_path = Path.(source_root / Path.v "tusk.toml") in
  let source_file_path = Path.(source_root / Path.v "src/std.ml") in
  match Fs.create_dir_all archive_parent with
  | Error err -> Error ("failed to create archive parent directory: " ^ IO.error_message err)
  | Ok () -> (
      match Fs.read manifest_path, Fs.read source_file_path with
      | (Error err, _)
      | (_, Error err) -> Error ("failed to read source fixture for test archive: "
      ^ IO.error_message err)
      | Ok manifest, Ok source ->
          let buffer = IO.Buffer.create 2_048 in
          let add_entry ~name ~kind ~mode data =
            let size = Int64.of_int (String.length data) in
            IO.Buffer.add_bytes buffer (tar_make_header ~name ~kind ~mode ~size);
            IO.Buffer.add_string buffer data;
            IO.Buffer.add_string buffer (tar_pad_data data)
          in
          add_entry ~name:"./" ~kind:'5' ~mode:0o755L "";
          add_entry ~name:"./src/" ~kind:'5' ~mode:0o755L "";
          add_entry ~name:"./tusk.toml" ~kind:'0' ~mode:0o644L manifest;
          add_entry ~name:"./src/std.ml" ~kind:'0' ~mode:0o644L source;
          IO.Buffer.add_string buffer (String.make (tar_block_size * 2) '\000');
          Fs.write (IO.Buffer.contents buffer) archive_path
          |> Result.map_err (fun err -> "failed to write test archive: " ^ IO.error_message err)
    )

let gzip_file = fun ~src ~dst ->
  let parent =
    match Path.parent dst with
    | Some parent -> parent
    | None -> Path.v "."
  in
  match Fs.create_dir_all parent with
  | Error err -> Error ("failed to create gzip output parent directory: " ^ IO.error_message err)
  | Ok () ->
      Compress.Gzip.compress_file ~src ~dst |> Result.map_err
        (
          function
          | Compress.Gzip.File_io_error err -> "failed to gzip test archive: " ^ IO.error_message err
          | Compress.Gzip.File_gzip_error err -> (
              match err with
              | Compress.Gzip.Kernel_error Kernel.Compress.Gzip.Invalid_data -> "failed to gzip test archive: invalid gzip data"
              | Compress.Gzip.Kernel_error Kernel.Compress.Gzip.Need_dictionary -> "failed to gzip test archive: gzip stream requires a preset dictionary"
              | Compress.Gzip.Kernel_error Kernel.Compress.Gzip.Buffer_error -> "failed to gzip test archive: gzip encoder buffer error"
              | Compress.Gzip.Kernel_error Kernel.Compress.Gzip.Out_of_memory -> "failed to gzip test archive: gzip encoder out of memory"
              | Compress.Gzip.Kernel_error (Kernel.Compress.Gzip.Unknown_error msg) -> "failed to gzip test archive: "
              ^ msg
              | Compress.Gzip.Truncated_input -> "failed to gzip test archive: truncated gzip input"
            )
        )

let test_filesystem_registry_materializes_cached_release = fun () ->
  match
    Fs.with_tempdir ~prefix:"pkgs_ml_filesystem_materialize"
      (fun tempdir ->
        let cache = Pkgs_ml.Registry_cache.create
          ~tusk_home:Path.(tempdir / Path.v ".tusk")
          ~registry_name:"pkgs.ml"
          ()
        |> Result.expect ~msg:"expected registry cache to be created" in
        let source_root = Path.(tempdir / Path.v "source/std-0.1.0") in
        let source_file = Path.(source_root / Path.v "src/std.ml") in
        Fs.create_dir_all Path.(source_root / Path.v "src") |> Result.expect ~msg:"expected source directory to be created";
        Fs.write
          "[package]\nname = \"std\"\nversion = \"0.1.0\"\n"
          Path.(source_root / Path.v "tusk.toml")
        |> Result.expect ~msg:"expected manifest to be written";
        Fs.write "let answer = 42\n" source_file |> Result.expect ~msg:"expected source file to be written";
        let archive_path = Pkgs_ml.Registry_cache.archive_path cache ~package_name:"std" ~version:"0.1.0" in
        match create_test_archive ~source_root ~archive_path with
        | Error err -> Error err
        | Ok () ->
            let registry = Pkgs_ml.Registry.filesystem cache in
            match Pkgs_ml.Registry.materialize_release registry ~package_name:"std" ~version:"0.1.0" with
            | Error err ->
                Error err
            | Ok `Already_present ->
                Error "expected cached archive to materialize on first attempt"
            | Ok `Materialized ->
                let manifest_path = Pkgs_ml.Registry_cache.package_src_dir
                  cache
                  ~package_name:"std"
                  ~version:"0.1.0"
                |> fun root -> Path.(root / Path.v "tusk.toml") in
                let materialized_source = Pkgs_ml.Registry_cache.package_src_dir
                  cache
                  ~package_name:"std"
                  ~version:"0.1.0"
                |> fun root -> Path.(root / Path.v "src/std.ml") in
                match Fs.read manifest_path, Fs.read materialized_source with
                | Ok manifest, Ok source when String.equal manifest "[package]\nname = \"std\"\nversion = \"0.1.0\"\n"
                && String.equal source "let answer = 42\n" -> Ok ()
                | Ok _, Ok _ -> Error "expected filesystem registry to extract the cached archive into src/"
                | (Error err, _)
                | (_, Error err) -> Error (IO.error_message err))
  with
  | Error err -> Error (IO.error_message err)
  | Ok result -> result

let test_filesystem_registry_materializes_gzip_cached_release = fun () ->
  match
    Fs.with_tempdir ~prefix:"pkgs_ml_filesystem_materialize_gzip"
      (fun tempdir ->
        let cache = Pkgs_ml.Registry_cache.create
          ~tusk_home:Path.(tempdir / Path.v ".tusk")
          ~registry_name:"pkgs.ml"
          ()
        |> Result.expect ~msg:"expected registry cache to be created" in
        let source_root = Path.(tempdir / Path.v "source/std-0.1.0") in
        let plain_archive = Path.(tempdir / Path.v "downloads/std-0.1.0.tar") in
        let source_file = Path.(source_root / Path.v "src/std.ml") in
        Fs.create_dir_all Path.(source_root / Path.v "src") |> Result.expect ~msg:"expected source directory to be created";
        Fs.write
          "[package]\nname = \"std\"\nversion = \"0.1.0\"\n"
          Path.(source_root / Path.v "tusk.toml")
        |> Result.expect ~msg:"expected manifest to be written";
        Fs.write "let answer = 42\n" source_file |> Result.expect ~msg:"expected source file to be written";
        let archive_path = Pkgs_ml.Registry_cache.archive_path cache ~package_name:"std" ~version:"0.1.0" in
        match create_test_archive ~source_root ~archive_path:plain_archive with
        | Error err -> Error err
        | Ok () -> (
            match gzip_file ~src:plain_archive ~dst:archive_path with
            | Error err -> Error err
            | Ok () ->
                let registry = Pkgs_ml.Registry.filesystem cache in
                match Pkgs_ml.Registry.materialize_release registry ~package_name:"std" ~version:"0.1.0" with
                | Error err ->
                    Error err
                | Ok `Already_present ->
                    Error "expected gzipped cached archive to materialize on first attempt"
                | Ok `Materialized ->
                    let manifest_path = Pkgs_ml.Registry_cache.package_src_dir
                      cache
                      ~package_name:"std"
                      ~version:"0.1.0"
                    |> fun root -> Path.(root / Path.v "tusk.toml") in
                    let materialized_source = Pkgs_ml.Registry_cache.package_src_dir
                      cache
                      ~package_name:"std"
                      ~version:"0.1.0"
                    |> fun root -> Path.(root / Path.v "src/std.ml") in
                    match Fs.read manifest_path, Fs.read materialized_source with
                    | Ok manifest, Ok source when String.equal manifest "[package]\nname = \"std\"\nversion = \"0.1.0\"\n"
                    && String.equal source "let answer = 42\n" -> Ok ()
                    | Ok _, Ok _ -> Error "expected filesystem registry to extract a gzipped cached archive into src/"
                    | (Error err, _)
                    | (_, Error err) -> Error (IO.error_message err)
          ))
  with
  | Error err -> Error (IO.error_message err)
  | Ok result -> result

let test_filesystem_registry_downloads_release_archive_on_cache_miss = fun () ->
  match
    Fs.with_tempdir ~prefix:"pkgs_ml_filesystem_registry_download"
      (fun tempdir ->
        let cache = Pkgs_ml.Registry_cache.create
          ~tusk_home:Path.(tempdir / Path.v ".tusk")
          ~registry_name:"pkgs.ml"
          ()
        |> Result.expect ~msg:"expected registry cache to be created" in
        let source_root = Path.(tempdir / Path.v "source/std-0.1.0") in
        let source_file = Path.(source_root / Path.v "src/std.ml") in
        let downloaded_archive = Path.(tempdir / Path.v "downloads/std-0.1.0.tar") in
        Fs.create_dir_all Path.(source_root / Path.v "src") |> Result.expect ~msg:"expected source directory to be created";
        Fs.write
          "[package]\nname = \"std\"\nversion = \"0.1.0\"\n"
          Path.(source_root / Path.v "tusk.toml")
        |> Result.expect ~msg:"expected manifest to be written";
        Fs.write "let answer = 42\n" source_file |> Result.expect ~msg:"expected source file to be written";
        match create_test_archive ~source_root ~archive_path:downloaded_archive with
        | Error err -> Error err
        | Ok () -> (
            match Fs.read downloaded_archive with
            | Error err -> Error ("failed to read test archive: " ^ IO.error_message err)
            | Ok archive_body ->
                let fetch, requests =
                  make_fetch_recorder
                    (fun uri ->
                      match Net.Uri.to_string uri with
                      | "https://cdn.pkgs.ml/index/v1/config.json" -> Ok {
                        Pkgs_ml.Registry.status_code = 200;
                        body = sparse_index_config_json
                      }
                      | "https://cdn.pkgs.ml/index/v1/3/s/std.json" -> Ok {
                        Pkgs_ml.Registry.status_code = 200;
                        body = sparse_index_std_release_json
                      }
                      | "https://cdn.pkgs.ml/sources/std/0.1.0.tar" -> Ok {
                        Pkgs_ml.Registry.status_code = 200;
                        body = archive_body
                      }
                      | url -> Error ("unexpected fetch url " ^ url))
                in
                let registry = Pkgs_ml.Registry.filesystem ~fetch cache in
                match Pkgs_ml.Registry.materialize_release registry ~package_name:"std" ~version:"0.1.0" with
                | Error err ->
                    Error err
                | Ok `Already_present ->
                    Error "expected uncached release to download and materialize on first attempt"
                | Ok `Materialized ->
                    let archive_path = Pkgs_ml.Registry_cache.archive_path
                      cache
                      ~package_name:"std"
                      ~version:"0.1.0" in
                    let manifest_path = Pkgs_ml.Registry_cache.package_src_dir
                      cache
                      ~package_name:"std"
                      ~version:"0.1.0"
                    |> fun root -> Path.(root / Path.v "tusk.toml") in
                    let materialized_source = Pkgs_ml.Registry_cache.package_src_dir
                      cache
                      ~package_name:"std"
                      ~version:"0.1.0"
                    |> fun root -> Path.(root / Path.v "src/std.ml") in
                    match Fs.exists archive_path, Fs.read manifest_path, Fs.read materialized_source with
                    | (Error err, _, _)
                    | (_, Error err, _)
                    | (_, _, Error err) ->
                        Error (IO.error_message err)
                    | Ok false, _, _ ->
                        Error "expected downloaded archive to be cached"
                    | Ok true, Ok manifest, Ok source ->
                        let requested = List.rev !requests |> List.map (fun request -> request.url) in
                        if
                          String.equal manifest "[package]\nname = \"std\"\nversion = \"0.1.0\"\n"
                          && String.equal source "let answer = 42\n"
                          && requested
                          = [
                            "https://cdn.pkgs.ml/index/v1/config.json";
                            "https://cdn.pkgs.ml/index/v1/3/s/std.json";
                            "https://cdn.pkgs.ml/sources/std/0.1.0.tar";
                          ]
                        then
                          Ok ()
                        else
                          Error "unexpected registry download/materialization state"
          ))
  with
  | Error err -> Error (IO.error_message err)
  | Ok result -> result

let test_registry_publish_from_locator_posts_tarball_to_publish_route = fun () ->
  let cache = Pkgs_ml.Registry_cache.create
    ~tusk_home:(Path.v "/tmp/.tusk")
    ~registry_name:"pkgs.ml"
    ()
  |> Result.expect ~msg:"expected registry cache to be created" in
  let artifact = "fake-tarball-bytes" in
  let fetch, requests =
    make_fetch_recorder
      ~post_handler:(fun _uri ~headers:_ ~body:_ ->
        Ok {
          Pkgs_ml.Registry.status_code = 200;
          body =
            {|{
  "package_name": "minttea",
  "package_version": "0.4.2",
  "artifact_sha256": "0123456789abcdef0123456789abcdef01234567",
  "manifest": {
    "key": "packages/minttea/0.4.2/0123456789abcdef0123456789abcdef01234567.manifest.json",
    "cdn_url": "https://cdn.pkgs.ml/packages/minttea/0.4.2/0123456789abcdef0123456789abcdef01234567.manifest.json"
  },
  "source_archive": {
    "key": "sources/minttea/0.4.2/0123456789abcdef0123456789abcdef01234567.tar.gz",
    "cdn_url": "https://cdn.pkgs.ml/sources/minttea/0.4.2/0123456789abcdef0123456789abcdef01234567.tar.gz"
  },
  "claim": {
    "key": "claims/minttea.json",
    "created": true
  },
  "release": {
    "key": "releases/minttea/0.4.2.json",
    "created": true
  },
  "materialization": {
    "manifest": false,
    "source": false
  }
}|};
        })
      (fun uri -> Error ("unexpected GET " ^ Net.Uri.to_string uri))
  in
  let registry = Pkgs_ml.Registry.filesystem ~fetch cache in
  match Pkgs_ml.Registry.publish_from_locator
    registry
    ~locator:"github.com/leostera/minttea"
    ~selector:"main"
    ~api_token:"root-secret"
    ~artifact with
  | Error err -> Error err
  | Ok published ->
      let requested = List.rev !requests in
      match requested with
      | [ request ] ->
          let has_header name value =
            List.exists
              (fun (header_name, header_value) ->
                String.equal header_name name && String.equal header_value value)
              request.headers
          in
          if
            String.equal request.method_ "POST"
            && String.equal request.url "https://api.pkgs.ml/v1/publish"
            && request.body = Some artifact
            && has_header "authorization" "Bearer root-secret"
            && has_header "content-type" "application/gzip"
            && String.equal published.artifact_sha256 "0123456789abcdef0123456789abcdef01234567"
            && String.equal published.package_name "minttea"
            && String.equal published.package_version "0.4.2"
            && String.equal published.manifest.key "packages/minttea/0.4.2/0123456789abcdef0123456789abcdef01234567.manifest.json"
            && String.equal published.source_archive.key "sources/minttea/0.4.2/0123456789abcdef0123456789abcdef01234567.tar.gz"
            && published.claim.created
            && published.release.created
            && not published.materialization.manifest
            && not published.materialization.source
          then
            Ok ()
          else
            Error "unexpected publish-from-locator request or response"
      | _ -> Error "expected exactly one publish request"

let test_registry_publish_from_locator_bubbles_registry_error_message = fun () ->
  let cache = Pkgs_ml.Registry_cache.create
    ~tusk_home:(Path.v "/tmp/.tusk")
    ~registry_name:"pkgs.ml"
    ()
  |> Result.expect ~msg:"expected registry cache to be created" in
  let fetch, _requests =
    make_fetch_recorder
      ~post_handler:(fun _uri ~headers:_ ~body:_ ->
        Ok {
          Pkgs_ml.Registry.status_code = 404;
          body =
            {|{
  "error": "package_not_found",
  "message": "package `std` was not found in registry `pkgs.ml`"
}|};
        })
      (fun uri -> Error ("unexpected GET " ^ Net.Uri.to_string uri))
  in
  let registry = Pkgs_ml.Registry.filesystem ~fetch cache in
  match Pkgs_ml.Registry.publish_from_locator
    registry
    ~locator:"github.com/leostera/riot/packages/std"
    ~selector:"main"
    ~api_token:"root-secret"
    ~artifact:"tarball" with
  | Ok _ -> Error "expected publish artifact to return the registry error"
  | Error err ->
      if String.equal err "package `std` was not found in registry `pkgs.ml`" then
        Ok ()
      else
        Error ("unexpected publish artifact error: " ^ err)

let test_registry_publish_artifact_posts_tarball_to_artifact_publish_route = fun () ->
  let cache = Pkgs_ml.Registry_cache.create
    ~tusk_home:(Path.v "/tmp/.tusk")
    ~registry_name:"pkgs.ml"
    ()
  |> Result.expect ~msg:"expected registry cache to be created" in
  let artifact = "fake-tarball-bytes" in
  let fetch, requests =
    make_fetch_recorder
      ~post_handler:(fun _uri ~headers:_ ~body:_ ->
        Ok {
          Pkgs_ml.Registry.status_code = 200;
          body =
            {|{
  "package_name": "minttea",
  "package_version": "0.4.2",
  "artifact_sha256": "0123456789abcdef0123456789abcdef01234567",
  "manifest": {
    "key": "packages/minttea/0.4.2/0123456789abcdef0123456789abcdef01234567.manifest.json",
    "cdn_url": "https://cdn.pkgs.ml/packages/minttea/0.4.2/0123456789abcdef0123456789abcdef01234567.manifest.json"
  },
  "source_archive": {
    "key": "sources/minttea/0.4.2/0123456789abcdef0123456789abcdef01234567.tar.gz",
    "cdn_url": "https://cdn.pkgs.ml/sources/minttea/0.4.2/0123456789abcdef0123456789abcdef01234567.tar.gz"
  },
  "claim": {
    "key": "claims/minttea.json",
    "created": true
  },
  "release": {
    "key": "releases/minttea/0.4.2.json",
    "created": true
  },
  "materialization": {
    "manifest": false,
    "source": false
  }
}|};
        })
      (fun uri -> Error ("unexpected GET " ^ Net.Uri.to_string uri))
  in
  let registry = Pkgs_ml.Registry.filesystem ~fetch cache in
  match Pkgs_ml.Registry.publish_artifact registry ~api_token:"root-secret" ~artifact with
  | Error err -> Error err
  | Ok published ->
      let requested = List.rev !requests in
      match requested with
      | [ request ] ->
          let has_header name value =
            List.exists
              (fun (header_name, header_value) ->
                String.equal header_name name && String.equal header_value value)
              request.headers
          in
          if
            String.equal request.method_ "POST"
            && String.equal request.url "https://api.pkgs.ml/v1/publish"
            && request.body = Some artifact
            && has_header "authorization" "Bearer root-secret"
            && has_header "content-type" "application/gzip"
            && String.equal published.artifact_sha256 "0123456789abcdef0123456789abcdef01234567"
            && String.equal published.package_name "minttea"
            && String.equal published.package_version "0.4.2"
          then
            Ok ()
          else
            Error "unexpected artifact publish request or response"
      | _ -> Error "expected exactly one publish request"

let test_registry_publish_artifact_bubbles_transport_exceptions_as_errors = fun () ->
  let cache = Pkgs_ml.Registry_cache.create
    ~tusk_home:(Path.v "/tmp/.tusk")
    ~registry_name:"pkgs.ml"
    ()
  |> Result.expect ~msg:"expected registry cache to be created" in
  let fetch, _requests =
    make_fetch_recorder
      ~post_handler:(fun _uri ~headers:_ ~body:_ -> raise (Failure "SSL_write error"))
      (fun uri -> Error ("unexpected GET " ^ Net.Uri.to_string uri))
  in
  let registry = Pkgs_ml.Registry.filesystem ~fetch cache in
  match Pkgs_ml.Registry.publish_artifact registry ~api_token:"root-secret" ~artifact:"tarball" with
  | Ok _ -> Error "expected publish artifact to return the transport exception as an error"
  | Error err ->
      if String.equal err "SSL_write error" then
        Ok ()
      else
        Error ("unexpected publish artifact transport error: " ^ err)

let tests =
  Test.[
    case "registry cache: uses cargo-style split layout" test_registry_split_layout;
    case "sparse index: resolves cache path from normalized package name" test_sparse_index_layout;
    case "sparse index: parses package documents" test_sparse_index_document_parsing;
    case "registry: in-memory registry returns config and packages" test_sparse_index_cached_reads;
    case "registry: filesystem registry fetches config on cache miss" test_filesystem_registry_fetches_config_on_cache_miss;
    case "registry: filesystem registry fetches package document on cache miss" test_filesystem_registry_fetches_package_document_on_cache_miss;
    case "registry: filesystem registry returns none for missing package document" test_filesystem_registry_returns_none_for_missing_package_document;
    case "registry: in-memory registry materializes release source trees" test_registry_materializes_in_memory_release;
    case "registry: materialization skips existing release sources" test_registry_materialize_skips_existing_release;
    case "registry: filesystem registry materializes cached release archives" test_filesystem_registry_materializes_cached_release;
    case "registry: filesystem registry materializes gzipped cached release archives" test_filesystem_registry_materializes_gzip_cached_release;
    case "registry: filesystem registry downloads release archives on cache miss" test_filesystem_registry_downloads_release_archive_on_cache_miss;
    case "registry: publish from locator posts tarball to publish route" test_registry_publish_from_locator_posts_tarball_to_publish_route;
    case "registry: publish from locator bubbles registry error message" test_registry_publish_from_locator_bubbles_registry_error_message;
    case "registry: publish artifact posts tarball to artifact publish route" test_registry_publish_artifact_posts_tarball_to_artifact_publish_route;
    case "registry: publish artifact bubbles transport exceptions as errors" test_registry_publish_artifact_bubbles_transport_exceptions_as_errors;
  ]

let name = "pkgs-ml Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()

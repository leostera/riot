open Std

let ( let* ) = Result.and_then

type config = {
  schema_version: int;
  kind: string;
  package_path_strategy: string;
  index_base_url: string;
  artifact_base_url: string;
}

type dependency = {
  name: string;
  raw: Data.Json.t;
}

type release = {
  version: string;
  published_at: string;
  canonical_locator: string;
  repo_url: string;
  subdir: string;
  artifact_sha256: string;
  description: string option;
  license: string option;
  homepage: string option;
  repository: string option;
  root_module: string option;
  categories: string list;
  keywords: string list;
  manifest_key: string;
  source_key: string;
  dependencies: dependency list;
}

type package_document = {
  schema_version: int;
  name: string;
  latest: string;
  updated_at: string;
  releases: release list;
}

let object_field = fun ~context ~field fields ->
  match List.assoc_opt field fields with
  | Some value -> Ok value
  | None -> Error (context ^ " is missing required field '" ^ field ^ "'")

let string_field = fun ~context ~field fields ->
  match object_field ~context ~field fields with
  | Error _ as err -> err
  | Ok (Data.Json.String value) -> Ok value
  | Ok _ -> Error (context ^ "." ^ field ^ " must be a string")

let int_field = fun ~context ~field fields ->
  match object_field ~context ~field fields with
  | Error _ as err -> err
  | Ok (Data.Json.Int value) -> Ok value
  | Ok _ -> Error (context ^ "." ^ field ^ " must be an integer")

let optional_string_field = fun ~field fields ->
  match List.assoc_opt field fields with
  | None
  | Some Data.Json.Null -> Ok None
  | Some (Data.Json.String value) -> Ok (Some value)
  | Some _ -> Error ("field '" ^ field ^ "' must be a string when present")

let string_field_with_fallback = fun ~context ~field ~fallback fields ->
  match List.assoc_opt field fields with
  | Some (Data.Json.String value) -> Ok value
  | Some _ -> Error (context ^ "." ^ field ^ " must be a string")
  | None -> (
      match List.assoc_opt fallback fields with
      | Some (Data.Json.String value) -> Ok value
      | Some _ -> Error (context ^ "." ^ fallback ^ " must be a string")
      | None -> Error (context ^ " is missing required field '" ^ field ^ "'")
    )

let optional_string_list_field = fun ~field fields ->
  match List.assoc_opt field fields with
  | None
  | Some Data.Json.Null ->
      Ok []
  | Some (Data.Json.Array items) ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | Data.Json.String value :: rest -> loop (value :: acc) rest
        | _ :: _ -> Error ("field '" ^ field ^ "' must be an array of strings")
      in
      loop [] items
  | Some _ ->
      Error ("field '" ^ field ^ "' must be an array of strings")

let dependency_of_json = fun json ->
  match json with
  | Data.Json.Object fields -> (
      match string_field ~context:"dependency" ~field:"name" fields with
      | Ok name -> Ok { name; raw = json }
      | Error _ as err -> err
    )
  | _ -> Error "dependency entries must be objects"

let dependencies_of_json = fun json ->
  match json with
  | Data.Json.Array items ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | item :: rest -> (
            match dependency_of_json item with
            | Ok dependency -> loop (dependency :: acc) rest
            | Error _ as err -> err
          )
      in
      loop [] items
  | _ -> Error "release.dependencies must be an array"

let release_of_json = fun json ->
  match json with
  | Data.Json.Object fields ->
      let* _schema_version =
        match int_field ~context:"release" ~field:"schema_version" fields with
        | Ok version -> Ok version
        | Error _ -> Ok 1
      in
      let* version = string_field ~context:"release" ~field:"version" fields in
      let* published_at = string_field ~context:"release" ~field:"published_at" fields in
      let* canonical_locator = string_field ~context:"release" ~field:"canonical_locator" fields in
      let* repo_url = string_field ~context:"release" ~field:"repo_url" fields in
      let* subdir = string_field ~context:"release" ~field:"subdir" fields in
      let* artifact_sha256 =
        string_field_with_fallback
          ~context:"release"
          ~field:"artifact_sha256"
          ~fallback:"sha"
          fields
      in
      let* description = optional_string_field ~field:"description" fields in
      let* license = optional_string_field ~field:"license" fields in
      let* homepage = optional_string_field ~field:"homepage" fields in
      let* repository = optional_string_field ~field:"repository" fields in
      let* root_module = optional_string_field ~field:"root_module" fields in
      let* categories = optional_string_list_field ~field:"categories" fields in
      let* keywords = optional_string_list_field ~field:"keywords" fields in
      let* manifest_key = string_field ~context:"release" ~field:"manifest_key" fields in
      let* source_key = string_field ~context:"release" ~field:"source_key" fields in
      let* dependency_json = object_field ~context:"release" ~field:"dependencies" fields in
      let* dependencies = dependencies_of_json dependency_json in
      Ok {
        version;
        published_at;
        canonical_locator;
        repo_url;
        subdir;
        artifact_sha256;
        description;
        license;
        homepage;
        repository;
        root_module;
        categories;
        keywords;
        manifest_key;
        source_key;
        dependencies;
      }
  | _ -> Error "release entries must be objects"

let releases_of_json = fun json ->
  match json with
  | Data.Json.Array items ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | item :: rest -> (
            match release_of_json item with
            | Ok release -> loop (release :: acc) rest
            | Error _ as err -> err
          )
      in
      loop [] items
  | _ -> Error "package document releases must be an array"

let config_of_json = fun json ->
  match json with
  | Data.Json.Object fields ->
      let* schema_version = int_field ~context:"config" ~field:"schema_version" fields in
      let* kind = string_field ~context:"config" ~field:"kind" fields in
      let* package_path_strategy = string_field ~context:"config" ~field:"package_path_strategy" fields in
      let* index_base_url = string_field ~context:"config" ~field:"index_base_url" fields in
      let* artifact_base_url = string_field ~context:"config" ~field:"artifact_base_url" fields in
      Ok {
        schema_version;
        kind;
        package_path_strategy;
        index_base_url;
        artifact_base_url;
      }
  | _ -> Error "sparse index config must be an object"

let config_of_string = fun source ->
  match Data.Json.of_string source with
  | Ok json -> config_of_json json
  | Error err -> Error ("failed to parse sparse index config JSON: " ^ Data.Json.error_to_string err)

let package_document_of_json = fun json ->
  match json with
  | Data.Json.Object fields ->
      let* schema_version = int_field ~context:"package document" ~field:"schema_version" fields in
      let* name = string_field ~context:"package document" ~field:"name" fields in
      let* latest = string_field ~context:"package document" ~field:"latest" fields in
      let* updated_at = string_field ~context:"package document" ~field:"updated_at" fields in
      let* releases_json = object_field ~context:"package document" ~field:"releases" fields in
      let* releases = releases_of_json releases_json in
      Ok {
        schema_version;
        name;
        latest;
        updated_at;
        releases;
      }
  | _ -> Error "package index document must be an object"

let package_document_of_string = fun source ->
  match Data.Json.of_string source with
  | Ok json -> package_document_of_json json
  | Error err -> Error ("failed to parse package index JSON: " ^ Data.Json.error_to_string err)

let normalized_name = fun package_name -> String.lowercase_ascii package_name

let package_prefix = fun package_name ->
  let name = normalized_name package_name in
  match String.length name with
  | 0 -> Path.v ""
  | 1 -> Path.v "1"
  | 2 -> Path.v "2"
  | 3 -> Path.(Path.v "3" / Path.v (String.sub name 0 1))
  | _ -> Path.(Path.v (String.sub name 0 2) / Path.v (String.sub name 2 2))

let package_relpath = fun package_name ->
  let name = normalized_name package_name in
  Path.(package_prefix name / Path.v (name ^ ".json"))

let ensure_dir_url = fun url ->
  if String.length url > 0 && url.[String.length url - 1] = '/' then
    url
  else
    url ^ "/"

let bootstrap_config_url = fun ~registry_name ->
  let url = "https://cdn." ^ registry_name ^ "/index/v1/config.json" in
  match Net.Uri.of_string url with
  | Ok uri -> Ok uri
  | Error _ -> Error ("failed to build sparse index config url '" ^ url ^ "'")

let package_document_url = fun config ~package_name ->
  let base_url = ensure_dir_url config.index_base_url in
  match Net.Uri.of_string base_url with
  | Error _ -> Error ("failed to parse sparse index base url '" ^ base_url ^ "'")
  | Ok base -> Net.Uri.join base (Path.to_string (package_relpath package_name))
  |> Result.map_error
    (fun _ -> "failed to build sparse index package url for '" ^ package_name ^ "'")

let release_source_url = fun config (release: release) ->
  let base_url = ensure_dir_url config.artifact_base_url in
  match Net.Uri.of_string base_url with
  | Error _ -> Error ("failed to parse sparse index artifact base url '" ^ base_url ^ "'")
  | Ok base -> Net.Uri.join base release.source_key
  |> Result.map_error
    (fun _ -> "failed to build sparse index archive url for '" ^ release.source_key ^ "'")

let package_cache_path = fun cache ~package_name ->
  Path.(Registry_cache.index_dir cache / package_relpath package_name)

let config_cache_path = fun cache -> Path.(Registry_cache.index_dir cache / Path.v "config.json")

let read_cached_json = fun ~path ~decode ->
  match Fs.exists path with
  | Error err ->
      Error ("failed to check sparse index file '" ^ Path.to_string path ^ "': " ^ IO.error_message err)
  | Ok false ->
      Ok None
  | Ok true -> (
      match Fs.read path with
      | Error err -> Error ("failed to read sparse index file '"
      ^ Path.to_string path
      ^ "': "
      ^ IO.error_message err)
      | Ok source -> (
          match decode source with
          | Ok document -> Ok (Some document)
          | Error err -> Error ("failed to decode sparse index file '"
          ^ Path.to_string path
          ^ "': "
          ^ err)
        )
    )

let read_cached_config = fun cache -> read_cached_json ~path:(config_cache_path cache) ~decode:config_of_string

let read_cached_package_document = fun cache ~package_name ->
  read_cached_json ~path:(package_cache_path cache ~package_name) ~decode:package_document_of_string

let write_cached_json = fun ~path ~source ->
  let ensure_parent =
    match Path.parent path with
    | Some parent -> Fs.create_dir_all parent
    | None -> Ok ()
  in
  match ensure_parent with
  | Error err -> Error ("failed to create sparse index parent directory for '"
  ^ Path.to_string path
  ^ "': "
  ^ IO.error_message err)
  | Ok () -> (
      match Fs.write source path with
      | Ok () -> Ok ()
      | Error err -> Error ("failed to write sparse index file '"
      ^ Path.to_string path
      ^ "': "
      ^ IO.error_message err)
    )

let write_cached_config = fun cache ~source -> write_cached_json ~path:(config_cache_path cache) ~source

let write_cached_package_document = fun cache ~package_name ~source ->
  write_cached_json ~path:(package_cache_path cache ~package_name) ~source

module Tests = struct
  let expect_relpath = fun ~package_name ~expected ->
    let actual = package_relpath package_name |> Path.to_string in
    if String.equal actual expected then
      Ok ()
    else
      Error ("expected sparse index path '" ^ expected ^ "', got '" ^ actual ^ "'")

  let test_single_character_name () = expect_relpath ~package_name:"a" ~expected:"1/a.json" [@test]

  let test_two_character_name () = expect_relpath ~package_name:"ab" ~expected:"2/ab.json" [@test]

  let test_three_character_name () = expect_relpath
    ~package_name:"abc"
    ~expected:"3/a/abc.json" [@test]

  let test_longer_name () = expect_relpath ~package_name:"cargo" ~expected:"ca/rg/cargo.json" [@test]

  let test_names_are_normalized_to_lowercase () = expect_relpath
    ~package_name:"AbCd"
    ~expected:"ab/cd/abcd.json" [@test]

  let test_config_of_json () =
    let source = {|{
  "schema_version": 1,
  "kind": "sparse",
  "package_path_strategy": "cargo-lowercase-v1",
  "index_base_url": "https://cdn.pkgs.ml/index/v1",
  "artifact_base_url": "https://cdn.pkgs.ml"
}|}
    in
    match config_of_string source with
    | Ok config ->
        if
          config.schema_version = 1
          && String.equal config.kind "sparse"
          && String.equal config.package_path_strategy "cargo-lowercase-v1"
          && String.equal config.index_base_url "https://cdn.pkgs.ml/index/v1"
          && String.equal config.artifact_base_url "https://cdn.pkgs.ml"
        then
          Ok ()
        else
          Error "unexpected sparse index config contents"
    | Error err -> Error err [@test]

  let test_package_document_of_json () =
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
      "sha": "2aef0372bf5b6687db05bda80cde55f960cbfd9d",
      "description": "Actor runtime kernel primitives for Riot",
      "license": "Apache-2.0",
      "homepage": "https://riot.ml",
      "repository": "https://github.com/leostera/riot-new",
      "root_module": "Kernel",
      "manifest_key": "packages/github.com/leostera/riot-new/packages/kernel/2aef0372bf5b6687db05bda80cde55f960cbfd9d.manifest.json",
      "source_key": "sources/github.com/leostera/riot-new/2aef0372bf5b6687db05bda80cde55f960cbfd9d.tar.gz",
      "dependencies": [
        {
          "name": "std",
          "path": "../std"
        }
      ]
    }
  ]
}|}
    in
    match package_document_of_string source with
    | Ok document -> (
        match document.releases with
        | [ release ] ->
            if
              document.schema_version = 1
              && String.equal document.name "kernel"
              && String.equal document.latest "0.0.1"
              && String.equal release.version "0.0.1"
              && String.equal release.manifest_key "packages/github.com/leostera/riot-new/packages/kernel/2aef0372bf5b6687db05bda80cde55f960cbfd9d.manifest.json"
              && List.length release.dependencies = 1
              && String.equal (List.hd release.dependencies).name "std"
            then
              Ok ()
            else
              Error "unexpected sparse index package document contents"
        | _ -> Error "expected exactly one indexed release"
      )
    | Error err -> Error err [@test]
end [@test]

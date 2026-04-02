open Std

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
val normalized_name: string -> string

val package_prefix: string -> Path.t

val package_relpath: string -> Path.t

val bootstrap_config_url: registry_name:string -> (Net.Uri.t, string) result

val package_document_url: config -> package_name:string -> (Net.Uri.t, string) result

val release_source_url: config -> release -> (Net.Uri.t, string) result

val package_cache_path: Registry_cache.t -> package_name:string -> Path.t

val config_cache_path: Registry_cache.t -> Path.t

val config_of_json: Data.Json.t -> (config, string) result

val config_of_string: string -> (config, string) result

val package_document_of_json: Data.Json.t -> (package_document, string) result

val package_document_of_string: string -> (package_document, string) result

val read_cached_config: Registry_cache.t -> (config option, string) result

val read_cached_package_document:
  Registry_cache.t -> package_name:string -> (package_document option, string) result

val write_cached_config: Registry_cache.t -> source:string -> (unit, string) result

val write_cached_package_document:
  Registry_cache.t -> package_name:string -> source:string -> (unit, string) result

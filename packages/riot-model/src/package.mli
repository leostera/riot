open Std

type dependency_source = {
  workspace: bool;
  builtin: bool;
  path: Path.t option;
  source_locator: string option;
  ref_: string option;
  version: Std.Version.requirement option;
}
type dependency_scope =
  Normal
  | Dev
  | Build
type key
type dependency = {
  name: string;
  source: dependency_source;
}
type resolved_dependency = {
  requirement: dependency;
  resolved_id: Lockfile.package_id;
}
type publish_metadata = {
  version: Std.Version.t option;
  description: string option;
  license: string option;
  is_public: bool option;
}
type binary = {
  name: string;
  path: Path.t;
}
type library = {
  path: Path.t;
}
type sources = {
  src: Path.t list;
  native: Path.t list;
  tests: Path.t list;
  examples: Path.t list;
  bench: Path.t list;
}
type target_platform = string
(* "macos", "linux", "windows", etc. *)

(** Re-export from Profile for convenience *)
type 'a override = 'a Profile.override
type profile_override = Profile.profile_override
(** Target-specific override *)
type target_override = {
  profile_override: Profile.profile_override option;
}
type compiler_config = {
  profile_overrides: (string * profile_override) list;
  target_overrides: (target_platform * target_override) list;
}
type foreign_dependency = {
  name: string;
  path: Path.t;
  inputs: Path.t list;
  build_cmd: string list;
  clean_cmd: string list option;
  test_cmd: string list option;
  outputs: Path.t list;
  env: (string * string) list;
}
type t = private {
  name: string;
  path: Path.t;
  relative_path: Path.t;
  dependencies: dependency list;
  dev_dependencies: dependency list;
  build_dependencies: dependency list;
  foreign_dependencies: foreign_dependency list;
  binaries: binary list;
  library: library option;
  sources: sources;
  compiler: compiler_config;
  commands: Package_command.t list;
  fix_providers: Fix_provider.t list;
  macro_providers: Macro_provider.t list;
  publish: publish_metadata;
}
type resolved = {
  package: t;
  id: Lockfile.package_id;
  manifest_path: Path.t;
  materialized_root: Path.t;
  provenance: Lockfile.provenance;
  runtime_resolved: resolved_dependency list;
  build_resolved: resolved_dependency list;
  dev_resolved: resolved_dependency list;
}
val equal: t -> t -> bool

val is_workspace_member: t -> bool

(** Check if this package is a workspace member (not an external dependency).
    External dependencies have relative_path that escapes the workspace (starts with "../")
    or uses absolute paths. *)
val validate_name: string -> (string, string) result

(** Validate a package name according to Riot naming conventions:
    - Must start with a lowercase letter
    - Can only contain lowercase letters, numbers, hyphens, and underscores
    - Cannot start or end with hyphens or underscores
    - Cannot be empty *)
val is_builtin_dependency_name: string -> bool

val is_builtin_dependency: dependency -> bool

val from_toml:
  Std.Data.Toml.value ->
  workspace_deps:dependency list ->
  workspace_dev_deps:dependency list ->
  workspace_build_deps:dependency list ->
  path:Path.t ->
  relative_path:Path.t ->
  (t, string) result

val to_json: t -> Std.Data.Json.t

val from_json: Std.Data.Json.t -> (t, string) result

val make:
  name:string ->
  path:Path.t ->
  relative_path:Path.t ->
  ?dependencies:dependency list ->
  ?dev_dependencies:dependency list ->
  ?build_dependencies:dependency list ->
  ?foreign_dependencies:foreign_dependency list ->
  ?binaries:binary list ->
  ?library:library ->
  ?sources:sources ->
  ?compiler:compiler_config ->
  ?commands:Package_command.t list ->
  ?fix_providers:Fix_provider.t list ->
  ?publish:publish_metadata ->
  unit ->
  t

val synthetic: name:string -> path:Path.t -> relative_path:Path.t -> t

val key_of_string: string -> key

val key_to_string: key -> string

val key_equal: key -> key -> bool

val key_compare: key -> key -> int

val dependencies_for_scope: dependency_scope -> t -> dependency list

val scope_of_binary_name: t -> binary_name:string -> dependency_scope option

val binaries_for_scope: dependency_scope -> t -> binary list

val for_scope: dependency_scope -> t -> t

val build_graph_dependencies: t -> dependency list

val all_dependencies: t -> dependency list

val resolve:
  package:t ->
  lock_package:Lockfile.package ->
  manifest_path:Path.t ->
  materialized_root:Path.t ->
  (resolved, string) result

(** Hash package metadata into a Sha256 hasher state *)
val hash: Crypto.Sha256.state -> t -> unit

open Std
open Riot_model

module G = Std.Graph.SimpleGraph

type root_mode =
  | Library_root of {
      library_name: string;
      public_root_name: string;
    }
  | Loose_sources
type source_group = {
  source_dir: Path.t;
  allowed_source_files: Path.t list;
  root_mode: root_mode;
  namespace: Namespace.t;
}
type config = {
  root: Path.t;
  source_groups: source_group list;
  package: Package.t;
  toolchain: Riot_toolchain.t;
  workspace: Workspace.t;
}
type analyzed_module = {
  display_path: Path.t;
  source_hash: Crypto.hash;
  implicit_opens: string list;
  parse_result: Syn.Parser.parse_result;
  deps: (Syn.Deps.t, Syn.Deps.parse_error) result;
  resolved_deps: Module_name.t list;
  resolved_dep_ids: G.Node_id.t list;
  unresolved_deps: string list;
}
type source_analysis_progress = {
  source: Path.t;
  source_index: int;
  source_count: int;
}
type t

val create: config -> t

val add_direct_dependency_root: t -> package_name:Package_name.t -> root_module:string -> unit

val add_direct_dependency_package: t -> Package.t -> unit

val add_relocation_aliases: t -> aliases:(string * string) list -> unit

val add_self_relocation_alias: t -> unit

val wire_dependencies:
  ?on_source_analyzed:(source_analysis_progress -> unit) ->
  t ->
  (unit, Planning_error.t) result

val add_library_node: t -> name:string -> includes:Path.t list -> unit

val add_binary_node:
  t ->
  name:string ->
  source:Path.t ->
  libraries:Path.t list ->
  includes:Path.t list ->
  unit

val add_command_node:
  t ->
  name:string ->
  source:Path.t ->
  libraries:Path.t list ->
  includes:Path.t list ->
  unit

val graph: t -> Module_node.t G.t

val analyzed_modules: t -> (G.Node_id.t * analyzed_module) list

val registry: t -> Module_registry.t

val entries: t -> Module_scanner.entry list

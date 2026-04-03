open Std
open Riot_model

module G = Std.Graph.SimpleGraph

type config = {
  root: Path.t;
  source_dir: Path.t;
  allowed_source_files: Path.t list;
  namespace: string;
  package: Package.t;
  toolchain: Riot_toolchain.t;
  workspace: Workspace.t;
}
type t
val create: config -> t

val wire_dependencies: t -> (unit, Planning_error.t) result

val add_library_node: t -> name:string -> includes:Path.t list -> unit

val add_binary_node:
  t -> name:string -> source:Path.t -> libraries:Path.t list -> includes:Path.t list -> unit

val add_command_node:
  t -> name:string -> source:Path.t -> libraries:Path.t list -> includes:Path.t list -> unit

val graph: t -> Module_node.t G.t

val registry: t -> Module_registry.t

val entries: t -> Module_scanner.entry list

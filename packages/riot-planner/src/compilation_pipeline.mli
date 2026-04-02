open Std
open Riot_model

type planned_source = {
  actions: Action.t list;
  compile_source: Path.t;
  copied_sources: Path.t list;
}

module Stage: sig
  type source

  type parsed

  type expanded

  val from_source_file: package:Package.t -> Path.t -> (source, string) result

  val syn_parse: source -> parsed

  val macro_expand: ?providers:Macro.Provider.t list -> parsed -> (expanded, string) result

  val to_planned_source: expanded -> planned_source
end

val plan_concrete_source:
  ?providers:Macro.Provider.t list ->
  package:Package.t ->
  Path.t ->
  (planned_source, string) result

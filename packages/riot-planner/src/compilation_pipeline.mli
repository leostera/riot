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
  type runner_expansion
  val from_source_file: package:Package.t -> Path.t -> (source, string) result

  val syn_parse: source -> parsed

  val macro_expand: ?providers:Macro.Provider.t list -> parsed -> (expanded, string) result

  val plan_runner_expansion:
    workspace_root:Path.t ->
    target_dir_root:Path.t ->
    providers:Riot_model.Macro_provider.t list ->
    parsed ->
    (runner_expansion option, string) result

  val to_planned_source: expanded -> planned_source

  val runner_expansion_to_planned_source: runner_expansion -> planned_source
end

val plan_concrete_source:
  ?providers:Macro.Provider.t list ->
  ?macro_providers:Riot_model.Macro_provider.t list ->
  ?workspace_root:Path.t ->
  ?target_dir_root:Path.t ->
  package:Package.t ->
  Path.t ->
  (planned_source, string) result

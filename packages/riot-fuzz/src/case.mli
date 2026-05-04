open Std
open Types

val collect_cases:
  ?on_event:(Riot_test.Test_runtime.test_event -> unit) ->
  workspace:Riot_model.Workspace.t ->
  package_filters:Riot_model.Package_name.t list ->
  filter:string option ->
  unit ->
  (fuzz_case list, Error.t) Result.t

val case_dir: workspace:Riot_model.Workspace.t -> fuzz_case -> Path.t

val target_for_case: workspace:Riot_model.Workspace.t -> fuzz_case -> target

val corpus_for_case: workspace:Riot_model.Workspace.t -> fuzz_case -> corpus

val mutator_for_case: fuzz_case -> mutator

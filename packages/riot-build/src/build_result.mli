open Std

type failure = {
  package_name: Riot_model.Package_name.t;
  unit_key: Riot_planner.Build_unit.key;
  reason: failure_reason;
  message: string;
  ocamlc_warnings: string list;
  duration_ms: int;
}

and failure_reason =
  | PackagePlanningFailed of Riot_planner.Planning_error.t
  | PackageExecutionFailed of { message: string }
  | PackageActionFailed of { message: string }
  | PackageActionOutputsNotCreated of {
      missing: Std.Path.t list;
    }
  | PackageActionDependenciesFailed of {
      failed: Std.Graph.SimpleGraph.Node_id.t list;
    }
  | PackageSkipped of { reason: string }
  | UnknownFailure
type package_status =
  | Built of Riot_store.Artifact.t
  | Cached of Riot_store.Artifact.t
  | Skipped of string
  | Failed of string
type package_result
type t

val from_build_results: Package_builder.build_result list -> t

val packages: t -> package_result list

val find_package: t -> Riot_model.Package_name.t -> package_result option

val package_name: package_result -> Riot_model.Package_name.t

val package_status: package_result -> package_status

val package_artifact: package_result -> Riot_store.Artifact.t option

val package_artifacts: package_result -> Riot_store.Artifact.t list

val find_export: package_result -> string -> Riot_store.Manifest.export_entry option

val failures_of_build_results: Package_builder.build_result list -> failure list

val failure_to_json: failure -> Data.Json.t

val failure_message: failure -> string

open Std
open Std.Collections

open Tusk_model
open Tusk_planner
open Telemetry_events

(** Coordinator currently owns workspace/package orchestration only.
    Action-level dependency scheduling and cache decisions are delegated to
    [Package_builder] -> [Action_executor].

    Stage-4 target shape (RFD0012) is to lift action readiness to workspace
    scope; until that lands, this module enforces a single concurrency budget
    by avoiding an additional package worker pool. *)

type workspace_result = {
  results : Package_builder.build_result list;
  total_duration : Time.Duration.t;
  cached_count : int;
  built_count : int;
  failed_count : int;
  package_graph : Package_graph.t;
}

let run_package ~workspace ~toolchain ~store ~package_graph ~build_ctx
    (node : Package_graph.package_node) =
  let package = Package_graph.get_package node in
  let result =
    Package_builder.build ~workspace ~toolchain ~store ~package_graph
      ~package_key:(Package_graph.get_key node) ~package ~build_ctx
  in
  let status =
    match result.status with
    | Cached _ -> "cached"
    | Built _ -> "built"
    | Failed _ -> "failed"
  in
  Log.info
    ("Package " ^ result.package.Package.name ^ ": " ^ status ^ " ("
    ^ Int.to_string (Time.Duration.to_millis result.duration) ^ "ms)");
  result

let summarize_results ~package_graph
    (results : Package_builder.build_result list) =
  let cached_count, built_count, failed_count =
    List.fold_left
      (fun (cached, built, failed) result ->
        match result.Package_builder.status with
        | Cached _ -> (cached + 1, built, failed)
        | Built _ -> (cached, built + 1, failed)
        | Failed _ -> (cached, built, failed + 1))
      (0, 0, 0) results
  in
  {
    results;
    total_duration = Time.Duration.zero;
    cached_count;
    built_count;
    failed_count;
    package_graph;
  }

let build_workspace ~workspace ~toolchain ~store ~target ~scope ~concurrency
    ~build_ctx ~session_id =
  let start = Time.Instant.now () in

  match Tusk_planner.plan_workspace ~workspace ~target ~scope ~load_errors:[] with
  | Error err -> Error err
  | Ok { packages; package_graph; _ } -> (
      Telemetry.emit
        (WorkspaceStarted
           { session_id; target; package_count = List.length packages });

      Log.info
        ("Building " ^ Int.to_string (List.length packages)
        ^ " packages with action-level concurrency budget "
        ^ Int.to_string concurrency);

      match Package_graph.topological_sort package_graph with
      | exception Package_graph.Cycle_detected cycle ->
          Error (Workspace_planner.CycleDetected { cycle })
      | nodes ->
          let results =
            List.map
              (run_package ~workspace ~toolchain ~store ~package_graph ~build_ctx)
              nodes
          in
          let result = summarize_results ~package_graph results in
          let total_duration =
            Time.Instant.duration_since ~earlier:start (Time.Instant.now ())
          in
          Ok { result with total_duration })

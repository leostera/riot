open Std
open Std.Collections
open Riot_model
open Riot_planner
open Telemetry_events

type workspace_result = {
  results: Package_builder.build_result list;
  total_duration: Time.Duration.t;
  cached_count: int;
  built_count: int;
  failed_count: int;
  package_graph: Package_graph.t;
}

type package_runtime = {
  package_key: Package.key;
  package: Package.t;
  hash: Crypto.hash;
  depset: Dependency.t list;
  module_graph: Module_node.t Graph.SimpleGraph.t;
  action_graph: Action_graph.t;
  sandbox: Sandbox.t;
  action_queue: Action_queue.t;
  completed_actions: (Graph.SimpleGraph.Node_id.t, Action_executor.execution_result) HashMap.t;
  export_entries: Riot_store.Store.export_entry list;
  target_dir: Path.t;
  profile_name: string;
  target: Target.t;
  total_actions: int;
  mutable active: bool;
  mutable compilation_started: bool;
}

type Message.t +=
  | WorkspaceAssignAction of {
      package_key: Package.key;
      runtime: package_runtime;
      node: Action_node.t
    }
  | WorkspaceActionCompleted of {
      worker_pid: Pid.t;
      package_key: Package.key;
      result: Action_executor.execution_result
    }

let action_error_to_package_error = function
  | Action_executor.ExecutionFailed { message } -> Package_builder.ActionExecutionFailed { message }
  | Action_executor.OutputsNotCreated { missing } -> Package_builder.ActionOutputsNotCreated {
    missing
  }
  | Action_executor.DependenciesFailed { failed } -> Package_builder.ActionDependenciesFailed {
    failed
  }

let package_error_to_telemetry_error = function
  | Package_builder.PlanningFailed err -> Telemetry_events.PlanningFailed err
  | Package_builder.ExecutionFailed { message } -> Telemetry_events.ExecutionFailed { message }
  | Package_builder.ActionExecutionFailed { message } -> Telemetry_events.ActionExecutionFailed {
    message
  }
  | Package_builder.ActionOutputsNotCreated { missing } -> Telemetry_events.ActionOutputsNotCreated {
    missing
  }
  | Package_builder.ActionDependenciesFailed { failed } -> Telemetry_events.ActionDependenciesFailed {
    failed
  }

let compute_export_entries: Action_graph.t -> Riot_store.Store.export_entry list = fun action_graph ->
  let entries =
    Action_graph.nodes action_graph
    |> List.flat_map
      ~fn:(fun (node: Action_node.t) ->
        let is_package_export =
          List.any node.value.actions
            ~fn:(
              function
              | Action.CreateLibrary _
              | Action.CreateExecutable _
              | Action.CreateSharedLibrary _ -> true
              | Action.CompileInterface _
              | Action.CompileImplementation _
              | Action.GenerateInterface _
              | Action.CompileC _
              | Action.CopyFile _
              | Action.WriteFile _
              | Action.BuildForeignDependency _ -> false
            )
        in
        if not is_package_export then
          []
        else
          let action_hash_hex = Crypto.Digest.hex (Action_node.get_hash node) in
          List.map node.value.outs
            ~fn:(fun out_path ->
              Riot_store.Store.{
                name = Path.basename out_path;
                path = out_path;
                action_hash = action_hash_hex
              }))
  in
  let seen = HashSet.create () in
  List.filter_map entries
    ~fn:(fun (entry: Riot_store.Store.export_entry) ->
      if HashSet.contains seen ~value:entry.name then
        None
      else
        (
          let _ = HashSet.insert seen ~value:entry.name in
          Some entry
        ))

let collect_package_artifact_outputs = fun ~sandbox_dir ~outputs ->
  let seen = HashSet.create () in
  outputs |> List.filter_map
    ~fn:(fun out_path ->
      let abs_path = Path.join sandbox_dir out_path in
      match Path.strip_prefix abs_path ~prefix:sandbox_dir with
      | Ok _ ->
          let abs_path_str = Path.to_string abs_path in
          if HashSet.contains seen ~value:abs_path_str then
            None
          else
            (
              let _ = HashSet.insert seen ~value:abs_path_str in
              Some abs_path
            )
      | Error _ -> None)

let collect_ocamlc_warnings = fun completed_actions ->
  let seen = HashSet.create () in
  HashMap.to_list completed_actions |> List.fold_left
    ~acc:[]
    ~fn:(fun acc ((_id, result): Graph.SimpleGraph.Node_id.t * Action_executor.execution_result) ->
      List.fold_left result.Action_executor.ocamlc_warnings
        ~acc
        ~fn:(fun acc warning ->
          if HashSet.contains seen ~value:warning then
            acc
          else
            let _ = HashSet.insert seen ~value:warning in
            acc @ [ warning ]))

let emit_package_ocamlc_warnings = fun ~session_id ~package ~target ~source ocamlc_warnings ->
  if List.length ocamlc_warnings > 0 then
    Telemetry.emit
      (
        PackageOcamlcWarnings {
          session_id;
          package;
          target;
          source;
          messages = ocamlc_warnings;
        }
      )

let summarize_results = fun ~package_graph (results: Package_builder.build_result list) ->
  let cached_count, built_count, failed_count =
    List.fold_left results
      ~acc:(0, 0, 0)
      ~fn:(fun (cached, built, failed) result ->
        match result.Package_builder.status with
        | Cached _ -> (cached + 1, built, failed)
        | Built _ -> (cached, built + 1, failed)
        | Skipped _ -> (cached, built, failed)
        | Failed _ -> (cached, built, failed + 1))
  in
  {
    results;
    total_duration = Time.Duration.zero;
    cached_count;
    built_count;
    failed_count;
    package_graph;
  }

let result_is_success = fun (result: Package_builder.build_result) ->
  match result.status with
  | Package_builder.Built _
  | Cached _ -> true
  | Skipped _
  | Failed _ -> false

let result_is_failed = fun (result: Package_builder.build_result) ->
  match result.status with
  | Package_builder.Failed _
  | Skipped _ -> true
  | Cached _
  | Built _ -> false

let dependency_keys = fun package_graph package_key ->
  match Package_graph.get_node_by_key package_graph package_key with
  | None -> []
  | Some node -> Package_graph.get_dependencies_for_node package_graph node
  |> List.map ~fn:Package_graph.get_key

let mark_package_failed_in_graph = fun package_graph ~package ~package_key ~hash ~error ->
  match Package_graph.get_node_by_key package_graph package_key with
  | None -> ()
  | Some node -> node.value <- Package_graph.Failed {
    package;
    scope = Package_graph.get_scope node.value;
    hash;
    error
  }

let mark_package_skipped_in_graph = fun package_graph ~package ~package_key ~reason ->
  match Package_graph.get_node_by_key package_graph package_key with
  | None -> ()
  | Some node -> node.value <- Package_graph.Skipped {
    package;
    scope = Package_graph.get_scope node.value;
    reason
  }

let mark_package_built_in_graph = fun package_graph ~runtime ~artifact ~status ->
  match Package_graph.get_node_by_key package_graph runtime.package_key with
  | None -> ()
  | Some node ->
      node.value <- Package_graph.Built {
        package = runtime.package;
        scope = Package_graph.get_scope node.value;
        module_graph = runtime.module_graph;
        action_graph = runtime.action_graph;
        hash = runtime.hash;
        artifact;
        status;
        depset = runtime.depset;
      }

let mark_package_cached_in_graph = fun package_graph ~package ~package_key ~hash ~artifact ~depset ~exports ->
  match Package_graph.get_node_by_key package_graph package_key with
  | None -> ()
  | Some node ->
      node.value <- Package_graph.Cached {
        package;
        scope = Package_graph.get_scope node.value;
        hash;
        artifact;
        depset;
        exports;
      }

let persist_package_artifact = fun ~store ~runtime ~ocamlc_warnings ->
  let ( let* ) result fn = Result.and_then result ~fn in
  let runtime_package_name = Package_name.to_string runtime.package.name in
  let* () = Riot_store.Store.materialize_package_exports
    store
    ~exports:runtime.export_entries
    ~target_dir:runtime.target_dir in
  let sandbox_dir = Sandbox.get_dir runtime.sandbox in
  let package_outputs =
    collect_package_artifact_outputs
      ~sandbox_dir
      ~outputs:(Action_graph.nodes runtime.action_graph
        |> List.flat_map ~fn:(fun (node: Action_node.t) -> node.value.outs))
  in
  Riot_store.Store.save
    store
    ~package:runtime_package_name
    ~ocamlc_warnings
    ~exports:runtime.export_entries
    ~hash:runtime.hash
    ~sandbox_dir
    ~outs:package_outputs

let complete_package_artifact = fun ~session_id ~runtime ~ocamlc_warnings artifact ->
  let all_cached =
    HashMap.iter runtime.completed_actions
    |> Iter.Iterator.to_list
    |> List.all
      ~fn:(fun ((_, r)) ->
        match r.Action_executor.status with
        | Action_executor.Cached _ -> true
        | Action_executor.Executed
        | Failed _
        | Skipped -> false)
  in
  let status =
    if all_cached then
      `Cached
    else
      `Fresh
  in
  emit_package_ocamlc_warnings
    ~session_id
    ~package:runtime.package
    ~target:(Workspace_planner.Package runtime.package.name)
    ~source:status
    ocamlc_warnings;
  Telemetry.emit
    (
        BuildCompleted {
          session_id;
          package = runtime.package;
          target = Workspace_planner.Package runtime.package.name;
          status;
          duration = Time.Duration.zero;
        }
    );
  match status with
  | `Cached -> (Package_builder.Cached artifact, ocamlc_warnings)
  | `Fresh -> (Built artifact, ocamlc_warnings)

let finalize_package_success = fun ~session_id ~store ~runtime ->
  let ocamlc_warnings = collect_ocamlc_warnings runtime.completed_actions in
  match persist_package_artifact ~store ~runtime ~ocamlc_warnings with
  | Error error ->
      (Package_builder.Failed (ExecutionFailed {
        message = "Failed to finalize package " ^ Package_name.to_string runtime.package.name
          ^ ": " ^ Riot_store.Store.error_message error;
      }), [])
  | Ok artifact -> complete_package_artifact ~session_id ~runtime ~ocamlc_warnings artifact

let build_workspace_actions = fun ~(workspace:Workspace.t) ~toolchain ~store ~package_graph ~target ~build_ctx ~session_id ~nodes ->
  let profile_name = build_ctx.Build_ctx.profile.name in
  let planning_duration = ref Time.Duration.zero in
  let planning_results: (Package.key, package_planning_status) HashMap.t = HashMap.create () in
  let runtimes: (Package.key, package_runtime) HashMap.t = HashMap.create () in
  let package_results: (Package.key, Package_builder.build_result) HashMap.t = HashMap.create () in
  let pending_planning: (Package.key, Package_graph.package_node) HashMap.t = HashMap.create () in
  let action_ready_queue: (Package.key * Action_node.t) Queue.t = Queue.create () in
  let ready_count = ref 0 in
  let coordinator_pid = self () in
  let enqueue_ready item =
    Queue.push action_ready_queue ~value:item;
    ready_count := !ready_count + 1
  in
  let pop_ready () =
    match Queue.pop action_ready_queue with
    | None -> None
    | Some item ->
        ready_count := !ready_count - 1;
        Some item
  in
  Telemetry.emit (PlanningWorkspaceStarted { session_id; target; package_count = List.length nodes });
  let materialize_initial_result package_key (package: Package.t) status =
    let result =
      Package_builder.{
        package_key;
        package;
        status;
        ocamlc_warnings = [];
        duration = Time.Duration.zero;
      }
    in
    let _ = HashMap.insert package_results ~key:package_key ~value:result in
    (
      match status with
      | Package_builder.Failed err -> Telemetry.emit
        (BuildFailed {
          session_id;
          package;
          target = Workspace_planner.Package package.name;
          error = package_error_to_telemetry_error err
        })
      | Package_builder.Skipped { reason } -> Telemetry.emit
        (BuildSkipped {
          session_id;
          package;
          target = Workspace_planner.Package package.name;
          reason
        })
      | Package_builder.Cached _
      | Package_builder.Built _ -> ()
    );
    result
  in
  let update_planning_progress package_key status ~duration ~package ~reason =
    planning_duration := Time.Duration.add !planning_duration duration;
    let _ = HashMap.insert planning_results ~key:package_key ~value:status in
    Telemetry.emit
      (
        PackagePlanningResult {
          session_id;
          package;
          target;
          status;
          duration;
          reason;
        }
      )
  in
  let emit_package_planning_breakdown package breakdown =
    let breakdown: Telemetry_events.package_planning_breakdown = {
      dependency_count = breakdown.Riot_planner.Package_planner.dependency_count;
      dependency_check_duration = breakdown.Riot_planner.Package_planner.dependency_check_duration;
      input_hash_duration = breakdown.Riot_planner.Package_planner.input_hash_duration;
      artifact_lookup_duration = breakdown.Riot_planner.Package_planner.artifact_lookup_duration;
      artifact_cache_hit = breakdown.Riot_planner.Package_planner.artifact_cache_hit;
      plan_bundle_lookup_duration = breakdown.Riot_planner.Package_planner.plan_bundle_lookup_duration;
      plan_bundle_decode_duration = breakdown.Riot_planner.Package_planner.plan_bundle_decode_duration;
      plan_bundle_cache_hit = breakdown.Riot_planner.Package_planner.plan_bundle_cache_hit;
      module_plan_duration = breakdown.Riot_planner.Package_planner.module_plan_duration;
    } in
    Telemetry.emit
      (
        PackagePlanningBreakdown {
          session_id;
          package;
          target;
          breakdown;
        }
      )
  in
  let target_triplet = Build_ctx.target_triplet build_ctx in
  let target_dir_for package_name =
    Path.(Riot_dirs.out_dir_in_workspace
      ~workspace
      ~profile:profile_name
      ~target:target_triplet
    / Path.v (Package_name.to_string package_name)) in
  let finalize_cached_package ~package_key ~(package:Package.t) ~hash ~artifact ~depset ~exports =
    let materialized = Ok () in
    match materialized with
    | Ok () ->
        let result =
          Package_builder.{
            package_key;
            package;
            status = Cached artifact;
            ocamlc_warnings = artifact.ocamlc_warnings;
            duration = Time.Duration.zero;
          }
        in
        let _ = HashMap.insert package_results ~key:package_key ~value:result in
        mark_package_cached_in_graph package_graph ~package ~package_key ~hash ~artifact ~depset ~exports;
        emit_package_ocamlc_warnings
          ~session_id
          ~package
          ~target:(Workspace_planner.Package package.name)
          ~source:`Cached artifact.ocamlc_warnings;
        Telemetry.emit
          (
            BuildCompleted {
              session_id;
              package;
              target = Workspace_planner.Package package.name;
              status = `Cached;
              duration = Time.Duration.zero;
            }
          )
    | Error message ->
        let error = Package_builder.ExecutionFailed { message } in
        let result =
          Package_builder.{
            package_key;
            package;
            status = Failed error;
            ocamlc_warnings = [];
            duration = Time.Duration.zero;
          }
        in
        let _ = HashMap.insert package_results ~key:package_key ~value:result in
        mark_package_failed_in_graph package_graph ~package ~package_key ~hash ~error:message;
        Telemetry.emit
          (BuildFailed {
            session_id;
            package;
            target = Workspace_planner.Package package.name;
            error = package_error_to_telemetry_error error
          })
  in
  let stage_runtime ~package_key ~(package:Package.t) ~hash ~depset ~module_graph ~action_graph =
    let inputs = List.concat [ package.sources.src; package.sources.native; package.sources.tests; ] in
    let target_dir = target_dir_for package.name in
    let sandbox = Sandbox.create
      ~workspace
      ~profile:profile_name
      ~target:target_triplet
      ()
      ~package_name:package.name
    in
    Sandbox.prepare ~sandbox ~package ~inputs ~depset ~store;
    let action_queue = Action_queue.create () in
    let action_nodes = Action_graph.nodes action_graph in
    List.for_each action_nodes
      ~fn:(Action_queue.queue action_queue);
    let runtime = {
      package_key;
      package;
      hash;
      depset;
      module_graph;
      action_graph;
      sandbox;
      action_queue;
      completed_actions = action_queue.completed;
      export_entries = compute_export_entries action_graph;
      target_dir;
      profile_name;
      target = target_triplet;
      total_actions = List.length action_nodes;
      active = false;
      compilation_started = false;
    }
    in
    let _ = HashMap.insert runtimes ~key:package_key ~value:runtime in
    match Package_graph.get_node_by_key package_graph package_key with
    | Some node ->
        node.value <- Package_graph.Planned {
          package;
          scope = Package_graph.get_scope node.value;
          module_graph;
          action_graph;
          hash;
        }
    | None -> ()
  in
  let enqueue_all_ready_actions ~package_key runtime enqueue_ready =
    let rec loop () =
      match Action_queue.next runtime.action_queue with
      | Some node ->
          enqueue_ready (package_key, node);
          loop ()
      | None -> ()
    in
    loop ()
  in
  let rec plan_pass pending_nodes =
    if pending_nodes = [] then
      ()
    else
      let progressed = ref false in
      let still_pending = vec [] in
      List.for_each pending_nodes
        ~fn:(fun package_node ->
          let package = Package_graph.get_package package_node in
          let package_key = Package_graph.get_key package_node in
          let planning_start = Time.Instant.now () in
          match Riot_planner.plan_package_with_graph
            ~workspace
            ~toolchain
            ~store
            ~package_graph
            ~package_key
            ~package
            ~build_ctx with
          | Error err ->
              update_planning_progress
                package_key
                `Failed
                ~duration:(Time.Instant.duration_since ~earlier:planning_start (Time.Instant.now ()))
                ~package
                ~reason:(Some (Planning_error.to_string err));
              let _ = materialize_initial_result
                package_key
                package
                (Package_builder.Failed (PlanningFailed err)) in
              ()
          | Ok (MissingDependencies { missing; breakdown }) ->
              emit_package_planning_breakdown package breakdown;
              update_planning_progress
                package_key
                `MissingDependencies
                ~duration:(Time.Instant.duration_since ~earlier:planning_start (Time.Instant.now ()))
                ~package
                ~reason:(Some ("Missing dependencies: "
                ^ (missing
                  |> List.map ~fn:(fun p -> Package_name.to_string p.Package.name)
                  |> String.concat ", ")));
              Vector.push still_pending ~value:package_node
          | Ok (FailedDependencies { failed; breakdown; _ }) ->
              emit_package_planning_breakdown package breakdown;
              update_planning_progress
                package_key
                `FailedDependencies
                ~duration:(Time.Instant.duration_since ~earlier:planning_start (Time.Instant.now ()))
                ~package
                ~reason:(Some ("Failed dependencies: "
                ^ (failed
                  |> List.map ~fn:(fun p -> Package_name.to_string p.Package.name)
                  |> String.concat ", ")));
              Vector.push still_pending ~value:package_node
          | Ok (Cached {
            package_key;
            hash;
            artifact;
            depset;
            exports;
            breakdown;
            _;

          }) ->
              emit_package_planning_breakdown package breakdown;
              update_planning_progress
                package_key
                `Planned
                ~duration:(Time.Instant.duration_since ~earlier:planning_start (Time.Instant.now ()))
                ~package
                ~reason:None;
              progressed := true;
              finalize_cached_package ~package_key ~package ~hash ~artifact ~depset ~exports
          | Ok (Planned {
            package_key;
            hash;
            depset;
            module_graph;
            action_graph;
            breakdown;
            _;

          }) ->
              emit_package_planning_breakdown package breakdown;
              update_planning_progress
                package_key
                `Planned
                ~duration:(Time.Instant.duration_since ~earlier:planning_start (Time.Instant.now ()))
                ~package
                ~reason:None;
              progressed := true;
              stage_runtime ~package_key ~package ~hash ~depset ~module_graph ~action_graph);
      let next_pending = Vector.iter still_pending |> Iter.Iterator.to_list in
      if next_pending = [] then
        ()
      else if !progressed then
        plan_pass next_pending
      else
        List.for_each next_pending
          ~fn:(fun package_node ->
            let package_key = Package_graph.get_key package_node in
            let _ = HashMap.insert pending_planning ~key:package_key ~value:package_node in
            ())
  in
  plan_pass nodes;
  let try_plan_pending_packages () =
    let pending_entries = HashMap.to_list pending_planning in
    List.for_each pending_entries
      ~fn:(fun (package_key, package_node) ->
        if Option.is_some (HashMap.get package_results ~key:package_key) then
          let _ = HashMap.remove pending_planning ~key:package_key in
          ()
        else if Option.is_some (HashMap.get runtimes ~key:package_key) then
          let _ = HashMap.remove pending_planning ~key:package_key in
          ()
        else
          let package = Package_graph.get_package package_node in
          let dep_keys = dependency_keys package_graph package_key in
          let deps_failed =
            List.any dep_keys
              ~fn:(fun dep_key ->
                match HashMap.get package_results ~key:dep_key with
                | Some result -> result_is_failed result
                | None -> false)
          in
          if deps_failed then
            (
              let names =
                List.filter_map dep_keys
                  ~fn:(fun dep_key ->
                    match HashMap.get package_results ~key:dep_key with
                    | Some result when result_is_failed result ->
                        Some (Package_name.to_string result.package.Package.name)
                    | _ -> None)
              in
              update_planning_progress
                package_key
                `FailedDependencies
                ~duration:Time.Duration.zero
                ~package
                ~reason:(Some ("Failed dependencies: " ^ String.concat ", " names));
              let _ = materialize_initial_result
                package_key
                package
                (Package_builder.Skipped { reason = "needs " ^ String.concat ", " names }) in
              mark_package_skipped_in_graph
                package_graph
                ~package
                ~package_key
                ~reason:("needs " ^ String.concat ", " names);
              let _ = HashMap.remove pending_planning ~key:package_key in
              ()
            )
          else
            let deps_satisfied =
              List.all dep_keys
                ~fn:(fun dep_key ->
                  match HashMap.get package_results ~key:dep_key with
                  | Some result -> result_is_success result
                  | None -> false)
            in
            if deps_satisfied then
              let planning_start = Time.Instant.now () in
              match Riot_planner.plan_package_with_graph
                ~workspace
                ~toolchain
                ~store
                ~package_graph
                ~package_key
                ~package
                ~build_ctx with
              | Error err ->
                  update_planning_progress
                    package_key
                    `Failed
                    ~duration:(Time.Instant.duration_since
                      ~earlier:planning_start
                      (Time.Instant.now ()))
                    ~package
                    ~reason:(Some (Planning_error.to_string err));
                  let _ = materialize_initial_result
                    package_key
                    package
                    (Package_builder.Failed (PlanningFailed err)) in
                  let _ = HashMap.remove pending_planning ~key:package_key in
                  ()
              | Ok (MissingDependencies { missing; breakdown }) ->
                  emit_package_planning_breakdown package breakdown;
                  update_planning_progress
                    package_key
                    `MissingDependencies
                    ~duration:(Time.Instant.duration_since
                      ~earlier:planning_start
                      (Time.Instant.now ()))
                    ~package
                    ~reason:(Some ("Missing dependencies: "
                    ^ (missing
                      |> List.map ~fn:(fun p -> Package_name.to_string p.Package.name)
                      |> String.concat ", ")));
                  ()
              | Ok (FailedDependencies { failed; breakdown; _ }) ->
                  emit_package_planning_breakdown package breakdown;
                  update_planning_progress
                    package_key
                    `FailedDependencies
                    ~duration:(Time.Instant.duration_since
                      ~earlier:planning_start
                      (Time.Instant.now ()))
                    ~package
                    ~reason:(Some ("Failed dependencies: "
                    ^ (failed
                      |> List.map ~fn:(fun p -> Package_name.to_string p.Package.name)
                      |> String.concat ", ")));
                  let names =
                    List.map failed ~fn:(fun p -> Package_name.to_string p.Package.name)
                  in
                  let _ = materialize_initial_result
                    package_key
                    package
                    (Package_builder.Skipped { reason = "needs " ^ String.concat ", " names }) in
                  mark_package_skipped_in_graph
                    package_graph
                    ~package
                    ~package_key
                    ~reason:("needs " ^ String.concat ", " names);
                  let _ = HashMap.remove pending_planning ~key:package_key in
                  ()
              | Ok (Cached {
                package_key;
                hash;
                artifact;
                depset;
                exports;
                breakdown;
                _;

              }) ->
                  emit_package_planning_breakdown package breakdown;
                  update_planning_progress
                    package_key
                    `Planned
                    ~duration:(Time.Instant.duration_since
                      ~earlier:planning_start
                      (Time.Instant.now ()))
                    ~package
                    ~reason:None;
                  finalize_cached_package ~package_key ~package ~hash ~artifact ~depset ~exports;
                  let _ = HashMap.remove pending_planning ~key:package_key in
                  ()
              | Ok (Planned {
                package_key;
                hash;
                depset;
                module_graph;
                action_graph;
                breakdown;
                _;

              }) ->
                  emit_package_planning_breakdown package breakdown;
                  update_planning_progress
                    package_key
                    `Planned
                    ~duration:(Time.Instant.duration_since
                      ~earlier:planning_start
                      (Time.Instant.now ()))
                    ~package
                    ~reason:None;
                  stage_runtime ~package_key ~package ~hash ~depset ~module_graph ~action_graph;
                  let _ = HashMap.remove pending_planning ~key:package_key in
                  ())
      
  in
  let activate_ready_packages () =
    try_plan_pending_packages ();
    HashMap.for_each runtimes
      ~fn:(fun package_key runtime ->
        if runtime.active then
          ()
        else if Option.is_some (HashMap.get package_results ~key:package_key) then
          ()
        else
          let dep_keys = dependency_keys package_graph package_key in
          let deps_failed =
            List.any dep_keys
              ~fn:(fun dep_key ->
                match HashMap.get package_results ~key:dep_key with
                | Some result -> result_is_failed result
                | None -> false)
          in
          if deps_failed then
            (
              let reason = "needs failed dependencies" in
              let _ = materialize_initial_result
                package_key
                runtime.package
                (Package_builder.Skipped { reason }) in
              mark_package_skipped_in_graph package_graph ~package:runtime.package ~package_key ~reason;
              Sandbox.cleanup runtime.sandbox
            )
          else
            let deps_satisfied =
              List.all dep_keys
                ~fn:(fun dep_key ->
                  match HashMap.get package_results ~key:dep_key with
                  | Some result -> result_is_success result
                  | None -> false)
            in
	            if deps_satisfied then
	              (
	                runtime.active <- true;
	                Telemetry.emit
	                  (BuildStarted {
	                    session_id;
	                    package = runtime.package;
	                    target = Workspace_planner.Package runtime.package.name
	                  });
	                enqueue_all_ready_actions ~package_key runtime enqueue_ready
	              )
	      );
	  in
  let finalize_if_complete package_key runtime =
    if
      Option.is_none (HashMap.get package_results ~key:package_key)
      && Action_queue.is_complete runtime.action_queue ~total_nodes:runtime.total_actions
    then
      let failures =
        HashMap.iter runtime.completed_actions
        |> Iter.Iterator.to_list
        |> List.filter_map
          ~fn:(fun ((_, r)) ->
            match r.Action_executor.status with
            | Failed err -> Some err
            | Cached _
            | Executed
            | Skipped -> None)
      in
      match failures with
      | first_failure :: _ ->
          let pkg_err = action_error_to_package_error first_failure in
          let result =
            Package_builder.{
              package_key;
              package = runtime.package;
              status = Failed pkg_err;
              ocamlc_warnings = [];
              duration = Time.Duration.zero;
            }
          in
          let _ = HashMap.insert package_results ~key:package_key ~value:result in
          mark_package_failed_in_graph
            package_graph
            ~package:runtime.package
            ~package_key
            ~hash:runtime.hash
            ~error:(Package_builder.package_error_to_string pkg_err);
          Telemetry.emit
            (BuildFailed {
              session_id;
              package = runtime.package;
              target = Workspace_planner.Package runtime.package.name;
              error = package_error_to_telemetry_error pkg_err
            });
          Sandbox.cleanup runtime.sandbox
      | [] ->
          let status, ocamlc_warnings = finalize_package_success ~session_id ~store ~runtime in
          let build_status =
            match status with
            | Package_builder.Cached artifact ->
                mark_package_built_in_graph package_graph ~runtime ~artifact ~status:Package_graph.Cached;
                Package_builder.Cached artifact
            | Package_builder.Built artifact ->
                mark_package_built_in_graph package_graph ~runtime ~artifact ~status:Package_graph.Fresh;
                Package_builder.Built artifact
            | Package_builder.Skipped _ ->
                panic "Unexpected skipped status during success finalization"
            | Package_builder.Failed error ->
                mark_package_failed_in_graph package_graph ~package:runtime.package
                  ~package_key ~hash:runtime.hash
                  ~error:(Package_builder.package_error_to_string error);
                Telemetry.emit (BuildFailed {
                  session_id; package = runtime.package;
                  target = Workspace_planner.Package runtime.package.name;
                  error = package_error_to_telemetry_error error;
                });
                Package_builder.Failed error
          in
          let result =
            Package_builder.{
              package_key;
              package = runtime.package;
              status = build_status;
              ocamlc_warnings;
              duration = Time.Duration.zero;
            }
          in
          let _ = HashMap.insert package_results ~key:package_key ~value:result in
          Sandbox.cleanup runtime.sandbox
  in
  let total_packages = List.length nodes in
  let worker_count = max 1 build_ctx.Build_ctx.available_parallelism in
  let rec workspace_worker_loop () =
    match receive_any () with
    | WorkspaceAssignAction { package_key; runtime; node } ->
        let sandbox_dir = Sandbox.get_dir runtime.sandbox in
        let result = Action_executor.execute_node
          ~completed:runtime.completed_actions
          ~store
          ~session_id
          toolchain
          sandbox_dir
          node in
        send coordinator_pid (WorkspaceActionCompleted { worker_pid = self (); package_key; result });
        workspace_worker_loop ()
    | _ -> workspace_worker_loop ()
  in
  let workers =
    List.init ~count:worker_count ~fn:(fun _ -> spawn workspace_worker_loop)
  in
  let idle_workers: Pid.t Queue.t = Queue.create () in
  List.for_each workers
    ~fn:(fun pid -> Queue.push idle_workers ~value:pid);
  let busy_workers: (Pid.t, Package.key) HashMap.t = HashMap.create () in
  let rec drain_work_queue () =
    match Queue.pop idle_workers with
    | None -> ()
    | Some worker_pid -> (
        match pop_ready () with
        | None -> Queue.push idle_workers ~value:worker_pid
        | Some (package_key, action_node) -> (
            match HashMap.get runtimes ~key:package_key with
            | None ->
                Queue.push idle_workers ~value:worker_pid;
                drain_work_queue ()
            | Some runtime ->
                let _ = HashMap.insert busy_workers ~key:worker_pid ~value:package_key in
                send worker_pid (WorkspaceAssignAction { package_key; runtime; node = action_node });
                drain_work_queue ()
          )
      )
  in
  let rec loop () =
    if HashMap.length package_results = total_packages then
      ()
    else (
      activate_ready_packages ();
      drain_work_queue ();
      if HashMap.length package_results = total_packages then
        ()
      else if HashMap.length busy_workers = 0 && !ready_count = 0 then
        (
          let before = HashMap.length package_results in
          HashMap.iter runtimes
          |> Iter.Iterator.to_list
          |> List.for_each
            ~fn:(fun ((package_key, pkg_runtime): Package.key * package_runtime) ->
              finalize_if_complete package_key pkg_runtime);
          let after = HashMap.length package_results in
          if after = before then
            (
              HashMap.iter runtimes |> Iter.Iterator.to_list |> List.for_each
                ~fn:(fun ((package_key, pkg_runtime): Package.key * package_runtime) ->
                  if Option.is_none (HashMap.get package_results ~key:package_key) then
                    (
                      let error = "No ready actions remaining for package" in
                      let result =
                        Package_builder.{
                          package_key;
                          package = pkg_runtime.package;
                          status = Failed (ExecutionFailed { message = error });
                          ocamlc_warnings = [];
                          duration = Time.Duration.zero;
                        }
                      in
                      let _ = HashMap.insert package_results ~key:package_key ~value:result in
                      mark_package_failed_in_graph
                        package_graph
                        ~package:pkg_runtime.package
                        ~package_key
                        ~hash:pkg_runtime.hash
                        ~error;
                      Sandbox.cleanup pkg_runtime.sandbox
                    ))
            );
          loop ()
        )
      else
        match receive_any () with
        | WorkspaceActionCompleted { worker_pid; package_key; result } -> (
            let _ = HashMap.remove busy_workers ~key:worker_pid in
            Queue.push idle_workers ~value:worker_pid;
            match HashMap.get runtimes ~key:package_key with
            | None -> loop ()
            | Some runtime ->
                if (not runtime.compilation_started) && match result.status with
                  | Action_executor.Cached _ -> false
                  | Executed
                  | Failed _
                  | Skipped -> true then
                  (
                    runtime.compilation_started <- true;
                    Telemetry.emit
                      (CompilationStarted {
                        session_id;
                        package = runtime.package;
                        target = Workspace_planner.Package runtime.package.name
                      })
                  );
                Action_queue.mark_completed runtime.action_queue result;
                enqueue_all_ready_actions ~package_key runtime enqueue_ready;
                finalize_if_complete package_key runtime;
                loop ()
          )
        | _ -> loop ()
    )
  in
  let planning_counts () =
    HashMap.iter planning_results
    |> Iter.Iterator.to_list
    |> List.fold_left
      ~acc:(0, 0, 0)
      ~fn:(fun (planned, missing, failed) (_, status) ->
        match status with
        | `Planned -> (planned + 1, missing, failed)
        | `MissingDependencies -> (planned, missing + 1, failed)
        | `FailedDependencies
        | `Failed -> (planned, missing, failed + 1))
  in
  loop ();
  let planned_count, missing_count, failed_count = planning_counts () in
  let planning_duration = !planning_duration in
  Telemetry.emit
    (
      PlanningWorkspaceCompleted {
        session_id;
        target;
        duration = planning_duration;
        planned_count;
        missing_count;
        failed_count;
      }
    );
  package_results
  |> HashMap.iter
  |> Iter.Iterator.to_list
  |> List.map ~fn:(fun (_, result) -> result)

let build_workspace = fun ~workspace ~toolchain ~store ~target ~scope ~concurrency ~build_ctx ~session_id ->
  let start = Time.Instant.now () in
  let workspace_package_count = List.length workspace.Riot_model.Workspace.packages in
  let planning_started_at = Time.Instant.now () in
  Telemetry.emit (WorkspacePlanStarted {
    session_id;
    target;
    workspace_package_count;
  });
  match Riot_planner.plan_workspace ~workspace ~target ~scope ~load_errors:[] with
  | Error err -> Error err
  | Ok { packages; nodes; package_graph; breakdown; _ } -> (
      let planning_duration =
        Time.Instant.duration_since ~earlier:planning_started_at (Time.Instant.now ())
      in
      Telemetry.emit (WorkspaceManifestFilterCompleted {
        session_id;
        target;
        filtered_workspace_package_count = breakdown.filtered_workspace_package_count;
        duration = breakdown.manifest_filter_duration;
      });
      Telemetry.emit (WorkspaceGraphCreated {
        session_id;
        target;
        node_count = breakdown.package_graph_node_count;
        breakdown = {
          Telemetry_events.build_node_realization_count =
            breakdown.package_graph_create_breakdown.build_node_realization_count;
          build_node_realization_duration =
            breakdown.package_graph_create_breakdown.build_node_realization_duration;
          runtime_node_realization_count =
            breakdown.package_graph_create_breakdown.runtime_node_realization_count;
          runtime_node_realization_duration =
            breakdown.package_graph_create_breakdown.runtime_node_realization_duration;
          dev_node_realization_count =
            breakdown.package_graph_create_breakdown.dev_node_realization_count;
          dev_node_realization_duration =
            breakdown.package_graph_create_breakdown.dev_node_realization_duration;
          edge_wiring_duration =
            breakdown.package_graph_create_breakdown.edge_wiring_duration;
        };
        duration = breakdown.package_graph_duration;
      });
      Telemetry.emit (WorkspaceTargetGraphFiltered {
        session_id;
        target;
        node_count = breakdown.target_graph_node_count;
        duration = breakdown.target_graph_filter_duration;
      });
      Telemetry.emit (WorkspaceTopologicalSortCompleted {
        session_id;
        target;
        sorted_package_count = breakdown.sorted_package_count;
        duration = breakdown.topological_sort_duration;
      });
      Telemetry.emit (WorkspacePlanCompleted {
        session_id;
        target;
        workspace_package_count;
        planned_package_count = List.length packages;
        duration = planning_duration;
      });
      Telemetry.emit (WorkspaceStarted { session_id; target; package_count = List.length packages });
      Log.info
        ("Building "
        ^ Int.to_string (List.length packages)
        ^ " packages with action-level concurrency budget "
        ^ Int.to_string concurrency);
      let results = build_workspace_actions
        ~workspace
        ~toolchain
        ~store
        ~package_graph
        ~target
        ~build_ctx
        ~session_id
        ~nodes in
      let result = summarize_results ~package_graph results in
      let total_duration = Time.Instant.duration_since ~earlier:start (Time.Instant.now ()) in
      Telemetry.emit
        (
          WorkspaceCompleted {
            session_id;
            target;
            total_duration;
            cached_count = result.cached_count;
                built_count = result.built_count;
                failed_count = result.failed_count;
              }
            );
          Ok { result with total_duration }
    )

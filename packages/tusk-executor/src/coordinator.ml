open Std
open Std.Collections

open Tusk_model
open Tusk_planner
open Telemetry_events

type workspace_result = {
  results : Package_builder.build_result list;
  total_duration : Time.Duration.t;
  cached_count : int;
  built_count : int;
  failed_count : int;
  package_graph : Package_graph.t;
}

type package_runtime = {
  package_key : Package.key;
  package : Package.t;
  hash : Crypto.hash;
  depset : Dependency.t list;
  module_graph : Module_node.t Graph.SimpleGraph.t;
  action_graph : Action_graph.t;
  sandbox : Sandbox.t;
  action_queue : Action_queue.t;
  completed_actions :
    (Graph.SimpleGraph.Node_id.t, Action_executor.execution_result) HashMap.t;
  export_entries : Tusk_store.Store.export_entry list;
  target_dir : Path.t;
  profile_name : string;
  target_name : string;
  total_actions : int;
  mutable active : bool;
  mutable compilation_started : bool;
}

type Message.t +=
  | WorkspaceAssignAction of {
      package_key : Package.key;
      runtime : package_runtime;
      node : Action_node.t;
    }
  | WorkspaceActionCompleted of {
      worker_pid : Pid.t;
      package_key : Package.key;
      result : Action_executor.execution_result;
    }

let action_error_to_package_error = function
  | Action_executor.ExecutionFailed { message } ->
      Package_builder.ActionExecutionFailed { message }
  | Action_executor.OutputsNotCreated { missing } ->
      Package_builder.ActionOutputsNotCreated { missing }
  | Action_executor.DependenciesFailed { failed } ->
      Package_builder.ActionDependenciesFailed { failed }

let package_error_to_telemetry_error = function
  | Package_builder.PlanningFailed err -> Telemetry_events.PlanningFailed err
  | Package_builder.ExecutionFailed { message } ->
      Telemetry_events.ExecutionFailed { message }
  | Package_builder.ActionExecutionFailed { message } ->
      Telemetry_events.ActionExecutionFailed { message }
  | Package_builder.ActionOutputsNotCreated { missing } ->
      Telemetry_events.ActionOutputsNotCreated { missing }
  | Package_builder.ActionDependenciesFailed { failed } ->
      Telemetry_events.ActionDependenciesFailed { failed }

let compute_export_entries (action_graph : Action_graph.t) :
    Tusk_store.Store.export_entry list =
  let entries =
    Action_graph.nodes action_graph
    |> List.concat_map (fun (node : Action_node.t) ->
           let action_hash_hex = Crypto.Digest.hex (Action_node.get_hash node) in
           List.map
             (fun out_path ->
               Tusk_store.Store.
                 {
                   name = Path.basename out_path;
                   path = out_path;
                   action_hash = action_hash_hex;
                 })
             node.value.outs)
  in
  let seen = HashSet.create () in
  List.filter_map
    (fun (entry : Tusk_store.Store.export_entry) ->
      if HashSet.contains seen entry.name then None
      else (
        let _ = HashSet.insert seen entry.name in
        Some entry))
    entries

let artifact_from_exports ~package_hash
    (exports : Tusk_store.Store.export_entry list) =
  let files =
    List.map
      (fun (entry : Tusk_store.Store.export_entry) -> Path.v entry.name)
      exports
  in
  Tusk_store.Artifact.{ hash = package_hash; files }

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

let result_is_success (result : Package_builder.build_result) =
  match result.status with
  | Package_builder.Built _ | Cached _ -> true
  | Failed _ -> false

let result_is_failed (result : Package_builder.build_result) =
  match result.status with
  | Package_builder.Failed _ -> true
  | Cached _ | Built _ -> false

let dependency_keys package_graph package_key =
  match Package_graph.get_node_by_key package_graph package_key with
  | None -> []
  | Some node ->
      Package_graph.get_dependencies_for_node package_graph node
      |> List.map Package_graph.get_key

let mark_package_failed_in_graph package_graph ~package ~package_key ~hash ~error =
  match Package_graph.get_node_by_key package_graph package_key with
  | None -> ()
  | Some node ->
      node.value <-
        Package_graph.Failed
          {
            package;
            scope = Package_graph.get_scope node.value;
            hash;
            error;
          }

let mark_package_built_in_graph package_graph ~runtime ~artifact ~status =
  match Package_graph.get_node_by_key package_graph runtime.package_key with
  | None -> ()
  | Some node ->
      node.value <-
        Package_graph.Built
          {
            package = runtime.package;
            scope = Package_graph.get_scope node.value;
            module_graph = runtime.module_graph;
            action_graph = runtime.action_graph;
            hash = runtime.hash;
            artifact;
            status;
            depset = runtime.depset;
          }

let finalize_package_success ~session_id ~store ~runtime =
  let _ =
    Tusk_store.Store.save_package_exports store ~package:runtime.package.name
      ~profile:runtime.profile_name ~target:runtime.target_name
      ~exports:runtime.export_entries
    |> Result.expect
         ~msg:
           ("Failed to save package export manifest for " ^ runtime.package.name)
  in
  Tusk_store.Store.materialize_package_exports store ~exports:runtime.export_entries
    ~target_dir:runtime.target_dir
  |> Result.expect
       ~msg:
         ("Failed to materialize package exports for " ^ runtime.package.name);
  let package_outs =
    List.map
      (fun (entry : Tusk_store.Store.export_entry) ->
        Path.(runtime.target_dir / Path.v entry.name))
      runtime.export_entries
  in
  let _ =
    Tusk_store.Store.save store ~package:runtime.package.name ~hash:runtime.hash
      ~sandbox_dir:runtime.target_dir ~outs:package_outs
    |> Result.expect
         ~msg:
           ("Failed to save package hash artifact for "
          ^ runtime.package.name)
  in
  let artifact = artifact_from_exports ~package_hash:runtime.hash runtime.export_entries in
  let all_cached =
    HashMap.into_iter runtime.completed_actions
    |> Iter.Iterator.to_list
    |> List.for_all (fun (_, r) ->
           match r.Action_executor.status with
           | Action_executor.Cached _ -> true
           | Action_executor.Executed | Failed _ | Skipped -> false)
  in
  let status =
    if all_cached then `Cached else `Fresh
  in
  Telemetry.emit
    (BuildCompleted
       {
         session_id;
         package = runtime.package;
         target = Workspace_planner.Package runtime.package.name;
         status;
         duration = Time.Duration.zero;
       });
  match status with
  | `Cached -> Package_builder.Cached artifact
  | `Fresh -> Built artifact

let build_workspace_actions ~(workspace : Workspace.t) ~toolchain ~store ~package_graph
    ~build_ctx ~session_id ~nodes =
  let profile_name = build_ctx.Build_ctx.profile.name in
  let target_triple_str =
    Kernel.System.Host.to_string (Build_ctx.target_triplet build_ctx)
  in
  let runtimes : (Package.key, package_runtime) HashMap.t = HashMap.create () in
  let package_results : (Package.key, Package_builder.build_result) HashMap.t =
    HashMap.create ()
  in
  let pending_planning :
      (Package.key, Package_graph.package_node) HashMap.t =
    HashMap.create ()
  in
  let action_ready_queue : (Package.key * Action_node.t) Queue.t = Queue.create () in
  let ready_count = ref 0 in
  let coordinator_pid = self () in
  let enqueue_ready item =
    Queue.push action_ready_queue item;
    ready_count := !ready_count + 1
  in
  let pop_ready () =
    match Queue.pop action_ready_queue with
    | None -> None
    | Some item ->
        ready_count := !ready_count - 1;
        Some item
  in

  let materialize_initial_failure package_key package status =
    let result =
      Package_builder.
        {
          package_key;
          package;
          status;
          duration = Time.Duration.zero;
        }
    in
    let _ = HashMap.insert package_results package_key result in
    result
  in

  let target_dir_for package_name =
    Path.(
      Tusk_dirs.out_dir_with_target ~workspace_root:workspace.root
        ~profile:profile_name ~target:target_triple_str
      / Path.v package_name)
  in

  let maybe_short_circuit_cached_package ~package_key ~(package : Package.t) ~hash
      ~depset ~module_graph ~action_graph =
    let export_entries = compute_export_entries action_graph in
    let target_dir = target_dir_for package.name in
    let all_exports_present =
      List.for_all
        (fun (entry : Tusk_store.Store.export_entry) ->
          let dst = Path.(target_dir / Path.v entry.name) in
          match Fs.exists dst with
          | Ok true -> true
          | Ok false | Error _ -> false)
        export_entries
    in
    match Tusk_store.Store.get store hash with
    | None -> false
    | Some artifact ->
        let materialized =
          if all_exports_present then Ok ()
          else
            Tusk_store.Store.materialize_package_exports store
              ~exports:export_entries ~target_dir
        in
        (match materialized with
        | Error _ -> false
        | Ok () ->
            let result =
              Package_builder.
                {
                  package_key;
                  package;
                  status = Cached artifact;
                  duration = Time.Duration.zero;
                }
            in
            let _ = HashMap.insert package_results package_key result in
            (match Package_graph.get_node_by_key package_graph package_key with
            | None -> ()
            | Some node ->
                node.value <-
                  Package_graph.Built
                    {
                      package;
                      scope = Package_graph.get_scope node.value;
                      module_graph;
                      action_graph;
                      hash;
                      artifact;
                      status = Package_graph.Cached;
                      depset;
                    });
            Telemetry.emit
              (BuildCompleted
                 {
                   session_id;
                   package;
                   target = Workspace_planner.Package package.name;
                   status = `Cached;
                   duration = Time.Duration.zero;
                 });
            true)
  in

  let stage_runtime ~package_key ~(package : Package.t) ~hash ~depset ~module_graph
      ~action_graph =
    let inputs =
      List.concat
        [
          package.sources.src;
          package.sources.native;
          package.sources.tests;
        ]
    in
    let target_dir = target_dir_for package.name in
    let sandbox = Sandbox.create ~workspace ~package_name:package.name in
    Sandbox.prepare ~sandbox ~package ~inputs ~depset ~store;
    let action_queue = Action_queue.create () in
    let action_nodes = Action_graph.nodes action_graph in
    List.iter (Action_queue.queue action_queue) action_nodes;
    let runtime =
      {
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
        target_name = target_triple_str;
        total_actions = List.length action_nodes;
        active = false;
        compilation_started = false;
            }
    in
    let _ = HashMap.insert runtimes package_key runtime in
    match Package_graph.get_node_by_key package_graph package_key with
    | Some node ->
        node.value <-
          Package_graph.Planned
            {
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
    if pending_nodes = [] then ()
    else
      let progressed = ref false in
      let still_pending = vec[] in
      List.iter
        (fun package_node ->
          let package = Package_graph.get_package package_node in
          let package_key = Package_graph.get_key package_node in
          match
            Tusk_planner.plan_package_with_graph ~workspace ~toolchain ~store
              ~package_graph ~package_key ~package ~build_ctx
          with
          | Error err ->
              let _ =
                materialize_initial_failure package_key package
                  (Package_builder.Failed (PlanningFailed err))
              in
              ()
          | Ok (MissingDependencies _) | Ok (FailedDependencies _) ->
              Vector.push still_pending package_node
          | Ok
              (Planned
                {
                  package_key;
                  hash;
                  depset;
                  module_graph;
                  action_graph;
                  _;
                }) ->
              progressed := true;
              if
                not
                  (maybe_short_circuit_cached_package ~package_key ~package
                     ~hash ~depset ~module_graph ~action_graph)
              then
                stage_runtime ~package_key ~package ~hash ~depset ~module_graph
                  ~action_graph)
        pending_nodes;
      let next_pending = Vector.into_iter still_pending |> Iter.Iterator.to_list in
      if next_pending = [] then ()
      else if !progressed then plan_pass next_pending
      else
        List.iter
          (fun package_node ->
            let package_key = Package_graph.get_key package_node in
            let _ = HashMap.insert pending_planning package_key package_node in
            ())
          next_pending
  in
  plan_pass nodes;

  let try_plan_pending_packages () =
    let pending_entries = HashMap.to_list pending_planning in
    List.iter
      (fun (package_key, package_node) ->
        if Option.is_some (HashMap.get package_results package_key) then
          let _ = HashMap.remove pending_planning package_key in
          ()
        else if Option.is_some (HashMap.get runtimes package_key) then
          let _ = HashMap.remove pending_planning package_key in
          ()
        else
          let package = Package_graph.get_package package_node in
          let dep_keys = dependency_keys package_graph package_key in
          let deps_failed =
            List.exists
              (fun dep_key ->
                match HashMap.get package_results dep_key with
                | Some result -> result_is_failed result
                | None -> false)
              dep_keys
          in
          if deps_failed then (
            let names =
              List.filter_map
                (fun dep_key ->
                  match HashMap.get package_results dep_key with
                  | Some result when result_is_failed result ->
                      Some result.package.Package.name
                  | _ -> None)
                dep_keys
            in
            let _ =
              materialize_initial_failure package_key package
                (Package_builder.Failed
                   (ExecutionFailed
                      {
                        message =
                          "Failed dependencies: " ^ String.concat ", " names;
                      }))
            in
            let _ = HashMap.remove pending_planning package_key in
            ())
          else
            let deps_satisfied =
              List.for_all
                (fun dep_key ->
                  match HashMap.get package_results dep_key with
                  | Some result -> result_is_success result
                  | None -> false)
                dep_keys
            in
            if deps_satisfied then
              match
                Tusk_planner.plan_package_with_graph ~workspace ~toolchain ~store
                  ~package_graph ~package_key ~package ~build_ctx
              with
              | Error err ->
                  let _ =
                    materialize_initial_failure package_key package
                      (Package_builder.Failed (PlanningFailed err))
                  in
                  let _ = HashMap.remove pending_planning package_key in
                  ()
              | Ok (MissingDependencies _) -> ()
              | Ok (FailedDependencies { failed; _ }) ->
                  let names = List.map (fun p -> p.Package.name) failed in
                  let _ =
                    materialize_initial_failure package_key package
                      (Package_builder.Failed
                         (ExecutionFailed
                            {
                              message =
                                "Failed dependencies: "
                                ^ String.concat ", " names;
                            }))
                  in
                  let _ = HashMap.remove pending_planning package_key in
                  ()
              | Ok
                  (Planned
                    {
                      package_key;
                      hash;
                      depset;
                      module_graph;
                      action_graph;
                      _;
                    }) ->
                  if
                    not
                      (maybe_short_circuit_cached_package ~package_key ~package
                         ~hash ~depset ~module_graph ~action_graph)
                  then
                    stage_runtime ~package_key ~package ~hash ~depset
                      ~module_graph ~action_graph;
                  let _ = HashMap.remove pending_planning package_key in
                  ())
      pending_entries
  in

  let activate_ready_packages () =
    try_plan_pending_packages ();
    HashMap.iter
      (fun package_key runtime ->
        if runtime.active then ()
        else if Option.is_some (HashMap.get package_results package_key) then ()
        else
          let dep_keys = dependency_keys package_graph package_key in
          let deps_failed =
            List.exists
              (fun dep_key ->
                match HashMap.get package_results dep_key with
                | Some result -> result_is_failed result
                | None -> false)
              dep_keys
          in
          if deps_failed then (
            let reason = "needs failed dependencies" in
            Telemetry.emit
              (BuildSkipped
                 {
                   session_id;
                   package = runtime.package;
                   target = Workspace_planner.Package runtime.package.name;
                   reason;
                 });
            let result =
              Package_builder.
                {
                  package_key;
                  package = runtime.package;
                  status =
                    Failed
                      (ExecutionFailed
                         {
                           message =
                             "Skipped " ^ runtime.package.name ^ " (" ^ reason ^ ")";
                         });
                  duration = Time.Duration.zero;
                }
            in
            let _ = HashMap.insert package_results package_key result in
            mark_package_failed_in_graph package_graph ~package:runtime.package
              ~package_key ~hash:runtime.hash ~error:reason;
            Sandbox.cleanup runtime.sandbox)
          else
            let deps_satisfied =
              List.for_all
                (fun dep_key ->
                  match HashMap.get package_results dep_key with
                  | Some result -> result_is_success result
                  | None -> false)
                dep_keys
            in
            if deps_satisfied then (
              runtime.active <- true;
              Telemetry.emit
                (BuildStarted
                   {
                     session_id;
                     package = runtime.package;
                     target = Workspace_planner.Package runtime.package.name;
                   });
              enqueue_all_ready_actions ~package_key runtime enqueue_ready))
      runtimes
  in

  let finalize_if_complete package_key runtime =
    if
      Option.is_none (HashMap.get package_results package_key)
      && Action_queue.is_complete runtime.action_queue
           ~total_nodes:runtime.total_actions
    then
      let failures =
        HashMap.into_iter runtime.completed_actions
        |> Iter.Iterator.to_list
        |> List.filter_map (fun (_, r) ->
               match r.Action_executor.status with
               | Failed err -> Some err
               | Cached _ | Executed | Skipped -> None)
      in
      match failures with
      | first_failure :: _ ->
          let pkg_err = action_error_to_package_error first_failure in
          let result =
            Package_builder.
              {
                package_key;
                package = runtime.package;
                status = Failed pkg_err;
                duration = Time.Duration.zero;
              }
          in
          let _ = HashMap.insert package_results package_key result in
          mark_package_failed_in_graph package_graph ~package:runtime.package
            ~package_key ~hash:runtime.hash
            ~error:(Package_builder.package_error_to_string pkg_err);
          Telemetry.emit
            (BuildFailed
               {
                 session_id;
                 package = runtime.package;
                 target = Workspace_planner.Package runtime.package.name;
                 error = package_error_to_telemetry_error pkg_err;
               });
          Sandbox.cleanup runtime.sandbox
      | [] ->
          let status = finalize_package_success ~session_id ~store ~runtime in
          let build_status =
            match status with
            | Package_builder.Cached artifact ->
                mark_package_built_in_graph package_graph ~runtime ~artifact
                  ~status:Package_graph.Cached;
                Package_builder.Cached artifact
            | Package_builder.Built artifact ->
                mark_package_built_in_graph package_graph ~runtime ~artifact
                  ~status:Package_graph.Fresh;
                Package_builder.Built artifact
            | Package_builder.Failed _ ->
                panic "Unexpected failed status during success finalization"
          in
          let result =
            Package_builder.
              {
                package_key;
                package = runtime.package;
                status = build_status;
                duration = Time.Duration.zero;
              }
          in
          let _ = HashMap.insert package_results package_key result in
          Sandbox.cleanup runtime.sandbox
  in

  let total_packages = List.length nodes in
  let worker_count = max 1 build_ctx.Build_ctx.available_parallelism in
  let rec workspace_worker_loop () =
    match receive_any () with
    | WorkspaceAssignAction { package_key; runtime; node } ->
        let sandbox_dir = Sandbox.get_dir runtime.sandbox in
        let result =
          Action_executor.execute_node ~completed:runtime.completed_actions
            ~store ~session_id toolchain sandbox_dir node
        in
        send coordinator_pid
          (WorkspaceActionCompleted
             { worker_pid = self (); package_key; result });
        workspace_worker_loop ()
    | _ -> workspace_worker_loop ()
  in
  let workers =
    List.make ~len:worker_count ~fn:(fun _ -> spawn workspace_worker_loop)
  in
  let idle_workers : Pid.t Queue.t = Queue.create () in
  List.iter (fun pid -> Queue.push idle_workers pid) workers;
  let busy_workers : (Pid.t, Package.key) HashMap.t = HashMap.create () in

  let rec drain_work_queue () =
    match Queue.pop idle_workers with
    | None -> ()
    | Some worker_pid -> (
        match pop_ready () with
        | None ->
            Queue.push idle_workers worker_pid
        | Some (package_key, action_node) -> (
            match HashMap.get runtimes package_key with
            | None ->
                Queue.push idle_workers worker_pid;
                drain_work_queue ()
            | Some runtime ->
                let _ = HashMap.insert busy_workers worker_pid package_key in
                send worker_pid
                  (WorkspaceAssignAction
                     { package_key; runtime; node = action_node });
                drain_work_queue ()))
  in

  let rec loop () =
    if HashMap.len package_results = total_packages then ()
    else (
      activate_ready_packages ();
      drain_work_queue ();
      if HashMap.len package_results = total_packages then ()
      else if HashMap.len busy_workers = 0 && !ready_count = 0 then (
        let before = HashMap.len package_results in
        HashMap.into_iter runtimes
        |> Iter.Iterator.to_list
        |> List.iter
             (fun ((package_key, pkg_runtime) :
                    Package.key * package_runtime) ->
               finalize_if_complete package_key pkg_runtime);
        let after = HashMap.len package_results in
        if after = before then
          (HashMap.into_iter runtimes
          |> Iter.Iterator.to_list
          |> List.iter (fun ((package_key, pkg_runtime) :
                               Package.key * package_runtime) ->
                 if Option.is_none (HashMap.get package_results package_key) then (
                   let error = "No ready actions remaining for package" in
                   let result =
                     Package_builder.
                       {
                         package_key;
                         package = pkg_runtime.package;
                         status = Failed (ExecutionFailed { message = error });
                         duration = Time.Duration.zero;
                       }
                   in
                   let _ = HashMap.insert package_results package_key result in
                   mark_package_failed_in_graph package_graph
                     ~package:pkg_runtime.package ~package_key
                     ~hash:pkg_runtime.hash
                     ~error;
                   Sandbox.cleanup pkg_runtime.sandbox)));
        loop ())
      else
        match receive_any () with
        | WorkspaceActionCompleted { worker_pid; package_key; result } -> (
            let _ = HashMap.remove busy_workers worker_pid in
            Queue.push idle_workers worker_pid;
            match HashMap.get runtimes package_key with
            | None -> loop ()
            | Some runtime ->
                if
                  (not runtime.compilation_started)
                  &&
                  match result.status with
                  | Action_executor.Cached _ -> false
                  | Executed | Failed _ | Skipped -> true
                then (
                  runtime.compilation_started <- true;
                  Telemetry.emit
                    (CompilationStarted
                       {
                         session_id;
                         package = runtime.package;
                         target = Workspace_planner.Package runtime.package.name;
                       }));
                Action_queue.mark_completed runtime.action_queue result;
                enqueue_all_ready_actions ~package_key runtime enqueue_ready;
                finalize_if_complete package_key runtime;
                loop ())
        | _ -> loop ())
  in
  loop ();
  package_results
  |> HashMap.into_iter
  |> Iter.Iterator.to_list
  |> List.map (fun (_, result) -> result)

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
            build_workspace_actions ~workspace ~toolchain ~store ~package_graph
              ~build_ctx ~session_id ~nodes
          in
          let result = summarize_results ~package_graph results in
          let total_duration =
            Time.Instant.duration_since ~earlier:start (Time.Instant.now ())
          in
          Telemetry.emit
            (WorkspaceCompleted
               {
                 session_id;
                 target;
                 total_duration;
                 cached_count = result.cached_count;
                 built_count = result.built_count;
                 failed_count = result.failed_count;
               });
          Ok { result with total_duration })

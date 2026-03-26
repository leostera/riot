(** Build server - Handles build execution in a spawned process *)

open Std

open Tusk_model
open Tusk_executor

(* Build workers only report build progress and results back to the local
   session. *)

let init ~(workspace : Workspace.t) ~load_errors ~toolchain ~store ~concurrency ~session_id ~client_pid
    ~server_pid ~target ~scope ~target_arch =
  Log.debug
    ("Build worker started for session " ^ Session_id.to_string session_id ^
     (match target_arch with
      | Some arch -> ", target_arch: " ^ arch
      | None -> ""));

  send client_pid
    (Protocol.ServerResponse
       (Protocol.BuildStarted { session_id; started_at = Datetime.now () }));

  let stats = Protocol.BuildStats.make () in
  Protocol.BuildStats.mark_started stats;

  let handler_name =
    "build-worker-" ^ Session_id.to_string session_id
  in
  Telemetry.attach handler_name (fun event ->
      (match event with
      | Tusk_executor.Telemetry_events.CacheHit _ ->
          Protocol.BuildStats.inc_action_cache_hits stats
      | Tusk_executor.Telemetry_events.CacheMiss _ ->
          Protocol.BuildStats.inc_action_cache_misses stats
      | _ -> ());
      (* Filter events by session_id to prevent cross-contamination *)
      let event_session_id = match event with
        | Tusk_executor.Telemetry_events.BuildStarted { session_id; _ } -> Some session_id
        | Tusk_executor.Telemetry_events.CompilationStarted { session_id; _ } -> Some session_id
        | Tusk_executor.Telemetry_events.BuildCompleted { session_id; _ } -> Some session_id
        | Tusk_executor.Telemetry_events.BuildFailed { session_id; _ } -> Some session_id
        | Tusk_executor.Telemetry_events.BuildSkipped { session_id; _ } -> Some session_id
        | Tusk_executor.Telemetry_events.WorkspaceStarted { session_id; _ } -> Some session_id
        | Tusk_executor.Telemetry_events.WorkspaceCompleted { session_id; _ } -> Some session_id
        | Tusk_executor.Telemetry_events.ActionStarted { session_id; _ } -> Some session_id
        | Tusk_executor.Telemetry_events.ActionCompleted { session_id; _ } -> Some session_id
        | Tusk_executor.Telemetry_events.ActionFailed { session_id; _ } -> Some session_id
        | Tusk_executor.Telemetry_events.CacheHit { session_id; _ } -> Some session_id
        | Tusk_executor.Telemetry_events.CacheMiss { session_id; _ } -> Some session_id
        | _ -> None
      in
      match event_session_id with
      | Some event_sid when Session_id.to_string event_sid = Session_id.to_string session_id ->
          send client_pid
            (Protocol.ServerResponse (Protocol.BuildEvent { session_id; event }))
      | _ ->
          (* Skip events from other sessions or non-build events *)
          ());

  let planner_target =
    match target with
    | Protocol.All -> Tusk_planner.Workspace_planner.All
    | Protocol.Package name -> Tusk_planner.Workspace_planner.Package name
  in

  (* Check for package load errors first *)
  if List.length load_errors > 0 then (
    let error_msg = 
      load_errors
      |> List.map Workspace_manager.load_error_to_string
      |> String.concat "\n"
    in
    send client_pid
      (Protocol.ServerResponse
         (Protocol.PlanningFailed
            {
              session_id;
              failed_at = Datetime.now ();
              reason = "Could not load external packages:\n" ^ error_msg;
            }))
  ) else (
    Log.debug "Build worker calling Coordinator.build_workspace";
    
    (* Create build context: start with debug profile, apply workspace overrides *)
    let profile = Profile.(apply_overrides debug workspace.profile_overrides) in
    Log.debug ("Build started with profile " ^ (Data.Json.to_string (Profile.to_json profile)));
    
    (* Convert target_arch string to Tusk_model.Target.t if provided *)
    let target = match target_arch with
      | Some arch_str -> 
          (match Kernel.System.Host.from_string arch_str with
          | Ok target_triplet -> 
              Log.info ("✓ Parsed target architecture: " ^ arch_str ^ " -> os=" ^ target_triplet.os);
              let host_triplet = Kernel.System.Host.current in
              if Kernel.System.Host.equal target_triplet host_triplet then (
                Log.info "Target matches host - native compilation";
                Some Tusk_model.Target.Host
              ) else (
                Log.info ("Cross-compiling from " ^ Kernel.System.Host.to_string host_triplet ^ 
                          " to " ^ Kernel.System.Host.to_string target_triplet);
                let detection = Tusk_toolchain.CrossCompilingToolchain.detect ~target_triplet in
                Some (Tusk_model.Target.make_cross_with_config 
                  ~target_triplet
                  ~sysroot:detection.sysroot
                  ~bin_dir:detection.bin_dir
                  ~bin_prefix:detection.bin_prefix)
              )
          | Error msg -> 
              Log.warn ("Invalid target architecture '" ^ arch_str ^ "': " ^ msg ^ ", using host");
              None)
      | None -> None
    in
    
    let build_ctx =
      Build_ctx.make ~session_id ~profile ?target
        ~available_parallelism:concurrency ()
    in
    Log.info ("Build context created: target_platform=" ^ (Build_ctx.target_platform_name build_ctx) ^ 
              ", host_platform=" ^ (Build_ctx.host_platform_name build_ctx));
    
    (* Get the appropriate toolchain for the target architecture *)
    let toolchain = match target_arch with
      | Some arch_str ->
          let config = Tusk_model.Toolchain_config.from_workspace workspace in
          (match Tusk_toolchain.init_for_target ~config ~target:arch_str with
          | Ok tc -> 
              Log.debug ("Using cross-compilation toolchain for " ^ arch_str);
              tc
          | Error msg ->
              Log.error ("Failed to init toolchain for " ^ arch_str ^ ": " ^ msg ^ ", using host toolchain");
              toolchain)
      | None -> toolchain
    in
    
    let result =
      Coordinator.build_workspace ~workspace ~toolchain ~store
        ~target:planner_target
        ~scope:
          (match scope with
          | Protocol.Runtime -> Tusk_planner.Package_graph.Runtime
          | Protocol.Dev -> Tusk_planner.Package_graph.Dev)
        ~concurrency ~build_ctx ~session_id
    in

    Log.debug "Build worker finished, sending result to client";
    match result with
  | Ok workspace_result ->
      Protocol.BuildStats.set_total_modules stats
        (List.length workspace_result.results);

      List.iter
        (fun (result : Package_builder.build_result) ->
          match result.status with
          | Package_builder.Built _ ->
              Protocol.BuildStats.inc_packages_built stats;
              Protocol.BuildStats.inc_cache_misses stats
          | Package_builder.Cached _ -> Protocol.BuildStats.inc_cache_hits stats
          | Package_builder.Failed _ ->
              Protocol.BuildStats.inc_packages_failed stats)
        workspace_result.results;

      Protocol.BuildStats.mark_completed stats;

      if workspace_result.failed_count > 0 then (
        let errors =
          List.filter
            (fun (result : Package_builder.build_result) ->
              match result.status with
              | Package_builder.Failed _ -> true
              | _ -> false)
            workspace_result.results
        in
        let built =
          List.filter
            (fun (result : Package_builder.build_result) ->
              match result.status with
              | Package_builder.Failed _ -> false
              | _ -> true)
            workspace_result.results
        in
        send client_pid
          (Protocol.ServerResponse
             (Protocol.BuildFailed
                {
                  session_id;
                  failed_at = Datetime.now ();
                  stats;
                  built;
                  errors;
                }));
        (* Keep the server's canonical graph runtime-shaped. *)
        (match scope with
        | Protocol.Runtime ->
            send server_pid
              (Protocol.UpdatePackageGraph workspace_result.package_graph)
        | Protocol.Dev -> ()))
      else (
        send client_pid
          (Protocol.ServerResponse
             (Protocol.BuildCompleted
                {
                  session_id;
                  completed_at = Datetime.now ();
                  stats;
                  results = workspace_result.results;
                }));
        (match scope with
        | Protocol.Runtime ->
            send server_pid
              (Protocol.UpdatePackageGraph workspace_result.package_graph)
        | Protocol.Dev -> ()))
  | Error err -> (
      match err with
      | Tusk_planner.Workspace_planner.PackageNotFound { name; available } ->
          send client_pid
            (Protocol.ServerResponse
               (Protocol.PackageNotFound
                  {
                    session_id;
                    package_name = name;
                    available_packages = available;
                  }))
      | Tusk_planner.Workspace_planner.CycleDetected { cycle } ->
          send client_pid
            (Protocol.ServerResponse
               (Protocol.CycleDetected
                  {
                    session_id;
                    cycle_nodes = cycle;
                    detected_at = Datetime.now ();
                  }))
      | Tusk_planner.Workspace_planner.MissingDependencies { missing } ->
          Log.error "Planning failed: Missing dependencies";
          List.iter (fun { Tusk_planner.Package_graph.package; dependency } ->
            Log.error ("  " ^ package ^ " requires: " ^ dependency)
          ) missing;
          
          (* Group missing deps by package for cleaner error messages *)
          let grouped = Collections.HashMap.create () in
          List.iter (fun { Tusk_planner.Package_graph.package; dependency } ->
            match Collections.HashMap.get grouped package with
            | None -> let _ = Collections.HashMap.insert grouped package [dependency] in ()
            | Some deps -> let _ = Collections.HashMap.insert grouped package (dependency :: deps) in ()
          ) missing;
          
          let error_msg = 
            grouped
            |> Collections.HashMap.into_iter
            |> Iter.Iterator.map ~fn:(fun (pkg, deps) ->
                "  • " ^ pkg ^ " requires: " ^ String.concat ", " (List.rev deps))
            |> Iter.Iterator.to_list
            |> String.concat "\n"
          in
          
          send client_pid
            (Protocol.ServerResponse
               (Protocol.PlanningFailed
                  {
                    session_id;
                    failed_at = Datetime.now ();
                    reason = "Missing dependencies:\n" ^ error_msg;
                  }))
      | Tusk_planner.Workspace_planner.PackageLoadFailed { errors } ->
          Log.error "Planning failed: Could not load external packages";
          List.iter (fun err ->
            Log.error ("  " ^ Workspace_manager.load_error_to_string err)
          ) errors;
          
          let error_msg = 
            errors
            |> List.map Workspace_manager.load_error_to_string
            |> String.concat "\n  "
          in
          
          send client_pid
            (Protocol.ServerResponse
               (Protocol.PlanningFailed
                  {
                    session_id;
                    failed_at = Datetime.now ();
                    reason = "Could not load external packages:\n  " ^ error_msg;
                  }))
    )
  );

  Telemetry.detach handler_name;
  Log.debug "Build worker exiting";
  Ok ()

(** Start a build in a spawned worker process *)
let start ~workspace ~load_errors ~toolchain ~store ~concurrency ~session_id ~client_pid
    ~server_pid ~target ~scope ~target_arch =
  let _ =
    spawn (fun () ->
        init ~workspace ~load_errors ~toolchain ~store ~concurrency ~session_id ~client_pid
          ~server_pid ~target ~scope ~target_arch)
  in
  ()

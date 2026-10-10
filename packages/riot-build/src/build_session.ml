open Std
open Riot_model

module Session_protocol = Build_session_protocol

type t = {
  session_pid: Pid.t;
  target_dir_root: Path.t;
}

type build_stats = {
  duration_ms: int;
  packages_built: int;
  packages_failed: int;
  total_modules: int;
  cache_hits: int;
  cache_misses: int;
}

type error =
  | StartupFailed of { error: Build_session_runtime.error }
  | PackageNotFound of {
      package_name: Package_name.t;
      available_packages: Package_name.t list
    }
  | PackagesNotFound of {
      package_names: Package_name.t list;
      available_packages: Package_name.t list
    }
  | BuildFailed of { errors: Package_builder.build_result list }
  | PlanningFailed of { reason: string }
  | CycleDetected of { cycle_nodes: string list }
  | BuildAlreadyRunning of { lock_path: Path.t }
  | UnexpectedEvent of { reason: string }

type streaming_event =
  | BuildStarted of Session_id.t
  | BuildEvent of Telemetry.event
  | BuildCompleted of {
      session_id: Session_id.t;
      completed_at: DateTime.t;
      stats: build_stats;
      results: Package_builder.build_result list
    }
  | BuildFailed of {
      session_id: Session_id.t;
      failed_at: DateTime.t;
      stats: build_stats;
      built: Package_builder.build_result list;
      errors: Package_builder.build_result list
    }
  | PlanningFailed of { session_id: Session_id.t; failed_at: DateTime.t; reason: string }
  | CycleDetected of { session_id: Session_id.t; detected_at: DateTime.t; cycle_nodes: string list }

type build_target =
  BuildPackage of Package_name.t
  | BuildPackages of Package_name.t list
  | BuildAll

type build_scope =
  Runtime
  | Dev

let no_emit: Riot_model.Event.kind -> unit = fun _ -> ()

let error_message = function
  | StartupFailed { error } ->
      Build_session_runtime.error_message error
  | PackageNotFound { package_name; _ } ->
      format Format.[ str "Package '"; str (Package_name.to_string package_name); str "' not found" ]
  | PackagesNotFound { package_names; _ } ->
      format
        Format.[
          str "Packages not found: ";
          str (String.concat ", " (List.map package_names ~fn:Package_name.to_string))
        ]
  | BuildFailed { errors } ->
      let render_error (result: Package_builder.build_result) =
        match result.status with
        | Package_builder.Failed err -> format
          Format.[
            str (Package_name.to_string result.package.name);
            str ": ";
            str (Package_builder.package_error_to_string err);
          ]
        | Package_builder.Skipped { reason } -> format
          Format.[
            str (Package_name.to_string result.package.name);
            str ": skipped (";
            str reason;
            char ')';
          ]
        | Package_builder.Built _
        | Package_builder.Cached _ -> format
          Format.[ str (Package_name.to_string result.package.name); str ": build failed" ]
      in
      (
        match errors with
        | [] -> "build failed"
        | [ result ] -> render_error result
        | results -> format
          Format.[
            str "build failed:\n";
            str (String.concat "\n" (List.map results ~fn:render_error));
          ]
      )
  | PlanningFailed { reason } ->
      format Format.[ str "planning failed: "; str reason ]
  | CycleDetected { cycle_nodes } ->
      format Format.[ str "cyclic dependency detected: "; str (String.concat " -> " cycle_nodes) ]
  | BuildAlreadyRunning { lock_path } ->
      format
        Format.[
          str "another riot build is already running (";
          str (Path.to_string lock_path);
          char ')'
        ]
  | UnexpectedEvent { reason } ->
      reason

let start = fun ~workspace () ->
  match Build_session_runtime.start
    ~workspace
    ~config:Build_session_config.default
    () with
  | Ok session_pid -> Ok { session_pid; target_dir_root = workspace.target_dir_root }
  | Error err -> Error (StartupFailed { error = err })

let close = fun _t -> ()

let send_request = fun t request -> send t.session_pid (Session_protocol.RequestMessage request)

let receive_response = fun ~selector -> receive ~selector ()

let scan_workspace = fun t ~current_dir ->
  send_request t (Session_protocol.ScanWorkspace { reply_to = self (); current_dir });
  let selector msg =
    match msg with
    | Session_protocol.ResponseMessage Session_protocol.WorkspaceScanned -> `select (Ok ())
    | _ -> `skip
  in
  receive_response ~selector

module BuildLock = struct
  type nonrec t = {
    path: Path.t;
    file: Fs.File.t;
  }

  let reentrant_counts = Collections.HashMap.create ()

  let reentrant_counts_lock = Sync.Mutex.create ()

  let retry_interval = Time.Duration.from_millis 500

  let path = fun ~target_dir_root ~profile ~target ->
    Path.(target_dir_root / Path.v profile / Path.v (Riot_model.Target.to_string target) / Path.v "riot.lock")

  let path_key = fun path -> Path.to_string path

  let increment_reentrant = fun path ->
    let key = path_key path in
    Sync.Mutex.lock reentrant_counts_lock;
    let count =
        match Collections.HashMap.get reentrant_counts ~key with
      | Some count ->
          let next = count + 1 in
          let _ = Collections.HashMap.insert reentrant_counts ~key ~value:next in
          next
      | None ->
          let _ = Collections.HashMap.insert reentrant_counts ~key ~value:1 in
          1
    in
    Sync.Mutex.unlock reentrant_counts_lock;
    count

  let decrement_reentrant = fun path ->
    let key = path_key path in
    Sync.Mutex.lock reentrant_counts_lock;
    let remaining =
        match Collections.HashMap.get reentrant_counts ~key with
      | Some count when count > 1 ->
          let next = count - 1 in
          let _ = Collections.HashMap.insert reentrant_counts ~key ~value:next in
          next
      | Some _ ->
          let _ = Collections.HashMap.remove reentrant_counts ~key in
          0
      | None ->
          0
    in
    Sync.Mutex.unlock reentrant_counts_lock;
    remaining

  let is_reentrant = fun path ->
    let key = path_key path in
    Sync.Mutex.lock reentrant_counts_lock;
    let held = Collections.HashMap.has_key reentrant_counts ~key in
    Sync.Mutex.unlock reentrant_counts_lock;
    held

  let release = fun t ->
    let _ = Fs.File.unlock t.file in
    let _ = Fs.File.close t.file in
    ()

  let lock_failure = fun action path ->
    Failure (format
      Format.[ str "Failed to "; str action; str " build lock file at "; str (Path.to_string path) ])

  let rec retry = fun ?(announced = false) t ->
    if not announced then
      eprintln "build lock is taken, waiting...";
    sleep retry_interval;
    match Fs.File.try_lock_exclusive t.file with
    | Ok true ->
        Ok t
    | Ok false ->
        retry ~announced:true t
    | Error _ ->
        release t;
        raise (lock_failure "lock" t.path)

  let wait = fun ~target_dir_root ~profile ~target ->
    let build_dir = Path.(target_dir_root / Path.v profile / Path.v (Riot_model.Target.to_string target)) in
    let _ = Fs.create_dir_all build_dir |> Result.expect ~msg:"Failed to create build directory" in
    let path = path ~target_dir_root ~profile ~target in
    let file =
      match Fs.File.open_write path with
      | Ok file -> file
      | Error _ -> raise (lock_failure "open" path)
    in
    let t = { path; file } in
    match Fs.File.try_lock_exclusive file with
    | Ok true ->
        Ok t
    | Ok false ->
        retry t
    | Error _ ->
        release t;
        raise (lock_failure "lock" path)

  let acquire_lane = fun ~target_dir_root ~profile ~target fn ->
    let lock_path = path ~target_dir_root ~profile ~target in
    if is_reentrant lock_path then (
      let _ = increment_reentrant lock_path in
      try
        let result = fn () in
        let _ = decrement_reentrant lock_path in
        result
      with
      | exn ->
          let _ = decrement_reentrant lock_path in
          raise exn
    ) else
      match wait ~target_dir_root ~profile ~target with
      | Error err -> Error err
      | Ok t ->
          let _ = increment_reentrant lock_path in
          try
            let result = fn () in
            let _ = decrement_reentrant lock_path in
            release t;
            result
          with
          | exn ->
              let _ = decrement_reentrant lock_path in
              release t;
              raise exn

  let acquire = fun ~target_dir_root ~profile ~target fn ->
    match Riot_store.Build_root_lock.acquire_shared ~target_dir_root with
    | Error error -> raise (Failure (Riot_store.Build_root_lock.error_message error))
    | Ok lock ->
        try
          let result = acquire_lane ~target_dir_root ~profile ~target fn in
          Riot_store.Build_root_lock.release lock;
          result
        with
        | exn -> Riot_store.Build_root_lock.release lock; raise exn
end

let convert_build_stats: Session_protocol.BuildStats.t -> build_stats = fun stats ->
  {
    duration_ms = Int.from_float (Session_protocol.BuildStats.get_build_duration stats *. 1000.0);
    packages_built = Session_protocol.BuildStats.get_packages_built stats;
    packages_failed = Session_protocol.BuildStats.get_packages_failed stats;
    total_modules = Session_protocol.BuildStats.get_total_modules stats;
    cache_hits = Session_protocol.BuildStats.get_cache_hits stats;
    cache_misses = Session_protocol.BuildStats.get_cache_misses stats;
  }

let same_session = fun left right -> Session_id.to_string left = Session_id.to_string right

let elapsed_us_since = fun started_at ->
  Time.Instant.elapsed started_at |> Time.Duration.to_micros

let trace_build_session = fun ~started_at message ->
  let _ = started_at in
  let _ = message in
  ()

let rec handle_streaming_events = fun t ~started_at session_id callback ->
  let selector msg =
    match msg with
    | Session_protocol.ResponseMessage (Session_protocol.BuildEvent { session_id=event_session_id; event }) -> `select (`BuildEvent (
      event_session_id,
      event
    ))
    | Session_protocol.ResponseMessage (Session_protocol.BuildCompleted {
      session_id=event_session_id;
      completed_at;
      stats;
      results
    }) -> `select (`BuildCompleted (event_session_id, completed_at, stats, results))
    | Session_protocol.ResponseMessage (Session_protocol.BuildFailed {
      session_id=event_session_id;
      failed_at;
      stats;
      built;
      errors
    }) -> `select (`BuildFailed (event_session_id, failed_at, stats, built, errors))
    | Session_protocol.ResponseMessage (Session_protocol.PlanningFailed {
      session_id=event_session_id;
      failed_at;
      reason
    }) -> `select (`PlanningFailed (event_session_id, failed_at, reason))
    | Session_protocol.ResponseMessage (Session_protocol.CycleDetected {
      session_id=event_session_id;
      detected_at;
      cycle_nodes
    }) -> `select (`CycleDetected (event_session_id, detected_at, cycle_nodes))
    | Session_protocol.ResponseMessage (Session_protocol.PackageNotFound {
      session_id=event_session_id;
      package_name;
      available_packages
    }) -> `select (`PackageNotFound (event_session_id, package_name, available_packages))
    | Session_protocol.ResponseMessage (Session_protocol.PackagesNotFound {
      session_id=event_session_id;
      package_names;
      available_packages
    }) -> `select (`PackagesNotFound (event_session_id, package_names, available_packages))
    | _ -> `skip
  in
  match receive_response ~selector with
  | `BuildEvent (event_session_id, event) ->
      if same_session session_id event_session_id then
        callback (BuildEvent event);
      handle_streaming_events t ~started_at session_id callback
  | `BuildCompleted (event_session_id, completed_at, stats, results) ->
      if same_session session_id event_session_id then
        let () =
          trace_build_session
            ~started_at
            ("build-completed-received results=" ^ Int.to_string (List.length results))
        in
        let final_event = BuildCompleted {
          session_id = event_session_id;
          completed_at;
          stats = convert_build_stats stats;
          results
        } in
        callback final_event;
        let () = trace_build_session ~started_at "build-completed-callback-done" in
        Ok final_event
      else
        handle_streaming_events t ~started_at session_id callback
  | `BuildFailed (event_session_id, failed_at, stats, built, errors) ->
      if same_session session_id event_session_id then
        let final_event = BuildFailed {
          session_id = event_session_id;
          failed_at;
          stats = convert_build_stats stats;
          built;
          errors;
        }
        in
        callback final_event;
        Error ((BuildFailed { errors }): error)
      else
        handle_streaming_events t ~started_at session_id callback
  | `PlanningFailed (event_session_id, failed_at, reason) ->
      if same_session session_id event_session_id then
        let final_event = PlanningFailed { session_id = event_session_id; failed_at; reason } in
        callback final_event;
        Error ((PlanningFailed { reason }): error)
      else
        handle_streaming_events t ~started_at session_id callback
  | `CycleDetected (event_session_id, detected_at, cycle_nodes) ->
      if same_session session_id event_session_id then
        let final_event = CycleDetected { session_id = event_session_id; detected_at; cycle_nodes } in
        callback final_event;
        Error ((CycleDetected { cycle_nodes }): error)
      else
        handle_streaming_events t ~started_at session_id callback
  | `PackageNotFound (event_session_id, package_name, available_packages) ->
      if same_session session_id event_session_id then
        Error (PackageNotFound { package_name; available_packages })
      else
        handle_streaming_events t ~started_at session_id callback
  | `PackagesNotFound (event_session_id, package_names, available_packages) ->
      if same_session session_id event_session_id then
        Error (PackagesNotFound { package_names; available_packages })
      else
        handle_streaming_events t ~started_at session_id callback

let build_streaming = fun t target ?(scope = Runtime) ?(profile = "debug") ?target_arch callback ->
  let started_at = Time.Instant.now () in
  let lock_target =
    match target_arch with
    | Some target -> target
    | None -> Riot_model.Target.host ()
  in
  BuildLock.acquire ~target_dir_root:t.target_dir_root ~profile ~target:lock_target
    (fun () ->
      let request_target =
        match target with
        | BuildPackage package -> Session_protocol.Package package
        | BuildPackages packages -> Session_protocol.Packages packages
        | BuildAll -> Session_protocol.All
      in
      let session_id = Session_id.make () in
      send_request t
        (
          Session_protocol.Build {
            reply_to = self ();
            target = request_target;
            scope =
              (
                match scope with
                | Runtime -> Session_protocol.Runtime
                | Dev -> Session_protocol.Dev
              );
            profile;
            target_arch;
            session_id;
          }
        );
      let selector msg =
        match msg with
        | Session_protocol.ResponseMessage (Session_protocol.BuildStarted {
          session_id=started_session_id;
          started_at=_
        }) when same_session session_id started_session_id -> `select (Ok started_session_id)
        | Session_protocol.ResponseMessage (Session_protocol.PackageNotFound {
          session_id=event_session_id;
          package_name;
          available_packages
        }) when same_session session_id event_session_id -> `select (Error (PackageNotFound {
          package_name;
          available_packages
        }))
        | Session_protocol.ResponseMessage (Session_protocol.PackagesNotFound {
          session_id=event_session_id;
          package_names;
          available_packages
        }) when same_session session_id event_session_id -> `select (Error (PackagesNotFound {
          package_names;
          available_packages
        }))
        | _ -> `skip
      in
      match receive_response ~selector with
      | Ok started_session_id ->
          callback (BuildStarted started_session_id);
          let result = handle_streaming_events t ~started_at started_session_id callback in
          let () = trace_build_session ~started_at "handle-streaming-events-returned" in
          result
      | Error err -> Error err)

let find_executable = fun t name ->
  send_request t (Session_protocol.FindExecutable { reply_to = self (); name });
  let selector msg =
    match msg with
    | Session_protocol.ResponseMessage (Session_protocol.ExecutableFound { package; binary }) -> `select (Ok (Some (
      package,
      binary
    )))
    | Session_protocol.ResponseMessage Session_protocol.ExecutableNotFound -> `select (Ok None)
    | _ -> `skip
  in
  receive_response ~selector

let new_package = fun t ~path ~name ~is_library ->
  send_request t (Session_protocol.NewPackage { reply_to = self (); path; name; is_library });
  let selector msg =
    match msg with
    | Session_protocol.ResponseMessage (Session_protocol.PackageCreated { path; name }) -> `select (Ok (path, name))
    | Session_protocol.ResponseMessage (Session_protocol.PackageCreationError { error }) -> `select (Error error)
    | _ -> `skip
  in
  receive_response ~selector

open Std
open Std.Result.Syntax

type build_event =
  | Pm of Riot_model.Event.t
  | BuildingTarget of { target: Riot_model.Target.t; host: bool }
  | CacheGc of Riot_store.Cache_gc.event
  | Phase of Event.runtime_phase
  | Streaming of Build_session.streaming_event

type build_error =
  | ToolchainInstallFailed of { target: Riot_model.Target.t; error: string }
  | ToolchainInitializationFailed of { target: Riot_model.Target.t; error: string }
  | BuildSessionError of Build_session.error

type build_context = {
  session_id: Riot_model.Session_id.t;
  workspace: Riot_model.Workspace.t;
  package_names: Riot_model.Package_name.t list;
  targets: Riot_model.Target.Set.t;
  scope: Build_spec.scope;
  profile: Riot_model.Profile.t;
  host: Riot_model.Target.t;
  toolchain_config: Riot_model.Toolchain_config.t;
  session_target: Build_session.build_target;
  session_scope: Build_session.build_scope;
  allow_partial_failures: bool;
  record_cache_generation: bool;
  on_event: build_event -> unit;
}

let no_event: build_event -> unit = fun _ -> ()

let emit_runtime_phase = fun context phase ->
  context.on_event (Phase phase)

let emit_targets_resolved = fun context targets ->
  emit_runtime_phase context (Event.TargetsResolved { target_count = List.length targets })

let emit_toolchains_ensured = fun context targets ->
  emit_runtime_phase context (Event.ToolchainsEnsured { target_count = List.length targets })

let emit_toolchains_validated = fun context targets ->
  emit_runtime_phase context (Event.ToolchainsValidated { target_count = List.length targets })

let emit_runtime_starting = fun context ->
  emit_runtime_phase context Event.RuntimeStarting

let emit_runtime_started = fun context ->
  emit_runtime_phase context Event.RuntimeStarted

let emit_target_build_started = fun context target ->
  let host = Riot_model.Target.equal target context.host in
  context.on_event (BuildingTarget { target; host });
  emit_runtime_phase context (Event.TargetBuildStarted { target; host })

let emit_target_build_finished = fun context ~target ~result_count ~had_partial_failure ->
  emit_runtime_phase context (Event.TargetBuildFinished {
    target;
    result_count;
    had_partial_failure;
  })

let emit_cache_generation_recording_started = fun context ~lane_count ~new_entry_count ->
  emit_runtime_phase context (Event.CacheGenerationRecordingStarted { lane_count; new_entry_count })

let emit_cache_generation_recorded = fun context ~lane_count ~new_entry_count ->
  emit_runtime_phase context (Event.CacheGenerationRecorded { lane_count; new_entry_count })

let emit_returning_results = fun context ~result_count ~had_partial_failure ->
  emit_runtime_phase context (Event.ReturningResults { result_count; had_partial_failure })

let error_message = function
  | ToolchainInstallFailed { target; error } ->
      "Failed to install toolchain for "
      ^ Riot_model.Target.to_string target
      ^ ": "
      ^ error
  | ToolchainInitializationFailed { target; error } ->
      "Failed to initialize toolchain for "
      ^ Riot_model.Target.to_string target
      ^ ": "
      ^ error
  | BuildSessionError err ->
      Build_session.error_message err

let session_scope = function
  | Build_spec.Runtime -> Build_session.Runtime
  | Build_spec.Dev -> Build_session.Dev

let session_target = fun package_names ->
  match package_names with
  | [] -> Build_session.BuildAll
  | [ package_name ] -> Build_session.BuildPackage package_name
  | packages -> Build_session.BuildPackages packages

let make_context = fun ~allow_partial_failures ?(record_cache_generation = true) ?(on_event = no_event) spec ->
  let session_id = Riot_model.Session_id.make () in
  let workspace = Build_spec.workspace spec in
  let host = Riot_model.Target.current in
  let toolchain_config = Riot_model.Toolchain_config.from_root ~root:workspace.Riot_model.Workspace.root in
  {
    session_id;
    workspace;
    package_names = Build_spec.package_names spec;
    targets = Build_spec.targets spec;
    scope = Build_spec.scope spec;
    profile = Build_spec.profile spec;
    host;
    toolchain_config;
    session_target = session_target (Build_spec.package_names spec);
    session_scope = session_scope (Build_spec.scope spec);
    allow_partial_failures;
    record_cache_generation;
    on_event;
  }

let ensure_toolchains_for_targets = fun context targets ->
  let targets = Riot_model.Target.Set.to_list targets in
  let missing =
    List.filter targets ~fn:(fun target ->
        match
          Riot_toolchain.check_toolchain_status
            ~version:context.toolchain_config.version
            ~target
        with
        | Riot_toolchain.NotInstalled _
        | Riot_toolchain.Incomplete _ -> true
        | Riot_toolchain.Installed _ -> false)
  in
  let rec loop = function
    | [] -> Ok ()
    | target :: rest -> (
        match
          Riot_toolchain.download_and_install_toolchain
            context.toolchain_config.version
            ~host:context.host
            ~target
        with
        | Ok () -> loop rest
        | Error error ->
            Error (ToolchainInstallFailed { target; error }))
  in
  let* () = loop missing in
  emit_toolchains_ensured context targets;
  Ok ()

let validate_target_toolchains = fun context targets ->
  let targets = Riot_model.Target.Set.to_list targets in
  let rec loop = function
    | [] -> Ok ()
    | target :: rest -> (
        match
          Riot_toolchain.init_for_target
            ~config:context.toolchain_config
            ~target
        with
        | Ok _ -> loop rest
        | Error error ->
            Error (ToolchainInitializationFailed { target; error }))
  in
  let* () = loop targets in
  emit_toolchains_validated context targets;
  Ok ()

let start_build_session = fun context ->
  emit_runtime_starting context;
  let* session =
    Build_session.start ~workspace:context.workspace ()
    |> Result.map_err ~fn:(fun err -> BuildSessionError err)
  in
  emit_runtime_started context;
  Ok session

let sort_uniq_strings = fun values ->
  let rec dedupe acc = function
    | [] -> List.reverse acc
    | [ value ] -> List.reverse (value :: acc)
    | left :: ((right :: _) as rest) ->
        if String.equal left right then
          dedupe acc rest
        else
          dedupe (left :: acc) rest
  in
  values |> List.sort ~compare:String.compare |> dedupe []

let referenced_hashes_of_artifact = fun (artifact: Riot_store.Artifact.t) ->
  Std.Crypto.Digest.hex artifact.hash
  :: List.map
       artifact.exports
       ~fn:(fun (entry: Riot_store.Manifest.export_entry) -> entry.action_hash)
  |> sort_uniq_strings

let generation_lane_of_results = fun ~profile ~target results ->
  let hashes =
    List.flat_map results ~fn:(fun (result: Package_builder.build_result) ->
        match result.status with
        | Package_builder.Built artifact
        | Package_builder.Cached artifact -> referenced_hashes_of_artifact artifact
        | Package_builder.Skipped _
        | Package_builder.Failed _ -> [])
    |> sort_uniq_strings
  in
  Riot_store.Cache_gc.{ profile; target; hashes }

let new_entries_of_results = fun ~profile ~target results ->
  List.filter_map results ~fn:(fun (result: Package_builder.build_result) ->
      match result.status with
      | Package_builder.Built artifact -> Some Riot_store.Cache_gc.{
        profile;
        target;
        hash = Std.Crypto.Digest.hex artifact.Riot_store.Artifact.hash;
      }
      | Package_builder.Cached _
      | Package_builder.Skipped _
      | Package_builder.Failed _ -> None)

let record_successful_build_cache_generation = fun context lane_results ->
  let lanes =
    List.map lane_results ~fn:(fun (target, results) ->
        generation_lane_of_results
          ~profile:context.profile.name
          ~target
          results)
  in
  let new_entries =
    List.map lane_results ~fn:(fun (target, results) ->
        new_entries_of_results
          ~profile:context.profile.name
          ~target
          results)
    |> List.concat
  in
  match
    Riot_store.Cache_gc.record_successful_build
      ~workspace:context.workspace
      ~lanes
      ~new_entries
  with
  | Ok _ -> Ok ()
  | Error _ -> Ok ()

let new_entry_count_of_lane_results = fun context lane_results ->
  List.map lane_results ~fn:(fun (target, results) ->
      new_entries_of_results
        ~profile:context.profile.name
        ~target
        results)
  |> List.concat
  |> List.length

let record_cache_generation_if_needed = fun context lane_results had_partial_failure ->
  let new_entry_count = new_entry_count_of_lane_results context lane_results in
  if context.record_cache_generation && not had_partial_failure then (
    let lane_count = List.length lane_results in
    let emit_cache_generation_events = new_entry_count > 0 in
    if emit_cache_generation_events then
      emit_cache_generation_recording_started
        context
        ~lane_count
        ~new_entry_count;
    let* () = record_successful_build_cache_generation context lane_results in
    if emit_cache_generation_events then
      emit_cache_generation_recorded context ~lane_count ~new_entry_count;
    Ok ()
  ) else
    Ok ()

let build_loop = fun context session ->
  let targets = Riot_model.Target.Set.to_list context.targets in
  let rec loop acc had_partial_failure = function
    | [] -> Ok (List.reverse acc, had_partial_failure)
    | target :: rest ->
        emit_target_build_started context target;
        let target_arch =
          if Riot_model.Target.equal target context.host then
            None
          else
            Some target
        in
        let lane_results = ref None in
        let lane_failed = ref false in
        match
          Build_session.build_streaming
            session
            context.session_target
            ~scope:context.session_scope
            ~profile:context.profile.name
            ?target_arch
            (fun event ->
              (match event with
              | Build_session.BuildCompleted { results; _ } ->
                  lane_results := Some results
              | Build_session.BuildFailed { built; errors; _ } ->
                  lane_failed := true;
                  lane_results := Some (built @ errors)
              | Build_session.BuildStarted _
              | Build_session.BuildEvent _
              | Build_session.PlanningFailed _
              | Build_session.CycleDetected _ ->
                  ());
              context.on_event (Streaming event))
        with
        | Ok (Build_session.BuildCompleted { results; _ }) ->
            emit_target_build_finished
              context
              ~target
              ~result_count:(List.length results)
              ~had_partial_failure;
            loop ((target, results) :: acc) had_partial_failure rest
        | Ok _ -> (
            match !lane_results with
            | Some results ->
                let next_partial_failure = !lane_failed || had_partial_failure in
                emit_target_build_finished
                  context
                  ~target
                  ~result_count:(List.length results)
                  ~had_partial_failure:next_partial_failure;
                loop ((target, results) :: acc) next_partial_failure rest
            | None ->
                loop acc had_partial_failure rest)
        | Error err when context.allow_partial_failures && !lane_failed -> (
            match !lane_results with
            | Some results ->
                emit_target_build_finished
                  context
                  ~target
                  ~result_count:(List.length results)
                  ~had_partial_failure:true;
                loop ((target, results) :: acc) true rest
            | None ->
                Error (BuildSessionError err))
        | Error err ->
            Error (BuildSessionError err)
  in
  loop [] false targets

let do_build = fun context ->
  let targets = Riot_model.Target.Set.to_list context.targets in
  emit_targets_resolved context targets;
  let* () = ensure_toolchains_for_targets context context.targets in
  let* () = validate_target_toolchains context context.targets in
  let* session = start_build_session context in
  let result =
    let* (lane_results, had_partial_failure) = build_loop context session in
    let all_results =
      List.map lane_results ~fn:(fun (_, results) -> results) |> List.concat
    in
    let* () =
      record_cache_generation_if_needed
        context
        lane_results
        had_partial_failure
    in
    emit_returning_results
      context
      ~result_count:(List.length all_results)
      ~had_partial_failure;
    Ok all_results
  in
  Build_session.close session;
  result

let execute = fun ?(allow_partial_failures = false) ?(record_cache_generation = true) ?(on_event = no_event) spec ->
  let context =
    make_context
      ~allow_partial_failures
      ~record_cache_generation
      ~on_event
      spec
  in
  match Riot_store.Build_root_lock.acquire_shared ~target_dir_root:context.workspace.target_dir_root with
  | Error error ->
      Error (BuildSessionError (Build_session.UnexpectedEvent {
        reason = Riot_store.Build_root_lock.error_message error;
      }))
  | Ok lock ->
      try
        let result = do_build context in
        Riot_store.Build_root_lock.release lock;
        result
      with
      | exn -> Riot_store.Build_root_lock.release lock; raise exn

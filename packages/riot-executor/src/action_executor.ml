open Std
open Std.Collections
open Std.Time
open Riot_planner
open Riot_store
module G = Graph.SimpleGraph

type action_error = Action_queue.action_error =
  | ExecutionFailed of { message: string }
  | OutputsNotCreated of { missing: Path.t list }
  | DependenciesFailed of { failed: G.Node_id.t list }

type action_status = Action_queue.action_status =
  | Cached of Crypto.hash
  | Executed
  | Failed of action_error
  | Skipped

type execution_result = Action_queue.execution_result = {
  node_id: G.Node_id.t;
  status: action_status;
  ocamlc_warnings: string list;
  duration: Duration.t;
  started_at: Instant.t;
  completed_at: Instant.t;
}

type t = {
  completed: (G.Node_id.t, execution_result) HashMap.t;
}

type Message.t +=
  | ActionCompleted of { worker_pid: Pid.t; result: execution_result }
  | AssignAction of Action_node.t

let make_flags_absolute = fun sandbox_dir flags ->
  List.map
    (fun flag ->
      match flag with
      | Riot_toolchain.Ocamlc.Impl path -> Riot_toolchain.Ocamlc.Impl (Path.join sandbox_dir path)
      | other -> other)
    flags

let resolve_include_paths = fun sandbox_dir includes ->
  List.map
    (fun inc ->
      let inc_str = Path.to_string inc in
      if Path.is_absolute inc then
        inc
      else if String.starts_with ~prefix:"+" inc_str then
        inc
      else
        Path.join sandbox_dir inc)
    includes

let emit_action_command = fun ~session_id ~package ~node command ->
  Telemetry.emit
    Telemetry_events.(ActionCommandStarted { session_id; package; action = node; command })

let ocamlc_success = fun message -> Riot_toolchain.Ocamlc.Success { message; diagnostics = [] }

let ocamlc_failed = fun message -> Riot_toolchain.Ocamlc.Failed { message; diagnostics = [] }

let run_ocamlc_invocation = fun ~session_id ~package ~node ~sandbox_dir invocation ->
  emit_action_command ~session_id ~package ~node (Riot_toolchain.Ocamlc.to_string invocation);
  Riot_toolchain.Ocamlc.run invocation |> Diagnostic_rewrite.rewrite_ocamlc_result ~package ~sandbox_dir

let run_action = fun ~session_id ~package ~node ocamlc sandbox_dir action ->
  match action with
  | Action.CompileInterface { source; outputs=output :: _; includes; flags } ->
      let abs_source = Path.join sandbox_dir source in
      let abs_output = Path.join sandbox_dir output in
      let abs_includes = resolve_include_paths sandbox_dir includes in
      let abs_flags = make_flags_absolute sandbox_dir flags in
      let invocation = Riot_toolchain.Ocamlc.compile_interface
        ocamlc
        ~cwd:sandbox_dir
        ~includes:abs_includes
        ~flags:abs_flags
        ~output:abs_output
        abs_source in
      run_ocamlc_invocation ~session_id ~package ~node ~sandbox_dir invocation
  | Action.CompileImplementation { source; outputs=output :: _; includes; flags } ->
      let abs_source = Path.join sandbox_dir source in
      let abs_output = Path.join sandbox_dir output in
      let abs_includes = resolve_include_paths sandbox_dir includes in
      let abs_flags = make_flags_absolute sandbox_dir flags in
      let invocation = Riot_toolchain.Ocamlc.compile_impl
        ocamlc
        ~cwd:sandbox_dir
        ~includes:abs_includes
        ~flags:abs_flags
        ~output:abs_output
        abs_source in
      run_ocamlc_invocation ~session_id ~package ~node ~sandbox_dir invocation
  | Action.GenerateInterface { source; outputs=output :: _; includes; flags } ->
      let abs_source = Path.join sandbox_dir source in
      let abs_output = Path.join sandbox_dir output in
      let abs_includes = resolve_include_paths sandbox_dir includes in
      let abs_flags = make_flags_absolute sandbox_dir flags in
      let invocation = Riot_toolchain.Ocamlc.generate_interface
        ocamlc
        ~cwd:sandbox_dir
        ~includes:abs_includes
        ~flags:abs_flags
        ~output:abs_output
        abs_source in
      run_ocamlc_invocation ~session_id ~package ~node ~sandbox_dir invocation
  | Action.CompileC { source; outputs=output :: _; ccflags } ->
      let abs_source = Path.join sandbox_dir source in
      let abs_output = Path.join sandbox_dir output in
      let source_dir =
        match Path.parent source with
        | Some dir -> [ Path.join sandbox_dir dir ]
        | None -> [ sandbox_dir ]
      in
      Log.debug ("[ACTION_EXECUTOR] CompileC ccflags: " ^ String.concat " " ccflags);
      let invocation = Riot_toolchain.Ocamlc.compile_c
        ocamlc
        ~cwd:sandbox_dir
        ~includes:source_dir
        ~ccflags
        ~output:abs_output
        abs_source in
      run_ocamlc_invocation ~session_id ~package ~node ~sandbox_dir invocation
  | Action.CreateLibrary { outputs=output :: _; objects; includes } ->
      let abs_output = Path.join sandbox_dir output in
      let rel_objects = objects in
      let abs_includes = resolve_include_paths sandbox_dir includes in
      let invocation = Riot_toolchain.Ocamlc.create_library
        ocamlc
        ~cwd:sandbox_dir
        ~includes:abs_includes
        ~output:abs_output
        rel_objects in
      run_ocamlc_invocation ~session_id ~package ~node ~sandbox_dir invocation
  | Action.CreateExecutable {
    outputs=output :: _;
    objects;
    libraries;
    includes;
    cclibs;
    ccopt_flags;
    cclib_flags
  } -> (
      Log.debug
        ("[ACTION_EXECUTOR] CreateExecutable: output="
        ^ Path.to_string output
        ^ ", "
        ^ Int.to_string (List.length objects)
        ^ " objects, "
        ^ Int.to_string (List.length libraries)
        ^ " libraries: ["
        ^ String.concat ", " (List.map Path.to_string libraries)
        ^ "], cclibs: ["
        ^ String.concat ", " (List.map Path.to_string cclibs)
        ^ "], ccopt_flags: ["
        ^ String.concat ", " ccopt_flags
        ^ "], cclib_flags: ["
        ^ String.concat ", " cclib_flags
        ^ "]");
      let abs_output = Path.join sandbox_dir output in
      let abs_objects = List.map (Path.join sandbox_dir) objects in
      let abs_libraries = libraries in
      let abs_includes = resolve_include_paths sandbox_dir includes in
      let abs_cclibs = cclibs in
      let invocation = Riot_toolchain.Ocamlc.create_executable
        ocamlc
        ~cwd:sandbox_dir
        ~includes:abs_includes
        ~libs:abs_libraries
        ~cclibs:abs_cclibs
        ~ccopt_flags
        ~cclib_flags
        ~output:abs_output
        abs_objects in
      let result = run_ocamlc_invocation ~session_id ~package ~node ~sandbox_dir invocation in
      match result with
      | Riot_toolchain.Ocamlc.Success _ -> (
          match Fs.set_permissions abs_output (Fs.Permissions.of_mode 0o755) with
          | Ok () -> result
          | Error err ->
              Log.warn
                ("Failed to set executable permissions on "
                ^ Path.to_string abs_output
                ^ ": "
                ^ IO.error_message err);
              result
        )
      | _ -> result
    )
  | Action.CreateSharedLibrary {
    outputs=output :: _;
    objects;
    libraries;
    includes;
    cclibs;
    ccopt_flags;
    cclib_flags
  } -> (
      Log.debug
        ("[ACTION_EXECUTOR] CreateSharedLibrary: output="
        ^ Path.to_string output
        ^ ", "
        ^ Int.to_string (List.length objects)
        ^ " objects, "
        ^ Int.to_string (List.length libraries)
        ^ " libraries: ["
        ^ String.concat ", " (List.map Path.to_string libraries)
        ^ "], cclibs: ["
        ^ String.concat ", " (List.map Path.to_string cclibs)
        ^ "], ccopt_flags: ["
        ^ String.concat ", " ccopt_flags
        ^ "], cclib_flags: ["
        ^ String.concat ", " cclib_flags
        ^ "]");
      let abs_output = Path.join sandbox_dir output in
      let abs_objects = List.map (Path.join sandbox_dir) objects in
      let abs_libraries = libraries in
      let abs_includes = resolve_include_paths sandbox_dir includes in
      let abs_cclibs = cclibs in
      let invocation = Riot_toolchain.Ocamlc.create_shared_library
        ocamlc
        ~cwd:sandbox_dir
        ~includes:abs_includes
        ~libs:abs_libraries
        ~cclibs:abs_cclibs
        ~ccopt_flags
        ~cclib_flags
        ~output:abs_output
        abs_objects in
      run_ocamlc_invocation ~session_id ~package ~node ~sandbox_dir invocation
    )
  | Action.CompileInterface { outputs=[]; _ }
  | Action.CompileImplementation { outputs=[]; _ }
  | Action.GenerateInterface { outputs=[]; _ }
  | Action.CompileC { outputs=[]; _ }
  | Action.CreateLibrary { outputs=[]; _ }
  | Action.CreateExecutable { outputs=[]; _ }
  | Action.CreateSharedLibrary { outputs=[]; _ } ->
      ocamlc_failed "Action has no outputs"
  | Action.CopyFile { source; destination } -> (
      let src_path =
        if Path.is_absolute source then
          source
        else
          Path.join sandbox_dir source
      in
      let dst_path = Path.join sandbox_dir destination in
      match Fs.copy ~src:src_path ~dst:dst_path with
      | Ok () -> ocamlc_success "Copied"
      | Error _ -> ocamlc_failed
        ("Copy failed: " ^ Path.to_string source ^ " -> " ^ Path.to_string destination)
    )
  | Action.WriteFile { destination; content } -> (
      let dest_path = Path.join sandbox_dir destination in
      Log.debug
        ("WriteFile: writing to "
        ^ Path.to_string dest_path
        ^ " ("
        ^ Int.to_string (String.length content)
        ^ " bytes)");
      match Fs.write content dest_path with
      | Ok () ->
          Log.debug ("WriteFile: success - file written to " ^ Path.to_string dest_path);
          ocamlc_success "Written"
      | Error err ->
          let msg = IO.error_message err in
          Log.error ("WriteFile: failed - " ^ msg);
          ocamlc_failed ("Write failed: " ^ Path.to_string destination ^ " - " ^ msg)
    )
  | Action.RunMacroExpansion {
    source;
    destination;
    workspace_root;
    target_dir_root;
    providers;
    _
  } -> (
      let input_path =
        if Path.is_absolute source then
          source
        else
          Path.join sandbox_dir source
      in
      let output_path =
        if Path.is_absolute destination then
          destination
        else
          Path.join sandbox_dir destination
      in
      match Macro.Runner.run_file ~workspace_root ~target_dir_root providers ~input_path ~output_path with
      | Ok () -> ocamlc_success "Expanded macros"
      | Error message -> ocamlc_failed message
    )
  | Action.BuildForeignDependency {
    name;
    path;
    build_cmd;
    outputs;
    env
  } -> (
      let build_cmd_str = String.concat " " build_cmd in
      Log.info ("   \027[1;32mCompiling\027[0m " ^ name ^ " (" ^ build_cmd_str ^ ")");
      match build_cmd with
      | [] -> ocamlc_failed "BuildForeignDependency: empty build_cmd"
      | cmd_name :: cmd_args ->
          let normalized_path = Path.normalize path in
          let cmd = Command.make ~cwd:(Path.to_string normalized_path) ~env ~args:cmd_args cmd_name in
          let cmd_str = Command.to_string cmd in
          emit_action_command ~session_id ~package ~node cmd_str;
          Log.debug ("Executing: " ^ cmd_str);
          match Command.output cmd with
          | Ok output when output.Command.status = 0 ->
              Log.debug ("Foreign build succeeded: " ^ name);
              if String.length output.Command.stdout > 0 then
                Log.debug ("stdout: " ^ output.Command.stdout);
              let abs_outputs =
                List.map (fun out -> Path.normalize (Path.join path out)) outputs
              in
              let missing =
                List.filter
                  (fun out ->
                    match Fs.exists out with
                    | Ok true -> false
                    | Ok false
                    | Error _ -> true)
                  abs_outputs
              in
              if List.length missing > 0 then
                ocamlc_failed
                  ("Foreign build succeeded but outputs not created: "
                  ^ String.concat ", " (List.map Path.to_string missing))
              else
                ocamlc_success ("Built foreign dependency: " ^ name)
          | Ok output ->
              Log.error
                ("Foreign build failed: " ^ name ^ " - exit code " ^ Int.to_string output.Command.status);
              if String.length output.Command.stderr > 0 then
                Log.error ("stderr: " ^ output.Command.stderr);
              ocamlc_failed
                ("Foreign build failed: " ^ name ^ " - exit code " ^ Int.to_string output.Command.status)
          | Error (Command.SystemError msg) ->
              Log.error ("Failed to execute foreign build: " ^ msg);
              ocamlc_failed ("Failed to execute foreign build command: " ^ msg)
    )

let execute_actions = fun ~session_id ~(node:Action_node.t) toolchain sandbox_dir actions ->
  let ocamlc = Riot_toolchain.ocamlc toolchain in
  let package = node.value.package in
  let rec execute_next ocamlc_warnings = function
    | [] -> Ok ocamlc_warnings
    | action :: rest -> (
        let result = run_action ~session_id ~package ~node ocamlc sandbox_dir action in
        match result with
        | Riot_toolchain.Ocamlc.Success _ ->
            let action_warnings = Riot_toolchain.Ocamlc.get_ocamlc_warnings result in
            execute_next (ocamlc_warnings @ action_warnings) rest
        | Riot_toolchain.Ocamlc.Failed _ -> Error (Riot_toolchain.Ocamlc.get_output result)
      )
  in
  execute_next [] actions

let verify_outputs = fun outputs ->
  let missing =
    List.filter
      (fun out ->
        match Fs.exists out with
        | Ok true -> false
        | Ok false
        | Error _ -> true)
      outputs
  in
  if List.length missing > 0 then
    Error missing
  else
    Ok ()

let has_failed_dependencies = fun completed (node: Action_node.t) ->
  List.exists
    (fun dep_id ->
      match HashMap.get completed dep_id with
      | Some { status=Failed _ | Skipped; _ } -> true
      | _ -> false)
    node.deps

let resolve_source_for_copy = fun ~(package:Riot_model.Package.t) ~src_path ->
  let pkg_dir = package.path in
  let workspace_root_candidate =
    let pkg_path_str = Path.to_string package.path in
    let rel_path_str = Path.to_string package.relative_path in
    if String.length rel_path_str > 0 && String.ends_with ~suffix:rel_path_str pkg_path_str then
      let root_len = String.length pkg_path_str - String.length rel_path_str in
      let raw_root = String.sub pkg_path_str 0 root_len in
      let normalized_root =
        if String.ends_with ~suffix:"/" raw_root then
          String.sub raw_root 0 (String.length raw_root - 1)
        else
          raw_root
      in
      if String.length normalized_root = 0 then
        Some (Path.v ".")
      else
        Some (Path.v normalized_root)
    else
      None
  in
  let candidates =
    if Path.is_absolute src_path then
      [ src_path ]
    else
      let base = [ Path.join pkg_dir src_path ] in
      let with_workspace =
        match workspace_root_candidate with
        | Some root -> base @ [ Path.join root src_path ]
        | None -> base
      in
      with_workspace @ [ src_path ]
  in
  let rec first_existing = function
    | [] -> None
    | p :: rest -> (
        match Fs.exists p with
        | Ok true -> Some p
        | Ok false
        | Error _ -> first_existing rest
      )
  in
  match first_existing candidates with
  | Some p -> Ok p
  | None -> Error ("source not found for "
  ^ Path.to_string src_path
  ^ " (checked "
  ^ String.concat ", " (List.map Path.to_string candidates)
  ^ ")")

let execute_node = fun ~completed ~store ~session_id toolchain sandbox_dir (node: Action_node.t) ->
  let start = Instant.now () in
  if has_failed_dependencies completed node then
    (
      let now = Instant.now () in
      {
        node_id = node.id;
        status = Skipped;
        ocamlc_warnings = [];
        duration = Instant.duration_since ~earlier:start now;
        started_at = start;
        completed_at = now;
      }
    )
  else
    (
      let action_hash = Action_node.get_hash node in
      match Riot_store.Store.get store action_hash with
      | Some artifact ->
          Telemetry.emit
            Telemetry_events.(CacheHit {
              session_id;
              package = node.value.package;
              action = node;
              hash = action_hash
            });
          let _ = Riot_store.Store.promote store artifact.Riot_store.Artifact.hash ~target_dir:sandbox_dir
          |> Result.expect
            ~msg:(("Failed to materialize cached action artifact: " ^ G.Node_id.to_string node.id)) in
          let completed_at = Instant.now () in
          let duration = Instant.duration_since ~earlier:start completed_at in
          Telemetry.emit
            Telemetry_events.(
              ActionCompleted {
                session_id;
                package = node.value.package;
                action = node;
                artifact;
                status = `Cached;
                duration;
              }
            );
          {
            node_id = node.id;
            status = Cached action_hash;
            ocamlc_warnings = artifact.Riot_store.Artifact.ocamlc_warnings;
            duration;
            started_at = start;
            completed_at;
          }
      | None ->
          Telemetry.emit
            Telemetry_events.(CacheMiss {
              session_id;
              package = node.value.package;
              action = node;
              hash = action_hash
            });
          let actions = node.value.actions in
          let outputs = node.value.outs in
          let sources = node.value.srcs in
          Telemetry.emit
            Telemetry_events.(ActionStarted {
              session_id;
              package = node.value.package;
              action = node
            });
          let copy_result: (unit, string) Result.t =
            List.fold_left
              (fun acc src_path ->
                match acc with
                | Error _ -> acc
                | Ok () ->
                    match resolve_source_for_copy ~package:node.value.package ~src_path with
                    | Error msg -> Error msg
                    | Ok abs_src ->
                        let abs_dst = Path.join sandbox_dir src_path in
                        Log.debug
                          ("[EXECUTOR] Copying source: "
                          ^ Path.to_string src_path
                          ^ " from "
                          ^ Path.to_string abs_src);
                        (
                          match Path.parent abs_dst with
                          | Some dst_dir -> (
                              match Fs.create_dir_all dst_dir with
                              | Ok ()
                              | Error _ -> ()
                            )
                          | None -> ()
                        );
                        (
                          match Fs.copy ~src:abs_src ~dst:abs_dst with
                          | Ok () -> Ok ()
                          | Error err -> Error (IO.error_message err)
                        ))
              (Ok ())
              sources
          in
          let completed_at = Instant.now () in
          let duration = Instant.duration_since ~earlier:start completed_at in
          match copy_result with
          | Error msg ->
              {
                node_id = node.id;
                status = Failed (ExecutionFailed { message = "Failed to copy sources: " ^ msg });
                ocamlc_warnings = [];
                duration;
                started_at = start;
                completed_at;
              }
          | Ok () -> (
              match execute_actions ~session_id ~node toolchain sandbox_dir actions with
              | Error msg ->
                  Telemetry.emit
                    Telemetry_events.(ActionFailed {
                      session_id;
                      package = node.value.package;
                      action = node;
                      error = msg
                    });
                  {
                    node_id = node.id;
                    status = Failed (ExecutionFailed { message = msg });
                    ocamlc_warnings = [];
                    duration;
                    started_at = start;
                    completed_at;
                  }
              | Ok ocamlc_warnings ->
                  let needs_output_verification =
                    List.exists
                      (
                        function
                        | Action.BuildForeignDependency _ -> false
                        | _ -> true
                      )
                      actions
                  in
                  if not needs_output_verification then
                    let artifact = Riot_store.Store.save
                      store
                      ~package:node.value.package.name
                      ~ocamlc_warnings
                      ~hash:action_hash
                      ~sandbox_dir
                      ~outs:(List.map (Path.join sandbox_dir) outputs)
                    |> Result.expect
                      ~msg:(("Failed to store action artifact for node " ^ G.Node_id.to_string node.id)) in
                    Telemetry.emit
                      Telemetry_events.(
                        ActionCompleted {
                          session_id;
                          package = node.value.package;
                          action = node;
                          artifact;
                          status = `Fresh;
                          duration;
                        }
                      );
                    {
                      node_id = node.id;
                      status = Executed;
                      ocamlc_warnings;
                      duration;
                      started_at = start;
                      completed_at;
                    }
                  else
                    let abs_outputs = List.map (Path.join sandbox_dir) outputs in
                    match verify_outputs abs_outputs with
                    | Error missing ->
                        {
                          node_id = node.id;
                          status = Failed (OutputsNotCreated { missing });
                          ocamlc_warnings = [];
                          duration;
                          started_at = start;
                          completed_at;
                        }
                    | Ok () ->
                        let artifact = Riot_store.Store.save
                          store
                          ~package:node.value.package.name
                          ~ocamlc_warnings
                          ~hash:action_hash
                          ~sandbox_dir
                          ~outs:abs_outputs
                        |> Result.expect
                          ~msg:(("Failed to store action artifact for node "
                          ^ G.Node_id.to_string node.id)) in
                        Telemetry.emit
                          Telemetry_events.(
                            ActionCompleted {
                              session_id;
                              package = node.value.package;
                              action = node;
                              artifact;
                              status = `Fresh;
                              duration;
                            }
                          );
                        {
                          node_id = node.id;
                          status = Executed;
                          ocamlc_warnings;
                          duration;
                          started_at = start;
                          completed_at;
                        }
            )
    )

let rec worker_loop = fun ~coordinator ~toolchain ~sandbox_dir ~completed ~store ~session_id ->
  match receive_any () with
  | AssignAction node ->
      let result = execute_node ~completed ~store ~session_id toolchain sandbox_dir node in
      send coordinator (ActionCompleted { worker_pid = self (); result });
      worker_loop ~coordinator ~toolchain ~sandbox_dir ~completed ~store ~session_id
  | _ -> worker_loop ~coordinator ~toolchain ~sandbox_dir ~completed ~store ~session_id

let execute = fun ~action_graph ~sandbox ~store ~session_id toolchain ~concurrency ->
  let sandbox_dir = Sandbox.get_dir sandbox in
  let sorted_nodes = Action_graph.nodes action_graph in
  let total_nodes = List.length sorted_nodes in
  let coordinator_pid = self () in
  Log.info
    ("Starting action executor: total_nodes="
    ^ Int.to_string total_nodes
    ^ " concurrency="
    ^ Int.to_string concurrency);
  let queue = Action_queue.create () in
  List.iter
    (fun node ->
      Action_queue.queue queue node)
    sorted_nodes;
  let workers =
    List.make
      ~len:concurrency
      ~fn:(fun _ ->
        spawn
          (fun () ->
            worker_loop
              ~coordinator:coordinator_pid
              ~toolchain
              ~sandbox_dir
              ~completed:queue.completed
              ~store
              ~session_id))
  in
  let idle_workers = Queue.create () in
  List.iter
    (fun pid ->
      Queue.push idle_workers pid)
    workers;
  let busy_workers: (Pid.t, Action_node.t) HashMap.t = HashMap.create () in
  let completed_count = ref 0 in
  let rec drain_work_queue () =
    match Queue.pop idle_workers with
    | None -> ()
    | Some worker_pid -> (
        match Action_queue.next queue with
        | None -> Queue.push idle_workers worker_pid
        | Some node ->
            let _ = HashMap.insert busy_workers worker_pid node in
            send worker_pid (AssignAction node);
            drain_work_queue ()
      )
  in
  let rec dispatch_loop () =
    drain_work_queue ();
    if !completed_count = total_nodes then
      (
        let _, _, _, completed, succeeded, failed = Action_queue.stats queue in
        Log.info
          ("Action executor: all done, completed="
          ^ Int.to_string completed
          ^ " succeeded="
          ^ Int.to_string succeeded
          ^ " failed="
          ^ Int.to_string failed
          ^ " total="
          ^ Int.to_string total_nodes);
        ()
      )
    else
      match receive_any () with
      | ActionCompleted { worker_pid; result } ->
          Action_queue.mark_completed queue result;
          completed_count := !completed_count + 1;
          let _ = HashMap.remove busy_workers worker_pid in
          Queue.push idle_workers worker_pid;
          let status_str =
            match result.status with
            | Cached _ -> "cached"
            | Executed -> "executed"
            | Failed _ -> "failed"
            | Skipped -> "skipped"
          in
          Log.info
            ("Action node "
            ^ G.Node_id.to_string result.node_id
            ^ " completed: "
            ^ status_str
            ^ " ("
            ^ Int.to_string (Duration.to_millis result.duration)
            ^ "ms)");
          (
            match result.status with
            | Failed (ExecutionFailed { message }) -> Log.error ("Action failed: " ^ message)
            | Failed (OutputsNotCreated { missing }) -> Log.error
              ("Expected outputs not created: "
              ^ String.concat ", " (List.map Path.to_string missing))
            | Failed (DependenciesFailed { failed }) -> Log.error
              ("Action dependencies failed: "
              ^ String.concat ", " (List.map G.Node_id.to_string failed))
            | Skipped -> Log.warn "Action skipped due to failed dependencies"
            | _ -> ()
          );
          dispatch_loop ()
      | _ -> dispatch_loop ()
  in
  dispatch_loop ();
  { completed = queue.completed }

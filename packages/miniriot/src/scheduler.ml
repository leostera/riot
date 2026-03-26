open Kernel
open Kernel.Collections
open Kernel.Sync
open Kernel.Sync.Cell

(* Local message type extensions for scheduler *)
type Message.t +=
  | EXIT of { from : Pid.t; reason : (unit, Process.exit_reason) result }
  | DOWN of {
      ref : Process.monitor_ref;
      pid : Pid.t;
      reason : (unit, Process.exit_reason) result;
    }

type worker = {
  id : int;
  queue : Process.t Queue.t;
}

let create_worker id = { id; queue = Queue.create () }

let default_worker_count config =
  let requested = Config.worker_count config in
  if requested < 1 then 1 else requested

type t = {
  mutable stop : bool;
  mutable status : int;
  workers : worker array;
  mutable next_worker : int;
  process_workers : (Pid.t, int) HashMap.t;
  processes : (Pid.t, Process.t) HashMap.t;
  mutable current_process : Process.t option;
  io_poll : Async.Poll.t;
  config : Config.t;
  timer_wheel : Timer_wheel.t;
  mutable last_timer_check : int64;
}

let create ~config =
  match Async.Poll.make () with
  | Ok io_poll ->
      let timer_wheel = Timer_wheel.create ~config in
      let worker_count = default_worker_count config in
      let workers = Array.init worker_count create_worker in
      {
        stop = false;
        status = 0;
        workers;
        next_worker = 0;
        process_workers = HashMap.with_capacity 128;
        processes = HashMap.with_capacity 128;
        current_process = None;
        io_poll;
        config;
        timer_wheel;
        last_timer_check = 0L;
      }
  | Error err ->
      eprintln ("[Scheduler] ERROR: Failed to create Async.Poll: " ^
        (IO.error_message err));
      panic "Failed to create I/O polling system"

let current_scheduler = Cell.create None
let has_run = Cell.create false

let get_scheduler () =
  match !current_scheduler with
  | None -> panic "No scheduler running"
  | Some s -> s

let worker_count t = Array.length t.workers

let is_valid_worker t idx = idx >= 0 && idx < worker_count t

let pick_spawn_worker t =
  if worker_count t = 1 then 0 else Random.int (worker_count t)

let worker_for_pid t pid =
  match HashMap.get t.process_workers pid with
  | Some worker_id -> worker_id
  | None -> 0

let add_to_run_queue ?worker_id t proc =
  let owner =
    let pid = Process.pid proc in
    match worker_id with
    | Some candidate when is_valid_worker t candidate -> candidate
    | _ -> worker_for_pid t pid
  in
  let queue = t.workers.(owner).queue in
  Queue.push queue proc

let assign_worker t proc =
  let worker_id = pick_spawn_worker t in
  HashMap.insert t.process_workers (Process.pid proc) worker_id;
  worker_id

let spawn t fn =
  let proc = Process.make fn in
  let pid = Process.pid proc in
  HashMap.insert t.processes pid proc;
  let worker_id = assign_worker t proc in
  add_to_run_queue ~worker_id t proc;
  pid

let self () =
  let t = get_scheduler () in
  match t.current_process with
  | None -> panic "No process running"
  | Some proc -> Process.pid proc

let send pid msg =
  let t = get_scheduler () in
  match HashMap.get t.processes pid with
  | None -> ()
  | Some proc ->
      let was_waiting = Process.is_waiting proc in
      Process.send_message proc msg;
      if was_waiting then add_to_run_queue t proc

let shutdown t ~status =
  t.stop <- true;
  t.status <- status

let get_current_process t =
  match t.current_process with
  | None -> panic "No process currently running"
  | Some proc -> proc

let get_process t pid = HashMap.get t.processes pid

let handle_receive k t proc ~selector ~timeout =
  let open Proc_state in

  (* Check if we woke up from a timeout *)
  (* If timeout is set, we were woken by the timer. The rest of the function
     will check if there's a matching message. If not, we'll suspend again,
     but this time WITHOUT a timeout, which means we genuinely timed out. *)
  let should_timeout =
    match Process.receive_timeout proc with
    | Some timer_id ->
        (* Clear and cancel the timeout - we've been woken *)
        Process.clear_receive_timeout proc;
        Timer_wheel.cancel_timer t.timer_wheel timer_id;
        (* If mailbox is empty, it's definitely a timeout *)
        Process.has_empty_mailbox proc
    | None -> false
  in

  if should_timeout then (
    k (Discontinue Effects.Exception.Receive_timeout))
  else if Process.has_empty_mailbox proc then (

    (* Set up timeout if specified *)
    (match timeout with
    | `infinity -> ()
    | `after secs ->
        let now = Time.monotonic_time_nanos () in
        let duration_nanos = Int64.of_float (secs *. 1_000_000_000.0) in
        let timer_id =
          Timer_wheel.add_timer t.timer_wheel ~now ~duration_nanos
            ~mode:Timer.One_shot ~action:(Timer.Wake_process proc)
        in
        Process.set_receive_timeout proc timer_id);

    Process.mark_as_awaiting_message proc;
    k Suspend)
  else
    let fuel = Process.message_count proc in
    let rec go fuel =
      if fuel = 0 then (
        (* No matching messages found. If we have a timeout, we should suspend
           to wait for either a new message or the timeout. *)
        match timeout with
        | `infinity -> k Delay (* No timeout, yield and try again *)
        | `after secs ->
            (* We have a timeout set. Only set it up if it's not already set. *)
            (match Process.receive_timeout proc with
            | Some _timer_id ->
                (* Timeout already set, just suspend *)
                ()
            | None ->
                (* Set up the timeout *)
                let now = Time.monotonic_time_nanos () in
                let duration_nanos = Int64.of_float (secs *. 1_000_000_000.0) in
                let timer_id =
                  Timer_wheel.add_timer t.timer_wheel ~now ~duration_nanos
                    ~mode:Timer.One_shot ~action:(Timer.Wake_process proc)
                in
                Process.set_receive_timeout proc timer_id);
            Process.mark_as_awaiting_message proc;
            k Suspend)
      else
        match Process.next_message proc with
        | None ->
            Process.read_save_queue proc;
            k Delay
        | Some msg -> (
            match selector Message.(msg.msg) with
            | `select msg ->
                k (Continue msg)
            | `skip ->
                Process.add_to_save_queue proc msg;
                go (fuel - 1))
    in
    go fuel

let handle_syscall k t proc name interest source timeout =
  let open Proc_state in
  let pid_str = Pid.to_string (Process.pid proc) in

  (* Check for syscall timeout *)
  (* If syscall_timeout is set, we were woken by the timer. 
     If there are no ready tokens, it means the IO didn't complete, so it's a timeout. *)
  let should_timeout =
    match Process.syscall_timeout proc with
    | Some timer_id ->
        (* Clear and cancel the timeout - we've been woken *)
        Process.clear_syscall_timeout proc;
        Timer_wheel.cancel_timer t.timer_wheel timer_id;
        (* If no ready tokens, it's definitely a timeout *)
        Process.has_no_ready_tokens proc
    | None -> false
  in

  if should_timeout then (
    k (Discontinue Effects.Exception.Syscall_timeout))
  else
    match Process.get_ready_token proc with
    | Some (_token, _source) ->
        (* Clear timeout if set *)
        (match Process.syscall_timeout proc with
        | Some timer_id ->
            Timer_wheel.cancel_timer t.timer_wheel timer_id;
            Process.clear_syscall_timeout proc
        | None -> ());
        k (Continue ())
    | None -> (
        let token = Async.Token.make proc in
        Process.mark_as_awaiting_io proc ~name token source;
        match Async.Poll.register t.io_poll token interest source with
        | Ok () ->

            (* Set up timeout if specified *)
            (match timeout with
            | `infinity -> ()
            | `after secs ->
                let now = Time.monotonic_time_nanos () in
                let duration_nanos = Int64.of_float (secs *. 1_000_000_000.0) in
                let timer_id =
                  Timer_wheel.add_timer t.timer_wheel ~now ~duration_nanos
                    ~mode:Timer.One_shot ~action:(Timer.Wake_process proc)
                in
                Process.set_syscall_timeout proc timer_id);

            k Suspend
        | Error err ->
            eprintln
              ("[Scheduler] ERROR: Failed to register I/O for process " ^
              pid_str ^": "^
              (IO.error_message err));
            (* Don't continue - the I/O operation failed to register *)
            k (Discontinue (Failure "Failed to register I/O")))

let perform t proc =
  let open Proc_state in
  let open Proc_effect in
  let perform : type a b. (a, b) step_callback =
   fun k eff ->
    match eff with
    | Receive { selector; timeout } ->
        handle_receive k t proc ~selector ~timeout
    | Yield -> k Yield
    | Syscall { name; interest; source; timeout } ->
        handle_syscall k t proc name interest source timeout
    | _ -> k Suspend
  in
  { perform }

let handle_exit_proc t proc reason =
  let exit_reason = reason in
  let pid = Process.pid proc in

  (* 1. Send DOWN messages to all monitors *)
  List.iter
    (fun (monitor_pid, monitor_ref) ->
      match HashMap.get t.processes monitor_pid with
      | Some monitor_proc ->
          let down_msg =
            DOWN { ref = monitor_ref; pid = Process.pid proc; reason = exit_reason }
          in
          Process.send_message monitor_proc down_msg;
          if Process.is_waiting monitor_proc then add_to_run_queue t monitor_proc
      | None -> ())
    (Process.get_monitored_by proc);

  (* 2. Send EXIT to all linked processes *)
  List.iter
    (fun linked_pid ->
      match HashMap.get t.processes linked_pid with
      | Some linked_proc -> (
          let exit_msg =
            EXIT { from = Process.pid proc; reason = exit_reason }
          in

          if Process.get_trap_exit linked_proc then (
            (* trap_exit = true: convert to message *)
            Process.send_message linked_proc exit_msg;
            if Process.is_waiting linked_proc then add_to_run_queue t linked_proc)
          else
            (* trap_exit = false: kill the linked process on abnormal exit *)
            match exit_reason with
            | Ok () ->
                (* Normal exit doesn't kill links *)
                ()
            | Error exn ->
                (* Abnormal exit kills the link *)
                Process.mark_as_exited linked_proc (Error exn);
                add_to_run_queue t linked_proc)
      | None -> ())
    (Process.get_links proc);

  (* 3. Clean up the dying process *)
  if Process.is_main proc then (
    let status = match reason with Ok () -> 0 | Error _ -> 1 in
    shutdown t ~status;

    (* Clean up any I/O registrations *)
    Process.consume_ready_tokens proc (fun (_token, source) ->
        match Async.Poll.deregister t.io_poll source with
        | Ok () -> ()
        | Error err ->
            eprintln ("[Scheduler] WARN: Failed to deregister I/O: "^
              (IO.error_message err)));

    HashMap.remove t.processes pid;
    HashMap.remove t.process_workers pid;
    Process.mark_as_finalized proc)
  else (
    (* Non-main process: just finalize *)
    HashMap.remove t.processes pid;
    HashMap.remove t.process_workers pid;
    Process.mark_as_finalized proc)

let handle_wait_proc t proc =
  if Process.has_messages proc then (
    Process.mark_as_runnable proc;
    add_to_run_queue t proc)

let handle_run_proc t proc =
  let pid_str = Pid.to_string (Process.pid proc) in
  Process.mark_as_running proc;
  t.current_process <- Some proc;
  let perform = perform t proc in
  let cont =
    match Proc_state.run ~reductions:100 ~perform (Process.cont proc) with
    | Some cont -> cont
    | None ->
        eprintln ("[Scheduler] ERROR: Proc_state.run returned None for process "^ pid_str);
        eprintln "[Scheduler] This should never happen - investigating...";
        panic "Proc_state.run returned None"
  in
  Process.set_cont proc cont;
  match cont with
  | Proc_state.Finished (Ok reason) ->
      Process.mark_as_exited proc reason;
      add_to_run_queue t proc
  | Proc_state.Finished (Error { exn; backtrace }) ->
      (* Always print exception details and backtrace *)
      eprintln ("[Scheduler] Process " ^ pid_str ^ " finished with exception: " ^
        (Exception.to_string exn));
      eprintln "[Scheduler] Backtrace:";
      eprintln (Exception.raw_backtrace_to_string backtrace);
      Process.mark_as_exited proc (Error exn);
      add_to_run_queue t proc
  | _ when Process.is_waiting proc -> ()
  | Proc_state.Suspended _ ->
      add_to_run_queue t proc
  | Proc_state.Unhandled _ ->
      add_to_run_queue t proc

let handle_init_proc t proc =
  Process.init proc;
  handle_run_proc t proc

let handle_wait_io_proc _t _proc =
  (* Process will be woken up by I/O polling *)
  ()

let step_process t proc =
  match Process.state proc with
  | Uninitialized -> handle_init_proc t proc
  | Finalized -> panic "finalized processes should never be stepped on"
  | Waiting_message -> handle_wait_proc t proc
  | Waiting_io _ -> handle_wait_io_proc t proc
  | Exited reason -> handle_exit_proc t proc reason
  | Running | Runnable -> handle_run_proc t proc

let poll_io t =

  (* Use timer resolution as poll timeout for consistent tick rate *)
  let timeout_nanos = Config.resolution_to_nanos t.config.timer_resolution in

  let events =
    match Async.Poll.poll t.io_poll ~timeout:timeout_nanos with
    | Ok events -> events
    | Error err ->
        eprintln ("[Scheduler] ERROR: Failed to poll I/O: " ^
          (IO.error_message err));
        []
  in
  List.iter
    (fun event ->
      let token = Async.Event.token event in
      let proc : Process.t = Async.Token.unsafe_to_value token in
      match Process.state proc with
       | Waiting_io { source; _ } ->
            (match Async.Poll.deregister t.io_poll source with
            | Ok () -> ()
            | Error err ->
                eprintln ("[Scheduler] WARN: Failed to deregister I/O for process "^
                  (Pid.to_string (Process.pid proc)) ^": "^
                  (IO.error_message err)));
           if Process.is_alive proc then (
             Process.add_ready_token proc token source;
             Process.mark_as_runnable proc;
             add_to_run_queue t proc)
      | _ -> ())
    events

let process_timers t =
  (* Skip timer processing entirely if there are no timers *)
  if Timer_wheel.size t.timer_wheel = 0 then ()
  else
    let now = Time.monotonic_time_nanos () in
    let expired = Timer_wheel.tick t.timer_wheel ~now in

    List.iter
      (fun timer ->
        match timer.Timer.action with
        | Timer.Wake_process proc -> (
            if Process.is_alive proc then (
              Process.mark_as_runnable proc;
              add_to_run_queue t proc);

            (* Handle intervals *)
            match timer.mode with
            | Timer.One_shot -> ()
            | Timer.Interval interval ->
                let now = Time.monotonic_time_nanos () in
                let _new_timer_id =
                  Timer_wheel.add_timer t.timer_wheel ~now
                    ~duration_nanos:interval ~mode:timer.mode
                    ~action:timer.action
                in
                ())
        | Timer.Send_message (target_pid, msg) -> (
            send target_pid msg;

            (* Handle intervals *)
            match timer.mode with
            | Timer.One_shot -> ()
            | Timer.Interval interval ->
                let now = Time.monotonic_time_nanos () in
                let _new_timer_id =
                  Timer_wheel.add_timer t.timer_wheel ~now
                    ~duration_nanos:interval ~mode:timer.mode
                    ~action:timer.action
                in
                ()))
      expired

let has_runnable_processes t =
  Array.exists (fun worker -> not (Queue.is_empty worker.queue)) t.workers

let run_next_runnable t =
  let total_workers = worker_count t in
  let rec scan offset =
    if offset = total_workers then
      None
    else
      let worker_idx = (t.next_worker + offset) mod total_workers in
      match Queue.pop t.workers.(worker_idx).queue with
      | None -> scan (offset + 1)
      | Some proc ->
          t.next_worker <- (worker_idx + 1) mod total_workers;
          Some proc
  in
  scan 0

let run_loop t =
  while (not t.stop) && has_runnable_processes t do
    match run_next_runnable t with
    | None -> ()
    | Some proc -> step_process t proc
  done;

  (* Process expired timers only if there are any *)
  if Timer_wheel.size t.timer_wheel > 0 then process_timers t;

  (* Always poll for I/O with timer resolution as timeout *)
  (* This ensures we tick at the configured precision for both timers and I/O *)
  if (not t.stop) && HashMap.len t.processes > 0 then
    poll_io t

let shutdown t ~status =
  (* Mark scheduler to stop *)
  t.stop <- true;
  t.status <- status;
  (* Clear all run queues *)
  Array.iter (fun worker -> Queue.clear worker.queue) t.workers;
  (* Clear all processes *)
  HashMap.clear t.processes;
  HashMap.clear t.process_workers

let add_timer t ~now ~duration_nanos ~mode ~action =
  Timer_wheel.add_timer t.timer_wheel ~now ~duration_nanos ~mode ~action

let cancel_timer t timer_id = Timer_wheel.cancel_timer t.timer_wheel timer_id

let run ~config ~main =
  if !has_run then
    panic
      "Miniriot.run can only be called once per process. Each test should be \
       in a separate executable.";
  has_run := true;

  let t = create ~config in
  current_scheduler := Some t;

  (* Spawn main process with PID 0 *)
  let _ = spawn t main in

  (* Run until completion *)
  while not t.stop do
    run_loop t;
    (* If no processes are running and none are waiting for I/O, we're done *)
    if (not (has_runnable_processes t)) && HashMap.len t.processes = 0 then
      t.stop <- true
    else if (not (has_runnable_processes t)) then
      (* Still have processes, they might be waiting for I/O or timers *)
      let has_waiting_io =
        HashMap.fold
          (fun _ proc acc -> acc || Process.is_waiting_io proc)
          t.processes false
      in
      let has_active_timers = Timer_wheel.size t.timer_wheel > 0 in
      if (not has_waiting_io) && not has_active_timers then t.stop <- true
  done;

  t.status

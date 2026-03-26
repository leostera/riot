open Kernel
open Kernel.Collections
open Kernel.Sync
open Kernel.Sync.Cell

type Message.t +=
  | EXIT of { from : Pid.t; reason : (unit, Process.exit_reason) result }
  | DOWN of {
      ref : Process.monitor_ref;
      pid : Pid.t;
      reason : (unit, Process.exit_reason) result;
    }

type worker = {
  id : Scheduler_id.t;
  queue : Process.t Queue.t;
  lock : Mutex.t;
  cond : Condition.t;
}

type 'a response = {
  lock : Mutex.t;
  cond : Condition.t;
  mutable value : 'a option;
}

type reactor_command =
  | Add_timer of {
      now : int64;
      duration_nanos : int64;
      mode : Timer.mode;
      action : Timer.action;
      reply : Timer.id response;
    }
  | Cancel_timer of Timer.id
  | Register_io of {
      token : Async.Token.t;
      interest : Async.Interest.t;
      source : Async.Source.t;
      reply : (unit, IO.error) result response;
    }
  | Deregister_io of Async.Source.t

type t = {
  stop : bool Atomic.t;
  status : int Atomic.t;
  workers : worker array;
  processes : (Pid.t, Process.t) HashMap.t;
  processes_lock : Mutex.t;
  relations_lock : Mutex.t;
  reactor_commands : reactor_command Queue.t;
  reactor_lock : Mutex.t;
  io_poll : Async.Poll.t;
  timer_wheel : Timer_wheel.t;
  config : Config.t;
}

type domain_context = {
  scheduler : t;
  worker_id : Scheduler_id.t option;
  mutable current_process : Process.t option;
}

let current_context : domain_context option Domain.DLS.key =
  Domain.DLS.new_key (fun () -> None)

let has_run = Cell.create false

let make_response () =
  {
    lock = Mutex.create ();
    cond = Condition.create ();
    value = None;
  }

let with_response response f =
  Mutex.lock response.lock;
  try
    let result = f () in
    Mutex.unlock response.lock;
    result
  with exn ->
    Mutex.unlock response.lock;
    raise exn

let resolve_response response value =
  with_response response (fun () ->
      response.value <- Some value;
      Condition.signal response.cond)

let await_response response =
  with_response response (fun () ->
      let rec wait () =
        match response.value with
        | Some value -> value
        | None ->
            Condition.wait response.cond response.lock;
            wait ()
      in
      wait ())

let with_processes t f =
  Mutex.lock t.processes_lock;
  try
    let result = f () in
    Mutex.unlock t.processes_lock;
    result
  with exn ->
    Mutex.unlock t.processes_lock;
    raise exn

let with_reactor_commands t f =
  Mutex.lock t.reactor_lock;
  try
    let result = f () in
    Mutex.unlock t.reactor_lock;
    result
  with exn ->
    Mutex.unlock t.reactor_lock;
    raise exn

let create_worker id =
  {
    id;
    queue = Queue.create ();
    lock = Mutex.create ();
    cond = Condition.create ();
  }

let default_worker_count config =
  let requested = Config.worker_count config in
  if requested < 1 then 1 else requested

let create ~config =
  match Async.Poll.make () with
  | Ok io_poll ->
      let timer_wheel = Timer_wheel.create ~config in
      let worker_count = default_worker_count config in
      let workers =
        Array.init worker_count (fun index ->
            create_worker (Scheduler_id.of_int index))
      in
      {
        stop = Atomic.make false;
        status = Atomic.make 0;
        workers;
        processes = HashMap.with_capacity 128;
        processes_lock = Mutex.create ();
        relations_lock = Mutex.create ();
        reactor_commands = Queue.create ();
        reactor_lock = Mutex.create ();
        io_poll;
        timer_wheel;
        config;
      }
  | Error err ->
      eprintln
        ("[Scheduler] ERROR: Failed to create Async.Poll: "
       ^ IO.error_message err);
      panic "Failed to create I/O polling system"

let get_context () =
  match Domain.DLS.get current_context with
  | None -> panic "No scheduler running"
  | Some ctx -> ctx

let get_scheduler () = (get_context ()).scheduler

let self () =
  let ctx = get_context () in
  match ctx.current_process with
  | None -> panic "No process running"
  | Some proc -> Process.pid proc

let worker_count t = Array.length t.workers

let worker_by_id t worker_id =
  t.workers.(Scheduler_id.to_int worker_id)

let is_valid_worker_id t worker_id =
  let idx = Scheduler_id.to_int worker_id in
  idx >= 0 && idx < worker_count t

let pick_spawn_worker t =
  let total = worker_count t in
  if total = 1 then
    Scheduler_id.zero
  else
    Scheduler_id.of_int (Random.int total)

let with_relations_lock t f =
  Mutex.lock t.relations_lock;
  try
    let result = f () in
    Mutex.unlock t.relations_lock;
    result
  with exn ->
    Mutex.unlock t.relations_lock;
    raise exn

let request_shutdown t ~status =
  if Atomic.compare_and_set t.stop false true then Atomic.set t.status status
  else if not (Int.equal status 0) then Atomic.set t.status status;
  Array.iter
    (fun (worker : worker) ->
      Mutex.lock worker.lock;
      Condition.broadcast worker.cond;
      Mutex.unlock worker.lock)
    t.workers

let shutdown t ~status = request_shutdown t ~status

let enqueue_on_worker t worker_id proc =
  if is_valid_worker_id t worker_id && Process.try_mark_queued proc then (
    let worker = worker_by_id t worker_id in
    Mutex.lock worker.lock;
    Queue.push worker.queue proc;
    Condition.signal worker.cond;
    Mutex.unlock worker.lock)

let enqueue_owned_process t proc =
  let owner = Process.owner_worker proc in
  let worker_id = if is_valid_worker_id t owner then owner else Scheduler_id.zero in
  enqueue_on_worker t worker_id proc

let wake_process t proc =
  if Process.try_set_runnable_if_waiting proc then
    enqueue_owned_process t proc
  else if Process.is_runnable proc then
    enqueue_owned_process t proc

let wake_process_from_message t proc =
  if Process.try_mark_runnable_from_waiting_message proc then
    enqueue_owned_process t proc
  else if Process.is_runnable proc then
    enqueue_owned_process t proc

let get_process t pid =
  with_processes t (fun () -> HashMap.get t.processes pid)

let add_process t proc =
  with_processes t (fun () ->
      HashMap.insert t.processes (Process.pid proc) proc |> ignore)

let remove_process t pid =
  with_processes t (fun () ->
      HashMap.remove t.processes pid |> ignore)

let process_count t =
  with_processes t (fun () -> HashMap.len t.processes)

let maybe_shutdown_if_empty t =
  if process_count t = 0 then request_shutdown t ~status:(Atomic.get t.status)

let push_reactor_command t cmd =
  with_reactor_commands t (fun () -> Queue.push t.reactor_commands cmd)

let drain_reactor_commands t =
  with_reactor_commands t (fun () ->
      let rec drain acc =
        match Queue.pop t.reactor_commands with
        | None -> List.rev acc
        | Some cmd -> drain (cmd :: acc)
      in
      drain [])

let has_pending_reactor_commands t =
  with_reactor_commands t (fun () -> not (Queue.is_empty t.reactor_commands))

let add_timer t ~now ~duration_nanos ~mode ~action =
  if Atomic.get t.stop then
    Timer_id.make ()
  else
    let reply = make_response () in
    push_reactor_command t
      (Add_timer { now; duration_nanos; mode; action; reply });
    await_response reply

let cancel_timer t timer_id = push_reactor_command t (Cancel_timer timer_id)

let register_io t ~token ~interest ~source =
  if Atomic.get t.stop then
    Error IO.Closed
  else
    let reply = make_response () in
    push_reactor_command t (Register_io { token; interest; source; reply });
    await_response reply

let deregister_io t source = push_reactor_command t (Deregister_io source)

let send_internal t pid msg =
  match get_process t pid with
  | None -> ()
  | Some proc ->
      Process.send_message proc msg;
      wake_process_from_message t proc

let send pid msg = send_internal (get_scheduler ()) pid msg

let spawn_on_worker t ~worker_id fn =
  let proc = Process.make fn in
  let pid = Process.pid proc in
  Process.set_owner_worker proc worker_id;
  add_process t proc;
  enqueue_on_worker t worker_id proc;
  pid

let spawn t fn =
  let worker_id = pick_spawn_worker t in
  spawn_on_worker t ~worker_id fn

let get_current_process t =
  let ctx = get_context () in
  let _ = t in
  match ctx.current_process with
  | None -> panic "No process currently running"
  | Some proc -> proc

let clear_receive_timeout t proc =
  match Process.receive_timeout proc with
  | None -> ()
  | Some timer_id ->
      Process.clear_receive_timeout proc;
      cancel_timer t timer_id

let clear_syscall_timeout t proc =
  match Process.syscall_timeout proc with
  | None -> ()
  | Some timer_id ->
      Process.clear_syscall_timeout proc;
      cancel_timer t timer_id

let install_receive_timeout t proc secs =
  match Process.receive_timeout proc with
  | Some _ -> ()
  | None ->
      let now = Time.monotonic_time_nanos () in
      let duration_nanos = Int64.of_float (secs *. 1_000_000_000.0) in
      let timer_id =
        add_timer t ~now ~duration_nanos ~mode:Timer.One_shot
          ~action:(Timer.Wake_process proc)
      in
      Process.set_receive_timeout proc timer_id

let install_syscall_timeout t proc secs =
  match Process.syscall_timeout proc with
  | Some _ -> ()
  | None ->
      let now = Time.monotonic_time_nanos () in
      let duration_nanos = Int64.of_float (secs *. 1_000_000_000.0) in
      let timer_id =
        add_timer t ~now ~duration_nanos ~mode:Timer.One_shot
          ~action:(Timer.Wake_process proc)
      in
      Process.set_syscall_timeout proc timer_id

let handle_receive k t proc ~selector ~timeout =
  let open Proc_state in

  let timeout_fired =
    match Process.receive_timeout proc with
    | None -> false
    | Some timer_id ->
        if Process.take_receive_timeout_fired proc then (
          Process.clear_receive_timeout proc;
          cancel_timer t timer_id;
          true)
        else if Process.has_empty_mailbox proc then
          false
        else (
          (* A message woke this process before the timer fired.
             Cancel the timeout and keep receiving. *)
          Process.clear_receive_timeout proc;
          cancel_timer t timer_id;
          false)
  in

  let park_for_receive () =
    (match timeout with
    | `infinity -> ()
    | `after secs -> install_receive_timeout t proc secs);
    if Process.try_mark_awaiting_message proc then
      if Process.has_empty_mailbox proc then
        k Suspend
      else (
        ignore (Process.try_mark_runnable_from_waiting_message proc);
        clear_receive_timeout t proc;
        k Delay)
    else
      k Delay
  in

  if timeout_fired && Process.has_empty_mailbox proc then
    k (Discontinue Effects.Exception.Receive_timeout)
  else if Process.has_empty_mailbox proc then
    park_for_receive ()
  else
    let rec scan remaining =
      if remaining = 0 then
        park_for_receive ()
      else
        match Process.next_message proc with
        | None -> park_for_receive ()
        | Some msg -> (
            match selector Message.(msg.msg) with
            | `select selected -> k (Continue selected)
            | `skip ->
                Process.add_to_save_queue proc msg;
                scan (remaining - 1))
    in
    scan (Process.message_count proc)

let handle_syscall k t proc name interest source timeout =
  let open Proc_state in

  let timeout_state =
    match Process.syscall_timeout proc with
    | None -> `none
    | Some timer_id ->
        if Process.take_syscall_timeout_fired proc then (
          Process.clear_syscall_timeout proc;
          cancel_timer t timer_id;
          `fired)
        else
          `armed
  in

  match Process.get_ready_token proc with
  | Some _ ->
      (match timeout_state with
      | `armed -> clear_syscall_timeout t proc
      | `fired | `none -> ());
      k (Continue ())
  | None -> (
      match timeout_state with
      | `fired ->
          k (Discontinue Effects.Exception.Syscall_timeout)
      | `armed ->
          (* Spurious wakeup while still waiting on a registered syscall. *)
          k Suspend
      | `none ->
          let token = Async.Token.make proc in
          Process.mark_as_awaiting_io proc ~name token source;
          (match register_io t ~token ~interest ~source with
          | Ok () ->
              (match timeout with
              | `infinity -> ()
              | `after secs -> install_syscall_timeout t proc secs);
              k Suspend
          | Error err ->
              eprintln
                ("[Scheduler] ERROR: Failed to register I/O for process "
               ^ Pid.to_string (Process.pid proc)
               ^ ": " ^ IO.error_message err);
              Process.mark_as_runnable proc;
              k (Discontinue (Failure "Failed to register I/O"))))

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
  let pid = Process.pid proc in

  List.iter
    (fun (monitor_pid, monitor_ref) ->
      match get_process t monitor_pid with
      | None -> ()
      | Some monitor_proc ->
          Process.send_message monitor_proc
            (DOWN { ref = monitor_ref; pid; reason });
          wake_process t monitor_proc)
    (Process.get_monitored_by proc);

  List.iter
    (fun linked_pid ->
      match get_process t linked_pid with
      | None -> ()
      | Some linked_proc -> (
          if Process.get_trap_exit linked_proc then (
            Process.send_message linked_proc (EXIT { from = pid; reason });
            wake_process t linked_proc)
          else
            match reason with
            | Ok () -> ()
            | Error exn ->
                Process.mark_as_exited linked_proc (Error exn);
                wake_process t linked_proc))
    (Process.get_links proc);

  (match Process.state proc with
  | Waiting_io { source; _ } ->
      deregister_io t source;
      clear_syscall_timeout t proc
  | _ -> ());
  clear_receive_timeout t proc;

  remove_process t pid;
  Process.mark_as_finalized proc;

  if Process.is_main proc then (
    let status = match reason with Ok () -> 0 | Error _ -> 1 in
    request_shutdown t ~status)
  else
    maybe_shutdown_if_empty t

let handle_run_proc t ctx proc =
  let pid_str = Pid.to_string (Process.pid proc) in
  Process.mark_as_running proc;
  ctx.current_process <- Some proc;
  let next =
    match Proc_state.run ~reductions:100 ~perform:(perform t proc) (Process.cont proc) with
    | Some cont -> cont
    | None ->
        eprintln
          ("[Scheduler] ERROR: Proc_state.run returned None for process "
         ^ pid_str);
        panic "Proc_state.run returned None"
  in
  Process.set_cont proc next;
  ctx.current_process <- None;

  match next with
  | Proc_state.Finished (Ok reason) ->
      Process.mark_as_exited proc reason;
      handle_exit_proc t proc reason
  | Proc_state.Finished (Error { exn; backtrace }) ->
      eprintln
        ("[Scheduler] Process " ^ pid_str ^ " finished with exception: "
       ^ Exception.to_string exn);
      eprintln "[Scheduler] Backtrace:";
      eprintln (Exception.raw_backtrace_to_string backtrace);
      Process.mark_as_exited proc (Error exn);
      handle_exit_proc t proc (Error exn)
  | _ when Process.is_waiting proc || Process.is_waiting_io proc -> ()
  | _ ->
      if Process.is_alive proc then (
        Process.mark_as_runnable proc;
        enqueue_owned_process t proc)

let step_process t ctx proc =
  match Process.state proc with
  | Uninitialized ->
      Process.init proc;
      handle_run_proc t ctx proc
  | Finalized -> ()
  | Waiting_message ->
      if Process.has_messages proc
         && Process.try_mark_runnable_from_waiting_message proc
      then
        handle_run_proc t ctx proc
  | Waiting_io _ -> ()
  | Exited reason -> handle_exit_proc t proc reason
  | Running | Runnable -> handle_run_proc t ctx proc

let pop_local (worker : worker) =
  Mutex.lock worker.lock;
  let proc = Queue.pop worker.queue in
  Mutex.unlock worker.lock;
  match proc with
  | None -> None
  | Some p ->
      Process.mark_as_dequeued p;
      Some p

let wait_for_local_work t (worker : worker) =
  Mutex.lock worker.lock;
  while Queue.is_empty worker.queue && not (Atomic.get t.stop) do
    Condition.wait worker.cond worker.lock
  done;
  let proc = if Atomic.get t.stop then None else Queue.pop worker.queue in
  Mutex.unlock worker.lock;
  match proc with
  | None -> None
  | Some p ->
      Process.mark_as_dequeued p;
      Some p

let steal_batch (victim : worker) =
  Mutex.lock victim.lock;
  let available = Queue.len victim.queue in
  let steal_count = min 32 (available / 2) in
  let rec steal n acc =
    if n = 0 then List.rev acc
    else
      match Queue.pop victim.queue with
      | None -> List.rev acc
      | Some proc -> steal (n - 1) (proc :: acc)
  in
  let batch = steal steal_count [] in
  Mutex.unlock victim.lock;
  batch

let push_batch (worker : worker) batch =
  if not (List.is_empty batch) then (
    Mutex.lock worker.lock;
    List.iter (Queue.push worker.queue) batch;
    Condition.signal worker.cond;
    Mutex.unlock worker.lock)

let attempt_steal t (worker : worker) =
  let total = worker_count t in
  let self_idx = Scheduler_id.to_int worker.id in
  let rec scan offset =
    if offset = total then
      false
    else
      let victim_idx = (self_idx + offset) mod total in
      if victim_idx = self_idx then
        scan (offset + 1)
      else
        let victim = t.workers.(victim_idx) in
        let batch = steal_batch victim in
        if List.is_empty batch then
          scan (offset + 1)
        else (
          List.iter (fun proc -> Process.set_owner_worker proc worker.id) batch;
          push_batch worker batch;
          true)
  in
  if total <= 1 then false else scan 1

let reactor_poll_timeout_nanos t =
  let configured = Config.resolution_to_nanos t.config.timer_resolution in
  let max_timeout = 1_000_000L in
  if Int64.compare configured 0L <= 0 then
    max_timeout
  else if Int64.compare configured max_timeout < 0 then
    configured
  else
    max_timeout

let process_timers t =
  if Timer_wheel.size t.timer_wheel = 0 then
    ()
  else
    let now = Time.monotonic_time_nanos () in
    let expired = Timer_wheel.tick t.timer_wheel ~now in
    List.iter
      (fun timer ->
        let timer_id = timer.Timer.id in
        (match timer.Timer.action with
        | Timer.Wake_process proc ->
            if Process.has_receive_timeout_id proc timer_id then
              Process.mark_receive_timeout_fired proc;
            if Process.has_syscall_timeout_id proc timer_id then
              Process.mark_syscall_timeout_fired proc;
            if Process.is_alive proc then wake_process t proc
        | Timer.Send_message (target_pid, msg) ->
            send_internal t target_pid msg);
        match timer.mode with
        | Timer.One_shot -> ()
        | Timer.Interval interval ->
            ignore
              (Timer_wheel.add_timer t.timer_wheel
                 ~now:(Time.monotonic_time_nanos ()) ~duration_nanos:interval
                 ~mode:timer.mode ~action:timer.action))
      expired

let handle_reactor_command t cmd =
  match cmd with
  | Add_timer { now; duration_nanos; mode; action; reply } ->
      let timer_id =
        Timer_wheel.add_timer t.timer_wheel ~now ~duration_nanos ~mode ~action
      in
      resolve_response reply timer_id
  | Cancel_timer timer_id ->
      Timer_wheel.cancel_timer t.timer_wheel timer_id
  | Register_io { token; interest; source; reply } ->
      resolve_response reply (Async.Poll.register t.io_poll token interest source)
  | Deregister_io source ->
      (match Async.Poll.deregister t.io_poll source with
      | Ok () -> ()
      | Error err ->
          eprintln
            ("[Scheduler] WARN: Failed to deregister I/O source: "
           ^ IO.error_message err))

let poll_io t =
  let timeout_nanos = reactor_poll_timeout_nanos t in
  let events =
    match Async.Poll.poll t.io_poll ~timeout:timeout_nanos with
    | Ok events -> events
    | Error err ->
        eprintln
          ("[Scheduler] ERROR: Failed to poll I/O: " ^ IO.error_message err);
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
              eprintln
                ("[Scheduler] WARN: Failed to deregister I/O for process "
               ^ Pid.to_string (Process.pid proc)
               ^ ": " ^ IO.error_message err));
          if Process.is_alive proc then (
            Process.add_ready_token proc token source;
            wake_process t proc)
      | _ -> ())
    events

let reactor_loop t =
  Domain.DLS.set current_context
    (Some { scheduler = t; worker_id = None; current_process = None });
  while (not (Atomic.get t.stop)) || has_pending_reactor_commands t do
    List.iter (handle_reactor_command t) (drain_reactor_commands t);
    process_timers t;
    if not (Atomic.get t.stop) then poll_io t
  done

let worker_loop t worker =
  let ctx =
    {
      scheduler = t;
      worker_id = Some worker.id;
      current_process = None;
    }
  in
  Domain.DLS.set current_context (Some ctx);
  while not (Atomic.get t.stop) do
    match pop_local worker with
    | Some proc -> step_process t ctx proc
    | None ->
        if not (attempt_steal t worker) then
          match wait_for_local_work t worker with
          | None -> ()
          | Some proc -> step_process t ctx proc
  done;
  ctx.current_process <- None

let run ~config ~main =
  if !has_run then
    panic
      "Miniriot.run can only be called once per process. Each test should be \
       in a separate executable.";
  has_run := true;

  let t = create ~config in
  ignore (spawn_on_worker t ~worker_id:Scheduler_id.zero main);

  let reactor_domain = Domain.spawn (fun () -> reactor_loop t) in
  let worker_domains =
    Array.init (worker_count t - 1) (fun idx ->
        let worker = t.workers.(idx + 1) in
        Domain.spawn (fun () -> worker_loop t worker))
  in

  worker_loop t t.workers.(0);

  Array.iter Domain.join worker_domains;
  Domain.join reactor_domain;
  Atomic.get t.status

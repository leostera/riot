open Kernel
open Kernel.Collections
open Kernel.Sync
open Kernel.Sync.Cell

type process_slot = {
  process : Process.t;
  (* Runtime-owned scheduling metadata.
     Process continuations/mailboxes live on [Process.t], while ownership and
     queue membership live here so workers can transfer slots without mutating
     process internals. *)
  owner_worker : Scheduler_id.t Atomic.t;
  queued : bool Atomic.t;
}

type worker = {
  id : Scheduler_id.t;
  queue : process_slot Queue.t;
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

type process_shard = {
  lock : Mutex.t;
  processes : (Pid.t, process_slot) HashMap.t;
}

type process_registry = {
  shards : process_shard array;
  size : int Atomic.t;
}

type runtime_counters = {
  steals : int Atomic.t;
  failed_steals : int Atomic.t;
  remote_wakeups : int Atomic.t;
  duplicate_enqueue_races : int Atomic.t;
}

type t = {
  stop : bool Atomic.t;
  status : int Atomic.t;
  workers : worker array;
  processes : process_registry;
  counters : runtime_counters;
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
let trace_enabled = Atomic.make false

let create_counters () =
  {
    steals = Atomic.make 0;
    failed_steals = Atomic.make 0;
    remote_wakeups = Atomic.make 0;
    duplicate_enqueue_races = Atomic.make 0;
  }

let increment counter = ignore (Atomic.fetch_and_add counter 1)

let trace msg =
  if Atomic.get trace_enabled then
    eprintln ("[Scheduler.Trace] " ^ msg)

let enable_trace () = Atomic.set trace_enabled true
let disable_trace () = Atomic.set trace_enabled false

let make_response () =
  {
    lock = Mutex.create ();
    cond = Condition.create ();
    value = None;
  }

let with_response (response : 'a response) f =
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

let process_shard_count worker_count =
  let desired = Int.max 4 (worker_count * 2) in
  let rec next_pow2 n =
    if n >= desired then
      n
    else
      next_pow2 (n * 2)
  in
  next_pow2 1

let create_process_registry worker_count =
  let shard_count = process_shard_count worker_count in
  let shards =
    Array.init shard_count (fun _ ->
        {
          lock = Mutex.create ();
          processes = HashMap.with_capacity 64;
        })
  in
  {
    shards;
    size = Atomic.make 0;
  }

let create_process_slot proc ~owner_worker =
  {
    process = proc;
    owner_worker = Atomic.make owner_worker;
    queued = Atomic.make false;
  }

let slot_process slot = slot.process
let slot_pid slot = Process.pid slot.process
let slot_owner_worker slot = Atomic.get slot.owner_worker
let set_slot_owner_worker slot worker_id = Atomic.set slot.owner_worker worker_id
let mark_slot_dequeued slot = Atomic.set slot.queued false
let try_mark_slot_queued slot = Atomic.compare_and_set slot.queued false true

let shard_for_pid registry pid =
  let idx = Pid.to_int pid mod Array.length registry.shards in
  registry.shards.(idx)

let with_process_shard registry pid f =
  let shard = shard_for_pid registry pid in
  Mutex.lock shard.lock;
  try
    let result = f shard in
    Mutex.unlock shard.lock;
    result
  with exn ->
    Mutex.unlock shard.lock;
    raise exn

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
        processes = create_process_registry worker_count;
        counters = create_counters ();
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
    Scheduler_id.of_int (Kernel.Random.int total)

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
  if Atomic.get trace_enabled then
    trace
      (Kernel.String.concat ""
         [ "shutdown status="; Int.to_string status; " steals=";
           Int.to_string (Atomic.get t.counters.steals); " failed_steals=";
           Int.to_string (Atomic.get t.counters.failed_steals);
           " remote_wakeups=";
           Int.to_string (Atomic.get t.counters.remote_wakeups);
           " duplicate_enqueue_races=";
           Int.to_string (Atomic.get t.counters.duplicate_enqueue_races) ]);
  Array.iter
    (fun (worker : worker) ->
      Mutex.lock worker.lock;
      Condition.broadcast worker.cond;
      Mutex.unlock worker.lock)
    t.workers

let shutdown t ~status = request_shutdown t ~status

let enqueue_on_worker t worker_id slot =
  (* The slot-level queued flag enforces "at most one runnable-queue entry per
     process" across wakeups, local reschedules, and steals. *)
  if is_valid_worker_id t worker_id then
    if try_mark_slot_queued slot then (
      let worker = worker_by_id t worker_id in
      Mutex.lock worker.lock;
      Queue.push worker.queue slot;
      Condition.signal worker.cond;
      Mutex.unlock worker.lock)
    else if Process.is_runnable (slot_process slot) then (
      increment t.counters.duplicate_enqueue_races;
      trace
        (Kernel.String.concat ""
           [ "duplicate enqueue prevented pid=";
             Pid.to_string (slot_pid slot) ]))

let enqueue_owned_process t slot =
  let owner = slot_owner_worker slot in
  let worker_id = if is_valid_worker_id t owner then owner else Scheduler_id.zero in
  enqueue_on_worker t worker_id slot

let current_worker_id_opt () =
  match Domain.DLS.get current_context with
  | Some { worker_id; _ } -> worker_id
  | None -> None

let wake_process t slot =
  let proc = slot_process slot in
  if Process.try_set_runnable_if_waiting proc then
    enqueue_owned_process t slot
  else if Process.is_runnable proc then
    enqueue_owned_process t slot

let wake_process_from_message t slot =
  let proc = slot_process slot in
  let owner = slot_owner_worker slot in
  let remote_wakeup =
    match current_worker_id_opt () with
    | Some worker_id -> not (Scheduler_id.equal worker_id owner)
    | None -> true
  in
  if Process.try_mark_runnable_from_waiting_message proc then (
    if remote_wakeup then increment t.counters.remote_wakeups;
    enqueue_owned_process t slot)
  else if Process.is_runnable proc then (
    if remote_wakeup then increment t.counters.remote_wakeups;
    enqueue_owned_process t slot)

let get_process_slot t pid =
  with_process_shard t.processes pid (fun shard ->
      HashMap.get shard.processes pid)

let get_process t pid =
  match get_process_slot t pid with
  | None -> None
  | Some slot -> Some (slot_process slot)

let add_process_slot t slot =
  let pid = slot_pid slot in
  with_process_shard t.processes pid (fun shard ->
      let replaced = HashMap.insert shard.processes pid slot in
      if Option.is_none replaced then
        ignore (Atomic.fetch_and_add t.processes.size 1))

let remove_process_slot t pid =
  with_process_shard t.processes pid (fun shard ->
      let removed = HashMap.remove shard.processes pid in
      if Option.is_some removed then
        ignore (Atomic.fetch_and_add t.processes.size (-1)))

let process_count t = Atomic.get t.processes.size

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
  match get_process_slot t pid with
  | None -> ()
  | Some slot ->
      let proc = slot_process slot in
      Process.send_message proc msg;
      wake_process_from_message t slot

let send pid msg = send_internal (get_scheduler ()) pid msg

let spawn_on_worker t ~worker_id fn =
  let proc = Process.make fn in
  let slot = create_process_slot proc ~owner_worker:worker_id in
  let pid = slot_pid slot in
  add_process_slot t slot;
  enqueue_on_worker t worker_id slot;
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
    let rec scan_saved remaining =
      if remaining = 0 then
        scan_mailbox (Process.mailbox_count proc)
      else
        match Process.next_saved_message proc with
        | None -> scan_mailbox (Process.mailbox_count proc)
        | Some msg -> (
            match selector Message.(msg.msg) with
            | `select selected -> k (Continue selected)
            | `skip ->
                Process.add_to_save_queue proc msg;
                scan_saved (remaining - 1))
    and scan_mailbox remaining =
      if remaining = 0 then
        park_for_receive ()
      else
        match Process.next_mailbox_message proc with
        | None -> park_for_receive ()
        | Some msg -> (
            match selector Message.(msg.msg) with
            | `select selected -> k (Continue selected)
            | `skip ->
                Process.add_to_save_queue proc msg;
                scan_mailbox (remaining - 1))
    in
    scan_saved (Process.save_queue_count proc)

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

  with_relations_lock t (fun () ->
      List.iter
        (fun (monitor_pid, monitor_ref) ->
          match get_process_slot t monitor_pid with
          | None -> ()
          | Some monitor_slot ->
              let monitor_proc = slot_process monitor_slot in
              Process.send_message monitor_proc
                (Process.Messages.DOWN { ref = monitor_ref; pid; reason });
              wake_process t monitor_slot)
        (Process.get_monitored_by proc);

      List.iter
        (fun linked_pid ->
          match get_process_slot t linked_pid with
          | None -> ()
          | Some linked_slot ->
              let linked_proc = slot_process linked_slot in
              if Process.get_trap_exit linked_proc then (
                Process.send_message linked_proc
                  (Process.Messages.EXIT { from = pid; reason });
                wake_process t linked_slot)
              else
                match reason with
                | Ok () -> ()
                | Error exn ->
                    Process.mark_as_exited linked_proc (Error exn);
                    enqueue_owned_process t linked_slot)
        (Process.get_links proc))
  ;

  (match Process.state proc with
  | Waiting_io { source; _ } ->
      deregister_io t source;
      clear_syscall_timeout t proc
  | _ -> ());
  clear_receive_timeout t proc;

  remove_process_slot t pid;
  Process.mark_as_finalized proc;

  if Process.is_main proc then (
    let status = match reason with Ok () -> 0 | Error _ -> 1 in
    request_shutdown t ~status)
  else
    maybe_shutdown_if_empty t

let handle_run_proc t ctx slot =
  let proc = slot_process slot in
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
        enqueue_owned_process t slot)

let step_process t ctx slot =
  let proc = slot_process slot in
  match Process.state proc with
  | Uninitialized ->
      Process.init proc;
      handle_run_proc t ctx slot
  | Finalized -> ()
  | Waiting_message ->
      if Process.has_messages proc
         && Process.try_mark_runnable_from_waiting_message proc
      then
        handle_run_proc t ctx slot
  | Waiting_io _ -> ()
  | Exited reason -> handle_exit_proc t proc reason
  | Running | Runnable -> handle_run_proc t ctx slot

let pop_local (worker : worker) =
  Mutex.lock worker.lock;
  let slot = Queue.pop worker.queue in
  Mutex.unlock worker.lock;
  match slot with
  | None -> None
  | Some slot ->
      mark_slot_dequeued slot;
      Some slot

let wait_for_local_work t (worker : worker) =
  Mutex.lock worker.lock;
  while Queue.is_empty worker.queue && not (Atomic.get t.stop) do
    Condition.wait worker.cond worker.lock
  done;
  let slot = if Atomic.get t.stop then None else Queue.pop worker.queue in
  Mutex.unlock worker.lock;
  match slot with
  | None -> None
  | Some slot ->
      mark_slot_dequeued slot;
      Some slot

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
  let rec scan start_offset seen =
    if seen >= total - 1 then
      false
    else
      let victim_idx = (self_idx + start_offset + seen) mod total in
      if Int.equal victim_idx self_idx then
        scan start_offset (seen + 1)
      else
        let victim = t.workers.(victim_idx) in
        let batch = steal_batch victim in
        if List.is_empty batch then
          scan start_offset (seen + 1)
        else (
          (* Ownership transfer happens before enqueuing locally so future
             remote wakeups route to the stealing worker. *)
          List.iter (fun slot -> set_slot_owner_worker slot worker.id) batch;
          push_batch worker batch;
          true)
  in
  if total <= 1 then
    false
  else
    let start_offset = 1 + Kernel.Random.int (total - 1) in
    let did_steal = scan start_offset 0 in
    if did_steal then
      increment t.counters.steals
    else
      increment t.counters.failed_steals;
    did_steal

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
            if Process.is_alive proc then (
              match get_process_slot t (Process.pid proc) with
              | None -> ()
              | Some slot -> wake_process t slot)
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
      match get_process_slot t (Process.pid proc) with
      | None -> ()
      | Some slot -> (
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
                wake_process t slot)
          | _ -> ()))
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

(* Internal runtime split used by RFD0010:
   - [Runtime] owns runtime-wide state and cross-worker operations
   - [Reactor] owns timer/I/O command processing and polling loop
   - [Worker] owns process execution and steal/park loop
*)
module Runtime = struct
  type runtime = t
  type slot = process_slot

  let create = create
  let request_shutdown = request_shutdown
  let shutdown = shutdown
  let worker_count = worker_count
  let with_relations_lock = with_relations_lock
  let get_process = get_process
  let get_process_slot = get_process_slot
  let get_current_process = get_current_process
  let spawn_on_worker = spawn_on_worker
  let spawn = spawn
  let send_internal = send_internal
  let enqueue_on_worker = enqueue_on_worker
  let enqueue_owned_process = enqueue_owned_process
  let wake_process = wake_process
  let wake_process_from_message = wake_process_from_message
end

module Reactor = struct
  type runtime = t
  type command = reactor_command

  let add_timer = add_timer
  let cancel_timer = cancel_timer
  let register_io = register_io
  let deregister_io = deregister_io
  let loop = reactor_loop
end

module Worker = struct
  type runtime = t
  type state = worker

  let loop = worker_loop
  let attempt_steal = attempt_steal
end

let run ~config ~main =
  if !has_run then
    panic
      "Miniriot.run can only be called once per process. Each test should be \
       in a separate executable.";
  has_run := true;

  let t = Runtime.create ~config in
  ignore (Runtime.spawn_on_worker t ~worker_id:Scheduler_id.zero main);

  let reactor_domain = Domain.spawn (fun () -> Reactor.loop t) in
  let worker_domains =
    Array.init (Runtime.worker_count t - 1) (fun idx ->
        let worker = t.workers.(idx + 1) in
        Domain.spawn (fun () -> Worker.loop t worker))
  in

  Worker.loop t t.workers.(0);

  Array.iter Domain.join worker_domains;
  Domain.join reactor_domain;
  Atomic.get t.status

open Kernel
open Kernel.Collections
open Kernel.Sync
open Kernel.Sync.Cell
open Scheduler_types

type process_slot = Scheduler_types.process_slot

type worker = Scheduler_types.worker

type 'a response = 'a Scheduler_types.response

type reactor_command = Scheduler_types.reactor_command

type process_shard = Scheduler_types.process_shard

type process_registry = Scheduler_types.process_registry

type runtime_counters = Scheduler_types.runtime_counters

type t = Scheduler_types.t

type domain_context = Scheduler_types.domain_context

let current_context = Scheduler_types.current_context

let has_run = Cell.create false

let trace_enabled = Atomic.make false

type trace_counters = {
  steals: int;
  failed_steals: int;
  remote_wakeups: int;
  duplicate_enqueue_races: int;
}

let create_counters (): runtime_counters = {
  steals = Atomic.make 0;
  failed_steals = Atomic.make 0;
  remote_wakeups = Atomic.make 0;
  duplicate_enqueue_races = Atomic.make 0
}

let increment = fun counter -> ignore (Atomic.fetch_and_add counter 1)

let trace = fun msg ->
  if Atomic.get trace_enabled then
    eprintln ("[Scheduler.Trace] " ^ msg)

let snapshot_counters = fun (counters: runtime_counters) ->
  {
    steals = Atomic.get counters.steals;
    failed_steals = Atomic.get counters.failed_steals;
    remote_wakeups = Atomic.get counters.remote_wakeups;
    duplicate_enqueue_races = Atomic.get counters.duplicate_enqueue_races
  }

let enable_trace = fun () ->
  Atomic.set trace_enabled true

let disable_trace = fun () ->
  Atomic.set trace_enabled false

let trace_counters = fun t -> snapshot_counters t.counters

let reset_trace_counters = fun t ->
  Atomic.set t.counters.steals 0;
  Atomic.set t.counters.failed_steals 0;
  Atomic.set t.counters.remote_wakeups 0;
  Atomic.set t.counters.duplicate_enqueue_races 0

let ensure_can_run_once = fun () ->
  if !has_run then
    panic
      "Actors.run can only be called once per process. Each test should be \
       in a separate executable.";
  has_run := true

let make_response = fun () -> { lock = Mutex.create (); cond = Condition.create (); value = None }

let with_response = fun (response: 'a response) f ->
  Mutex.lock response.lock;
  try
    let result = f () in
    Mutex.unlock response.lock;
    result
  with
  | exn ->
      Mutex.unlock response.lock;
      raise exn

let resolve_response = fun response value ->
  with_response response
    (fun () ->
      response.value <- Some value;
      Condition.signal response.cond)

let await_response = fun response ->
  with_response response
    (fun () ->
      let rec wait () =
        match response.value with
        | Some value -> value
        | None ->
            Condition.wait response.cond response.lock;
            wait ()
      in
      wait ())

let with_reactor_commands = fun t f ->
  Mutex.lock t.reactor_lock;
  try
    let result = f () in
    Mutex.unlock t.reactor_lock;
    result
  with
  | exn ->
      Mutex.unlock t.reactor_lock;
      raise exn

let create_worker = fun id ->
  { id; queue = Queue.create (); lock = Mutex.create (); cond = Condition.create () }

let default_worker_count = fun config ->
  let requested = Config.worker_count config in
  if requested < 1 then
    1
  else
    requested

let process_shard_count = fun worker_count ->
  let desired = Int.max 4 (worker_count * 2) in
  let rec next_pow2 n =
    if n >= desired then
      n
    else
      next_pow2 (n * 2)
  in
  next_pow2 1

let create_process_registry = fun worker_count ->
  let shard_count = process_shard_count worker_count in
  let shards =
    Array.init
      shard_count
      (fun _ -> { lock = Mutex.create (); processes = HashMap.with_capacity 64 })
  in
  { shards; size = Atomic.make 0 }

let create_process_slot = fun proc ~owner_worker ~placement ->
  {
    process = proc;
    placement;
    owner_worker = Atomic.make owner_worker;
    blocking_lane = None;
    queued = Atomic.make false;
    executing = Atomic.make false;
    pending = Atomic.make false;
  }

let slot_process = fun slot -> slot.process

let slot_pid = fun slot -> Process.pid slot.process

let slot_placement = fun slot -> slot.placement

let slot_owner_worker = fun slot -> Atomic.get slot.owner_worker

let set_slot_owner_worker = fun slot worker_id ->
  Atomic.set slot.owner_worker worker_id

let set_slot_blocking_lane = fun slot lane -> slot.blocking_lane <- Some lane

let mark_slot_dequeued = fun slot ->
  Atomic.set slot.queued false

let try_mark_slot_queued = fun slot ->
  Atomic.compare_and_set slot.queued false true

let try_mark_slot_executing = fun slot ->
  Atomic.compare_and_set slot.executing false true

let clear_slot_executing = fun slot ->
  Atomic.set slot.executing false

let mark_slot_pending = fun slot ->
  Atomic.set slot.pending true

let take_slot_pending = fun slot ->
  Atomic.exchange slot.pending false

let shard_for_pid = fun registry pid ->
  let idx = Pid.to_int pid mod Array.length registry.shards in
  registry.shards.(idx)

let with_process_shard = fun registry pid f ->
  let shard = shard_for_pid registry pid in
  Mutex.lock shard.lock;
  try
    let result = f shard in
    Mutex.unlock shard.lock;
    result
  with
  | exn ->
      Mutex.unlock shard.lock;
      raise exn

let create = fun ~config ->
  match Async.Poll.make () with
  | Ok io_poll ->
      let timer_wheel = Timer_wheel.create ~config in
      let worker_count = default_worker_count config in
      let workers =
        Array.init worker_count (fun index -> create_worker (Scheduler_id.of_int index))
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
        blocking_lanes_lock = Mutex.create ();
        blocking_lanes = [];
        config;
      }
  | Error err ->
      eprintln ("[Scheduler] ERROR: Failed to create Async.Poll: " ^ IO.error_message err);
      panic "Failed to create I/O polling system"

let get_context = fun () ->
  match Domain.DLS.get current_context with
  | None -> panic "No scheduler running"
  | Some ctx -> ctx

let get_scheduler = fun () -> (get_context ()).scheduler

let self = fun () ->
  let ctx = get_context () in
  match ctx.current_process with
  | None -> panic "No process running"
  | Some proc -> Process.pid proc

let worker_count = fun t -> Array.length t.workers

let worker_by_id = fun t worker_id -> t.workers.(Scheduler_id.to_int worker_id)

let is_valid_worker_id = fun t worker_id ->
  let idx = Scheduler_id.to_int worker_id in
  idx >= 0 && idx < worker_count t

let pick_spawn_worker = fun t ->
  let total = worker_count t in
  if total = 1 then
    Scheduler_id.zero
  else
    Scheduler_id.of_int (Kernel.Random.int total)

let with_relations_lock = fun t f ->
  Mutex.lock t.relations_lock;
  try
    let result = f () in
    Mutex.unlock t.relations_lock;
    result
  with
  | exn ->
      Mutex.unlock t.relations_lock;
      raise exn

let request_shutdown = fun t ~status ->
  if Atomic.compare_and_set t.stop false true then
    Atomic.set t.status status
  else if not (Int.equal status 0) then
    Atomic.set t.status status;
  let counters = snapshot_counters t.counters in
  if Atomic.get trace_enabled then
    trace
      (
        Kernel.String.concat ""
          [
            "shutdown status=";
            Int.to_string status;
            " steals=";
            Int.to_string counters.steals;
            " failed_steals=";
            Int.to_string counters.failed_steals;
            " remote_wakeups=";
            Int.to_string counters.remote_wakeups;
            " duplicate_enqueue_races=";
            Int.to_string counters.duplicate_enqueue_races
          ]
      );
  Array.iter
    (fun (worker: worker) ->
      Mutex.lock worker.lock;
      Condition.broadcast worker.cond;
      Mutex.unlock worker.lock)
    t.workers;
  Mutex.lock t.blocking_lanes_lock;
  List.iter
    (fun (lane: blocking_lane) ->
      Mutex.lock lane.lock;
      Condition.broadcast lane.cond;
      Mutex.unlock lane.lock)
    t.blocking_lanes;
  Mutex.unlock t.blocking_lanes_lock

let shutdown = fun t ~status -> request_shutdown t ~status

let enqueue_on_worker = fun t worker_id slot ->
  (* The slot-level queued flag enforces "at most one runnable-queue entry per
     process" across wakeups, local reschedules, and steals. *)
  if is_valid_worker_id t worker_id then
    if try_mark_slot_queued slot then
      (
        let worker = worker_by_id t worker_id in
        Mutex.lock worker.lock;
        Queue.push worker.queue slot;
        Condition.signal worker.cond;
        Mutex.unlock worker.lock
      )
    else if Process.is_runnable (slot_process slot) then
      (
        increment t.counters.duplicate_enqueue_races;
        trace
          (Kernel.String.concat
            ""
            [ "duplicate enqueue prevented pid="; Pid.to_string (slot_pid slot) ])
      )

let enqueue_on_blocking_lane = fun t slot ->
  if try_mark_slot_queued slot then
    match slot.blocking_lane with
    | None -> panic "blocking slot is missing its lane"
    | Some lane ->
        Mutex.lock lane.lock;
        Condition.signal lane.cond;
        Mutex.unlock lane.lock
  else if Process.is_runnable (slot_process slot) then
    (
      increment t.counters.duplicate_enqueue_races;
      trace
        (Kernel.String.concat
          ""
          [ "duplicate enqueue prevented pid="; Pid.to_string (slot_pid slot) ])
    )

let enqueue_owned_process = fun t slot ->
  match slot_placement slot with
  | Blocking -> enqueue_on_blocking_lane t slot
  | Normal
  | Pinned ->
      let owner = slot_owner_worker slot in
      let worker_id =
        if is_valid_worker_id t owner then
          owner
        else
          Scheduler_id.zero
      in
      enqueue_on_worker t worker_id slot

let current_worker_id_opt = fun () ->
  match Domain.DLS.get current_context with
  | Some { worker_id; _ } -> worker_id
  | None -> None

let wake_process = fun t slot ->
  let proc = slot_process slot in
  if Process.try_set_runnable_if_waiting proc then
    enqueue_owned_process t slot
  else if Process.is_runnable proc then
    enqueue_owned_process t slot

let wake_process_from_message = fun t slot ->
  let proc = slot_process slot in
  let owner = slot_owner_worker slot in
  let remote_wakeup =
    match current_worker_id_opt () with
    | Some worker_id -> not (Scheduler_id.equal worker_id owner)
    | None -> true
  in
  if Process.try_mark_runnable_from_waiting_message proc then
    (
      if remote_wakeup then
        increment t.counters.remote_wakeups;
      enqueue_owned_process t slot
    )
  else if Process.is_runnable proc then
    (
      if remote_wakeup then
        increment t.counters.remote_wakeups;
      enqueue_owned_process t slot
    )

let get_process_slot = fun t pid ->
  with_process_shard t.processes pid
    (fun shard ->
      HashMap.get shard.processes pid)

let get_process = fun t pid ->
  match get_process_slot t pid with
  | None -> None
  | Some slot -> Some (slot_process slot)

let add_process_slot = fun t slot ->
  let pid = slot_pid slot in
  with_process_shard t.processes pid
    (fun shard ->
      let replaced = HashMap.insert shard.processes pid slot in
      if Option.is_none replaced then
        ignore (Atomic.fetch_and_add t.processes.size 1))

let remove_process_slot = fun t pid ->
  with_process_shard t.processes pid
    (fun shard ->
      let removed = HashMap.remove shard.processes pid in
      if Option.is_some removed then
        ignore (Atomic.fetch_and_add t.processes.size (-1)))

let process_count = fun t -> Atomic.get t.processes.size

let maybe_shutdown_if_empty = fun t ->
  if process_count t = 0 then
    request_shutdown t ~status:(Atomic.get t.status)

let push_reactor_command = fun t cmd ->
  with_reactor_commands t
    (fun () ->
      Queue.push t.reactor_commands cmd)

let drain_reactor_commands = fun t ->
  with_reactor_commands t
    (fun () ->
      let rec drain acc =
        match Queue.pop t.reactor_commands with
        | None -> List.rev acc
        | Some cmd -> drain (cmd :: acc)
      in
      drain [])

let has_pending_reactor_commands = fun t ->
  with_reactor_commands t (fun () -> not (Queue.is_empty t.reactor_commands))

let add_timer = fun t ~now ~duration_nanos ~mode ~action ->
  if Atomic.get t.stop then
    Timer_id.make ()
  else
    let reply = make_response () in
    push_reactor_command t
      (
        Add_timer {
          now;
          duration_nanos;
          mode;
          action;
          reply;
        }
      );
    await_response reply

let cancel_timer = fun t timer_id -> push_reactor_command t (Cancel_timer timer_id)

let register_io = fun t ~token ~interest ~source ->
  if Atomic.get t.stop then
    Error IO.Closed
  else
    let reply = make_response () in
    push_reactor_command t (Register_io { token; interest; source; reply });
    await_response reply

let deregister_io = fun t source -> push_reactor_command t (Deregister_io source)

let send_internal = fun t pid msg ->
  match get_process_slot t pid with
  | None -> ()
  | Some slot ->
      let proc = slot_process slot in
      Process.send_message proc msg;
      wake_process_from_message t slot

let send = fun pid msg -> send_internal (get_scheduler ()) pid msg

let kill = fun t pid reason ->
  match get_process_slot t pid with
  | None -> ()
  | Some slot ->
      let proc = slot_process slot in
      Process.request_exit proc (Error reason);
      mark_slot_pending slot;
      wake_process t slot

let spawn_on_worker_with_placement = fun t ~worker_id ~placement fn ->
  let proc = Process.make fn in
  let slot = create_process_slot proc ~owner_worker:worker_id ~placement in
  let pid = slot_pid slot in
  add_process_slot t slot;
  (
    match placement with
    | Blocking -> enqueue_on_blocking_lane t slot
    | Normal
    | Pinned -> enqueue_on_worker t worker_id slot
  );
  pid

let spawn_on_worker = fun t ~worker_id fn ->
  spawn_on_worker_with_placement t ~worker_id ~placement:Normal fn

let spawn = fun t fn ->
  let worker_id = pick_spawn_worker t in
  spawn_on_worker t ~worker_id fn

let spawn_pinned = fun ?worker_id t fn ->
  let worker_id =
    match worker_id with
    | Some worker_id ->
        if is_valid_worker_id t worker_id then
          worker_id
        else
          panic "spawn_pinned got an invalid scheduler id"
    | None -> (
        match current_worker_id_opt () with
        | Some worker_id -> worker_id
        | None -> pick_spawn_worker t
      )
  in
  spawn_on_worker_with_placement t ~worker_id ~placement:Pinned fn

let with_blocking_lanes_lock = fun t f ->
  Mutex.lock t.blocking_lanes_lock;
  try
    let result = f () in
    Mutex.unlock t.blocking_lanes_lock;
    result
  with
  | exn ->
      Mutex.unlock t.blocking_lanes_lock;
      raise exn

let add_blocking_lane = fun t lane ->
  with_blocking_lanes_lock t (fun () -> t.blocking_lanes <- lane :: t.blocking_lanes)

let wait_for_blocking_work = fun t slot ->
  match slot.blocking_lane with
  | None -> None
  | Some lane ->
      let proc = slot_process slot in
      Mutex.lock lane.lock;
      while (not (Atomic.get t.stop)) && Process.is_alive proc && not (Atomic.get slot.queued) do
        Condition.wait lane.cond lane.lock
      done;
      Mutex.unlock lane.lock;
      if Atomic.get t.stop || not (Process.is_alive proc) then
        None
      else if Atomic.compare_and_set slot.queued true false then
        Some slot
      else
        None

let get_current_process = fun () ->
  let ctx = get_context () in
  match ctx.current_process with
  | None -> panic "No process currently running"
  | Some proc -> proc

let clear_receive_timeout = fun t proc ->
  match Process.receive_timeout proc with
  | None -> ()
  | Some timer_id ->
      Process.clear_receive_timeout proc;
      cancel_timer t timer_id

let clear_syscall_timeout = fun t proc ->
  match Process.syscall_timeout proc with
  | None -> ()
  | Some timer_id ->
      Process.clear_syscall_timeout proc;
      cancel_timer t timer_id

let install_receive_timeout = fun t proc secs ->
  match Process.receive_timeout proc with
  | Some _ -> ()
  | None ->
      let now = Time.monotonic_time_nanos () in
      let duration_nanos = Int64.of_float (secs *. 1_000_000_000.0) in
      let timer_id = add_timer
        t
        ~now
        ~duration_nanos
        ~mode:Timer.One_shot
        ~action:(Timer.Wake_process proc) in
      Process.set_receive_timeout proc timer_id

let install_syscall_timeout = fun t proc secs ->
  match Process.syscall_timeout proc with
  | Some _ -> ()
  | None ->
      let now = Time.monotonic_time_nanos () in
      let duration_nanos = Int64.of_float (secs *. 1_000_000_000.0) in
      let timer_id = add_timer
        t
        ~now
        ~duration_nanos
        ~mode:Timer.One_shot
        ~action:(Timer.Wake_process proc) in
      Process.set_syscall_timeout proc timer_id

let handle_receive = fun k t proc ~selector ~timeout ->
  let open Proc_state in
    let timeout_expired =
      match Process.receive_timeout proc with
      | None -> false
      | Some _ -> Process.take_receive_timeout_fired proc
    in
    let park_for_receive () =
      (
        match timeout with
        | `infinity -> ()
        | `after secs -> install_receive_timeout t proc secs
      );
      if Process.try_mark_awaiting_message proc then
        if Process.has_empty_mailbox proc then
          k Suspend
        else (
          ignore (Process.try_mark_runnable_from_waiting_message proc);
          k Delay
        )
      else
        k Delay
    in
    let continue_with selected =
      clear_receive_timeout t proc;
      k (Continue selected)
    in
    let timeout_receive () =
      clear_receive_timeout t proc;
      k (Discontinue Effects.Exception.Receive_timeout)
    in
    let rec scan_saved remaining =
      if remaining = 0 then
        scan_mailbox (Process.mailbox_count proc)
      else
        match Process.next_saved_message proc with
        | None -> scan_mailbox (Process.mailbox_count proc)
        | Some msg -> (
            match selector Message.(msg.msg) with
            | `select selected -> Some selected
            | `skip ->
                Process.add_to_save_queue proc msg;
                scan_saved (remaining - 1)
          )
    and scan_mailbox remaining =
      if remaining = 0 then
        None
      else
        match Process.next_mailbox_message proc with
        | None -> None
        | Some msg -> (
            match selector Message.(msg.msg) with
            | `select selected -> Some selected
            | `skip ->
                Process.add_to_save_queue proc msg;
                scan_mailbox (remaining - 1)
          )
    in
    if timeout_expired then
      timeout_receive ()
    else
      let selected =
        if Process.has_empty_mailbox proc then
          None
        else
          scan_saved (Process.save_queue_count proc)
      in
      match selected with
      | Some selected -> continue_with selected
      | None -> park_for_receive ()

let handle_syscall = fun k t proc name interest source timeout ->
  let open Proc_state in
    let timeout_state =
      match Process.syscall_timeout proc with
      | None -> `none
      | Some timer_id ->
          if Process.take_syscall_timeout_fired proc then
            (
              Process.clear_syscall_timeout proc;
              cancel_timer t timer_id;
              `fired
            )
          else
            `armed
    in
    match Process.get_ready_token proc with
    | Some _ ->
        (
          match timeout_state with
          | `armed -> clear_syscall_timeout t proc
          | `fired
          | `none -> ()
        );
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
            (
              match register_io t ~token ~interest ~source with
              | Ok () ->
                  (
                    match timeout with
                    | `infinity -> ()
                    | `after secs -> install_syscall_timeout t proc secs
                  );
                  k Suspend
              | Error err ->
                  eprintln
                    ("[Scheduler] ERROR: Failed to register I/O for process "
                    ^ Pid.to_string (Process.pid proc)
                    ^ ": "
                    ^ IO.error_message err);
                  Process.mark_as_runnable proc;
                  k (Discontinue (Failure "Failed to register I/O"))
            )
      )

let perform = fun t proc ->
  let open Proc_state in
    let open Proc_effect in
      let perform: type a b. (a, b) step_callback = fun k eff ->
        match eff with
        | Receive { selector; timeout } -> handle_receive k t proc ~selector ~timeout
        | Yield -> k Yield
        | Syscall { name; interest; source; timeout } -> handle_syscall
          k
          t
          proc
          name
          interest
          source
          timeout
        | _ -> k Suspend
      in
      { perform }

let handle_exit_proc = fun t proc reason ->
  let pid = Process.pid proc in
  with_relations_lock t
    (fun () ->
      List.iter
        (fun ((monitor_pid, monitor_ref)) ->
          match get_process_slot t monitor_pid with
          | None -> ()
          | Some monitor_slot ->
              let monitor_proc = slot_process monitor_slot in
              Process.demonitor monitor_proc monitor_ref;
              Process.send_message
                monitor_proc
                (Process.Messages.DOWN { ref = monitor_ref; pid; reason });
              wake_process t monitor_slot)
        (Process.get_monitored_by proc);
      List.iter
        (fun ((monitor_ref, monitored_pid)) ->
          match get_process_slot t monitored_pid with
          | None -> ()
          | Some monitored_slot ->
              let monitored_proc = slot_process monitored_slot in
              Process.remove_monitored_by monitored_proc pid monitor_ref)
        (Process.get_monitors proc);
      List.iter
        (fun linked_pid ->
          match get_process_slot t linked_pid with
          | None -> ()
          | Some linked_slot ->
              let linked_proc = slot_process linked_slot in
              Process.unlink linked_proc pid;
              if Process.get_trap_exit linked_proc then
                (
                  Process.send_message linked_proc (Process.Messages.EXIT { from = pid; reason });
                  wake_process t linked_slot
                )
              else
                match reason with
                | Ok () -> ()
                | Error exn ->
                    Process.mark_as_exited linked_proc (Error exn);
                    enqueue_owned_process t linked_slot)
        (Process.get_links proc));
  (
    match Process.state proc with
    | Waiting_io { source; _ } ->
        deregister_io t source;
        clear_syscall_timeout t proc
    | _ -> ()
  );
  clear_receive_timeout t proc;
  remove_process_slot t pid;
  Process.mark_as_finalized proc;
  if Process.is_main proc then
    (
      let status =
        match reason with
        | Ok () -> 0
        | Error _ -> 1
      in
      request_shutdown t ~status
    )
  else
    maybe_shutdown_if_empty t

let handle_run_proc = fun t ctx slot ->
  let proc = slot_process slot in
  let pid_str = Pid.to_string (Process.pid proc) in
  Process.mark_as_running proc;
  ctx.current_process <- Some proc;
  try
    let next =
      try
        match
          Proc_state.run
            ~consume_reduction:(fun () ->
              match Process.use_reduction proc with
              | Process.Continue -> false
              | Process.Yield -> true)
            ~perform:(perform t proc)
            (Process.cont proc)
        with
        | Some cont -> cont
        | None ->
            eprintln ("[Scheduler] ERROR: Proc_state.run returned None for process " ^ pid_str);
            panic "Proc_state.run returned None"
      with
      | exn ->
          eprintln
            ("[Scheduler] ERROR: Proc_state.run raised for process "
            ^ pid_str
            ^ ": "
            ^ Exception.to_string exn);
          raise exn
    in
    Process.set_cont proc next;
    ctx.current_process <- None;
    clear_slot_executing slot;
    let pending = take_slot_pending slot in
    match next with
    | Proc_state.Finished (Ok reason) ->
        Process.mark_as_exited proc reason;
        handle_exit_proc t proc reason
    | Proc_state.Finished (Error { exn; backtrace }) ->
        eprintln
          ("[Scheduler] Process " ^ pid_str ^ " finished with exception: " ^ Exception.to_string exn);
        eprintln "[Scheduler] Backtrace:";
        eprintln (Exception.raw_backtrace_to_string backtrace);
        Process.mark_as_exited proc (Error exn);
        handle_exit_proc t proc (Error exn)
    | _ when Process.is_waiting proc || Process.is_waiting_io proc ->
        if pending && Process.is_alive proc then
          enqueue_owned_process t slot
    | _ ->
        if Process.is_alive proc then
          (
            Process.mark_as_runnable proc;
            enqueue_owned_process t slot
          )
  with
  | exn ->
      ctx.current_process <- None;
      clear_slot_executing slot;
      raise exn

let step_process = fun t ctx slot ->
  let proc = slot_process slot in
  match Process.state proc with
  | Finalized ->
      ()
  | Exited reason ->
      handle_exit_proc t proc reason
  | Running ->
      mark_slot_pending slot
  | _ -> (
      match Process.take_exit_request proc with
      | Some reason when Process.is_alive proc ->
          Process.mark_as_exited proc reason;
          handle_exit_proc t proc reason
      | Some _ ->
          ()
      | None -> (
          match Process.state proc with
          | Uninitialized ->
              if try_mark_slot_executing slot then
                (
                  Process.init proc;
                  handle_run_proc t ctx slot
                )
              else
                mark_slot_pending slot
          | Waiting_message ->
              if Process.has_messages proc && Process.try_mark_runnable_from_waiting_message proc then
                if try_mark_slot_executing slot then
                  handle_run_proc t ctx slot
                else
                  mark_slot_pending slot
          | Waiting_io _ ->
              ()
          | Runnable ->
              if try_mark_slot_executing slot then
                handle_run_proc t ctx slot
              else
                mark_slot_pending slot
          | Finalized ->
              ()
          | Exited reason ->
              handle_exit_proc t proc reason
          | Running ->
              mark_slot_pending slot
        )
    )

let spawn_blocked = fun t fn ->
  let proc = Process.make fn in
  let slot = create_process_slot proc ~owner_worker:Scheduler_id.zero ~placement:Blocking in
  let lane = { lock = Mutex.create (); cond = Condition.create (); domain = None } in
  let ctx = { scheduler = t; worker_id = None; current_process = None } in
  let rec blocking_loop () =
    Domain.DLS.set current_context (Some ctx);
    step_process t ctx slot;
    let rec loop () =
      if Atomic.get t.stop || not (Process.is_alive proc) then
        ()
      else
        match wait_for_blocking_work t slot with
        | None -> ()
        | Some slot ->
            step_process t ctx slot;
            loop ()
    in
    loop ();
    ctx.current_process <- None
  in
  set_slot_blocking_lane slot lane;
  add_process_slot t slot;
  let domain = Domain.spawn blocking_loop in
  lane.domain <- Some domain;
  add_blocking_lane t lane;
  slot_pid slot

let pop_local = fun (worker: worker) ->
  Mutex.lock worker.lock;
  let slot = Queue.pop worker.queue in
  Mutex.unlock worker.lock;
  match slot with
  | None -> None
  | Some slot ->
      mark_slot_dequeued slot;
      Some slot

let wait_for_local_work = fun t (worker: worker) ->
  Mutex.lock worker.lock;
  while Queue.is_empty worker.queue && not (Atomic.get t.stop) do
    Condition.wait worker.cond worker.lock
  done;
  let slot =
    if Atomic.get t.stop then
      None
    else
      Queue.pop worker.queue
  in
  Mutex.unlock worker.lock;
  match slot with
  | None -> None
  | Some slot ->
      mark_slot_dequeued slot;
      Some slot

let steal_batch = fun (victim: worker) ->
  Mutex.lock victim.lock;
  let available = Queue.len victim.queue in
  let steal_goal = min 32 (available / 2) in
  let rec scan remaining wanted stolen kept =
    if remaining = 0 then
      (List.rev stolen, List.rev kept)
    else
      match Queue.pop victim.queue with
      | None -> (List.rev stolen, List.rev kept)
      | Some slot -> (
          match slot_placement slot with
          | Normal when wanted > 0 -> scan (remaining - 1) (wanted - 1) (slot :: stolen) kept
          | _ -> scan (remaining - 1) wanted stolen (slot :: kept)
        )
  in
  let batch, kept = scan available steal_goal [] [] in
  List.iter (Queue.push victim.queue) kept;
  Mutex.unlock victim.lock;
  batch

let push_batch = fun (worker: worker) batch ->
  if not (List.is_empty batch) then
    (
      Mutex.lock worker.lock;
      List.iter (Queue.push worker.queue) batch;
      Condition.signal worker.cond;
      Mutex.unlock worker.lock
    )

let attempt_steal = fun t (worker: worker) ->
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
          true
        )
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

let reactor_poll_timeout_nanos = fun t ->
  let configured = Config.resolution_to_nanos t.config.timer_resolution in
  let max_timeout = 1_000_000L in
  if Int64.compare configured 0L <= 0 then
    max_timeout
  else if Int64.compare configured max_timeout < 0 then
    configured
  else
    max_timeout

let deregister_io_in_reactor = fun t source ~context ->
  match Async.Poll.deregister t.io_poll source with
  | Ok () -> ()
  | Error err -> eprintln
    ("[Scheduler] WARN: Failed to deregister I/O " ^ context ^ ": " ^ IO.error_message err)

let process_timers = fun t ->
  if Timer_wheel.size t.timer_wheel = 0 then
    ()
  else
    let now = Time.monotonic_time_nanos () in
    let expired = Timer_wheel.tick t.timer_wheel ~now in
    List.iter
      (fun timer ->
        let timer_id = timer.Timer.id in
        (
          match timer.Timer.action with
          | Timer.Wake_process proc ->
              if Process.has_receive_timeout_id proc timer_id then
                Process.mark_receive_timeout_fired proc;
              if Process.has_syscall_timeout_id proc timer_id then
                (
                  Process.mark_syscall_timeout_fired proc;
                  match Process.state proc with
                  | Waiting_io { source; _ } ->
                      (* Syscall timeout resumes the waiting process with
                     [Syscall_timeout]. The wait registration must be removed
                     first so a subsequent syscall can reregister cleanly. *)
                      deregister_io_in_reactor
                        t
                        source
                        ~context:("for timed out process " ^ Pid.to_string (Process.pid proc))
                  | _ -> ()
                );
              if Process.is_alive proc then
                (
                  match get_process_slot t (Process.pid proc) with
                  | None -> ()
                  | Some slot -> wake_process t slot
                )
          | Timer.Send_message (target_pid, msg) -> send_internal t target_pid msg
        );
        match timer.mode with
        | Timer.One_shot -> ()
        | Timer.Interval _ -> Timer_wheel.reschedule_timer t.timer_wheel ~now timer)
      expired

let handle_reactor_command = fun t cmd ->
  match cmd with
  | Add_timer {
    now;
    duration_nanos;
    mode;
    action;
    reply
  } ->
      let timer_id = Timer_wheel.add_timer t.timer_wheel ~now ~duration_nanos ~mode ~action in
      resolve_response reply timer_id
  | Cancel_timer timer_id ->
      Timer_wheel.cancel_timer t.timer_wheel timer_id
  | Register_io { token; interest; source; reply } ->
      resolve_response reply (Async.Poll.register t.io_poll token interest source)
  | Deregister_io source -> (
      match Async.Poll.deregister t.io_poll source with
      | Ok () -> ()
      | Error err -> eprintln
        ("[Scheduler] WARN: Failed to deregister I/O source: " ^ IO.error_message err)
    )

let poll_io = fun t ->
  let timeout_nanos = reactor_poll_timeout_nanos t in
  let events =
    match Async.Poll.poll t.io_poll ~timeout:timeout_nanos with
    | Ok events -> events
    | Error err ->
        eprintln ("[Scheduler] ERROR: Failed to poll I/O: " ^ IO.error_message err);
        []
  in
  List.iter
    (fun event ->
      let token = Async.Event.token event in
      let proc: Process.t = Async.Token.unsafe_to_value token in
      match get_process_slot t (Process.pid proc) with
      | None -> ()
      | Some slot -> (
          match Process.state proc with
          | Waiting_io { source; _ } ->
              deregister_io_in_reactor
                t
                source
                ~context:("for process " ^ Pid.to_string (Process.pid proc));
              if Process.is_alive proc then
                (
                  Process.add_ready_token proc token source;
                  wake_process t slot
                )
          | _ -> ()
        ))
    events

module Reactor = struct
  let loop = fun scheduler ->
    Scheduler_reactor.loop
      ~has_pending_commands:has_pending_reactor_commands
      ~drain_commands:drain_reactor_commands
      ~handle_command:handle_reactor_command
      ~process_timers
      ~poll_io
      scheduler
end

module Worker = struct
  let loop = fun scheduler worker ->
    Scheduler_worker.loop ~pop_local ~step_process ~attempt_steal ~wait_for_local_work scheduler worker
end

let join_blocking_lanes = fun t ->
  let lanes =
    with_blocking_lanes_lock t (fun () -> t.blocking_lanes)
  in
  List.iter
    (fun (lane: blocking_lane) ->
      match lane.domain with
      | None -> ()
      | Some domain -> Domain.join domain)
    lanes

let runtime_deps: Scheduler_runtime.deps = {
  ensure_can_run_once;
  create;
  spawn_on_worker;
  worker_loop = Worker.loop;
  reactor_loop = Reactor.loop;
  join_blocking_lanes;
}

let run = fun ~config ~main -> Scheduler_runtime.run runtime_deps ~config ~main

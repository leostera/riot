module Runtime_process = Process
module Runtime_pid = Pid
module Runtime_scheduler_id = Scheduler_id
module Runtime_timer = Timer
module Cell = Sync.Cell
open Kernel
open Collections
open Sync
open Cell
open Scheduler_types

let panic = Kernel.SystemError.panic

(* Scheduler diagnostics cannot depend on the actor-backed logger or suspend
   the process whose failure they are reporting. Preserve stdout for payloads. *)
let eprintln = fun message ->
  let buffer = Kernel.Bytes.from_string (message ^ "\n") in
  let length = Kernel.Bytes.length buffer in
  let rec write pos =
    if pos < length then
      match Kernel.IO.Stderr.write buffer ~pos ~len:(length - pos) with
      | Ok count when count > 0 -> write (pos + count)
      | Ok _
      | Error _ -> ()
  in
  write 0

type process_slot = Scheduler_types.process_slot

type worker = Scheduler_types.worker

type 'a response = 'a Scheduler_types.response

type reactor_command = Scheduler_types.reactor_command

type process_shard = Scheduler_types.process_shard

type process_registry = Scheduler_types.process_registry

type runtime_counters = Scheduler_types.runtime_counters

type t = Scheduler_types.t

type domain_context = Scheduler_types.domain_context

type io_registration_error = Scheduler_types.io_registration_error

let io_registration_error_message = function
  | Closed -> "Closed"
  | Async error -> Async.error_to_string error

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

let increment = fun counter ->
  let _ = Atomic.fetch_and_add counter 1 in
  ()

let monotonic_time_nanos = fun () ->
  match Kernel.Time.Monotonic.now () with
  | Ok time ->
      let secs, nanos = Kernel.Time.Monotonic.to_parts time in
      Int64.add (Int64.mul (Int64.from_int secs) 1_000_000_000L) (Int64.from_int nanos)
  | Error err -> panic (Kernel.Time.Monotonic.error_to_string err)

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
      ~count:shard_count
      ~fn:(fun _ -> { lock = Mutex.create (); processes = HashMap.with_capacity ~size:64 })
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

let slot_pid = fun slot -> Runtime_process.pid slot.process

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
  let idx = Runtime_pid.to_int pid mod Array.length registry.shards in
  Array.get_unchecked registry.shards ~at:idx

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
        Array.init
          ~count:worker_count
          ~fn:(fun index -> create_worker (Runtime_scheduler_id.of_int index))
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
      eprintln
        ("[Scheduler] ERROR: Failed to create Async.Poll: "
        ^ Async.error_to_string err);
      panic "Failed to create I/O polling system"

let get_context = fun () ->
  match Thread.DLS.get current_context with
  | None -> panic "No scheduler running"
  | Some ctx -> ctx

let get_scheduler = fun () -> (get_context ()).scheduler

let self = fun () ->
  let ctx = get_context () in
  match ctx.current_process with
  | None -> panic "No process running"
  | Some proc -> Runtime_process.pid proc

let worker_count = fun t -> Array.length t.workers

let worker_by_id = fun t worker_id ->
  Array.get_unchecked t.workers ~at:(Runtime_scheduler_id.to_int worker_id)

let is_valid_worker_id = fun t worker_id ->
  let idx = Runtime_scheduler_id.to_int worker_id in
  idx >= 0 && idx < worker_count t

let pick_spawn_worker = fun t ->
  let total = worker_count t in
  if total = 1 then
    Runtime_scheduler_id.zero
  else
    Runtime_scheduler_id.of_int (Atomic.get t.processes.size mod total)

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
  Array.for_each t.workers
    ~fn:(fun (worker: worker) ->
      Mutex.lock worker.lock;
      Condition.broadcast worker.cond;
      Mutex.unlock worker.lock);
  Mutex.lock t.blocking_lanes_lock;
  List.for_each t.blocking_lanes
    ~fn:(fun (lane: blocking_lane) ->
      Mutex.lock lane.lock;
      Condition.broadcast lane.cond;
      Mutex.unlock lane.lock);
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
        Queue.push worker.queue ~value:slot;
        Condition.signal worker.cond;
        Mutex.unlock worker.lock
      )
    else if Runtime_process.is_runnable (slot_process slot) then
      (
        increment t.counters.duplicate_enqueue_races;
        trace
          (Kernel.String.concat
            ""
            [ "duplicate enqueue prevented pid="; Runtime_pid.to_string (slot_pid slot) ])
      )

let enqueue_on_blocking_lane = fun t slot ->
  if try_mark_slot_queued slot then
    match slot.blocking_lane with
    | None -> panic "blocking slot is missing its lane"
    | Some lane ->
        Mutex.lock lane.lock;
        Condition.signal lane.cond;
        Mutex.unlock lane.lock
  else if Runtime_process.is_runnable (slot_process slot) then
    (
      increment t.counters.duplicate_enqueue_races;
      trace
        (Kernel.String.concat
          ""
          [ "duplicate enqueue prevented pid="; Runtime_pid.to_string (slot_pid slot) ])
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
          Runtime_scheduler_id.zero
      in
      enqueue_on_worker t worker_id slot

let current_worker_id_opt = fun () ->
  match Thread.DLS.get current_context with
  | Some { worker_id; _ } -> worker_id
  | None -> None

let wake_process = fun t slot ->
  let proc = slot_process slot in
  if Runtime_process.try_set_runnable_if_waiting proc then
    enqueue_owned_process t slot
  else if Runtime_process.is_runnable proc then
    enqueue_owned_process t slot

let wake_process_from_message = fun t slot ->
  let proc = slot_process slot in
  let owner = slot_owner_worker slot in
  let remote_wakeup =
    match current_worker_id_opt () with
    | Some worker_id -> not (Runtime_scheduler_id.equal worker_id owner)
    | None -> true
  in
  if Runtime_process.try_mark_runnable_from_waiting_message proc then
    (
      if remote_wakeup then
        increment t.counters.remote_wakeups;
      enqueue_owned_process t slot
    )
  else if Runtime_process.is_runnable proc then
    (
      if remote_wakeup then
        increment t.counters.remote_wakeups;
      enqueue_owned_process t slot
    )

let get_process_slot = fun t pid ->
  with_process_shard t.processes pid (fun shard -> HashMap.get shard.processes ~key:pid)

let get_process = fun t pid ->
  match get_process_slot t pid with
  | None -> None
  | Some slot -> Some (slot_process slot)

let add_process_slot = fun t slot ->
  let pid = slot_pid slot in
  with_process_shard t.processes pid
    (fun shard ->
      let replaced = HashMap.insert shard.processes ~key:pid ~value:slot in
      if Option.is_none replaced then
        (
          let _ = Atomic.fetch_and_add t.processes.size 1 in
          ()
        ))

let remove_process_slot = fun t pid ->
  with_process_shard t.processes pid
    (fun shard ->
      let removed = HashMap.remove shard.processes ~key:pid in
      if Option.is_some removed then
        (
          let _ = Atomic.fetch_and_add t.processes.size (-1) in
          ()
        ))

let process_count = fun t -> Atomic.get t.processes.size

let maybe_shutdown_if_empty = fun t ->
  if process_count t = 0 then
    request_shutdown t ~status:(Atomic.get t.status)

let push_reactor_command = fun t cmd ->
  with_reactor_commands t (fun () -> Queue.push t.reactor_commands ~value:cmd)

let drain_reactor_commands = fun t ->
  with_reactor_commands t
    (fun () ->
      let rec drain acc =
        match Queue.pop t.reactor_commands with
        | None -> List.reverse acc
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
    Error Closed
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
      Runtime_process.send_message proc msg;
      wake_process_from_message t slot

let send = fun pid msg -> send_internal (get_scheduler ()) pid msg

let kill = fun t pid reason ->
  match get_process_slot t pid with
  | None -> ()
  | Some slot ->
      let proc = slot_process slot in
      Runtime_process.request_exit proc (Error reason);
      mark_slot_pending slot;
      wake_process t slot

let spawn_on_worker_with_placement = fun t ~worker_id ~placement fn ->
  let proc = Runtime_process.make fn in
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
      while (not (Atomic.get t.stop))
      && Runtime_process.is_alive proc
      && not (Atomic.get slot.queued) do
        Condition.wait lane.cond lane.lock
      done;
      Mutex.unlock lane.lock;
      if Atomic.get t.stop || not (Runtime_process.is_alive proc) then
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
  match Runtime_process.receive_timeout proc with
  | None -> ()
  | Some timer_id ->
      Runtime_process.clear_receive_timeout proc;
      cancel_timer t timer_id

let clear_syscall_timeout = fun t proc ->
  match Runtime_process.syscall_timeout proc with
  | None -> ()
  | Some timer_id ->
      Runtime_process.clear_syscall_timeout proc;
      cancel_timer t timer_id

let install_receive_timeout = fun t proc secs ->
  match Runtime_process.receive_timeout proc with
  | Some _ -> ()
  | None ->
      let now = monotonic_time_nanos () in
      let duration_nanos = Int64.from_float (secs *. 1_000_000_000.0) in
      let timer_id = add_timer
        t
        ~now
        ~duration_nanos
        ~mode:Runtime_timer.One_shot
        ~action:(Runtime_timer.Wake_process proc) in
      Runtime_process.set_receive_timeout proc timer_id

let install_syscall_timeout = fun t proc secs ->
  match Runtime_process.syscall_timeout proc with
  | Some _ -> ()
  | None ->
      let now = monotonic_time_nanos () in
      let duration_nanos = Int64.from_float (secs *. 1_000_000_000.0) in
      let timer_id = add_timer
        t
        ~now
        ~duration_nanos
        ~mode:Runtime_timer.One_shot
        ~action:(Runtime_timer.Wake_process proc) in
      Runtime_process.set_syscall_timeout proc timer_id

let handle_receive = fun k t proc ~selector ~timeout ->
  let open Proc_state in
    let timeout_expired =
      match Runtime_process.receive_timeout proc with
      | None -> false
      | Some _ -> Runtime_process.take_receive_timeout_fired proc
    in
    let park_for_receive () =
      (
        match timeout with
        | `infinity -> ()
        | `after secs -> install_receive_timeout t proc secs
      );
      if Runtime_process.try_mark_awaiting_message proc then
        if Runtime_process.has_empty_mailbox proc then
          k Suspend
        else
          (
            let _ = Runtime_process.try_mark_runnable_from_waiting_message proc in
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
        scan_mailbox (Runtime_process.mailbox_count proc)
      else
        match Runtime_process.next_saved_message proc with
        | None -> scan_mailbox (Runtime_process.mailbox_count proc)
        | Some msg -> (
            match selector Message.(msg.msg) with
            | `select selected -> Some selected
            | `skip ->
                Runtime_process.add_to_save_queue proc msg;
                scan_saved (remaining - 1)
          )
    and scan_mailbox remaining =
      if remaining = 0 then
        None
      else
        match Runtime_process.next_mailbox_message proc with
        | None -> None
        | Some msg -> (
            match selector Message.(msg.msg) with
            | `select selected -> Some selected
            | `skip ->
                Runtime_process.add_to_save_queue proc msg;
                scan_mailbox (remaining - 1)
          )
    in
    if timeout_expired then
      timeout_receive ()
    else
      let selected =
        if Runtime_process.has_empty_mailbox proc then
          None
        else
          scan_saved (Runtime_process.save_queue_count proc)
      in
      match selected with
      | Some selected -> continue_with selected
      | None -> park_for_receive ()

let handle_syscall = fun k t proc name interest source timeout ->
  let open Proc_state in
    let timeout_state =
      match Runtime_process.syscall_timeout proc with
      | None -> `none
      | Some timer_id ->
          if Runtime_process.take_syscall_timeout_fired proc then
            (
              Runtime_process.clear_syscall_timeout proc;
              cancel_timer t timer_id;
              `fired
            )
          else
            `armed
    in
    match Runtime_process.get_ready_token proc with
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
            Runtime_process.mark_as_awaiting_io proc ~name token source;
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
                    ^ Runtime_pid.to_string (Runtime_process.pid proc)
                    ^ ": "
                    ^ io_registration_error_message err);
                  Runtime_process.mark_as_runnable proc;
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
  let pid = Runtime_process.pid proc in
  with_relations_lock t
    (fun () ->
      List.for_each (Runtime_process.get_monitored_by proc)
        ~fn:(fun (monitor_pid, monitor_ref) ->
          match get_process_slot t monitor_pid with
          | None -> ()
          | Some monitor_slot ->
              let monitor_proc = slot_process monitor_slot in
              Runtime_process.demonitor monitor_proc monitor_ref;
              Runtime_process.send_message
                monitor_proc
                (Runtime_process.Messages.DOWN { ref = monitor_ref; pid; reason });
              wake_process t monitor_slot);
      List.for_each (Runtime_process.get_monitors proc)
        ~fn:(fun (monitor_ref, monitored_pid) ->
          match get_process_slot t monitored_pid with
          | None -> ()
          | Some monitored_slot ->
              let monitored_proc = slot_process monitored_slot in
              Runtime_process.remove_monitored_by monitored_proc pid monitor_ref);
      List.for_each (Runtime_process.get_links proc)
        ~fn:(fun linked_pid ->
          match get_process_slot t linked_pid with
          | None -> ()
          | Some linked_slot ->
              let linked_proc = slot_process linked_slot in
              Runtime_process.unlink linked_proc pid;
              if Runtime_process.get_trap_exit linked_proc then
                (
                  Runtime_process.send_message
                    linked_proc
                    (Runtime_process.Messages.EXIT { from = pid; reason });
                  wake_process t linked_slot
                )
              else
                match reason with
                | Ok () -> ()
                | Error exn ->
                    Runtime_process.mark_as_exited linked_proc (Error exn);
                    enqueue_owned_process t linked_slot));
  (
    match Runtime_process.state proc with
    | Waiting_io { source; _ } ->
        deregister_io t source;
        clear_syscall_timeout t proc
    | _ -> ()
  );
  clear_receive_timeout t proc;
  remove_process_slot t pid;
  Runtime_process.mark_as_finalized proc;
  if Runtime_process.is_main proc then
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
  let pid_str = Runtime_pid.to_string (Runtime_process.pid proc) in
  Runtime_process.mark_as_running proc;
  ctx.current_process <- Some proc;
  try
    let next =
      try
        match
          Proc_state.run
            ~consume_reduction:(fun () ->
              match Runtime_process.use_reduction proc with
              | Runtime_process.Continue -> false
              | Runtime_process.Yield -> true)
            ~perform:(perform t proc)
            (Runtime_process.cont proc)
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
            ^ Kernel.Exception.to_string exn);
          raise exn
    in
    Runtime_process.set_cont proc next;
    ctx.current_process <- None;
    clear_slot_executing slot;
    let pending = take_slot_pending slot in
    match next with
    | Proc_state.Finished (Ok reason) ->
        Runtime_process.mark_as_exited proc reason;
        handle_exit_proc t proc reason
    | Proc_state.Finished (Error { exn; backtrace }) ->
        eprintln
          ("[Scheduler] Process "
          ^ pid_str
          ^ " finished with exception: "
          ^ Kernel.Exception.to_string exn);
        eprintln "[Scheduler] Backtrace:";
        eprintln (Kernel.Exception.raw_backtrace_to_string backtrace);
        Runtime_process.mark_as_exited proc (Error exn);
        handle_exit_proc t proc (Error exn)
    | _ when Runtime_process.is_waiting proc || Runtime_process.is_waiting_io proc ->
        if pending && Runtime_process.is_alive proc then
          enqueue_owned_process t slot
    | _ ->
        if Runtime_process.is_alive proc then
          (
            Runtime_process.mark_as_runnable proc;
            enqueue_owned_process t slot
          )
  with
  | exn ->
      ctx.current_process <- None;
      clear_slot_executing slot;
      raise exn

let step_process = fun t ctx slot ->
  let proc = slot_process slot in
  match Runtime_process.state proc with
  | Finalized ->
      ()
  | Exited reason ->
      handle_exit_proc t proc reason
  | Running ->
      mark_slot_pending slot
  | _ -> (
      match Runtime_process.take_exit_request proc with
      | Some reason when Runtime_process.is_alive proc ->
          Runtime_process.mark_as_exited proc reason;
          handle_exit_proc t proc reason
      | Some _ ->
          ()
      | None -> (
          match Runtime_process.state proc with
          | Uninitialized ->
              if try_mark_slot_executing slot then
                (
                  Runtime_process.init proc;
                  handle_run_proc t ctx slot
                )
              else
                mark_slot_pending slot
          | Waiting_message ->
              if
                Runtime_process.has_messages proc
                && Runtime_process.try_mark_runnable_from_waiting_message proc
              then
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
  let proc = Runtime_process.make fn in
  let slot = create_process_slot proc ~owner_worker:Runtime_scheduler_id.zero ~placement:Blocking in
  let lane = { lock = Mutex.create (); cond = Condition.create (); domain = None } in
  let ctx = { scheduler = t; worker_id = None; current_process = None } in
  let rec blocking_loop () =
    Thread.DLS.set current_context (Some ctx);
    step_process t ctx slot;
    let rec loop () =
      if Atomic.get t.stop || not (Runtime_process.is_alive proc) then
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
  let domain = Thread.spawn blocking_loop in
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
  let available = Queue.length victim.queue in
  let steal_goal = min 32 (available / 2) in
  let rec scan remaining wanted stolen kept =
    if remaining = 0 then
      (List.reverse stolen, List.reverse kept)
    else
      match Queue.pop victim.queue with
      | None -> (List.reverse stolen, List.reverse kept)
      | Some slot -> (
          match slot_placement slot with
          | Normal when wanted > 0 -> scan (remaining - 1) (wanted - 1) (slot :: stolen) kept
          | _ -> scan (remaining - 1) wanted stolen (slot :: kept)
        )
  in
  let batch, kept = scan available steal_goal [] [] in
  List.for_each kept ~fn:(fun slot -> Queue.push victim.queue ~value:slot);
  Mutex.unlock victim.lock;
  batch

let push_batch = fun (worker: worker) batch ->
  if not (List.is_empty batch) then
    (
      Mutex.lock worker.lock;
      List.for_each batch ~fn:(fun slot -> Queue.push worker.queue ~value:slot);
      Condition.signal worker.cond;
      Mutex.unlock worker.lock
    )

let attempt_steal = fun t (worker: worker) ->
  let total = worker_count t in
  let self_idx = Runtime_scheduler_id.to_int worker.id in
  let rec scan start_offset seen =
    if seen >= total - 1 then
      false
    else
      let victim_idx = (self_idx + start_offset + seen) mod total in
      if Int.equal victim_idx self_idx then
        scan start_offset (seen + 1)
      else
        let victim = Array.get_unchecked t.workers ~at:victim_idx in
        let batch = steal_batch victim in
        if List.is_empty batch then
          scan start_offset (seen + 1)
        else (
          (* Ownership transfer happens before enqueuing locally so future
             remote wakeups route to the stealing worker. *)
          List.for_each batch ~fn:(fun slot -> set_slot_owner_worker slot worker.id);
          push_batch worker batch;
          true
        )
  in
  if total <= 1 then
    false
  else
    let start_offset = 1 in
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
    ("[Scheduler] WARN: Failed to deregister I/O "
    ^ context
    ^ ": "
    ^ Async.error_to_string err)

let process_timers = fun t ->
  if Timer_wheel.size t.timer_wheel = 0 then
    ()
  else
    let now = monotonic_time_nanos () in
    let expired = Timer_wheel.tick t.timer_wheel ~now in
    List.for_each expired
      ~fn:(fun timer ->
        let timer_id = timer.Runtime_timer.id in
        (
          match timer.Runtime_timer.action with
          | Runtime_timer.Wake_process proc ->
              if Runtime_process.has_receive_timeout_id proc timer_id then
                Runtime_process.mark_receive_timeout_fired proc;
              if Runtime_process.has_syscall_timeout_id proc timer_id then
                (
                  Runtime_process.mark_syscall_timeout_fired proc;
                  match Runtime_process.state proc with
                  | Waiting_io { source; _ } ->
                      (* Syscall timeout resumes the waiting process with
                     [Syscall_timeout]. The wait registration must be removed
                     first so a subsequent syscall can reregister cleanly. *)
                      deregister_io_in_reactor
                        t
                        source
                        ~context:("for timed out process "
                        ^ Runtime_pid.to_string (Runtime_process.pid proc))
                  | _ -> ()
                );
              if Runtime_process.is_alive proc then
                (
                  match get_process_slot t (Runtime_process.pid proc) with
                  | None -> ()
                  | Some slot -> wake_process t slot
                )
          | Runtime_timer.Send_message (target_pid, msg) -> send_internal t target_pid msg
        );
        match timer.mode with
        | Runtime_timer.One_shot -> ()
        | Runtime_timer.Interval _ -> Timer_wheel.reschedule_timer t.timer_wheel ~now timer)

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
      resolve_response reply
        (
          match Async.Poll.register t.io_poll token interest source with
          | Ok () -> Ok ()
          | Error err -> Error (Async err)
        )
  | Deregister_io source -> (
      match Async.Poll.deregister t.io_poll source with
      | Ok () -> ()
      | Error err -> eprintln
        ("[Scheduler] WARN: Failed to deregister I/O source: "
        ^ Async.error_to_string err)
    )

let poll_io = fun t ->
  let timeout_nanos = reactor_poll_timeout_nanos t in
  let events =
    match Async.Poll.poll t.io_poll ~timeout:timeout_nanos with
    | Ok events -> events
    | Error err ->
        eprintln
          ("[Scheduler] ERROR: Failed to poll I/O: "
          ^ Async.error_to_string err);
        []
  in
  List.for_each events
    ~fn:(fun event ->
      let token = Async.Event.token event in
      let proc: Runtime_process.t = Async.Token.unsafe_value token in
      match get_process_slot t (Runtime_process.pid proc) with
      | None -> ()
      | Some slot -> (
          match Runtime_process.state proc with
          | Waiting_io { source; _ } ->
              deregister_io_in_reactor
                t
                source
                ~context:("for process " ^ Runtime_pid.to_string (Runtime_process.pid proc));
              if Runtime_process.is_alive proc then
                (
                  Runtime_process.add_ready_token proc token source;
                  wake_process t slot
                )
          | _ -> ()
        ))

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
  List.for_each lanes
    ~fn:(fun (lane: blocking_lane) ->
      match lane.domain with
      | None -> ()
      | Some domain -> Thread.join domain)

let runtime_deps: Scheduler_runtime.deps = {
  ensure_can_run_once;
  create;
  spawn_on_worker;
  worker_loop = Worker.loop;
  reactor_loop = Reactor.loop;
  join_blocking_lanes;
}

let run = fun ~config ~main -> Scheduler_runtime.run runtime_deps ~config ~main

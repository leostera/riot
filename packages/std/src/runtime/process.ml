open Kernel

module Runtime_mutex = Kernel.Sync.Mutex
module Runtime_atomic = Kernel.Sync.Atomic

type exit_reason = exn

type monitor_ref =
  | Monitor_ref of int

type flag =
  | TrapExit of bool

type 'a atomic_ref = 'a Runtime_atomic.t

module Messages = struct
  type Message.t +=
    | EXIT of {
        from: Pid.t;
        reason: (unit, exit_reason) result;
      }
    | DOWN of {
        ref: monitor_ref;
        pid: Pid.t;
        reason: (unit, exit_reason) result;
      }
end

type state =
  | Uninitialized
  | Runnable
  | Waiting_message
  | Waiting_io of {
      name: string;
      token: Async.Token.t;
      source: Async.Source.t;
    }
  | Running
  | Exited of (unit, exit_reason) result
  | Finalized

type reduction_result =
  | Continue
  | Yield

let default_reduction_budget = 100

type t = {
  pid: Pid.t;
  state: state atomic_ref;
  exit_request: (unit, exit_reason) result option atomic_ref;
  receive_timeout_fired: bool atomic_ref;
  syscall_timeout_fired: bool atomic_ref;
  lock: Runtime_mutex.t;
  mutable cont: (unit, exit_reason) result Proc_state.t option;
  mutable fn: (unit -> (unit, exit_reason) result) option;
  mailbox: Mailbox.t;
  (* Owner-local FIFO for selective-receive skips. Only the owning worker
     mutates this queue while mailbox sends remain cross-domain MPSC.
  *)
  save_queue: Message.envelope Queue.t;
  mutable ready_tokens: (Async.Token.t * Async.Source.t) list;
  mutable receive_timeout: Timer_id.t option;
  mutable syscall_timeout: Timer_id.t option;
  (* Process links and monitors *)
  mutable links: Pid.t list;
  mutable monitors: (monitor_ref * Pid.t) list;
  mutable monitored_by: (Pid.t * monitor_ref) list;
  trap_exit: bool atomic_ref;
  mutable reductions_remaining: int;
}

let monitor_ref_counter = Runtime_atomic.make 0

let make_monitor_ref = fun () ->
  let rec next_id () =
    let current = Runtime_atomic.get monitor_ref_counter in
    let next = current + 1 in
    if Runtime_atomic.compare_and_set monitor_ref_counter current next then
      Monitor_ref current
    else
      next_id ()
  in
  next_id ()

let make = fun fn ->
  let pid = Pid.next () in
  {
    pid;
    cont = None;
    fn = Some fn;
    state = Runtime_atomic.make Uninitialized;
    exit_request = Runtime_atomic.make None;
    receive_timeout_fired = Runtime_atomic.make false;
    syscall_timeout_fired = Runtime_atomic.make false;
    lock = Runtime_mutex.create ();
    mailbox = Mailbox.create ();
    save_queue = Queue.create ();
    ready_tokens = [];
    receive_timeout = None;
    syscall_timeout = None;
    links = [];
    monitors = [];
    monitored_by = [];
    trap_exit = Runtime_atomic.make false;
    reductions_remaining = default_reduction_budget;
  }

let init = fun t ->
  let fn =
    match t.fn with
    | Some fn -> fn
    | None -> Kernel.SystemError.panic "process init requires an entry function"
  in
  t.cont <- Some (Proc_state.make fn Proc_effect.Yield);
  t.fn <- None;
  t.reductions_remaining <- default_reduction_budget;
  Runtime_atomic.set t.state Runnable

let reset_reductions = fun t remaining ->
  t.reductions_remaining <- if remaining <= 0 then
    1
  else
    remaining

let use_reduction = fun t ->
  let remaining = t.reductions_remaining - 1 in
  if remaining > 0 then (
    t.reductions_remaining <- remaining;
    Continue
  ) else (
    t.reductions_remaining <- default_reduction_budget;
    Yield
  )

let with_lock = fun t f ->
  Runtime_mutex.lock t.lock;
  try
    let result = f () in
    Runtime_mutex.unlock t.lock;
    result
  with
  | exn ->
      Runtime_mutex.unlock t.lock;
      raise exn

let pid = fun t -> t.pid

let state = fun t -> Runtime_atomic.get t.state

let is_alive = fun t ->
  match state t with
  | Finalized
  | Exited _ -> false
  | _ -> true

let is_exited = fun t ->
  match state t with
  | Finalized
  | Exited _ -> true
  | _ -> false

let is_waiting = fun t ->
  match state t with
  | Waiting_message -> true
  | _ -> false

let is_waiting_io = fun t ->
  match state t with
  | Waiting_io _ -> true
  | _ -> false

let is_runnable = fun t -> state t = Runnable

let is_running = fun t -> state t = Running

let is_main = fun t -> Pid.equal t.pid Pid.main

let try_set_runnable_if_waiting = fun t ->
  let current = state t in
  match current with
  | Waiting_message
  | Waiting_io _ -> Runtime_atomic.compare_and_set t.state current Runnable
  | _ -> false

let try_mark_awaiting_message = fun t ->
  Runtime_atomic.compare_and_set
    t.state
    Running
    Waiting_message

let try_mark_runnable_from_waiting_message = fun t ->
  Runtime_atomic.compare_and_set
    t.state
    Waiting_message
    Runnable

let has_empty_mailbox = fun t -> Queue.is_empty t.save_queue && Mailbox.is_empty t.mailbox

let has_messages = fun t -> not (has_empty_mailbox t)

let message_count = fun t -> Mailbox.size t.mailbox + Queue.length t.save_queue

let mailbox_count = fun t -> Mailbox.size t.mailbox

let save_queue_count = fun t -> Queue.length t.save_queue

let mark_as_running = fun t ->
  if is_alive t then
    Runtime_atomic.set t.state Running

let mark_as_runnable = fun t ->
  if is_alive t then
    Runtime_atomic.set t.state Runnable

let mark_as_awaiting_message = fun t ->
  if is_alive t then
    Runtime_atomic.set t.state Waiting_message

let mark_as_exited = fun t reason ->
  if not (is_exited t) then
    Runtime_atomic.set t.state (Exited reason)

let request_exit = fun t reason ->
  if is_alive t then
    Runtime_atomic.set t.exit_request (Some reason)

let take_exit_request = fun t -> Runtime_atomic.exchange t.exit_request None

let mark_as_finalized = fun t -> Runtime_atomic.set t.state Finalized

let cont = fun t ->
  match t.cont with
  | Some cont -> cont
  | None -> Kernel.SystemError.panic "process continuation is not initialized"

let set_cont = fun t c -> t.cont <- Some c

let pop_save_queue = fun t -> Queue.pop t.save_queue

let next_message = fun t ->
  with_lock
    t
    (fun () ->
      match pop_save_queue t with
      | Some msg -> Some msg
      | None -> Mailbox.next t.mailbox)

let next_saved_message = fun t -> with_lock t (fun () -> pop_save_queue t)

let next_mailbox_message = fun t -> Mailbox.next t.mailbox

let add_to_save_queue = fun t msg -> with_lock t (fun () -> Queue.push t.save_queue ~value:msg)

let send_message = fun t msg ->
  if is_alive t then
    let envelope = Message.envelope msg in
    Mailbox.queue t.mailbox envelope

(* I/O operations *)

let mark_as_awaiting_io = fun t ~name token source ->
  if is_alive t then
    Runtime_atomic.set t.state (Waiting_io { name; token; source })

let add_ready_token = fun t token source ->
  with_lock
    t
    (fun () -> t.ready_tokens <- (token, source) :: t.ready_tokens)

let get_ready_token = fun t ->
  with_lock
    t
    (fun () ->
      match t.ready_tokens with
      | [] -> None
      | token :: rest ->
          t.ready_tokens <- rest;
          Some token)

let consume_ready_tokens = fun t f ->
  with_lock
    t
    (fun () ->
      List.for_each t.ready_tokens ~fn:f;
      t.ready_tokens <- [])

let has_no_ready_tokens = fun t -> with_lock t (fun () -> List.is_empty t.ready_tokens)

(* Timer timeout management *)

let set_receive_timeout = fun t timer_id ->
  Runtime_atomic.set t.receive_timeout_fired false;
  with_lock t (fun () -> t.receive_timeout <- Some timer_id)

let clear_receive_timeout = fun t ->
  Runtime_atomic.set t.receive_timeout_fired false;
  with_lock t (fun () -> t.receive_timeout <- None)

let receive_timeout = fun t -> with_lock t (fun () -> t.receive_timeout)

let set_syscall_timeout = fun t timer_id ->
  Runtime_atomic.set t.syscall_timeout_fired false;
  with_lock t (fun () -> t.syscall_timeout <- Some timer_id)

let clear_syscall_timeout = fun t ->
  Runtime_atomic.set t.syscall_timeout_fired false;
  with_lock t (fun () -> t.syscall_timeout <- None)

let syscall_timeout = fun t -> with_lock t (fun () -> t.syscall_timeout)

let has_receive_timeout_id = fun t timer_id ->
  with_lock
    t
    (fun () ->
      match t.receive_timeout with
      | Some current -> Timer_id.equal current timer_id
      | None -> false)

let has_syscall_timeout_id = fun t timer_id ->
  with_lock
    t
    (fun () ->
      match t.syscall_timeout with
      | Some current -> Timer_id.equal current timer_id
      | None -> false)

let mark_receive_timeout_fired = fun t -> Runtime_atomic.set t.receive_timeout_fired true

let mark_syscall_timeout_fired = fun t -> Runtime_atomic.set t.syscall_timeout_fired true

let take_receive_timeout_fired = fun t -> Runtime_atomic.exchange t.receive_timeout_fired false

let receive_timeout_fired = fun t -> Runtime_atomic.get t.receive_timeout_fired

let take_syscall_timeout_fired = fun t -> Runtime_atomic.exchange t.syscall_timeout_fired false

(* Process links and monitors *)

let link = fun proc target_pid ->
  with_lock
    proc
    (fun () ->
      if not (List.contains proc.links ~value:target_pid) then
        proc.links <- target_pid :: proc.links)

let unlink = fun proc target_pid ->
  with_lock
    proc
    (fun () -> proc.links <- List.filter proc.links ~fn:(fun pid -> not (Pid.equal pid target_pid)))

let monitor = fun proc target_pid ->
  with_lock
    proc
    (fun () ->
      let ref = make_monitor_ref () in
      proc.monitors <- (ref, target_pid) :: proc.monitors;
      ref)

let demonitor = fun proc ref ->
  with_lock
    proc
    (fun () ->
      proc.monitors <- List.filter
        proc.monitors
        ~fn:(fun (r, _) ->
          match (ref, r) with
          | (Monitor_ref id1, Monitor_ref id2) -> not (Int.equal id1 id2)))

let monitored_pid_for_ref = fun proc ref ->
  with_lock
    proc
    (fun () ->
      let rec find = fun __tmp1 ->
        match __tmp1 with
        | [] -> None
        | (r, pid) :: rest -> (
            match (ref, r) with
            | (Monitor_ref id1, Monitor_ref id2) ->
                if Int.equal id1 id2 then
                  Some pid
                else
                  find rest
          )
      in
      find proc.monitors)

let set_flags = fun proc flags ->
  with_lock
    proc
    (fun () ->
      List.for_each
        flags
        ~fn:(fun flag ->
          match flag with
          | TrapExit value -> Runtime_atomic.set proc.trap_exit value))

let get_trap_exit = fun proc -> Runtime_atomic.get proc.trap_exit

let get_links = fun proc -> with_lock proc (fun () -> proc.links)

let get_monitors = fun proc -> with_lock proc (fun () -> proc.monitors)

let get_monitored_by = fun proc -> with_lock proc (fun () -> proc.monitored_by)

let add_monitored_by = fun proc monitor_pid ref ->
  with_lock
    proc
    (fun () -> proc.monitored_by <- (monitor_pid, ref) :: proc.monitored_by)

let remove_monitored_by = fun proc monitor_pid ref ->
  with_lock
    proc
    (fun () ->
      proc.monitored_by <- List.filter
        proc.monitored_by
        ~fn:(fun (pid, r) ->
          not
            (
              Pid.equal pid monitor_pid && match (ref, r) with
              | (Monitor_ref id1, Monitor_ref id2) -> id1 = id2
            )))

let is_linked = fun proc pid -> with_lock proc (fun () -> List.contains proc.links ~value:pid)

let is_monitoring = fun proc pid ->
  with_lock
    proc
    (fun () -> List.exists proc.monitors ~fn:(fun (_, monitored_pid) -> Pid.equal pid monitored_pid))

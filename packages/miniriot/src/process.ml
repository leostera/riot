open Kernel
open Kernel.Sync
open Kernel.Collections

type exit_reason = exn

type monitor_ref = Monitor_ref of int

type flag = TrapExit of bool

type 'a atomic_ref = 'a Sync.Atomic.t

module Messages = struct
  type Message.t +=
    | EXIT of { from : Pid.t; reason : (unit, exit_reason) result }
    | DOWN of {
        ref : monitor_ref;
        pid : Pid.t;
        reason : (unit, exit_reason) result;
      }
end

type state =
  | Uninitialized
  | Runnable
  | Waiting_message
  | Waiting_io of {
      name : string;
      token : Async.Token.t;
      source : Async.Source.t;
    }
  | Running
  | Exited of (unit, exit_reason) result
  | Finalized

type t = {
  pid : Pid.t;
  state : state atomic_ref;
  receive_timeout_fired : bool atomic_ref;
  syscall_timeout_fired : bool atomic_ref;
  lock : Mutex.t;
  mutable cont : (unit, exit_reason) result Proc_state.t option;
  mutable fn : (unit -> (unit, exit_reason) result) option;
  mailbox : Mailbox.t;
  save_queue : Mailbox.t;
  mutable ready_tokens : (Async.Token.t * Async.Source.t) list;
  mutable receive_timeout : Timer_id.t option;
  mutable syscall_timeout : Timer_id.t option;
  (* Process links and monitors *)
  mutable links : Pid.t list;
  mutable monitors : (monitor_ref * Pid.t) list;
  mutable monitored_by : (Pid.t * monitor_ref) list;
  trap_exit : bool atomic_ref;
}

let monitor_ref_counter = Sync.Atomic.make 0

let make_monitor_ref () =
  let rec next_id () =
    let current = Sync.Atomic.get monitor_ref_counter in
    let next = current + 1 in
    if Sync.Atomic.compare_and_set monitor_ref_counter current next then
      Monitor_ref current
    else next_id ()
  in
  next_id ()

let make fn =
  let pid = Pid.next () in
  {
    pid;
    cont = None;
    fn = Some fn;
    state = Sync.Atomic.make Uninitialized;
    receive_timeout_fired = Sync.Atomic.make false;
    syscall_timeout_fired = Sync.Atomic.make false;
    lock = Mutex.create ();
    mailbox = Mailbox.create ();
    save_queue = Mailbox.create ();
    ready_tokens = [];
    receive_timeout = None;
    syscall_timeout = None;
    links = [];
    monitors = [];
    monitored_by = [];
    trap_exit = Sync.Atomic.make false;
  }

let init t =
  let fn = Option.unwrap t.fn in
  t.cont <- Some (Proc_state.make fn Proc_effect.Yield);
  t.fn <- None;
  Sync.Atomic.set t.state Runnable

let with_lock t f =
  Mutex.lock t.lock;
  try
    let result = f () in
    Mutex.unlock t.lock;
    result
  with exn ->
    Mutex.unlock t.lock;
    raise exn

let pid t = t.pid

let state t = Sync.Atomic.get t.state
let is_alive t = match state t with Finalized | Exited _ -> false | _ -> true
let is_exited t = match state t with Finalized | Exited _ -> true | _ -> false
let is_waiting t = match state t with Waiting_message -> true | _ -> false
let is_waiting_io t = match state t with Waiting_io _ -> true | _ -> false
let is_runnable t = state t = Runnable
let is_running t = state t = Running
let is_main t = Pid.equal t.pid Pid.main

let try_set_runnable_if_waiting t =
  let current = state t in
  match current with
  | Waiting_message | Waiting_io _ ->
      Sync.Atomic.compare_and_set t.state current Runnable
  | _ -> false

let try_mark_awaiting_message t =
  Sync.Atomic.compare_and_set t.state Running Waiting_message

let try_mark_runnable_from_waiting_message t =
  Sync.Atomic.compare_and_set t.state Waiting_message Runnable

let has_empty_mailbox t =
  with_lock t (fun () ->
      Mailbox.is_empty t.save_queue && Mailbox.is_empty t.mailbox)

let has_messages t = not (has_empty_mailbox t)
let message_count t =
  with_lock t (fun () -> Mailbox.size t.mailbox + Mailbox.size t.save_queue)
let mailbox_count t = with_lock t (fun () -> Mailbox.size t.mailbox)
let save_queue_count t = with_lock t (fun () -> Mailbox.size t.save_queue)
let mark_as_running t = if is_alive t then Sync.Atomic.set t.state Running
let mark_as_runnable t = if is_alive t then Sync.Atomic.set t.state Runnable
let mark_as_awaiting_message t = if is_alive t then Sync.Atomic.set t.state Waiting_message
let mark_as_exited t reason = if not (is_exited t) then Sync.Atomic.set t.state (Exited reason)
let mark_as_finalized t = Sync.Atomic.set t.state Finalized
let cont t = Option.unwrap t.cont
let set_cont t c = t.cont <- Some c

let next_message t =
  with_lock t (fun () ->
      match Mailbox.next t.save_queue with
      | Some msg -> Some msg
      | None -> Mailbox.next t.mailbox)

let next_saved_message t =
  with_lock t (fun () -> Mailbox.next t.save_queue)

let next_mailbox_message t =
  with_lock t (fun () -> Mailbox.next t.mailbox)

let add_to_save_queue t msg =
  with_lock t (fun () -> Mailbox.queue t.save_queue msg)

let send_message t msg =
  if is_alive t then (
    with_lock t (fun () ->
        let envelope = Message.envelope msg in
        Mailbox.queue t.mailbox envelope))

(* I/O operations *)
let mark_as_awaiting_io t ~name token source =
  if is_alive t then Sync.Atomic.set t.state (Waiting_io { name; token; source })

let add_ready_token t token source =
  with_lock t (fun () -> t.ready_tokens <- (token, source) :: t.ready_tokens)

let get_ready_token t =
  with_lock t (fun () ->
      match t.ready_tokens with
      | [] -> None
      | token :: rest ->
          t.ready_tokens <- rest;
          Some token)

let consume_ready_tokens t f =
  with_lock t (fun () ->
      List.iter f t.ready_tokens;
      t.ready_tokens <- [])

let has_no_ready_tokens t = with_lock t (fun () -> List.is_empty t.ready_tokens)

(* Timer timeout management *)
let set_receive_timeout t timer_id =
  Sync.Atomic.set t.receive_timeout_fired false;
  with_lock t (fun () -> t.receive_timeout <- Some timer_id)

let clear_receive_timeout t =
  Sync.Atomic.set t.receive_timeout_fired false;
  with_lock t (fun () -> t.receive_timeout <- None)

let receive_timeout t =
  with_lock t (fun () -> t.receive_timeout)

let set_syscall_timeout t timer_id =
  Sync.Atomic.set t.syscall_timeout_fired false;
  with_lock t (fun () -> t.syscall_timeout <- Some timer_id)

let clear_syscall_timeout t =
  Sync.Atomic.set t.syscall_timeout_fired false;
  with_lock t (fun () -> t.syscall_timeout <- None)

let syscall_timeout t =
  with_lock t (fun () -> t.syscall_timeout)

let has_receive_timeout_id t timer_id =
  with_lock t (fun () ->
      match t.receive_timeout with
      | Some current -> Timer_id.equal current timer_id
      | None -> false)

let has_syscall_timeout_id t timer_id =
  with_lock t (fun () ->
      match t.syscall_timeout with
      | Some current -> Timer_id.equal current timer_id
      | None -> false)

let mark_receive_timeout_fired t = Sync.Atomic.set t.receive_timeout_fired true
let mark_syscall_timeout_fired t = Sync.Atomic.set t.syscall_timeout_fired true

let take_receive_timeout_fired t =
  Sync.Atomic.exchange t.receive_timeout_fired false

let take_syscall_timeout_fired t =
  Sync.Atomic.exchange t.syscall_timeout_fired false

(* Process links and monitors *)
let link proc target_pid =
  with_lock proc (fun () ->
      if not (List.mem target_pid proc.links) then
        proc.links <- target_pid :: proc.links)

let unlink proc target_pid =
  with_lock proc (fun () ->
      proc.links <-
        List.filter (fun pid -> not (Pid.equal pid target_pid)) proc.links)

let monitor proc target_pid =
  with_lock proc (fun () ->
      let ref = make_monitor_ref () in
      proc.monitors <- (ref, target_pid) :: proc.monitors;
      ref)

let demonitor proc ref =
  with_lock proc (fun () ->
      proc.monitors <-
        List.filter
          (fun (r, _) ->
            match (ref, r) with
            | Monitor_ref id1, Monitor_ref id2 -> not (Int.equal id1 id2))
          proc.monitors)

let monitored_pid_for_ref proc ref =
  with_lock proc (fun () ->
      let rec find = function
        | [] -> None
        | (r, pid) :: rest -> (
            match (ref, r) with
            | Monitor_ref id1, Monitor_ref id2 ->
                if Int.equal id1 id2 then Some pid else find rest)
      in
      find proc.monitors)

let set_flags proc flags =
  with_lock proc (fun () ->
      List.iter
        (fun flag ->
          match flag with
          | TrapExit value -> Sync.Atomic.set proc.trap_exit value)
        flags)

let get_trap_exit proc = Sync.Atomic.get proc.trap_exit
let get_links proc = with_lock proc (fun () -> proc.links)
let get_monitors proc = with_lock proc (fun () -> proc.monitors)
let get_monitored_by proc = with_lock proc (fun () -> proc.monitored_by)

let add_monitored_by proc monitor_pid ref =
  with_lock proc (fun () ->
      proc.monitored_by <- (monitor_pid, ref) :: proc.monitored_by)

let remove_monitored_by proc monitor_pid ref =
  with_lock proc (fun () ->
      proc.monitored_by <-
        List.filter
          (fun (pid, r) ->
            not
              (Pid.equal pid monitor_pid
              &&
              match (ref, r) with
              | Monitor_ref id1, Monitor_ref id2 -> id1 = id2))
          proc.monitored_by)

let is_linked proc pid = with_lock proc (fun () -> List.mem pid proc.links)

let is_monitoring proc pid =
  with_lock proc (fun () ->
      List.exists (fun (_, monitored_pid) -> Pid.equal pid monitored_pid) proc.monitors)

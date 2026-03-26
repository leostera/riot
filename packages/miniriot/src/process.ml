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
  mutable cont : (unit, exit_reason) result Proc_state.t option;
  mutable fn : (unit -> (unit, exit_reason) result) option;
  mailbox : Mailbox.t;
  save_queue : Mailbox.t;
  mutable read_save_queue : bool;
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
    mailbox = Mailbox.create ();
    save_queue = Mailbox.create ();
    read_save_queue = false;
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

let pid t = t.pid

let state t = Sync.Atomic.get t.state
let is_alive t = match state t with Finalized | Exited _ -> false | _ -> true
let is_exited t = match state t with Finalized | Exited _ -> true | _ -> false
let is_waiting t = match state t with Waiting_message -> true | _ -> false
let is_waiting_io t = match state t with Waiting_io _ -> true | _ -> false
let is_runnable t = state t = Runnable
let is_running t = state t = Running
let is_main t = Pid.equal t.pid Pid.main

let has_empty_mailbox t =
  Mailbox.is_empty t.save_queue && Mailbox.is_empty t.mailbox

let has_messages t = not (has_empty_mailbox t)
let message_count t = Mailbox.size t.mailbox + Mailbox.size t.save_queue
let mark_as_running t = if is_alive t then Sync.Atomic.set t.state Running
let mark_as_runnable t = if is_alive t then Sync.Atomic.set t.state Runnable
let mark_as_awaiting_message t = if is_alive t then Sync.Atomic.set t.state Waiting_message
let mark_as_exited t reason = if not (is_exited t) then Sync.Atomic.set t.state (Exited reason)
let mark_as_finalized t = Sync.Atomic.set t.state Finalized
let cont t = Option.unwrap t.cont
let set_cont t c = t.cont <- Some c

let next_message t =
  if t.read_save_queue then (
    match Mailbox.next t.save_queue with
    | Some m -> Some m
    | None ->
        t.read_save_queue <- false;
        None)
  else match Mailbox.next t.mailbox with Some m -> Some m | None -> None

let add_to_save_queue t msg = Mailbox.queue t.save_queue msg
let read_save_queue t = t.read_save_queue <- true

let send_message t msg =
  if is_alive t then (
    let envelope = Message.envelope msg in
    Mailbox.queue t.mailbox envelope;
    if is_waiting t then mark_as_runnable t)

(* I/O operations *)
let mark_as_awaiting_io t ~name token source =
  if is_alive t then Sync.Atomic.set t.state (Waiting_io { name; token; source })

let add_ready_token t token source =
  t.ready_tokens <- (token, source) :: t.ready_tokens

let get_ready_token t =
  match t.ready_tokens with
  | [] -> None
  | token :: rest ->
      t.ready_tokens <- rest;
      Some token

let consume_ready_tokens t f =
  List.iter f t.ready_tokens;
  t.ready_tokens <- []

let has_no_ready_tokens t = List.is_empty t.ready_tokens

(* Timer timeout management *)
let set_receive_timeout t timer_id = t.receive_timeout <- Some timer_id
let clear_receive_timeout t = t.receive_timeout <- None
let receive_timeout t = t.receive_timeout
let set_syscall_timeout t timer_id = t.syscall_timeout <- Some timer_id
let clear_syscall_timeout t = t.syscall_timeout <- None
let syscall_timeout t = t.syscall_timeout

(* Process links and monitors *)
let link proc target_pid =
  if not (List.mem target_pid proc.links) then
    proc.links <- target_pid :: proc.links

let unlink proc target_pid =
  proc.links <- List.filter (fun pid -> not (Pid.equal pid target_pid)) proc.links

let monitor proc target_pid =
  let ref = make_monitor_ref () in
  proc.monitors <- (ref, target_pid) :: proc.monitors;
  ref

let demonitor proc ref =
  proc.monitors <-
    List.filter
      (fun (r, _) ->
        match (ref, r) with 
        | Monitor_ref id1, Monitor_ref id2 -> id1 != id2)
      proc.monitors

let set_flags proc flags =
  List.iter
    (fun flag ->
      match flag with
      | TrapExit value -> Sync.Atomic.set proc.trap_exit value)
    flags

let get_trap_exit proc = Sync.Atomic.get proc.trap_exit
let get_links proc = proc.links
let get_monitors proc = proc.monitors
let get_monitored_by proc = proc.monitored_by

let add_monitored_by proc monitor_pid ref =
  proc.monitored_by <- (monitor_pid, ref) :: proc.monitored_by

let remove_monitored_by proc monitor_pid ref =
  proc.monitored_by <-
    List.filter
      (fun (pid, r) ->
        not
          (Pid.equal pid monitor_pid
          &&
          match (ref, r) with Monitor_ref id1, Monitor_ref id2 -> id1 = id2))
      proc.monitored_by

let is_linked proc pid = List.mem pid proc.links

let is_monitoring proc pid =
  List.exists (fun (_, monitored_pid) -> Pid.equal pid monitored_pid) proc.monitors

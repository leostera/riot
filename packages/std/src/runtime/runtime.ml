open Kernel
module Runtime = Reduction
module Pid = Pid
module Scheduler_id = Scheduler_id
module Message = Message
module Proc = Process
module Config = Config

module Actor = struct
  include Proc

  type exit_reason = Proc.exit_reason

  type flag = Proc.flag =
    TrapExit of bool

  include Proc.Messages

  module Monitor = struct
    type t = Proc.monitor_ref
  end

  let with_current_relations = fun f ->
    let t = Scheduler.get_scheduler () in
    let current = Scheduler.get_current_process () in
    Scheduler.with_relations_lock t (fun () -> f t current)

  let set_flags = fun flags ->
    let current = Scheduler.get_current_process () in
    Proc.set_flags current flags

  let link = fun pid ->
    with_current_relations
      (fun t current ->
        match Scheduler.get_process t pid with
        | None -> panic ("Cannot link to non-existent process " ^ Pid.to_string pid)
        | Some target ->
            Proc.link current pid;
            Proc.link target (Proc.pid current))

  let unlink = fun pid ->
    with_current_relations
      (fun t current ->
        match Scheduler.get_process t pid with
        | None -> ()
        | Some target ->
            Proc.unlink current pid;
            Proc.unlink target (Proc.pid current))

  let monitor = fun pid ->
    with_current_relations
      (fun t current ->
        match Scheduler.get_process t pid with
        | None -> panic ("Cannot monitor non-existent processs " ^ Pid.to_string pid)
        | Some target ->
            let ref = Proc.monitor current pid in
            Proc.add_monitored_by target (Proc.pid current) ref;
            ref)

  let demonitor = fun ref ->
    with_current_relations
      (fun t current ->
        let monitored_pid = Proc.monitored_pid_for_ref current ref in
        Proc.demonitor current ref;
        match monitored_pid with
        | None -> ()
        | Some pid -> (
            match Scheduler.get_process t pid with
            | None -> ()
            | Some target -> Proc.remove_monitored_by target (Proc.pid current) ref
          ))

  let kill = fun pid ~reason ->
    Scheduler.kill (Scheduler.get_scheduler ()) pid reason
end

module Process = Actor

let run = fun ~main ~args ?config () ->
  let config = Option.unwrap_or config ~default:Config.default in
  Kernel.Exception.record_backtrace true;
  Scheduler.run ~config ~main:(fun () -> main ~args) |> exit

let shutdown = fun ~status -> Scheduler.shutdown (Scheduler.get_scheduler ()) ~status

let spawn = fun fn ->
  Scheduler.spawn (Scheduler.get_scheduler ()) fn

let spawn_pinned = fun ?scheduler fn ->
  let scheduler = Option.map Scheduler_id.of_int scheduler in
  Scheduler.spawn_pinned ?worker_id:scheduler (Scheduler.get_scheduler ()) fn

let spawn_blocked = fun fn ->
  Scheduler.spawn_blocked (Scheduler.get_scheduler ()) fn

let spawn_link = fun fn ->
  let pid = spawn fn in
  Actor.link pid;
  pid

let self = fun () -> Scheduler.self ()

let current_scheduler_id = fun () -> Scheduler.current_worker_id_opt ()

let send = Scheduler.send

(* Cooperative I/O syscall for actor-aware I/O operations *)

let syscall = fun ~name ~interest ~source ~timeout ->
  Effect.perform (Proc_effect.Syscall { name; interest; source; timeout })

module Timer = struct
  type t = Timer_id.t

  type id = t

  let send_after = fun target_pid (msg: Message.t) ~after ->
    let sch = Scheduler.get_scheduler () in
    let now = Kernel.Time.monotonic_time_nanos () in
    let duration_nanos = Int64.of_float (after *. 1_000_000_000.0) in
    Scheduler.add_timer
      sch
      ~now
      ~duration_nanos
      ~mode:Timer.One_shot
      ~action:(Timer.Send_message (target_pid, msg))

  let send_interval = fun target_pid (msg: Message.t) ~interval ->
    let sch = Scheduler.get_scheduler () in
    let now = Kernel.Time.monotonic_time_nanos () in
    let duration_nanos = Int64.of_float (interval *. 1_000_000_000.0) in
    Scheduler.add_timer
      sch
      ~now
      ~duration_nanos
      ~mode:(Timer.Interval duration_nanos)
      ~action:(Timer.Send_message (target_pid, msg))

  let cancel = fun timer_id ->
    let sch = Scheduler.get_scheduler () in
    Scheduler.cancel_timer sch timer_id
end

include Effects

let yield = fun () ->
  let current = Scheduler.get_current_process () in
  match Actor.use_reduction current with
  | Actor.Continue -> ()
  | Actor.Yield -> Effects.yield ()

module Timer_id = Timer_id

let enable_trace = fun () -> Scheduler.enable_trace ()

let disable_trace = fun () -> Scheduler.disable_trace ()

type trace_counters = {
  steals: int;
  failed_steals: int;
  remote_wakeups: int;
  duplicate_enqueue_races: int;
}

let trace_counters = fun () ->
  let scheduler = Scheduler.get_scheduler () in
  let counters = Scheduler.trace_counters scheduler in
  {
    steals = counters.steals;
    failed_steals = counters.failed_steals;
    remote_wakeups = counters.remote_wakeups;
    duplicate_enqueue_races = counters.duplicate_enqueue_races
  }

let reset_trace_counters = fun () ->
  let scheduler = Scheduler.get_scheduler () in
  Scheduler.reset_trace_counters scheduler

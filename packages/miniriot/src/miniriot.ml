open Kernel

module Runtime = Runtime
module Pid = Pid
module Scheduler_id = Scheduler_id
module Message = Message
module Proc = Process
module Config = Config

module Process = struct
  include Proc

  type exit_reason = Proc.exit_reason
  type flag = Proc.flag = TrapExit of bool

  include Proc.Messages

  module Monitor = struct
    type t = Proc.monitor_ref
  end

  let set_flags flags =
    let t = Scheduler.get_scheduler () in
    let current = Scheduler.get_current_process t in
    Proc.set_flags current flags

  let link pid =
    let t = Scheduler.get_scheduler () in
    Scheduler.with_relations_lock t (fun () ->
        let current = Scheduler.get_current_process t in
        match Scheduler.get_process t pid with
        | None ->
            panic
              ("Cannot link to non-existent process "
             ^ Pid.to_string pid)
        | Some target ->
            Proc.link current pid;
            Proc.link target (Proc.pid current))

  let unlink pid =
    let t = Scheduler.get_scheduler () in
    Scheduler.with_relations_lock t (fun () ->
        let current = Scheduler.get_current_process t in
        match Scheduler.get_process t pid with
        | None -> ()
        | Some target ->
            Proc.unlink current pid;
            Proc.unlink target (Proc.pid current))

  let monitor pid =
    let t = Scheduler.get_scheduler () in
    Scheduler.with_relations_lock t (fun () ->
        let current = Scheduler.get_current_process t in
        match Scheduler.get_process t pid with
        | None ->
            panic
              ("Cannot monitor non-existent processs "
             ^ Pid.to_string pid)
        | Some target ->
            let ref = Proc.monitor current pid in
            Proc.add_monitored_by target (Proc.pid current) ref;
            ref)

  let demonitor ref =
    let t = Scheduler.get_scheduler () in
    Scheduler.with_relations_lock t (fun () ->
        let current = Scheduler.get_current_process t in
        let monitored_pid = Proc.monitored_pid_for_ref current ref in
        Proc.demonitor current ref;
        match monitored_pid with
        | None -> ()
        | Some pid -> (
            match Scheduler.get_process t pid with
            | None -> ()
            | Some target ->
                Proc.remove_monitored_by target (Proc.pid current) ref))
end

let run ~main ~args ?config () =
  let config = Option.unwrap_or config ~default:Config.default in
  Kernel.Exception.record_backtrace true;
  Scheduler.run ~config ~main:(fun () -> main ~args) |> exit

let shutdown ~status = Scheduler.shutdown (Scheduler.get_scheduler ()) ~status
let spawn fn = Scheduler.spawn (Scheduler.get_scheduler ()) fn

let spawn_link fn =
  let pid = spawn fn in
  Process.link pid;
  pid

let self () = Scheduler.self ()
let send pid msg = Scheduler.send pid msg

(* Cooperative I/O syscall for actor-aware I/O operations *)
let syscall ~name ~interest ~source ~timeout =
  Effect.perform (Proc_effect.Syscall { name; interest; source; timeout })

module Timer = struct
  type id = Timer_id.t

  let send_after target_pid (msg : Message.t) ~after =
    let sch = Scheduler.get_scheduler () in
    let now = Kernel.Time.monotonic_time_nanos () in
    let duration_nanos = Int64.of_float (after *. 1_000_000_000.0) in
    Scheduler.add_timer sch ~now ~duration_nanos ~mode:Timer.One_shot
      ~action:(Timer.Send_message (target_pid, msg))

  let send_interval target_pid (msg : Message.t) ~interval =
    let sch = Scheduler.get_scheduler () in
    let now = Kernel.Time.monotonic_time_nanos () in
    let duration_nanos = Int64.of_float (interval *. 1_000_000_000.0) in
    Scheduler.add_timer sch ~now ~duration_nanos
      ~mode:(Timer.Interval duration_nanos)
      ~action:(Timer.Send_message (target_pid, msg))

  let cancel timer_id =
    let sch = Scheduler.get_scheduler () in
    Scheduler.cancel_timer sch timer_id
end

include Effects

module Timer_id = Timer_id

let enable_trace () = ()  (* TODO: implement tracing *)
let disable_trace () = ()  (* TODO: implement tracing *)

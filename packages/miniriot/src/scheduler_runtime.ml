open Kernel

(** Runtime boundary module for scheduler internals.

    This module is kept in a separate compilation unit so the scheduler's
    internal role split (runtime/worker/reactor) is explicit in file layout. *)
module Make (Deps : sig
  type runtime
  type slot

  val create : config:Config.t -> runtime
  val request_shutdown : runtime -> status:int -> unit
  val shutdown : runtime -> status:int -> unit
  val worker_count : runtime -> int
  val with_relations_lock : runtime -> (unit -> 'a) -> 'a
  val get_process : runtime -> Pid.t -> Process.t option
  val get_process_slot : runtime -> Pid.t -> slot option
  val get_current_process : runtime -> Process.t
  val spawn_on_worker :
    runtime ->
    worker_id:Scheduler_id.t ->
    (unit -> (unit, Process.exit_reason) result) ->
    Pid.t
  val spawn : runtime -> (unit -> (unit, Process.exit_reason) result) -> Pid.t
  val send_internal : runtime -> Pid.t -> Message.t -> unit
  val enqueue_on_worker : runtime -> Scheduler_id.t -> slot -> unit
  val enqueue_owned_process : runtime -> slot -> unit
  val wake_process : runtime -> slot -> unit
  val wake_process_from_message : runtime -> slot -> unit
end) =
struct
  type runtime = Deps.runtime
  type slot = Deps.slot

  let create = Deps.create
  let request_shutdown = Deps.request_shutdown
  let shutdown = Deps.shutdown
  let worker_count = Deps.worker_count
  let with_relations_lock = Deps.with_relations_lock
  let get_process = Deps.get_process
  let get_process_slot = Deps.get_process_slot
  let get_current_process = Deps.get_current_process
  let spawn_on_worker = Deps.spawn_on_worker
  let spawn = Deps.spawn
  let send_internal = Deps.send_internal
  let enqueue_on_worker = Deps.enqueue_on_worker
  let enqueue_owned_process = Deps.enqueue_owned_process
  let wake_process = Deps.wake_process
  let wake_process_from_message = Deps.wake_process_from_message
end

(** Process scheduler for Miniriot.

    A scheduler owns a process registry plus one local runnable queue per worker.
    `Config.scheduler_count` determines how many runnable worker queues are
    created for one runtime invocation.
*)
open Kernel

type t
(** Opaque scheduler type *)

val get_scheduler : unit -> t
(** Get the current scheduler (must be called within a running scheduler) *)

val spawn : t -> (unit -> (unit, Process.exit_reason) result) -> Pid.t
(** Spawn a new process with the given function.

    The parent process no longer directly implies same-worker placement.
    Newly spawned processes are assigned to a worker chosen by the scheduler and
    enqueued directly onto that worker's runnable queue. *)

val self : unit -> Pid.t
(** Get the PID of the currently running process *)

val send : Pid.t -> Message.t -> unit
(** Send a message to a process *)

val shutdown : t -> status:int -> unit
(** Shutdown the scheduler with given exit status *)

val run :
  config:Config.t -> main:(unit -> (unit, Process.exit_reason) result) -> int
(** Run the scheduler with the given configuration and main function. Returns
    exit status. Can only be called once per process. *)

val add_timer :
  t ->
  now:int64 ->
  duration_nanos:int64 ->
  mode:Timer.mode ->
  action:Timer.action ->
  Timer.id
(** Add a timer to the scheduler's timer wheel *)

val cancel_timer : t -> Timer.id -> unit
(** Cancel a timer in the scheduler's timer wheel *)

val get_current_process : t -> Process.t
(** Get the currently running process *)

val get_process : t -> Pid.t -> Process.t option
(** Get a process by PID. Returns None if process not found. *)

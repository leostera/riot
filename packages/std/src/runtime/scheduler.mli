(** Multicore scheduler entrypoint for the actor runtime.

    The runtime owns:

    - a process registry shared by all workers
    - one runnable queue per scheduler worker
    - a dedicated reactor domain for timers and async I/O polling *)
open Kernel

(** Opaque scheduler runtime handle. *)
type t
(** Snapshot of scheduler counters used for multicore tracing. *)
type trace_counters = {
  steals: int;
  failed_steals: int;
  remote_wakeups: int;
  duplicate_enqueue_races: int;
}

(** Return the scheduler runtime bound to the current domain context. *)
val get_scheduler: unit -> t

(** Spawn a process on a scheduler chosen by the runtime placement policy. *)
val spawn: t -> (unit -> (unit, Process.exit_reason) result) -> Pid.t

(** Spawn a process pinned to a single normal scheduler. *)
val spawn_pinned:
  ?worker_id:Scheduler_id.t -> t -> (unit -> (unit, Process.exit_reason) result) -> Pid.t

(** Spawn a process on a dedicated blocking lane outside the normal
    work-stealing scheduler pool. *)
val spawn_blocked: t -> (unit -> (unit, Process.exit_reason) result) -> Pid.t

(** Return the PID of the currently running process in this domain context. *)
val self: unit -> Pid.t

(** Return the current normal scheduler identifier, or [None] when not running
    on a normal scheduler worker. *)
val current_worker_id_opt: unit -> Scheduler_id.t option

(** Send a message by PID through the runtime-wide process registry. *)
val send: Pid.t -> Message.t -> unit

(** Request that a process exit at its next scheduler boundary. *)
val kill: t -> Pid.t -> Process.exit_reason -> unit

(** Request runtime-wide shutdown with an exit status. *)
val shutdown: t -> status:int -> unit

(** Start worker domains and the reactor, then run until shutdown. *)
val run: config:Config.t -> main:(unit -> (unit, Process.exit_reason) result) -> int

(** Register a timer in the reactor-owned timer wheel. *)
val add_timer:
  t -> now:int64 -> duration_nanos:int64 -> mode:Timer.mode -> action:Timer.action -> Timer.id

(** Cancel a timer in the reactor-owned timer wheel. *)
val cancel_timer: t -> Timer.id -> unit

(** Return the current process for the calling worker domain context. *)
val get_current_process: unit -> Process.t

(** Look up a process by PID in the runtime-wide registry. *)
val get_process: t -> Pid.t -> Process.t option

(** Serialize link and monitor relation updates that span multiple processes. *)
val with_relations_lock: t -> (unit -> 'a) -> 'a

(** Enable scheduler trace logging and runtime counter emission. *)
val enable_trace: unit -> unit

(** Disable scheduler trace logging. *)
val disable_trace: unit -> unit

(** Return a point-in-time snapshot of scheduler counters. *)
val trace_counters: t -> trace_counters

(** Reset scheduler counters to zero. *)
val reset_trace_counters: t -> unit

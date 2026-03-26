(** Multicore scheduler/runtime entrypoint for Miniriot.

    The runtime owns:
    - a process registry shared by all workers
    - one runnable queue per scheduler worker
    - a dedicated reactor domain that owns timers and async I/O polling
*)
open Kernel

type t
(** Opaque scheduler runtime. *)

val get_scheduler : unit -> t
(** Return the scheduler runtime bound to the current domain context. *)

val spawn : t -> (unit -> (unit, Process.exit_reason) result) -> Pid.t
(** Spawn a process on a scheduler chosen by runtime placement policy. *)

val self : unit -> Pid.t
(** Return the PID of the currently running process in this domain context. *)

val send : Pid.t -> Message.t -> unit
(** Send a message by PID through the runtime-wide process registry. *)

val shutdown : t -> status:int -> unit
(** Request runtime-wide shutdown with an exit status. *)

val run :
  config:Config.t -> main:(unit -> (unit, Process.exit_reason) result) -> int
(** Start worker domains + reactor and run until shutdown. *)

val add_timer :
  t ->
  now:int64 ->
  duration_nanos:int64 ->
  mode:Timer.mode ->
  action:Timer.action ->
  Timer.id
(** Register a timer in the reactor-owned timer wheel. *)

val cancel_timer : t -> Timer.id -> unit
(** Cancel a timer in the reactor-owned timer wheel. *)

val get_current_process : t -> Process.t
(** Return the current process for the calling worker domain context. *)

val get_process : t -> Pid.t -> Process.t option
(** Runtime-wide process lookup by PID. *)

val with_relations_lock : t -> (unit -> 'a) -> 'a
(** Serialize link/monitor relation updates that span multiple processes. *)

val enable_trace : unit -> unit
(** Enable scheduler trace logging and runtime counter emission. *)

val disable_trace : unit -> unit
(** Disable scheduler trace logging. *)

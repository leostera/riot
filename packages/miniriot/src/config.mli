(** Runtime configuration for Miniriot *)

type timer_resolution =
  | Second  (** Timer resolution in seconds - lowest overhead *)
  | Millisecond  (** Timer resolution in milliseconds - balanced *)
  | Microsecond  (** Timer resolution in microseconds - high precision *)
  | Nanosecond  (** Timer resolution in nanoseconds - highest precision *)

type t = { timer_resolution : timer_resolution; scheduler_count : int }
(** Runtime configuration *)

val default : t
val default_scheduler_count : int
(** Default number of worker schedulers.

    The default is `max 1 (System.available_parallelism - 1)`, reserving one
    core for non-worker runtime work in a future reactor split. *)

val worker_count : t -> int
(** Get the number of runnable workers configured for this runtime. *)

val make :
  ?timer_resolution:timer_resolution -> ?scheduler_count:int -> unit -> t
(** Create a configuration with custom settings *)

val resolution_to_nanos : timer_resolution -> int64
(** Convert resolution to nanoseconds per tick *)

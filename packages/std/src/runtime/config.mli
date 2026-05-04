(** Runtime configuration for the actor runtime. *)
type timer_resolution =
  (** Second-resolution timers with the lowest overhead. *)
  | Second
  (** Millisecond-resolution timers with balanced cost and precision. *)
  | Millisecond
  (** Microsecond-resolution timers with higher precision. *)
  | Microsecond
  (** Nanosecond-resolution timers with the highest precision. *)
  | Nanosecond
(** Runtime configuration values. *)
type t = {
  timer_resolution: timer_resolution;
  scheduler_count: int;
}

(** Default runtime configuration. *)
val default: t

(**
   Default number of worker schedulers.

   If `RIOT_SCHEDULERS` is set to a positive integer, that value is used.
   Otherwise the default is `max 1 (System.available_parallelism - 1)`,
   reserving one core for the dedicated reactor domain that owns timer and I/O
   polling.
*)
val default_scheduler_count: int

(** Return the number of runnable workers configured for this runtime. *)
val worker_count: t -> int

(** Create a runtime configuration with custom settings. *)
val make: ?timer_resolution:timer_resolution -> ?scheduler_count:int -> unit -> t

(** Convert a timer resolution to nanoseconds per tick. *)
val resolution_to_nanos: timer_resolution -> int64

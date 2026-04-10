(** Hierarchical timing wheel for efficient timer management. *)
(** A hierarchical timing wheel. *)
type t

(** Create a timing wheel with the given configuration. *)
val create: config:Config.t -> t

(** Add a timer to the wheel and return its identifier. *)
val add_timer:
  t -> now:int64 -> duration_nanos:int64 -> mode:Timer.mode -> action:Timer.action -> Timer.id

(** Reinsert an existing interval timer without changing its identifier. *)
val reschedule_timer: t -> now:int64 -> Timer.t -> unit

(** Cancel a timer by its identifier. *)
val cancel_timer: t -> Timer.id -> unit

(** Advance the wheel to the given time and return expired timers. *)
val tick: t -> now:int64 -> Timer.t list

(** Return the next timer expiration in nanoseconds, if any. *)
val next_expiration: t -> now:int64 -> int64 option

(** Return the total number of active timers in the wheel. *)
val size: t -> int

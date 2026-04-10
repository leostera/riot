(** An opaque timer identifier. *)
type id = Timer_id.t
(** Timer firing mode. *)
type mode =
  (** Fire once, then remove the timer. *)
  | One_shot
  (** Fire repeatedly with the given interval in nanoseconds. *)
  | Interval of int64
(** Action performed when a timer fires. *)
type action =
  (** Wake a sleeping process, for example after a receive or syscall timeout. *)
  | Wake_process of Process.t
  (** Send a message to a process, for example for [`send_after`] or
      [`send_interval`] style timers. *)
  | Send_message of Pid.t * Message.t
(** A scheduled timer. *)
type t = {
  id: id;
  mode: mode;
  (** Start time in nanoseconds. *)
  mutable started_at: int64;
  (** Expiration time in nanoseconds. *)
  mutable expires_at: int64;
  (** Timer duration in nanoseconds. *)
  duration_nanos: int64;
  action: action;
  (** Current timer status. *)
  mutable status: 
    [
      `pending
      | `cancelled
    ];
}

(** Create a timer with the given clock state, duration, mode, and action. *)
val make: now:int64 -> duration_nanos:int64 -> mode:mode -> action:action -> t

(** Return `true` if the timer has been cancelled. *)
val is_cancelled: t -> bool

(** Mark the timer as cancelled. *)
val cancel: t -> unit

(** Return `true` if the timer should fire at [`now`]. *)
val should_fire: t -> now:int64 -> bool

(** Reschedule an interval timer after it fires. *)
val reschedule: t -> now:int64 -> unit

(** # Runtime - Std-owned actor runtime

    `Std.Runtime` owns Riot's actor runtime implementation: scheduling,
    mailboxes, timers, async I/O suspension, and process lifecycle.

    The separate `actors` package is now a compatibility facade over this
    module. *)
open Kernel

module Exception: sig
  (** Raised when a receive operation times out. *)
  exception Receive_timeout

  (** Raised when a syscall operation times out. *)
  exception Syscall_timeout
end

(** Runtime configuration. *)
module Config = Config

module Runtime: sig
  (** Reset the process-local reduction count to a new value. *)
  val reset_reductions: int -> unit

  (** Spend one cooperative reduction from the current process-local budget and
      yield when the budget is exhausted. *)
  val increment_reduction_count: unit -> unit
end

module Pid = Pid

(** Opaque scheduler identifier used by runtime internals. *)
module Scheduler_id = Scheduler_id

module Message: sig
  type t = ..
end

module Actor: sig
  (** The reason an actor exited. *)
  type exit_reason = exn
  (** Actor flags. *)
  type flag =
    | TrapExit of bool
  (** Opaque reference to a monitor registration. *)
  type monitor_ref
  type Message.t +=
    | EXIT of { from: Pid.t; reason: (unit, exit_reason) result }
    | DOWN of { ref: monitor_ref; pid: Pid.t; reason: (unit, exit_reason) result }

  (** Scheduler-visible actor state. *)
  type state =
    private | Uninitialized
    | Runnable
    | Waiting_message
    | Waiting_io of { name: string; token: Kernel.Async.Token.t; source: Kernel.Async.Source.t }
    | Running
    | Exited of (unit, exit_reason) result
    | Finalized
  (** Opaque actor handle. *)
  type t

  (** Create an actor from its entry function. *)
  val make: (unit -> (unit, exit_reason) result) -> t

  (** Return the actor identifier. *)
  val pid: t -> Pid.t

  (** Return the current actor state. *)
  val state: t -> state

  (** Return `true` if the actor is still alive. *)
  val is_alive: t -> bool

  (** Return `true` if the actor has pending messages. *)
  val has_messages: t -> bool

  (** Send a message to the actor from any scheduler domain. *)
  val send_message: t -> Message.t -> unit

  (** Mark the process as waiting for the given I/O operation. *)
  val mark_as_awaiting_io: t -> name:string -> Kernel.Async.Token.t -> Kernel.Async.Source.t -> unit

  (** Record a ready I/O token for the process. *)
  val add_ready_token: t -> Kernel.Async.Token.t -> Kernel.Async.Source.t -> unit

  (** Return a ready I/O token, if one is available. *)
  val get_ready_token: t -> (Kernel.Async.Token.t * Kernel.Async.Source.t) option

  (** Consume all ready I/O tokens with the given callback. *)
  val consume_ready_tokens: t -> (Kernel.Async.Token.t * Kernel.Async.Source.t -> unit) -> unit

  module Monitor: sig
    (** Opaque monitor reference. *)
    type t = monitor_ref
  end

  (** Link the current actor to another actor. *)
  val link: Pid.t -> unit

  (** Unlink the current actor from another actor. *)
  val unlink: Pid.t -> unit

  (** Monitor another actor. *)
  val monitor: Pid.t -> Monitor.t

  (** Stop monitoring an actor. *)
  val demonitor: Monitor.t -> unit

  (** Request that an actor exit at its next scheduler boundary. *)
  val kill: Pid.t -> reason:exit_reason -> unit

  (** Set flags for the currently running actor. *)
  val set_flags: flag list -> unit
end

module Process = Actor

(** Opaque timer identifiers. *)
module Timer_id = Timer_id

module Timer: sig
  (** Opaque timer identifier. *)
  type t = Timer_id.t
  type id = t

  (** Send a message to an actor after the given delay in seconds. *)
  val send_after: Pid.t -> Message.t -> after:float -> id

  (** Send a message to an actor repeatedly at the given interval in seconds. *)
  val send_interval: Pid.t -> Message.t -> interval:float -> id

  (** Cancel a timer. If the timer has already fired or does not exist, this is
      a no-op. *)
  val cancel: id -> unit
end

(** Return the PID of the currently running process. *)
val self: unit -> Pid.t

(** Spawn an actor using the normal placement policy. *)
val spawn: (unit -> (unit, Actor.exit_reason) result) -> Pid.t

(** Spawn an actor pinned to one normal scheduler. When [scheduler] is omitted,
    the runtime prefers the current normal scheduler and otherwise falls back to
    the normal placement policy. Pinned actors are not work-stolen. *)
val spawn_pinned: ?scheduler:int -> (unit -> (unit, Actor.exit_reason) result) -> Pid.t

(** Spawn an actor on a dedicated blocking lane outside the normal
    work-stealing scheduler pool. *)
val spawn_blocked: (unit -> (unit, Actor.exit_reason) result) -> Pid.t

(** Spawn an actor and link it to the current process. *)
val spawn_link: (unit -> (unit, Actor.exit_reason) result) -> Pid.t

(** Return the current normal scheduler identifier, or [None] when the caller
    is not running on a normal scheduler worker. *)
val current_scheduler_id: unit -> Scheduler_id.t option

(** Send a message to the given PID. *)
val send: Pid.t -> Message.t -> unit

(** Spend one process-local cooperative reduction and yield when the budget is
    exhausted. *)
val yield: unit -> unit

(** A mailbox selector that either returns a decoded message or skips the
    current mailbox entry. *)
type 'msg selector =
  Message.t -> [
    `select of 'msg
    | `skip
  ]

(** Receive a message selected by [`selector`]. Raises
    [Exception.Receive_timeout] when [`timeout`] expires. *)
val receive: selector:'value selector -> ?timeout:float -> unit -> 'value

(** Receive the next mailbox message. Raises [Exception.Receive_timeout] when
    [`timeout`] expires. *)
val receive_any: ?timeout:float -> unit -> Message.t

(** Request runtime shutdown with the given exit status. *)
val shutdown: status:int -> unit

(** Wait for an async source to become ready, then run the continuation.
    Raises [Exception.Syscall_timeout] when [`timeout`] expires. *)
val syscall:
  ?timeout:float ->
  name:string ->
  interest:Kernel.Async.Interest.t ->
  source:Kernel.Async.Source.t ->
  (unit -> 'a) ->
  'a

(** Start the runtime with optional configuration. Defaults to millisecond
    timer resolution and [Config.default_scheduler_count] workers. *)
val run:
  main:(args:string list -> (unit, Actor.exit_reason) result) ->
  args:string list ->
  ?config:Config.t ->
  unit ->
  unit

(** Enable debug tracing. *)
val enable_trace: unit -> unit

(** Disable debug tracing. *)
val disable_trace: unit -> unit

(** Snapshot of runtime multicore scheduler counters. *)
type trace_counters = {
  steals: int;
  failed_steals: int;
  remote_wakeups: int;
  duplicate_enqueue_races: int;
}

(** Return the current scheduler counter snapshot. *)
val trace_counters: unit -> trace_counters

(** Reset scheduler counters for the running runtime. *)
val reset_trace_counters: unit -> unit

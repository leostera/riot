(** Miniriot - Minimal single-core actor runtime *)
open Kernel

module Exception : sig
  exception Receive_timeout
  (** Raised when a receive operation times out *)

  exception Syscall_timeout
  (** Raised when a syscall operation times out *)
end

module Config = Config
(** Runtime configuration *)

module Runtime : sig
  (** Runtime support for reduction counting *)

  val reset_reductions : int -> unit
  (** Reset the reduction count to a new value *)

  val increment_reduction_count : unit -> unit
  (** Increment (actually decrement) the reduction count and yield if necessary.
      Due to how OCaml's bytecode works, we decrement from an initial value
      towards zero rather than counting up. *)
end

module Pid = Pid
module Scheduler_id = Scheduler_id
(** Opaque worker/scheduler identifier type used by runtime internals. *)

module Message : sig
  type t = ..
end

module Process : sig
  (** Process management *)

  type exit_reason = exn
  
  type monitor_ref

  type Message.t +=
    | EXIT of { from : Pid.t; reason : (unit, exit_reason) result }
    | DOWN of {
        ref : monitor_ref;
        pid : Pid.t;
        reason : (unit, exit_reason) result;
      }

  type state = private
    | Uninitialized
    | Runnable
    | Waiting_message
    | Waiting_io of {
        name : string;
        token : Kernel.Async.Token.t;
        source : Kernel.Async.Source.t;
      }
    | Running
    | Exited of (unit, exit_reason) result
    | Finalized  (** Process state - tracks current status in scheduler *)

  type t
  (** The process type *)

  val make : (unit -> (unit, exit_reason) result) -> t
  (** Create a new process with the given function *)

  val pid : t -> Pid.t
  (** Get the process ID *)

  val state : t -> state
  (** Get the current state *)

  val is_alive : t -> bool
  (** Check if process is alive (not exited or finalized) *)

  val has_messages : t -> bool
  (** Check if process has messages in its mailbox *)

  val send_message : t -> Message.t -> unit
  (** Send a message to the process *)

  val mark_as_awaiting_io :
    t -> name:string -> Kernel.Async.Token.t -> Kernel.Async.Source.t -> unit
  (** Mark process as waiting for I/O operation *)

  val add_ready_token :
    t -> Kernel.Async.Token.t -> Kernel.Async.Source.t -> unit
  (** Add a ready I/O token to the process *)

  val get_ready_token :
    t -> (Kernel.Async.Token.t * Kernel.Async.Source.t) option
  (** Get a ready I/O token if available *)

  val consume_ready_tokens :
    t -> (Kernel.Async.Token.t * Kernel.Async.Source.t -> unit) -> unit
  (** Consume all ready tokens with the given function *)

  module Monitor : sig
    type t = monitor_ref
    (** Monitor reference *)
  end

  val link : Pid.t -> unit
  (** Link the current process to another process *)

  val unlink : Pid.t -> unit
  (** Unlink the current process from another process *)

  val monitor : Pid.t -> Monitor.t
  (** Monitor another process *)

  val demonitor : Monitor.t -> unit
  (** Stop monitoring a process *)
end

module Timer_id = Timer_id
(** Opaque timer identifiers *)

module Timer : sig
  type id = Timer_id.t
  (** Opaque timer identifier *)

  val send_after : Pid.t -> Message.t -> after:float -> id
  (** Send a message to a process after a delay (in seconds). Returns a timer ID
      that can be used to cancel the timer. *)

  val send_interval : Pid.t -> Message.t -> interval:float -> id
  (** Send a message to a process repeatedly at a given interval (in seconds).
      Returns a timer ID that can be used to cancel the timer. *)

  val cancel : id -> unit
  (** Cancel a timer by its ID. If the timer has already fired or doesn't exist,
      this is a no-op. *)
end

val self : unit -> Pid.t
val spawn : (unit -> (unit, Process.exit_reason) result) -> Pid.t
val spawn_link : (unit -> (unit, Process.exit_reason) result) -> Pid.t
val send : Pid.t -> Message.t -> unit
val yield : unit -> unit

type 'msg selector = Message.t -> [ `select of 'msg | `skip ]

val receive : selector:'value selector -> ?timeout:float -> unit -> 'value
(** Receive a message using a selector. Optionally times out after [timeout]
    seconds, raising [Receive_timeout]. *)

val receive_any : ?timeout:float -> unit -> Message.t
(** Receive any message. Optionally times out after [timeout] seconds, raising
    [Receive_timeout]. *)

val shutdown : status:int -> unit

val syscall :
  ?timeout:float ->
  name:string ->
  interest:Kernel.Async.Interest.t ->
  source:Kernel.Async.Source.t ->
  (unit -> 'a) ->
  'a

val run :
  main:(args:string list -> (unit, Process.exit_reason) result) ->
  args:string list ->
  ?config:Config.t ->
  unit ->
  unit
(** Start the runtime with optional configuration. Defaults to millisecond timer
    resolution. *)

val enable_trace : unit -> unit
(** Enable debug tracing *)

val disable_trace : unit -> unit
(** Disable debug tracing *)

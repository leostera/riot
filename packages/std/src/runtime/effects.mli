open Kernel

module Exception: sig
  (** Raised when a receive operation times out. *)
  exception Receive_timeout

  (** Raised when a syscall operation times out. *)
  exception Syscall_timeout
end

(** Cooperatively yield control to the scheduler so other processes can run. *)
val yield: unit -> unit

(** Receive the next message from the mailbox.

    Use [`timeout`] to abort after the given number of seconds. Raises
    [Exception.Receive_timeout] on timeout. *)
val receive_any: ?timeout:float -> unit -> Message.t

(** A mailbox selector that either returns a decoded message or skips the
    current mailbox entry. *)
type 'msg selector =
  Message.t -> [
    `select of 'msg
    | `skip
  ]

(** Receive a message selected by [`selector`].

    Use [`timeout`] to abort after the given number of seconds. Raises
    [Exception.Receive_timeout] on timeout. *)
val receive: selector:'a selector -> ?timeout:float -> unit -> 'a

(** Exit the current process normally. *)
val exit: unit -> (unit, Process.exit_reason) result

(** Wait for the async source to become ready, then run the continuation.

    Use [`timeout`] to abort after the given number of seconds. Raises
    [Exception.Syscall_timeout] on timeout. *)
val syscall:
  ?timeout:float ->
  name:string ->
  interest:Kernel.Async.Interest.t ->
  source:Kernel.Async.Source.t ->
  (unit -> 'a) ->
  'a

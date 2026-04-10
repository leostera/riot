open Kernel

(** Timeout values for receive and syscall effects. [`after seconds`] uses
    seconds. *)
type timeout =
[
  `infinity
  | `after of float
]

(** Receive the next message selected by [`selector`], or abort when the
    timeout expires. *)
type _ Effect.t +=
  | Receive: {
      selector: 
        Message.t -> [
          `select of 'msg
          | `skip
        ];
      timeout: timeout;
    } -> 'msg Effect.t

(** Cooperatively yield control back to the scheduler. *)
type _ Effect.t +=
  | Yield: unit Effect.t

(** Wait for an async source to become ready for the requested interest, or
    abort when the timeout expires. *)
type _ Effect.t +=
  | Syscall: {
      name: string;
      interest: Kernel.Async.Interest.t;
      source: Kernel.Async.Source.t;
      timeout: timeout;
    } -> unit Effect.t

open Kernel

(* Timeout type for blocking operations *)

type timeout =
[
  | `infinity
  | `after of float
]

type _ Effect.t +=
  | Receive: {
      selector: 
        Message.t -> [
          `select of 'msg
          | `skip
        ];
      timeout: timeout;
    } -> 'msg Effect.t

type _ Effect.t +=
  Yield: unit Effect.t

(* I/O Effects *)

type _ Effect.t +=
  | Syscall: {
      name: string;
      interest: Kernel.Async.Interest.t;
      source: Kernel.Async.Source.t;
      timeout: timeout;
    } -> unit Effect.t

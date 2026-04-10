open Kernel

(** Internal exception used to unwind a running process continuation. *)
exception Unwind

(** Captured process continuation. *)
type ('a, 'b) continuation
(** Error details captured when a process finishes with an exception. *)
type error_info = {
  exn: exn;
  backtrace: Kernel.Exception.raw_backtrace;
}
(** The current state of a process continuation. *)
type 'a t =
  | Finished of ('a, error_info) result
  | Suspended: ('a, 'b) continuation * 'a Effect.t -> 'b t
  | Unhandled: ('a, 'b) continuation * 'a -> 'b t
(** A scheduler step produced while advancing a process continuation. *)
type 'a step =
  | Continue of 'a
  | Discontinue of exn
  | Reperform: 'a Effect.t -> 'a step
  | Delay: 'a step
  | Suspend: 'a step
  | Yield: unit step
  | Terminate: 'a step
(** Callback used to interpret an effect and resume the process. *)
type ('a, 'b) step_callback = ('a step -> 'b t) -> 'a Effect.t -> 'b t
(** Effect interpreter used while running a process. *)
type perform = {
  perform: 'a 'b. ('a, 'b) step_callback;
} [@@unboxed]

(** Create the initial process state from an entry function and its first
    effect. *)
val make: ('a -> 'b) -> 'a Effect.t -> 'b t

(** Run the process until it finishes, suspends, or exhausts its reduction
    budget. *)
val run: consume_reduction:(unit -> bool) -> perform:perform -> 'a t -> 'a t option

(** Return `true` if the process has already finished. *)
val is_finished: 'a t -> bool

(** Unwind the process with the given identifier. *)
val unwind: id:string -> 'a t -> unit

(** Process mailbox for message passing.

    The mailbox is multi-producer/single-consumer:
    - multiple schedulers may [queue] concurrently
    - only the process owner scheduler should call [next]
*)
open Kernel

type t
(** Opaque mailbox type containing queued messages *)

val create : unit -> t
(** Create a new empty mailbox *)

val queue : t -> Message.envelope -> unit
(** Add a message envelope to the mailbox from any producer domain. *)

val next : t -> Message.envelope option
(** Get the next message from the mailbox, returning None if empty *)

val size : t -> int
(** Get the current number of messages in the mailbox *)

val is_empty : t -> bool
(** Check if the mailbox is empty *)

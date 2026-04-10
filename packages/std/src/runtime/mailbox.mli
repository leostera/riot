(** Process mailbox for message passing.

    The mailbox is multi-producer/single-consumer:

    - multiple schedulers may call [queue] concurrently
    - only the owner scheduler should call [next] *)
open Kernel

(** Opaque mailbox type containing queued messages. *)
type t

(** Create an empty mailbox. *)
val create: unit -> t

(** Queue a message envelope from any producer domain. *)
val queue: t -> Message.envelope -> unit

(** Return the next queued message, or [None] if the mailbox is empty. *)
val next: t -> Message.envelope option

(** Return the current number of queued messages. *)
val size: t -> int

(** Return `true` if the mailbox is empty. *)
val is_empty: t -> bool

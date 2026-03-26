(** Domain primitives for multicore runtimes. *)

type 'a t

val spawn : (unit -> 'a) -> 'a t
val join : 'a t -> 'a
val recommended_count : unit -> int

module DLS : sig
  type 'a key

  val new_key : ?split_from_parent:('a -> 'a) -> (unit -> 'a) -> 'a key
  val get : 'a key -> 'a
  val set : 'a key -> 'a -> unit
end

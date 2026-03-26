(** Opaque scheduler/worker identifier. *)

type t

val zero : t
val of_int : int -> t
val to_int : t -> int
val succ : t -> t
val equal : t -> t -> bool
val compare : t -> t -> int

(** Opaque scheduler/worker identifier. *)
type t

(** The first scheduler identifier. *)
val zero: t

(** Build a scheduler identifier from an integer. *)
val of_int: int -> t

(** Convert a scheduler identifier to its integer representation. *)
val to_int: t -> int

(** Return the next scheduler identifier. *)
val succ: t -> t

(** Return `true` if both scheduler identifiers are equal. *)
val equal: t -> t -> bool

(** Compare two scheduler identifiers. *)
val compare: t -> t -> int

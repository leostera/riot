open Std

(** Identifier for one source origin entry in one snapshot's origin map. *)
type t

(** Total order over origin IDs for maps, sets, and deterministic rendering. *)
val compare: t -> t -> int

(** Equality over origin IDs. *)
val equal: t -> t -> bool

(** Build an origin ID from its current integer representation. *)
val of_int: int -> t

(** Expose the current integer representation for JSON and debug output. *)
val to_int: t -> int

(** Render a readable [origin#N] label. *)
val to_string: t -> string

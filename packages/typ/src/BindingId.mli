open Std

(** Stable identifier for one lowered binding node. *)
type t

(** Total order over binding IDs for maps, sets, and deterministic rendering. *)
val compare: t -> t -> int

(** Equality over binding IDs. *)
val equal: t -> t -> bool

(** Build a binding ID from its current integer representation. *)
val of_int: int -> t

(** Expose the current integer representation for JSON and debug output. *)
val to_int: t -> int

(** Render a readable [binding#N] label. *)
val to_string: t -> string

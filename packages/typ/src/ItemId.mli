open Std

(** Stable identifier for one top-level semantic item. *)
type t

(** Total order over item IDs for maps, sets, and deterministic rendering. *)
val compare: t -> t -> int

(** Equality over item IDs. *)
val equal: t -> t -> bool

(** Build an item ID from its current integer representation. *)
val of_int: int -> t

(** Expose the current integer representation for JSON and debug output. *)
val to_int: t -> int

(** Render a readable [item#N] label. *)
val to_string: t -> string

open Std

(** Stable identifier for one logical source across text updates. *)
type t

(** Total order over source IDs for maps, sets, and deterministic rendering. *)
val compare: t -> t -> int

(** Equality over source IDs. *)
val equal: t -> t -> bool

(** Build a source ID from its current integer representation. *)
val of_int: int -> t

(** Expose the current integer representation for JSON and debug output. *)
val to_int: t -> int

(** Render a readable [source#N] label. *)
val to_string: t -> string

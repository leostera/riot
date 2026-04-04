open Std

(** Best-effort stable identifier for one lowered pattern node. *)
type t

(** Total order over pattern IDs for maps, sets, and deterministic rendering. *)
val compare: t -> t -> int

(** Equality over pattern IDs. *)
val equal: t -> t -> bool

(** Build a pattern ID from its current integer representation. *)
val of_int: int -> t

(** Expose the current integer representation for JSON and debug output. *)
val to_int: t -> int

(** Render a readable [pat#N] label. *)
val to_string: t -> string

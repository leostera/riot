open Std

(** Best-effort stable identifier for one lowered expression node. *)
type t

(** Total order over expression IDs for maps, sets, and deterministic rendering. *)
val compare: t -> t -> int

(** Equality over expression IDs. *)
val equal: t -> t -> bool

(** Build an expression ID from its current integer representation. *)
val of_int: int -> t

(** Expose the current integer representation for JSON and debug output. *)
val to_int: t -> int

(** Render a readable [expr#N] label. *)
val to_string: t -> string

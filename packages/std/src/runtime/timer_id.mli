(** An opaque timer identifier. Use it to cancel a scheduled timer. *)
type t

(** Generate a fresh timer identifier. *)
val make: unit -> t

(** Return `true` if both timer identifiers refer to the same timer. *)
val equal: t -> t -> bool

(** Compare two timer identifiers. *)
val compare: t -> t -> int

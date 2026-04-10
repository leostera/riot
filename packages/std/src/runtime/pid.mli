(** Opaque process identifier *)
type t

(** The PID of the main process. *)
val main: t

(** Generate a fresh process identifier. *)
val next: unit -> t

(** Return `true` if both process identifiers are equal. *)
val equal: t -> t -> bool

(** Compare two process identifiers. *)
val compare: t -> t -> int

(** Convert PID to its underlying integer identifier. *)
val to_int: t -> int

(** Convert a process identifier to its string representation. *)
val to_string: t -> string

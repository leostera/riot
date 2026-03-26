(** Process identifiers *)

type t
(** Opaque process identifier *)

val main : t
(** The PID of the main process *)

val next : unit -> t
(** Generate the next unique PID *)

val equal : t -> t -> bool
(** Test PID equality *)

val compare : t -> t -> int
(** Compare PIDs for ordering *)

val to_int : t -> int
(** Convert PID to its underlying integer identifier. *)

val to_string : t -> string
(** Convert PID to string representation *)

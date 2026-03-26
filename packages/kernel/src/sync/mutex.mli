(** Thin wrapper around OCaml mutex primitives. *)

type t = Stdlib.Mutex.t

val create : unit -> t
val lock : t -> unit
val unlock : t -> unit
val try_lock : t -> bool

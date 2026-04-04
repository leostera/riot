open Std

(** Host-supplied configuration for one [Session]. *)
type env = (string * TypeScheme.t) list

type t = {
  (** Ambient bindings visible before any file-local declarations are checked. *)
  prelude: env;
}

(** Default host configuration used by the current prototype and tests. *)
val default: t

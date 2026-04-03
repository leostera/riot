open Std

(** Quantified type schemes exported from the prototype inferencer. *)

type t = Forall of int list * TypeRepr.t

val free_vars: t -> int list

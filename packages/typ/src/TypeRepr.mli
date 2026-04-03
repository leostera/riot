open Std

(** Mutable prototype type representation used inside one inference query. *)

type var = {
  id: int;
  mutable link: t option;
}

and t =
  | Int
  | Bool
  | String
  | Unit
  | Tuple of t list
  | Arrow of t * t
  | Var of var
  | Hole of int

val prune: t -> t

val union: int list -> int list -> int list

val diff: int list -> int list -> int list

val free_vars: t -> int list

val occurs: int -> t -> bool

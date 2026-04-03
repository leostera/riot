open Std

type t = Forall of int list * TypeRepr.t

let free_vars = fun (Forall (quantified, body)) ->
  TypeRepr.diff (TypeRepr.free_vars body) quantified

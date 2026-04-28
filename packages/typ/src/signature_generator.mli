(** Interface rendering helpers for the `Typ.Infer` path. *)
open Std

(**
   Render exported values as an inferred interface.

   The caller owns the ordering of the iterator, so environments that need
   source-order output should provide a source-ordered stream.
*)
val from_values: (Ast.ident * Ast.Type.t) Iter.Iterator.t -> string

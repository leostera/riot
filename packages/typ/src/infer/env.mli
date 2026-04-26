open Std
open Std.Collections

(**
   Mutable environment for one inference run.

   `Env.t` is query-local checker state. It records bindings discovered while
   typing a file and supports fast lookup for later expressions in the same
   run. The current slice stores only values.
*)
type t = {
  value_order: Ast.ident Vector.t;
  values: (Ast.ident, Ast.Type.t) HashMap.t;
}

(** Create an empty inference environment. *)
val create: unit -> t

(**
   Add or replace a value binding.

   Returns the previous type for `name` when one was already present. This
   matches the current "last binding wins" behavior of generated module
   summaries.
*)
val add_value: t -> name:Ast.ident -> type_:Ast.Type.t -> Ast.Type.t option

(** True when a value binding exists for `name`. *)
val has_value: t -> name:Ast.ident -> bool

(** Find the type currently bound to `name`, if any. *)
val get_value: t -> name:Ast.ident -> Ast.Type.t option

(**
   Iterate over visible value bindings in first-seen order.

   Rebinding an existing value updates the stored type but keeps the original
   export position. That matches the current "last binding wins" lookup
   behavior while keeping generated signatures stable.
*)
val values: t -> (Ast.ident * Ast.Type.t) Iter.Iterator.t

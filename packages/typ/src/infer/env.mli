open Std

(**
   Lexically scoped environment for one inference run.

   `Env.t` is query-local checker state. It records value bindings discovered
   while typing a file and supports lookup from the current lexical scope
   outwards. The root scope is the module scope; only values in that root scope
   are exported through `values`.
*)
type t

(** Create an empty environment with a single root/module scope. *)
val create: unit -> t

(**
   Push a new empty lexical scope.

   The new scope can see all bindings from its parent scopes, but bindings
   added to it disappear after `pop_scope`.
*)
val push_scope: t -> t

(**
   Pop the current lexical scope.

   Popping the root scope is a no-op, so the environment always has a module
   scope available.
*)
val pop_scope: t -> t

(**
   Add or replace a value binding in the current scope.

   At top level this creates or updates an exported module value. Inside a
   pushed scope it creates or updates a local value that shadows outer bindings
   during lookup but is never exported.
*)
val add_value: t -> name:Ast.ident -> scheme:TypeScheme.t -> t

(** True when a value binding exists in the current scope or any outer scope. *)
val has_value: t -> name:Ast.ident -> bool

(** Find the nearest type scheme currently bound to `name`, if any. *)
val get_value: t -> name:Ast.ident -> TypeScheme.t option

(**
   Iterate over exported value bindings from the root/module scope.

   Local scopes are intentionally ignored. The iterator order is the map's
   deterministic key order.
*)
val exports: t -> (Ast.ident * TypeScheme.t) Iter.Iterator.t

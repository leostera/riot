(**
   `ModuleInterface.t` is the small summary produced by the new one-shot
   inference path. It should eventually be the thing we render as an inferred
   signature and persist in the content store for dependency reuse.

   The current slice records exported values by identifier and type scheme.
*)
type t

(**
   Project the final inference environment into an exported module interface.

   This currently aliases the environment's value map as a temporary shortcut.
   Once interfaces are cached or retained beyond a single check, this should
   copy the visible exports instead.
*)
val from_env: Env.t -> t

(**
   Iterate over exported values.

   The iterator order comes from the inference environment, so the signature
   renderer can stay a pure formatter over already-ordered exports.
*)
val values: t -> (Ast.ident * TypeScheme.t) Std.Iter.Iterator.t

(**
   Identity for one binding introduced by the checker.

   A `Binding_id.t` is meant to distinguish declarations that may share the
   same surface name because of shadowing. For example, two declarations named
   `a` can have the same `Surface_path.t` name but different
   stamps.

   The `stamp` is allocated by the checker context that creates the binding.
   Treat it as stable inside one checked summary, not as a source-level name or
   a globally meaningful identifier.
*)
type t

(**
   `local ~stamp ~name` creates a binding identity for a locally introduced
   declaration.

   `name` is the source-facing name associated with the binding. `stamp` is the
   checker-allocated discriminator that makes shadowed bindings distinct.
*)
val local: stamp:int -> name:Surface_path.t -> t

(**
   Source-facing name attached to the binding.

   This is useful for diagnostics and rendering, but it is not enough by itself
   to identify a binding when shadowing exists.
*)
val name: t -> Surface_path.t

(** Checker-allocated discriminator for this binding. *)
val stamp: t -> int

(** Equality over both `stamp` and `name`. *)
val equal: t -> t -> bool

(** Deterministic ordering over `stamp`, then `name`. *)
val compare: t -> t -> Std.Order.t

(** Debug rendering in the form `"<surface-name>#<stamp>"`. *)
val to_string: t -> string

(** Serializer for persisting binding identities in checker summaries. *)
val serializer: t Serde.Ser.t

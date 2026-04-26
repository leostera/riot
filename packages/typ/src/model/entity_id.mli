(**
   Resolved identity for a semantic entity.

   An `Entity_id.t` combines the binding that introduced an entity with the
   surface path used to expose or refer to it. This is the model to use after
   name resolution, when the checker wants to say "this occurrence denotes that
   binding" while still retaining a user-facing path for diagnostics,
   signatures, and summaries.

   The split matters for aliases and qualified access: the same underlying
   binding can be visible through different surface paths.
*)
type t

(**
   `resolved ~binding_id ~surface_path` creates an entity identity after name
   resolution.

   `binding_id` is the canonical binding identity. `surface_path` is the path
   selected for the current exported/referenceable surface.
*)
val resolved: binding_id:Binding_id.t -> surface_path:Surface_path.t -> t

(**
   `from_binding_id binding_id` creates an entity whose surface path is the
   binding's own source-facing name.
*)
val from_binding_id: Binding_id.t -> t

(** Canonical binding that introduced the entity. *)
val binding_id: t -> Binding_id.t

(** User-facing path associated with this resolved entity. *)
val surface_path: t -> Surface_path.t

(** Equality over both canonical binding identity and surface path. *)
val equal: t -> t -> bool

(** Deterministic ordering over binding identity, then surface path. *)
val compare: t -> t -> Std.Order.t

(** Serializer for persisting resolved entity identities in checker summaries. *)
val serializer: t Serde.Ser.t

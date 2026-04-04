open Std

(** Semantic view for one successfully lowered source file.

    The long-lived state is split into [ItemTree], [BodyArena], and
    [OriginMap]. [SemanticTree.file] is a convenience wrapper that keeps those
    structures together for the current prototype APIs and reports. *)
type file = {
  (** Body-stable top-level item skeleton. *)
  item_tree: ItemTree.t;
  (** Normalized expressions, patterns, and bindings. *)
  body_arena: BodyArena.t;
  (** Source-backed origins for semantic IDs in this snapshot. *)
  origin_map: OriginMap.t;
  (** Lowering-time diagnostics emitted while building the semantic layers. *)
  diagnostics: Diagnostic.t list;
}

(** Empty semantic wrapper. *)
val empty: file

(** Find one origin entry by [OriginId]. *)
val find_origin: file -> OriginId.t -> OriginMap.origin option

(** Find one top-level item by [ItemId]. *)
val find_item: file -> ItemId.t -> ItemTree.item option

(** Find one binding by [BindingId]. *)
val find_binding: file -> BindingId.t -> BodyArena.binding option

(** Find one pattern by [PatId]. *)
val find_pattern: file -> PatId.t -> BodyArena.pattern_node option

(** Find one expression by [ExprId]. *)
val find_expr: file -> ExprId.t -> BodyArena.expr_node option

(** Render the wrapped semantic layers as debug text. *)
val to_string: file -> string

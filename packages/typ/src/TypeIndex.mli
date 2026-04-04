open Std

(** Query-oriented index of inferred expression types for one analyzed source. *)
type entry = {
  (** Expression indexed by this entry. *)
  expr_id: ExprId.t;
  (** Source origin attached to the expression. *)
  origin_id: OriginId.t;
  (** Source span used for position-based lookup. *)
  span: Syn.Ceibo.Span.t;
  (** Final inferred type for the indexed expression. *)
  inferred_type: TypeRepr.t;
}

(** Per-source type index used by [Query.type_at]. *)
type t

(** Minimal traced expression payload used to build the type index. *)
type traced_expr = {
  (** Expression traced by inference. *)
  expr_id: ExprId.t;
  (** Source origin attached to the expression. *)
  origin_id: OriginId.t;
  (** Final inferred type for the expression. *)
  inferred_type: TypeRepr.t;
}

(** Empty type index. *)
val empty: t

(** Build an index from expression traces and their source origins. *)
val of_traced_exprs:
  origin_map:OriginMap.t ->
  traced_expr list ->
  t

(** Enumerate all indexed entries. *)
val entries: t -> entry list

(** Find the smallest indexed span containing the given position. *)
val find_at: t -> Position.t -> entry option

(** Encode the index as structured JSON for snapshots and tooling. *)
val to_json: t -> Data.Json.t

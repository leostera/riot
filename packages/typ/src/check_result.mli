open Std

(** Shared output types for a single prototype type-check run. *)

type env = (string * TypeScheme.t) list

(** Environment snapshot captured before an expression is inferred. *)
type expr_trace = {
  (** Expression traced by this snapshot. *)
  expr_id: ExprId.t;
  (** Source origin attached to the expression. *)
  origin_id: OriginId.t;
  (** Visible environment before the expression was inferred. *)
  env_before: env;
  (** Final inferred type for the expression. *)
  inferred_type: TypeRepr.t;
}

(** Export-facing snapshot captured after a top-level item finishes. *)
type item_trace = {
  (** Item traced by this snapshot. *)
  item_id: ItemId.t;
  (** Names introduced by the item compared with the previous export state. *)
  binding_names: string list;
  (** Export environment visible after the item was processed. *)
  exports_after: env;
}

(** Full result of checking one source input through parse, lower, and infer. *)
type t = {
  (** Stable logical source identity assigned by the batch wrapper. *)
  source_id: SourceId.t;
  (** Host filename used when parsing this source text. *)
  filename: Path.t;
  (** Original source text checked by this run. *)
  source: string;
  (** Parser diagnostics emitted before CST building. *)
  parse_diagnostics: Syn.Diagnostic.t list;
  (** Lowered item skeleton when CST building and lowering succeeded. *)
  item_tree: ItemTree.t option;
  (** Lowered body arena when CST building and lowering succeeded. *)
  body_arena: BodyArena.t option;
  (** Source origin map when CST building and lowering succeeded. *)
  origin_map: OriginMap.t option;
  (** Convenience semantic wrapper built from the split semantic layers. *)
  semantic_tree: SemanticTree.file option;
  (** Diagnostics emitted during lowering. *)
  lowering_diagnostics: Diagnostic.t list;
  (** Diagnostics emitted during inference. *)
  typing_diagnostics: Diagnostic.t list;
  (** Export-facing summary for this checked source. *)
  file_summary: FileSummary.t;
  (** Query-oriented expression-type index derived from this checked source. *)
  type_index: TypeIndex.t;
  (** File-local exports after filtering the configured prelude. *)
  exports: env;
  (** Per-item export snapshots produced during inference. *)
  item_traces: item_trace list;
  (** Per-expression environment and type snapshots produced during inference. *)
  expr_traces: expr_trace list;
}

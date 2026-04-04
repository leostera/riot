open Std

(** One source analyzed within one immutable snapshot. *)
type t = {
  (** Logical source revision analyzed by this record. *)
  source: Source.t;
  (** Parser diagnostics collected before CST building. *)
  parse_diagnostics: Syn.Diagnostic.t list;
  (** Successful CST snapshot retained for source-backed tooling. *)
  cst: Syn.Cst.source_file option;
  (** Lowered semantic layers when CST building and lowering succeeded. *)
  semantic_tree: SemanticTree.file option;
  (** Diagnostics emitted during lowering. *)
  lowering_diagnostics: Diagnostic.t list;
  (** Diagnostics emitted during inference. *)
  typing_diagnostics: Diagnostic.t list;
  (** Export-facing summary for this analyzed source. *)
  file_summary: FileSummary.t;
  (** Query-oriented expression-type index derived from the inferred source. *)
  type_index: TypeIndex.t;
  (** Per-item export snapshots from inference. *)
  item_traces: Check_result.item_trace list;
  (** Per-expression environment and type snapshots from inference. *)
  expr_traces: Check_result.expr_trace list;
}

(** Parse, lower, and infer one [Source.t] inside the given host configuration. *)
val analyze: config:TypConfig.t -> Source.t -> t

(** Extract the export environment, or [[]] when no export was produced. *)
val exports: t -> FileSummary.exports

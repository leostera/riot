open Std

(** Query-oriented API over one immutable [Snapshot]. *)
type diagnostic =
  (** Parser diagnostic preserved from [syn]. *)
  | Parse of Syn.Diagnostic.t
  (** Lowering diagnostic emitted by [typ]. *)
  | Lowering of Diagnostic.t
  (** Inference diagnostic emitted by [typ]. *)
  | Typing of Diagnostic.t

(** Fetch the full per-source analysis record for one [SourceId]. *)
val analysis_of_source: Snapshot.t -> SourceId.t -> SourceAnalysis.t option

(** Gather parse, lowering, and typing diagnostics for one source. *)
val diagnostics: Snapshot.t -> SourceId.t -> diagnostic list

(** Fetch the full export-facing summary for one source, when present. *)
val file_summary_of: Snapshot.t -> SourceId.t -> FileSummary.t option

(** Fetch the canonical host-facing persisted summary for one rooted source's
    module, when present. *)
val persisted_summary_of: Snapshot.t -> SourceId.t -> PersistedSummary.t option

(** Fetch the canonical host-facing module summary for one rooted source's
    module, when present. *)
val module_summary_of: Snapshot.t -> SourceId.t -> ModuleSummary.t option

(** Fetch the export trust result for one source, when present. *)
val export_of: Snapshot.t -> SourceId.t -> FileSummary.export_result option

(** Find the smallest indexed expression containing one position and return its type. *)
val type_at: Snapshot.t -> SourceId.t -> Position.t -> TypeRepr.t option

(** Fetch the lowered semantic layers for one source, when present. *)
val semantic_tree_of_source: Snapshot.t -> SourceId.t -> SemanticTree.file option

(** Fetch the retained CST snapshot for one source, when present. *)
val source_file_of_source: Snapshot.t -> SourceId.t -> Syn.Cst.source_file option

open Std

(** Result of inferring types for one semantic tree. *)
type t = {
  (** File-local exports after filtering the configured prelude. *)
  exports: Check_result.env;
  (** Exported lowered type declarations, including reexports. *)
  type_decls: FileSummary.type_decl list;
  (** Per-item export snapshots produced during inference. *)
  item_traces: Check_result.item_trace list;
  (** Per-expression environment and type snapshots produced during inference. *)
  expr_traces: Check_result.expr_trace list;
  (** Typing diagnostics emitted while inferring the file. *)
  diagnostics: Diagnostic.t list;
}

(** Infer types for a semantic tree using the current prototype checker.

    The host configuration supplies only the intrinsic prelude plus ambient
    module summaries, so one-shot and session-based callers share the same
    inference rules without hardcoding package/library APIs in the inferencer. *)
val infer_file: config:TypConfig.t -> SemanticTree.file -> t

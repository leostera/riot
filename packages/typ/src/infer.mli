open Std

(** Result of inferring types for one semantic tree. *)
type t = {
  (** File-local exports after filtering the configured prelude. *)
  exports: Check_result.env;
  (** Per-item export snapshots produced during inference. *)
  item_traces: Check_result.item_trace list;
  (** Per-expression environment and type snapshots produced during inference. *)
  expr_traces: Check_result.expr_trace list;
  (** Typing diagnostics emitted while inferring the file. *)
  diagnostics: Diagnostic.t list;
}

(** Infer types for a semantic tree using the current prototype checker.

    The host configuration supplies the ambient prelude so one-shot and
    session-based callers share the same inference rules. *)
val infer_file: config:TypConfig.t -> SemanticTree.file -> t

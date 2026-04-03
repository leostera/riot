open Std

(** Result of inferring types for one semantic tree. *)
type t = {
  exports: Check_result.env;
  item_traces: Check_result.item_trace list;
  expr_traces: Check_result.expr_trace list;
  diagnostics: Diagnostic.t list;
}

(** Infer types for a semantic tree using the current prototype checker. *)
val infer_file: SemanticTree.file -> t

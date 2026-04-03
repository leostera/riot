open Std

(** Shared output types for a single prototype type-check run. *)

type env = (string * TypeScheme.t) list

(** Environment snapshot captured before an expression is inferred. *)
type expr_trace = {
  expr_id: int;
  origin_id: int;
  env_before: env;
  inferred_type: TypeRepr.t;
}

(** Export-facing snapshot captured after a top-level item finishes. *)
type item_trace = {
  item_id: int;
  binding_names: string list;
  exports_after: env;
}

(** Full result of checking one source input through parse, lower, and infer. *)
type t = {
  filename: Path.t;
  source: string;
  parse_diagnostics: Syn.Diagnostic.t list;
  semantic_tree: SemanticTree.file option;
  lowering_diagnostics: Diagnostic.t list;
  typing_diagnostics: Diagnostic.t list;
  exports: env;
  item_traces: item_trace list;
  expr_traces: expr_trace list;
}

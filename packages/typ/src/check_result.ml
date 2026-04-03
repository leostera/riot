open Std

type env = (string * TypeScheme.t) list

type expr_trace = {
  expr_id: int;
  origin_id: int;
  env_before: env;
  inferred_type: TypeRepr.t;
}

type item_trace = {
  item_id: int;
  binding_names: string list;
  exports_after: env;
}

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

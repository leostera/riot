open Std

type env = (string * TypeScheme.t) list

type expr_trace = {
  expr_id: ExprId.t;
  origin_id: OriginId.t;
  env_before: env;
  inferred_type: TypeRepr.t;
}

type item_trace = {
  item_id: ItemId.t;
  binding_names: string list;
  exports_after: env;
}

type t = {
  source_id: SourceId.t;
  filename: Path.t;
  source: string;
  parse_diagnostics: Syn.Diagnostic.t list;
  item_tree: ItemTree.t option;
  body_arena: BodyArena.t option;
  origin_map: OriginMap.t option;
  semantic_tree: SemanticTree.file option;
  lowering_diagnostics: Diagnostic.t list;
  typing_diagnostics: Diagnostic.t list;
  file_summary: FileSummary.t;
  type_index: TypeIndex.t;
  exports: env;
  item_traces: item_trace list;
  expr_traces: expr_trace list;
}

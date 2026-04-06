open Std

type diagnostic =
  | Parse of Syn.Diagnostic.t
  | Lowering of Diagnostic.t
  | Typing of Diagnostic.t

let analysis_of_source = Snapshot.find_analysis

let diagnostics = fun snapshot source_id ->
  match analysis_of_source snapshot source_id with
  | None -> []
  | Some analysis -> (analysis.parse_diagnostics |> List.map (fun diagnostic -> Parse diagnostic))
  @ (analysis.lowering_diagnostics |> List.map (fun diagnostic -> Lowering diagnostic))
  @ (analysis.typing_diagnostics |> List.map (fun diagnostic -> Typing diagnostic))

let file_summary_of = fun snapshot source_id ->
  match analysis_of_source snapshot source_id with
  | Some analysis -> Some analysis.file_summary
  | None -> None

let module_typings_of = Snapshot.find_module_typings

let export_of = fun snapshot source_id ->
  file_summary_of snapshot source_id |> Option.map (fun summary -> summary.FileSummary.export_result)

let semantic_tree_of_source = fun snapshot source_id ->
  match analysis_of_source snapshot source_id with
  | Some analysis -> analysis.semantic_tree
  | None -> None

let source_file_of_source = fun snapshot source_id ->
  match analysis_of_source snapshot source_id with
  | Some analysis -> analysis.cst
  | None -> None

let type_at = fun snapshot source_id position ->
  match analysis_of_source snapshot source_id with
  | None -> None
  | Some analysis ->
      TypeIndex.find_at analysis.type_index position |> function
      | Some entry -> Some entry.inferred_type
      | None -> None

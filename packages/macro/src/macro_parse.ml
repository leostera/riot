open Std

type node = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node

let expr = fun stream ->
  match Macro_token_stream.parsed_expr stream with
  | Some node -> Ok node
  | None ->
      Error
        (Macro_error.make
           ?span:(Macro_token_stream.span stream)
           "macro parse helpers currently require a parser-backed token stream")

let child_nodes = fun node ->
  Syn.Ceibo.Red.SyntaxNode.children_list node |> List.filter_map
    (function
      | Syn.Ceibo.Red.Node child -> Some child
      | _ -> None)

let rec unwrap_grouping = fun node ->
  if Syn.Ceibo.Red.SyntaxNode.kind node = Syn.SyntaxKind.PAREN_EXPR then
    match child_nodes node with
    | [ inner ] -> unwrap_grouping inner
    | _ -> node
  else
    node

let rec flatten_apply = fun acc node ->
  if Syn.Ceibo.Red.SyntaxNode.kind node = Syn.SyntaxKind.APPLY_EXPR then
    match child_nodes node with
    | [ func; arg ] -> flatten_apply (arg :: acc) func
    | _ -> node :: acc
  else
    node :: acc

let expr_arguments = fun stream ->
  match expr stream with
  | Error _ as err -> err
  | Ok body ->
      let body = unwrap_grouping body in
      (
        match Syn.Ceibo.Red.SyntaxNode.kind body with
        | Syn.SyntaxKind.APPLY_EXPR -> Ok (flatten_apply [] body)
        | Syn.SyntaxKind.TUPLE_EXPR -> Ok (child_nodes body)
        | _ -> Ok [ body ]
      )

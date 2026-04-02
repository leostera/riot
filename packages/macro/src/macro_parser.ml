open Std

type node = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node

type invocation = {
  name: string;
  span: Syn.Ceibo.Span.t;
  body: node;
}

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

let body_arguments = fun invocation ->
  let body = unwrap_grouping invocation.body in
  match Syn.Ceibo.Red.SyntaxNode.kind body with
  | Syn.SyntaxKind.APPLY_EXPR -> flatten_apply [] body
  | Syn.SyntaxKind.TUPLE_EXPR -> child_nodes body
  | _ -> [ body ]

let invocation_of_node = fun env node ->
  match Syn.Ceibo.Red.SyntaxNode.children_list node with
  | [
   Syn.Ceibo.Red.Node callee;
   Syn.Ceibo.Red.Token _bang;
   Syn.Ceibo.Red.Node body;
  ] ->
      (
        match Macro_environment.span_of_node node with
        | Some span -> Ok {
          name = Macro_environment.source_of_node env callee;
          span;
          body;
        }
        | None ->
            Error
              (Macro_error.make
                 ~span:(Syn.Ceibo.Red.SyntaxNode.span node)
                 "macro expansion could not recover the macro token span")
      )
  | _ ->
      Error
        (Macro_error.make
           ~span:(Syn.Ceibo.Red.SyntaxNode.span node)
           "macro expansion expected a macro invocation shape")

let collect_invocations = fun env ->
  let root = Syn.Ceibo.Red.new_root (Macro_environment.parsed env).tree in
  let rec collect_nodes acc node =
    if Syn.Ceibo.Red.SyntaxNode.kind node = Syn.SyntaxKind.MACRO_EXPR then
      node :: acc
    else
      List.fold_left
        (fun current element ->
          match element with
          | Syn.Ceibo.Red.Node child -> collect_nodes current child
          | Syn.Ceibo.Red.Token _ -> current)
        acc
        (Syn.Ceibo.Red.SyntaxNode.children_list node)
  in
  let nodes = List.rev (collect_nodes [] root) in
  let rec lift acc = function
    | [] -> Ok (List.rev acc)
    | node :: rest -> (
        match invocation_of_node env node with
        | Ok invocation -> lift (invocation :: acc) rest
        | Error err -> Error err
      )
  in
  lift [] nodes

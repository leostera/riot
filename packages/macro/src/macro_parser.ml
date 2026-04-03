open Std

type node = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node

type invocation = {
  callee_path: string list;
  span: Syn.Ceibo.Span.t;
  body: Macro_token_stream.t;
}

(* The parser already gave us a dedicated [MACRO_EXPR]. Collection here is about
   turning that parsed node back into an invocation record that preserves both
   the qualified callee path and the original body tokens. *)
let callee_path_of_node = fun node ->
  Syn.Ceibo.Red.SyntaxNode.tokens node |> List.filter_map
    (fun syntax_token ->
      let text = String.trim (Syn.Ceibo.Red.SyntaxToken.text syntax_token) in
      if String.equal text "." || String.equal text "" then
        None
      else
        Some text)

let invocation_of_node = fun env node ->
  match Syn.Ceibo.Red.SyntaxNode.children_list node with
  | [Syn.Ceibo.Red.Node callee;Syn.Ceibo.Red.Token _bang;Syn.Ceibo.Red.Node body;] -> (
      match Macro_environment.span_of_node node with
      | Some span -> Ok {
        callee_path = callee_path_of_node callee;
        span;
        body = Macro_token_stream.of_expr_node ~env body
      }
      | None -> Error (Macro_error.make ~span:(Syn.Ceibo.Red.SyntaxNode.span node) "macro expansion could not recover the macro token span")
    )
  | _ -> Error (Macro_error.make ~span:(Syn.Ceibo.Red.SyntaxNode.span node) "macro expansion expected a macro invocation shape")

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

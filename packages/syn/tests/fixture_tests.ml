open Std
open Std.Data
open Syn

let parse_result_to_json = fun result ->
  let span_to_json span = Json.Object [
    ("start", Json.Int span.Ceibo.Span.start);
    ("end", Json.Int span.Ceibo.Span.end_)
  ] in
  let span_text span =
    let width = span.Ceibo.Span.end_ - span.Ceibo.Span.start in
    if width <= 0 then
      ""
    else
      String.sub result.Parser.source span.Ceibo.Span.start width
  in
  let trivia_kind_to_json = function
    | Token.CommentTrivia { terminated; _ } -> Json.Object [
      ("kind", Json.String "comment");
      ("terminated", Json.Bool terminated)
    ]
    | Token.DocstringTrivia { terminated; _ } -> Json.Object [
      ("kind", Json.String "docstring");
      ("terminated", Json.Bool terminated)
    ]
    | Token.WhitespaceTrivia -> Json.Object [ ("kind", Json.String "whitespace") ]
  in
  let trivia_to_json (trivia: Token.trivia) =
    match trivia_kind_to_json trivia.kind with
    | Json.Object fields -> Json.Object ([
      ("span", span_to_json trivia.span);
      ("text", Json.String (span_text trivia.span))
    ]
    @ fields)
    | json -> json
  in
  let token_to_json (token: Token.t) = Json.Object [
    ("kind", Json.String (Token.show_kind token.kind));
    ("span", span_to_json token.span);
    ("text", Json.String (span_text token.span));
    ("leading_trivia", Json.Array (List.map trivia_to_json token.leading_trivia))
  ] in
  let kind_to_json kind = Json.String (SyntaxKind.to_string kind) in
  let text_to_json text = Json.String text in
  let tree_json = Ceibo.Green.to_json
    ~kind_to_json
    ~text_to_json
    (Ceibo.Green.Node result.Parser.tree) in
  Json.Object [
    ("tokens", Json.Array (List.map token_to_json result.Parser.tokens));
    ("tree", tree_json);
    ("diagnostics", Json.Array (List.map Diagnostic.to_json result.Parser.diagnostics))
  ]

let normalize_json = fun json -> Json.to_string json

let parse_expected_json = fun raw_json -> Json.of_string raw_json |> Result.expect ~msg:"Failed to parse expected JSON fixture"

let test_fixture = fun fixture_path expected_path ->
  let source = Fs.read (Path.v fixture_path) |> Result.expect ~msg:"Failed to read fixture" in
  let expected_json = Fs.read (Path.v expected_path) |> Result.expect ~msg:"Failed to read expected" in
  let parse_result = Syn.parse ~filename:(Path.v fixture_path) source in
  let actual_json = parse_result_to_json parse_result in
  let actual_str = normalize_json actual_json in
  let expected_str = parse_expected_json expected_json |> normalize_json in
  if actual_str = expected_str then
    Ok ()
  else
    Error ("Parse tree mismatch for "
    ^ fixture_path
    ^ "\nExpected:\n"
    ^ expected_str
    ^ "\n\nActual:\n"
    ^ actual_str
    ^ "\n")

let test_tagged_quoted_string_cst = fun _ctx ->
  let source = "let explanation = {explain|hello|explain}\n" in
  let parse_result = Syn.parse ~filename:(Path.v "tagged_quoted_string.ml") source in
  if List.length parse_result.Parser.diagnostics > 0 then
    let diagnostics = parse_result.Parser.diagnostics
    |> List.map Diagnostic.to_string
    |> String.concat "\n" in
    Error ("unexpected parse diagnostics:\n" ^ diagnostics)
  else
    match Syn.build_cst parse_result with
    | Ok cst -> (
        match cst with
        | Syn.Cst.Implementation {
          items=Syn.Cst.StructureItem.LetBinding {
            value=Syn.Cst.Expression.Literal (Syn.Cst.Literal.String {
              delimiter=Syn.Cst.Quoted { marker };
              contents;
              terminated;
              _
            });
            _
          } :: _;
          _
        } ->
            if String.equal marker "explain" && String.equal contents "hello" && terminated then
              Ok ()
            else
              Error ("unexpected tagged string CST payload: marker="
              ^ marker
              ^ ", contents="
              ^ contents
              ^ ", terminated="
              ^ Bool.to_string terminated)
        | _ -> Error "unexpected CST shape for tagged quoted string literal"
      )
    | Error (Syn.Cst_builder_error err) ->
        Error ("expected CST builder to succeed, got "
        ^ err.Syn.CstBuilder.message
        ^ " @ "
        ^ Syn.SyntaxKind.to_string err.Syn.CstBuilder.syntax_kind
        ^ " in "
        ^ String.concat " > " err.Syn.CstBuilder.context)
    | Error (Syn.Parse_diagnostics diagnostics) ->
        let diagnostics = diagnostics |> List.map Diagnostic.to_string |> String.concat "\n" in
        Error ("unexpected build_cst parse diagnostics:\n" ^ diagnostics)

let discover_fixtures = fun () ->
  let fixtures_dir = Path.v "packages/syn/tests/fixtures" in
  let entries_iter = Fs.read_dir fixtures_dir |> Result.expect ~msg:"Failed to read fixtures directory" in
  let entries = Iter.MutIterator.to_list entries_iter in
  List.filter_map
    (fun entry ->
      let path = Path.to_string (Path.join fixtures_dir entry) in
      if String.ends_with ~suffix:".ml" path || String.ends_with ~suffix:".mli" path then
        let expected_path = path ^ ".expected_lossless.json" in
        let exists = Fs.exists (Path.v expected_path) |> Result.unwrap_or ~default:false in
        if exists then
          Some (path, expected_path)
        else
          None
      else
        None)
    entries |> List.sort
    (fun ((a, _)) ((b, _)) ->
      String.compare a b)

let () =
  Miniriot.run
    ~main:(fun ~args ->
      let fixtures = discover_fixtures () in
      let tests = Test.case "tagged_quoted_string_cst" test_tagged_quoted_string_cst :: List.map
        (fun ((fixture_path, expected_path)) ->
          let name = Path.basename (Path.v fixture_path) in
          Test.case name (fun _ctx -> test_fixture fixture_path expected_path))
        fixtures
      in
      Test.Cli.main ~name:"syn-fixtures" ~tests ~args)
    ~args:Env.args
    ()

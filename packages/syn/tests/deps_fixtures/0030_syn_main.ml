open Syn
open Std

let now_nanos = fun () -> Time.SystemTime.now () |> Time.SystemTime.nanos

let duration_ms = fun ~start ~finish ->
  Int64.sub finish start |> Int64.to_float |> fun nanos -> nanos /. 1000000.0

let trace_cst_timings_enabled = fun () ->
  Env.var Env.Bool ~name:"SYN_TRACE_CST_TIMINGS" |> Option.unwrap_or ~default:false

let trace_cst_timing = fun label duration ->
  eprintln ("[syn] " ^ label ^ ": " ^ Float.to_string duration ^ "ms")

let parse_file = fun ~file ~source ->
  let tokens = Lexer.tokenize source in
  if String.ends_with ~suffix:".mli" file then
    Parser.parse_interface ~source tokens
  else
    Parser.parse_implementation ~source tokens

let span_to_json = fun span ->
  Data.Json.Object [
    ("start", Data.Json.int span.Ceibo.Span.start);
    ("end", Data.Json.int span.Ceibo.Span.end_)
  ]

let span_text = fun source span ->
  let width = span.Ceibo.Span.end_ - span.Ceibo.Span.start in
  if width <= 0 then
    ""
  else
    String.sub source span.Ceibo.Span.start width

let trivia_kind_to_json = function
  | Token.CommentTrivia { terminated; _ } -> Data.Json.Object [
    ("kind", Data.Json.string "comment");
    ("terminated", Data.Json.bool terminated)
  ]
  | Token.DocstringTrivia { terminated; _ } -> Data.Json.Object [
    ("kind", Data.Json.string "docstring");
    ("terminated", Data.Json.bool terminated)
  ]
  | Token.WhitespaceTrivia -> Data.Json.Object [ ("kind", Data.Json.string "whitespace") ]

let trivia_to_json = fun ~source (trivia: Token.trivia) ->
  match trivia_kind_to_json trivia.kind with
  | Data.Json.Object fields -> Data.Json.Object ([
    ("span", span_to_json trivia.span);
    ("text", Data.Json.string (span_text source trivia.span))
  ]
  @ fields)
  | json -> json

let token_to_json = fun ~source (token: Token.t) ->
  Data.Json.Object [
    ("kind", Data.Json.string (Token.show_kind token.kind));
    ("span", span_to_json token.span);
    ("text", Data.Json.string (span_text source token.span));
    ("leading_trivia", Data.Json.Array (List.map (trivia_to_json ~source) token.leading_trivia))
  ]

let parse_result_to_ceibo_json = fun result ->
  let kind_to_json kind = Data.Json.String (SyntaxKind.to_string kind) in
  let text_to_json text = Data.Json.String text in
  let tree_json = Ceibo.Green.to_json
    ~kind_to_json
    ~text_to_json
    (Ceibo.Green.Node result.Parser.tree) in
  Data.Json.Object [
    (
      "tokens",
      Data.Json.Array (List.map (token_to_json ~source:result.Parser.source) result.Parser.tokens)
    );
    ("tree", tree_json);
    ("diagnostics", Data.Json.Array (List.map Diagnostic.to_json result.diagnostics))
  ]

let handle_token_stream = fun sub_matches ->
  let file = ArgParser.get_one sub_matches "FILE" |> Option.expect ~msg:"FILE required" in
  let json = ArgParser.get_flag sub_matches "json" in
  match Fs.read (Path.v file) with
  | Error _err ->
      Log.error ("Error reading file " ^ file);
      exit 1
  | Ok content ->
      let tokens = Lexer.tokenize content in
      if json then
        let json_tokens = List.map (token_to_json ~source:content) tokens in
        println (Data.Json.to_string (Data.Json.array json_tokens))
      else
        List.iter (fun tok -> println (Token.show_kind tok.Token.kind)) tokens

let handle_parse = fun sub_matches ->
  let file = ArgParser.get_one sub_matches "FILE" |> Option.expect ~msg:"FILE required" in
  let json = ArgParser.get_flag sub_matches "json" in
  let red_tree = ArgParser.get_flag sub_matches "red-tree" in
  match Fs.read (Path.v file) with
  | Error _err ->
      Log.error ("Error reading file " ^ file);
      exit 1
  | Ok source ->
      let result = parse_file ~file ~source in
      if json then
        let kind_to_json kind = Data.Json.String (SyntaxKind.to_string kind) in
        let text_to_json text = Data.Json.String text in
        if red_tree then
          let red_root = Ceibo.Red.new_root result.tree in
          let tree_json = Ceibo.Red.to_json ~kind_to_json ~text_to_json (Ceibo.Red.Node red_root) in
          println (Data.Json.to_string tree_json)
        else
          println (Data.Json.to_string (parse_result_to_ceibo_json result))
      else if result.diagnostics != [] then
        DiagnosticReporter.print ~file ~source result.diagnostics
      else (
        Log.info "Parsed successfully";
        let width = Ceibo.Green.width (Ceibo.Green.Node result.tree) in
        Log.info ("Tree width: " ^ Int.to_string width ^ " bytes")
      )

let handle_print_ceibo = fun sub_matches ->
  let file = ArgParser.get_one sub_matches "FILE" |> Option.expect ~msg:"FILE required" in
  match Fs.read (Path.v file) with
  | Error _err ->
      Log.error ("Error reading file " ^ file);
      exit 1
  | Ok source -> parse_file ~file ~source |> parse_result_to_ceibo_json |> Data.Json.to_string |> println

let handle_print_cst = fun sub_matches ->
  let file = ArgParser.get_one sub_matches "FILE" |> Option.expect ~msg:"FILE required" in
  match Fs.read (Path.v file) with
  | Error _err ->
      Log.error ("Error reading file " ^ file);
      exit 1
  | Ok source ->
      let trace_timings = trace_cst_timings_enabled () in
      let parse_started = now_nanos () in
      let result = parse_file ~file ~source in
      let parse_finished = now_nanos () in
      let json =
        if result.diagnostics != [] then
          Data.Json.Object [
            ("status", Data.Json.String "parse_error");
            ("diagnostics", Data.Json.Array (List.map Diagnostic.to_json result.diagnostics))
          ]
        else
          (
            let build_started = now_nanos () in
            let cst_result =
              CstBuilder.create_from_ceibo
                ~kind:((
                  if String.ends_with ~suffix:".mli" file then
                    `Interface
                  else
                    `Implementation
                ))
                ~source
                ~tokens:result.tokens
                result.tree
            in
            let build_finished = now_nanos () in
            let json_started = now_nanos () in
            let json = CstJson.of_result cst_result in
            let json_finished = now_nanos () in
            if trace_timings then
              (
                trace_cst_timing
                  "parse_file"
                  (duration_ms ~start:parse_started ~finish:parse_finished);
                trace_cst_timing
                  "build_cst"
                  (duration_ms ~start:build_started ~finish:build_finished);
                trace_cst_timing "cst_json" (duration_ms ~start:json_started ~finish:json_finished)
              );
            json
          )
      in
      let stringify_started = now_nanos () in
      let output = Data.Json.to_string json in
      let stringify_finished = now_nanos () in
      if trace_timings then
        trace_cst_timing
          "json_to_string"
          (duration_ms ~start:stringify_started ~finish:stringify_finished);
      println output

let handle_explain = fun sub_matches ->
  let error_code = ArgParser.get_one sub_matches "ERROR_CODE" |> Option.expect ~msg:"ERROR_CODE required" in
  match Error.id_of_string error_code with
  | Some id -> println (Error.explain id ^ "\n")
  | None ->
      Log.error ("Unknown error code: " ^ error_code);
      exit 1

let main = fun ~args ->
  (* Parse command line arguments *)
  let cmd =
    let open ArgParser in
      let open Arg in command "syn"
      |> version "0.1.0"
      |> about "OCaml syntax analysis tool"
      |> subcommands
        [
          command "tokenize"
          |> about "Print token stream for a file"
          |> args
            [
              positional "FILE" |> help "OCaml source file to tokenize" |> required true;
              flag "json" |> long "json" |> help "Output in JSON format"
            ];
          command "parse"
          |> about "Parse file into Ceibo syntax tree"
          |> args
            [
              positional "FILE" |> help "OCaml source file to parse" |> required true;
              flag "json" |> long "json" |> help "Output syntax tree as JSON";
              flag "red-tree" |> long "red-tree" |> help "Output red tree (with spans) instead of green tree"
            ];
          command "print-ceibo"
          |> about "Print lossless Ceibo parse result as JSON"
          |> args [ positional "FILE" |> help "OCaml source file to parse" |> required true ];
          command "print-cst"
          |> about "Print typed CST lift result as JSON"
          |> args [ positional "FILE" |> help "OCaml source file to lift" |> required true ];
          command "explain"
          |> about "Explain an error code"
          |> args
            [
              positional "ERROR_CODE" |> help "Error code to explain (e.g., E0001)" |> required true
            ];
        ]
  in
  match ArgParser.get_matches cmd args with
  | Error err ->
      ArgParser.print_error err;
      ArgParser.print_help cmd;
      Error (Failure "invalid CLI arguments")
  | Ok matches -> (
      match ArgParser.get_subcommand matches with
      | Some ("tokenize", sub_matches) ->
          handle_token_stream sub_matches;
          Ok ()
      | Some ("parse", sub_matches) ->
          handle_parse sub_matches;
          Ok ()
      | Some ("print-ceibo", sub_matches) ->
          handle_print_ceibo sub_matches;
          Ok ()
      | Some ("print-cst", sub_matches) ->
          handle_print_cst sub_matches;
          Ok ()
      | Some ("explain", sub_matches) ->
          handle_explain sub_matches;
          Ok ()
      | _ ->
          ArgParser.print_help cmd;
          Error (Failure "missing subcommand")
    )

let () = Actors.run ~main ~args:Env.args ()

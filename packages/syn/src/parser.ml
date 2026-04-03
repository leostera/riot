open Std
open Std.Sync
open Std.Collections

(** New Parser Implementation

    Following the principles from NEW_PARSER.md:
    - Always return nodes (never options)
    - Explicit trivia control (not everywhere!)
    - Flat function structure (no nesting)
    - Grammar-driven (one function per EBNF rule)
    - TDD approach with snapshot-backed fixture suites *)
(** Parse result type *)
type parse_result = {
  source: string;
  tokens: Token.t list;
  kind: 
    [
      | `Implementation
      | `Interface
    ];
  tree: (Syntax_kind.t, string) Ceibo.Green.node;
  diagnostics: Diagnostic.t list;
}

type parser = {
  cursor: Token_cursor.t;
  diagnostics: Diagnostic.t list Cell.t;
  mutable object_update_depth: int;
  mutable case_expr_depth: int;
  mutable record_field_depth: int;
}

let create = fun ~source tokens ->
  {
    cursor = Token_cursor.create ~source tokens;
    diagnostics = Cell.create [];
    object_update_depth = 0;
    case_expr_depth = 0;
    record_field_depth = 0;
  }

let position = fun parser -> Token_cursor.position parser.cursor

(** Check if at end of tokens *)
let is_eof = fun parser -> Token_cursor.is_eof parser.cursor

(** Peek at current token without advancing *)
let peek = fun parser -> Token_cursor.peek parser.cursor

(** Peek at current token kind *)
let peek_kind = fun parser -> (peek parser).Token.kind

(** Check if current token matches a specific kind *)

(** Advance to next token *)
let peek_n = fun parser n ->
  Token_cursor.peek_n parser.cursor n

let at = fun parser kind -> peek_kind parser = kind

let advance = fun parser -> Token_cursor.advance parser.cursor

(** Get current span for error reporting *)
let report_diagnostic = fun parser diag ->
  let current_diags = Cell.get parser.diagnostics in
  Cell.set parser.diagnostics (diag :: current_diags)

let current_span = fun parser ->
  let token = peek parser in
  token.Token.span

(** Get span pointing to end of last consumed token (for "expected X" errors) *)
let expected_span = fun parser ->
  let last_tok = Token_cursor.last_token parser.cursor in
  let end_pos = last_tok.Token.span.end_ in
  { Ceibo.Span.start = end_pos; end_ = end_pos }

let point_span = fun pos -> Ceibo.Span.make ~start:pos ~end_:pos

let starts_structure_item = function
  | Token.Keyword Keyword.Let
  | Token.Keyword Keyword.Type
  | Token.Keyword Keyword.Module
  | Token.Keyword Keyword.Exception
  | Token.Keyword Keyword.External
  | Token.Keyword Keyword.Open
  | Token.Keyword Keyword.Include
  | Token.Keyword Keyword.Class -> true
  | _ -> false

let missing_let_in_expr_boundary = fun parser ->
  match peek_kind parser with
  | Token.EOF
  | Token.Keyword Keyword.In
  | Token.Keyword Keyword.And -> true
  | _ -> false

let missing_let_binding_boundary = fun parser ->
  match peek_kind parser with
  | Token.EOF -> true
  | Token.Keyword Keyword.Type
  | Token.Keyword Keyword.Module
  | Token.Keyword Keyword.Exception
  | Token.Keyword Keyword.External
  | Token.Keyword Keyword.Open
  | Token.Keyword Keyword.Include
  | Token.Keyword Keyword.Class -> true
  | _ -> false

let missing_type_definition_boundary = fun parser ->
  match peek_kind parser with
  | Token.EOF -> true
  | kind when starts_structure_item kind -> true
  | _ -> false

let unexpected_top_level_item_diagnostic = fun parser tok ~signature ->
  let text = Token_cursor.view parser.cursor tok.Token.span in
  match tok.Token.kind with
  | Token.CloseDelim _ -> Diagnostic.unexpected_closing_delimiter
    ~delimiter:text
    ~found:tok
    ~text
    ~span:tok.Token.span
  | _ ->
      if signature then
        Diagnostic.unexpected_signature_item ~found:tok ~text ~span:tok.Token.span
      else
        Diagnostic.unexpected_structure_item ~found:tok ~text ~span:tok.Token.span

(** Get text of a token from source *)
let token_text = fun parser token ->
  Token_cursor.view parser.cursor token.Token.span

let ident_text_payload = fun text ->
  try
    if String.length text >= 2 && text.[0] = '\\' && text.[1] = '#' then
      if String.length text = 2 then
        ""
      else
        String.sub text 2 (String.length text - 2)
    else
      text
  with
  | _ -> text

let ident_starts_uppercase = fun text ->
  let payload = ident_text_payload text in
  String.length payload > 0 && match payload.[0] with
  | 'A' .. 'Z' -> true
  | _ -> false

let green_nontrivia_token_texts = fun element ->
  let rec loop = fun acc ->
    function
    | Ceibo.Green.Token ((token: (Syntax_kind.t, string) Ceibo.Green.token)) -> (
        match Ceibo.Green.kind (Ceibo.Green.Token token) with
        | Syntax_kind.WHITESPACE
        | Syntax_kind.COMMENT
        | Syntax_kind.DOCSTRING -> acc
        | _ -> (
            match Ceibo.Green.text (Ceibo.Green.Token token) with
            | Some text -> text :: acc
            | None -> acc
          )
      )
    | Ceibo.Green.Node ((node: (Syntax_kind.t, string) Ceibo.Green.node)) -> Array.fold_left
      loop
      acc
      (Ceibo.Green.children node)
  in
  loop [] element |> List.rev

let green_expr_is_module_path_like = fun expr ->
  let texts = green_nontrivia_token_texts (Ceibo.Green.Node expr) in
  let rec loop = function
    | [ ident ] -> ident_starts_uppercase ident
    | ident :: "." :: rest -> ident_starts_uppercase ident && loop rest
    | _ -> false
  in
  match Ceibo.Green.kind (Ceibo.Green.Node expr) with
  | Syntax_kind.IDENT_EXPR
  | Syntax_kind.PATH_EXPR -> loop texts
  | _ -> false

let is_custom_index_operator_token = function
  | Token.At
  | Token.Question
  | Token.Percent
  | Token.Bang -> true
  | _ -> false

let is_custom_index_open_delim = function
  | Token.OpenDelim Token.Paren
  | Token.OpenDelim Token.Bracket
  | Token.OpenDelim Token.Brace -> true
  | _ -> false

let has_custom_index_payload_after_dot = fun parser ->
  let rec loop offset =
    match (peek_n parser offset).Token.kind with
    | kind when is_custom_index_operator_token kind -> loop (offset + 1)
    | kind -> offset > 0 && is_custom_index_open_delim kind
  in
  loop 0

let has_dot_ident_continuation = fun parser ->
  peek_kind parser = Token.Dot && match (peek_n parser 1).Token.kind with
  | Token.Ident _ -> true
  | _ -> false

let looks_like_record_field_after_offset = fun parser offset ->
  let rec loop index =
    match (peek_n parser index).Token.kind with
    | Token.Ident _ -> (
        match (peek_n parser (index + 1)).Token.kind with
        | Token.Dot -> (
            match (peek_n parser (index + 2)).Token.kind with
            | Token.Ident _ -> loop (index + 2)
            | _ -> false
          )
        | Token.Eq
        | Token.Semi
        | Token.CloseDelim Token.Brace ->
            true
        | _ ->
            false
      )
    | _ -> false
  in
  loop offset

let semicolon_belongs_to_record_field = fun parser ->
  peek_kind parser = Token.Semi && (
    match (peek_n parser 1).Token.kind with
    | Token.CloseDelim Token.Brace -> true
    | _ -> looks_like_record_field_after_offset parser 1
  )

let has_dot_open_paren_continuation = fun parser ->
  peek_kind parser = Token.Dot && match (peek_n parser 1).Token.kind with
  | Token.OpenDelim Token.Paren -> true
  | _ -> false

let uppercase_ident_starts_variant_representation = fun parser ->
  match peek_kind parser with
  | Token.Ident name when ident_starts_uppercase name -> not
    ((peek_n parser 1).Token.kind = Token.Dot)
  | _ -> false

let is_parenthesized_functor_type_start = fun parser ->
  let starts_with_parameter_name =
    match peek_kind parser with
    | Token.Ident name when ident_starts_uppercase name -> true
    | Token.Underscore -> true
    | _ -> false
  in
  let rec scan offset depth saw_colon =
    match (peek_n parser offset).Token.kind with
    | Token.EOF -> false
    | Token.OpenDelim Token.Paren -> scan (offset + 1) (depth + 1) saw_colon
    | Token.CloseDelim Token.Paren ->
        if depth = 0 then
          saw_colon && (peek_n parser (offset + 1)).Token.kind = Token.Arrow
        else
          scan (offset + 1) (depth - 1) saw_colon
    | Token.Colon when depth = 0 -> scan (offset + 1) depth true
    | _ -> scan (offset + 1) depth saw_colon
  in
  starts_with_parameter_name && scan 1 0 false

let is_parenthesized_module_type_expr_start = fun parser ->
  let rec scan offset =
    match (peek_n parser offset).Token.kind with
    | Token.OpenDelim Token.Paren -> scan (offset + 1)
    | Token.Keyword Keyword.Module -> (peek_n parser (offset + 1)).Token.kind
    = Token.Keyword Keyword.Type
    | Token.OpenDelim Token.SigEnd -> true
    | _ -> false
  in
  scan 0

let can_start_module_expr = fun parser ->
  match peek_kind parser with
  | Token.Keyword Keyword.Functor
  | Token.OpenDelim Token.StructEnd
  | Token.OpenDelim Token.SigEnd
  | Token.OpenDelim Token.Paren
  | Token.OpenDelim Token.Bracket
  | Token.OpenDelim Token.Brace -> true
  | Token.Ident name -> ident_starts_uppercase name
  | _ -> false

let can_start_module_type_expr = fun parser ->
  match peek_kind parser with
  | Token.Keyword Keyword.Functor
  | Token.OpenDelim Token.SigEnd
  | Token.Keyword Keyword.Module
  | Token.OpenDelim Token.Paren
  | Token.OpenDelim Token.Bracket
  | Token.OpenDelim Token.Brace -> true
  | Token.Ident name -> ident_starts_uppercase name
  | _ -> false

let starts_with_module_type_keyword = fun parser ->
  peek_kind parser = Token.Keyword Keyword.Module
  && (peek_n parser 1).Token.kind = Token.Keyword Keyword.Type

let starts_with_module_type_of_keywords = fun parser ->
  peek_kind parser = Token.Keyword Keyword.Module
  && (peek_n parser 1).Token.kind = Token.Keyword Keyword.Type
  && (peek_n parser 2).Token.kind = Token.Keyword Keyword.Of

let starts_with_class_type_keyword = fun parser ->
  peek_kind parser = Token.Keyword Keyword.Class
  && (peek_n parser 1).Token.kind = Token.Keyword Keyword.Type

let can_start_class_type_arrow_parameter = fun parser ->
  match peek_kind parser with
  | Token.Tilde
  | Token.Question
  | Token.Quote
  | Token.Underscore
  | Token.Ident _
  | Token.Keyword Keyword.True
  | Token.Keyword Keyword.False
  | Token.Hash
  | Token.OpenDelim Token.Bracket
  | Token.OpenDelim Token.Paren
  | Token.OpenDelim Token.Brace
  | Token.Lt -> true
  | _ -> false

(** Consume a single token WITHOUT consuming trivia after it.

    IMPORTANT: This is the primitive operation. It does NOT auto-consume trivia!
    Call consume_trivia explicitly where grammar allows it. *)
let consume = fun parser ->
  let token = peek parser in
  advance parser;
  token

(** Check if a token kind is trivia *)
let is_trivia_kind = function
  | Token.Comment _
  | Token.Docstring _
  | Token.Whitespace -> true
  | _ -> false

(** Error recovery: skip tokens until we reach a synchronization point.

    This helps prevent cascading errors by consuming tokens after an error until
    we reach a point where parsing can meaningfully continue.

    @param parser The parser state
    @param sync_tokens List of token kinds to stop at (but not consume)
    @return List of consumed tokens during recovery *)

(** Expect a specific token kind. If found, consume and return it.
    If not found, report diagnostic and return the found token without consuming.
    
    @param parser The parser state
    @param expected_kind The token kind we expect
    @param diagnostic_fn Function to create diagnostic given the found token
    @return The expected token if found, otherwise the found token (as dummy) *)
let consume_trivia = fun parser -> Token_cursor.consume_leading_trivia parser.cursor

let peek_trivia = fun parser -> Token_cursor.peek_leading_trivia parser.cursor |> List.map Token.trivia_to_token

let leading_trivia_contains_newline = fun parser ->
  Token_cursor.peek_leading_trivia parser.cursor |> List.exists
    (fun (trivia: Token.trivia) ->
      match trivia.Token.kind with
      | Token.WhitespaceTrivia -> String.contains
        (Token_cursor.view parser.cursor trivia.Token.span)
        "\n"
      | _ -> false)

let error_recover_until = fun parser ~sync_tokens ->
  let is_sync_token kind =
    List.exists (fun sync -> sync = kind) sync_tokens
  in
  let rec skip_to_sync acc =
    match peek_kind parser with
    | Token.EOF ->
        List.rev acc
    | kind when is_sync_token kind ->
        List.rev acc
    | _ when leading_trivia_contains_newline parser ->
        List.rev acc
    | _ ->
        let tok = consume parser in
        skip_to_sync (tok :: acc)
  in
  skip_to_sync []

let expect = fun parser expected_kind diagnostic_fn ->
  if peek_kind parser = expected_kind then
    consume parser
  else
    let found = peek parser in
    let diagnostic = diagnostic_fn found in
    report_diagnostic parser diagnostic;
    found

(* Return found token as dummy - don't consume for error recovery *)

(** Parse content within parentheses: (content)
    Returns all the parts needed to build a node.
    
    @param parser The parser state
    @param content_parser Function to parse the content inside parens
    @return Tuple of (open_paren, content, close_paren) *)

(** Parse content within braces: {content}
    Returns all the parts needed to build a node.
    
    @param parser The parser state
    @param content_parser Function to parse the content inside braces
    @return Tuple of (open_brace, content, close_brace) *)

(** Parse and expect an identifier.
    Reports diagnostic if not an identifier.
    
    @param parser The parser state
    @param diagnostic_fn Function to create diagnostic given the found token
    @return The identifier token *)
let parse_parens = fun parser content_parser ->
  (* Expect open paren *)
  let open_paren =
    expect
      parser
      (Token.OpenDelim Token.Paren)
      (fun found ->
        Diagnostic.unclosed_delimiter
          ~opener:"("
          ~found
          ~text:(token_text parser found)
          ~span:(current_span parser))
  in
  (* Parse content *)
  let content = content_parser parser in
  (* Expect close paren *)
  let close_paren =
    expect
      parser
      (Token.CloseDelim Token.Paren)
      (fun found ->
        Diagnostic.unclosed_delimiter
          ~opener:")"
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser))
  in
  (open_paren, content, close_paren)

let parse_braces = fun parser content_parser ->
  (* Expect open brace *)
  let open_brace =
    expect
      parser
      (Token.OpenDelim Token.Brace)
      (fun found ->
        Diagnostic.unclosed_delimiter
          ~opener:"{"
          ~found
          ~text:(token_text parser found)
          ~span:(current_span parser))
  in
  (* Parse content *)
  let content = content_parser parser in
  (* Expect close brace *)
  let close_brace =
    expect
      parser
      (Token.CloseDelim Token.Brace)
      (fun found ->
        Diagnostic.unclosed_delimiter
          ~opener:"}"
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser))
  in
  (open_brace, content, close_brace)

let parse_ident = fun parser diagnostic_fn ->
  match peek_kind parser with
  | Token.Ident _ -> consume parser
  | _ ->
      let found = peek parser in
      let diagnostic = diagnostic_fn parser found in
      report_diagnostic parser diagnostic;
      found

(* Return found token as dummy *)

(** Convert trivia kinds to their trivia syntax kinds. *)
let trivia_kind_to_syntax_kind = function
  | Token.WhitespaceTrivia -> Syntax_kind.WHITESPACE
  | Token.CommentTrivia _ -> Syntax_kind.COMMENT
  | Token.DocstringTrivia _ -> Syntax_kind.DOCSTRING

(** Convert token kinds to the syntax kinds used by parser-built green tokens.

    Most real tokens still reuse nearby expression/pattern kinds because the
    green tree models syntax nodes, not a separate token-kind enum. *)
let syntax_kind_of_token_kind = function
  | Token.Whitespace -> Syntax_kind.WHITESPACE
  | Token.Comment _ -> Syntax_kind.COMMENT
  | Token.Docstring _ -> Syntax_kind.DOCSTRING
  | Token.Literal (Token.Int _) -> Syntax_kind.INT_LITERAL
  | Token.Literal (Token.Float _) -> Syntax_kind.FLOAT_LITERAL
  | Token.Literal (Token.String _) -> Syntax_kind.STRING_LITERAL
  | Token.Literal (Token.Char _) -> Syntax_kind.CHAR_LITERAL
  | Token.Keyword Keyword.True
  | Token.Keyword Keyword.False -> Syntax_kind.BOOL_LITERAL
  | Token.Ident _ -> Syntax_kind.IDENT_EXPR
  | Token.Underscore -> Syntax_kind.WILDCARD_PATTERN
  | _ -> Syntax_kind.IDENT_EXPR

(* Catch-all: treat as identifier for now *)

(** Make a green tree node from children *)

(** Make a token green element *)

(** Convert list of tokens to green elements *)
let make_node = fun kind children ->
  let children_array = Array.of_list children in
  Ceibo.Green.make_node ~kind ~children:children_array

(** Make an ERROR node with diagnostic *)
let green_trivia_of_token_trivia = fun parser (trivia: Token.trivia) ->
  let kind = trivia_kind_to_syntax_kind trivia.Token.kind in
  let text = Token_cursor.view parser.cursor trivia.Token.span in
  let width = String.length text in
  Ceibo.Green.make_trivia ~kind ~text ~width

let make_token = fun parser token ->
  let token_kind = token.Token.kind in
  let syntax_kind = syntax_kind_of_token_kind token_kind in
  let text = Token_cursor.view parser.cursor token.Token.span in
  let width = String.length text in
  let leading_trivia = List.map (green_trivia_of_token_trivia parser) token.Token.leading_trivia in
  let green_token = Ceibo.Green.make_token ~leading_trivia ~kind:syntax_kind ~text ~width in
  Ceibo.Green.Token green_token

(** *
    ============================================================================
    * GRAMMAR SECTION 1: LEXICAL CONVENTIONS *
    ============================================================================
*)
let tokens_to_green = fun parser tokens ->
  List.filter_map
    (fun token ->
      if is_trivia_kind token.Token.kind then
        None
      else
        Some (make_token parser token))
    tokens

let make_error_node = fun parser ~diagnostic ~consumed_tokens ->
  report_diagnostic parser diagnostic;
  (* Wrap consumed tokens in ERROR node *)
  let children = tokens_to_green parser consumed_tokens in
  make_node Syntax_kind.ERROR children

(** Get operator precedence and associativity. Returns (precedence,
    is_right_associative). Higher precedence = tighter binding. *)
let operator_info = function
  | Token.PipeGt -> Some (0, false)
  | Token.Dollar -> Some (0, true)
  | Token.Or -> Some (1, false)
  | Token.And
  | Token.Ampersand -> Some (2, false)
  | Token.Eq
  | Token.Ne
  | Token.Lt
  | Token.Gt
  | Token.LtEq
  | Token.GtEq
  | Token.EqEq
  | Token.BangEq -> Some (3, false)
  | Token.LtPercent -> Some (3, false)
  | Token.At
  | Token.Hash
  | Token.Caret
  | Token.AtAt -> Some (4, true)
  | Token.ColonColon -> Some (5, true)
  | Token.Plus
  | Token.Minus
  | Token.PlusDot
  | Token.MinusDot -> Some (6, false)
  | Token.Star
  | Token.Slash
  | Token.Percent
  | Token.PercentGt
  | Token.StarDot
  | Token.SlashDot -> Some (7, false)
  | Token.StarStar -> Some (8, true)
  | _ -> None

(** Parse type variable: "'" ident

    CRITICAL: No trivia allowed between ' and ident! Grammar: typexpr ::= "'"
    ident *)
let rec parse_type_variable = fun parser ->
  match peek_kind parser with
  | Token.Quote -> (
      let quote = consume parser in
      (* IMMEDIATELY get identifier - NO trivia! *)
      match peek_kind parser with
      | Token.Ident name ->
          let ident = consume parser in
          let ident_text = token_text parser ident in
          (* Check if type variable starts with uppercase *)
          let first_char =
            if String.length ident_text > 0 then
              String.get ident_text 0
            else
              'a'
          in
          if first_char >= 'A' && first_char <= 'Z' then
            let diagnostic = Diagnostic.uppercase_type_variable
              ~text:ident_text
              ~found:ident
              ~text_found:ident_text
              ~span:(Ceibo.Span.make ~start:quote.Token.span.start ~end_:ident.Token.span.end_) in
            make_error_node parser ~diagnostic ~consumed_tokens:[ quote; ident ]
          else
            make_node Syntax_kind.TYPE_VAR [ make_token parser quote; make_token parser ident ]
      | found ->
          (* Error: expected identifier after quote *)
          let found_tok = peek parser in
          let diagnostic = Diagnostic.malformed_type_variable
            ~found:found_tok
            ~text:(token_text parser found_tok)
            ~span:(current_span parser) in
          make_error_node parser ~diagnostic ~consumed_tokens:[ quote ]
    )
  | found ->
      (* Error: expected quote to start type variable *)
      let found_tok = peek parser in
      let diagnostic = Diagnostic.malformed_type_variable
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(current_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** *
    ============================================================================
    * GRAMMAR SECTION 4: TYPE EXPRESSIONS *
    ============================================================================
*)
(** Parse a single type parameter: [ext-variance] "'" ident
    
    ext-variance ::= variance [injectivity] | injectivity [variance]
    variance ::= "+" | "-"
    injectivity ::= "!"
*)
and parse_type_param = fun parser ->
  (* Parse optional variance and injectivity annotations *)
  let variance_token = ref None in
  let injectivity_token = ref None in
  (* First, try to parse variance or injectivity *)
  let rec parse_annotations () =
    match peek_kind parser with
    | Token.Plus when !variance_token = None ->
        variance_token := Some (consume parser);
        parse_annotations ()
    | Token.Minus when !variance_token = None ->
        variance_token := Some (consume parser);
        parse_annotations ()
    | Token.Bang when !injectivity_token = None ->
        injectivity_token := Some (consume parser);
        parse_annotations ()
    | _ ->
        ()
  in
  parse_annotations ();
  (* Now parse the type variable: ' ident or _ (underscore) *)
  let type_var =
    match peek_kind parser with
    | Token.Underscore ->
        (* Wildcard type parameter for GADTs: type _ t *)
        let underscore = consume parser in
        make_node Syntax_kind.TYPE_VAR [ make_token parser underscore ]
    | Token.Quote ->
        parse_type_variable parser
    | _ ->
        let found = peek parser in
        let diagnostic = Diagnostic.invalid_type_expression
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser) in
        make_error_node parser ~diagnostic ~consumed_tokens:[]
  in
  (* Build the TYPE_PARAM node with optional variance/injectivity *)
  let children = (
    match !variance_token with
    | Some tok -> [ make_token parser tok ]
    | None -> []
  ) @ (
    match !injectivity_token with
    | Some tok -> [ make_token parser tok ]
    | None -> []
  ) @ [ Ceibo.Green.Node type_var ]
  in
  make_node Syntax_kind.TYPE_PARAM children

(** Parse type parameters: 'a or ('a, 'b, 'c) or _ Returns (type_params_green,
    trivia_tokens) *)
and parse_type_params = fun parser ->
  match peek_kind parser with
  | Token.Quote
  | Token.Plus
  | Token.Minus
  | Token.Bang
  | Token.Underscore ->
      (* Single type parameter: 'a, +'a, -'a, !'a, _ *)
      let param = parse_type_param parser in
      let trivia_tokens = consume_trivia parser in
      ([ Ceibo.Green.Node param ], trivia_tokens)
  | Token.OpenDelim Token.Paren -> (
      (* Multiple type parameters: ('a, 'b, 'c) *)
      let lparen = consume parser in
      let trivia_after_lparen = consume_trivia parser in
      (* Parse comma-separated list of type parameters *)
      let rec parse_params acc =
        match peek_kind parser with
        | Token.CloseDelim Token.Paren ->
            (* End of parameter list *)
            List.rev acc
        | Token.Quote
        | Token.Plus
        | Token.Minus
        | Token.Bang
        | Token.Underscore -> (
            let param = parse_type_param parser in
            let trivia_tokens = consume_trivia parser in
            let trivia_green = tokens_to_green parser trivia_tokens in
            (* Check for comma or closing paren *)
            match peek_kind parser with
            | Token.Comma ->
                let comma = consume parser in
                let trivia2_tokens = consume_trivia parser in
                let trivia2_green = tokens_to_green parser trivia2_tokens in
                parse_params
                  (trivia2_green
                  @ [ make_token parser comma ]
                  @ trivia_green
                  @ [ Ceibo.Green.Node param ]
                  @ acc)
            | Token.CloseDelim Token.Paren ->
                List.rev (trivia_green @ [ Ceibo.Green.Node param ] @ acc)
            | _ ->
                (* Expected comma or ) *)
                List.rev (trivia_green @ [ Ceibo.Green.Node param ] @ acc)
          )
        | _ ->
            (* Invalid token in type params *)
            List.rev acc
      in
      let params = parse_params [] in
      let trivia_before_close = consume_trivia parser in
      (* Expect closing paren *)
      match peek_kind parser with
      | Token.CloseDelim Token.Paren ->
          let rparen = consume parser in
          let trivia_after_rparen = consume_trivia parser in
          (
            [ make_token parser lparen ]
            @ tokens_to_green parser trivia_after_lparen
            @ params
            @ tokens_to_green parser trivia_before_close
            @ [ make_token parser rparen ],
            trivia_after_rparen
          )
      | _ ->
          (* Missing closing paren *)
          let found_tok = peek parser in
          let diagnostic = Diagnostic.unclosed_type_params
            ~found:found_tok
            ~text:(token_text parser found_tok)
            ~span:(expected_span parser) in
          report_diagnostic parser diagnostic;
          (
            [ make_token parser lparen ]
            @ tokens_to_green parser trivia_after_lparen
            @ params
            @ tokens_to_green parser trivia_before_close,
            []
          )
    )
  | _ -> (* No type parameters *)
    ([], [])

(** Parse class type parameters: ['a, _] *)
and parse_class_type_params = fun parser ->
  match peek_kind parser with
  | Token.OpenDelim Token.Bracket ->
      let open_bracket = consume parser in
      let trivia_after_open = consume_trivia parser in
      let rec parse_params acc =
        let param =
          match peek_kind parser with
          | Token.Quote
          | Token.Underscore
          | Token.Plus
          | Token.Minus
          | Token.Bang -> parse_type_param parser
          | _ ->
              let found = peek parser in
              let text = token_text parser found in
              let diagnostic = Diagnostic.invalid_type_parameter
                ~text
                ~found
                ~text_found:text
                ~span:(current_span parser) in
              make_error_node parser ~diagnostic ~consumed_tokens:[]
        in
        let trivia_after_param = consume_trivia parser in
        let acc = acc @ [ Ceibo.Green.Node param ] @ tokens_to_green parser trivia_after_param in
        match peek_kind parser with
        | Token.Comma ->
            let comma = consume parser in
            let trivia_after_comma = consume_trivia parser in
            parse_params
              (acc @ [ make_token parser comma ] @ tokens_to_green parser trivia_after_comma)
        | _ -> acc
      in
      let params = parse_params [] in
      let trivia_before_close = consume_trivia parser in
      let close_bracket =
        expect
          parser
          (Token.CloseDelim Token.Bracket)
          (fun found ->
            Diagnostic.unclosed_delimiter
              ~opener:"["
              ~found
              ~text:(token_text parser found)
              ~span:(expected_span parser))
      in
      (
        [ make_token parser open_bracket ]
        @ tokens_to_green parser trivia_after_open
        @ params
        @ tokens_to_green parser trivia_before_close
        @ [ make_token parser close_bracket ],
        consume_trivia parser
      )
  | _ -> ([], [])

(** Parse type expression dispatcher *)
and parse_typexpr = fun ?(stop_before_arrow = false) parser ->
  let base =
    (* Check for polymorphic type: 'a 'b. type *)
    match peek_kind parser with
    | Token.Quote ->
        (* Could be a poly type. Look ahead to check for pattern: 'a ... . *)
        let saved_pos = Token_cursor.position parser.cursor in
        let might_be_poly = ref true in
        let type_var_count = ref 0 in
        (* Scan ahead to see if we have: 'a ['b ...] . *)
        let rec scan_for_dot () =
          match peek_kind parser with
          | Token.Quote ->
              (* Consume ' *)
              let _ = consume parser in
              (* Expect identifier *)
              (
                match peek_kind parser with
                | Token.Ident _
                | Token.Underscore ->
                    let _ = consume parser in
                    type_var_count := !type_var_count + 1;
                    scan_for_dot ()
                | _ -> might_be_poly := false
              )
          | Token.Dot ->
              (* Found the dot! This is definitely a polymorphic type *)
              ()
          | _ ->
              (* Not a poly type pattern *)
              might_be_poly := false
        in
        scan_for_dot ();
        (* Restore position *)
        Token_cursor.set_position parser.cursor saved_pos;
        (* Now actually parse it if it's polymorphic *)
        if !might_be_poly && !type_var_count > 0 then
          parse_poly_type ~stop_before_arrow parser
        else
          parse_alias_type ~stop_before_arrow parser
    | Token.Keyword Keyword.Type ->
        (* Could be locally abstract type: type a b. type_expr *)
        let saved_pos = Token_cursor.position parser.cursor in
        let might_be_locally_abstract = ref true in
        let type_var_count = ref 0 in
        (* Consume 'type' keyword *)
        let _ = consume parser in
        (* Scan ahead to see if we have: ident [ident ...] . *)
        let rec scan_for_dot () =
          match peek_kind parser with
          | Token.Ident _ ->
              let _ = consume parser in
              type_var_count := !type_var_count + 1;
              scan_for_dot ()
          | Token.Dot ->
              (* Found the dot! This is a locally abstract type *)
              ()
          | _ ->
              (* Not a locally abstract type pattern *)
              might_be_locally_abstract := false
        in
        scan_for_dot ();
        (* Restore position *)
        Token_cursor.set_position parser.cursor saved_pos;
        (* Parse it if it matches the pattern *)
        if !might_be_locally_abstract && !type_var_count > 0 then
          parse_locally_abstract_type ~stop_before_arrow parser
        else
          parse_alias_type ~stop_before_arrow parser
    | _ ->
        parse_alias_type ~stop_before_arrow parser
  in
  attach_postfix_attributes parser base

(** Parse type alias: t as 'a *)
and parse_alias_type = fun ?(stop_before_arrow = false) parser ->
  let left = parse_arrow_type ~stop_before_arrow parser in
  let saved_pos = Token_cursor.position parser.cursor in
  let trivia_after_left = consume_trivia parser in
  match peek_kind parser with
  | Token.Keyword Keyword.As ->
      let as_kw = consume parser in
      let trivia_after_as = consume_trivia parser in
      let alias =
        match peek_kind parser with
        | Token.Quote
        | Token.Underscore -> parse_primary_type parser
        | _ ->
            let found_tok = peek parser in
            let diagnostic = Diagnostic.invalid_type_expression
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser) in
            make_error_node parser ~diagnostic ~consumed_tokens:[]
      in
      make_node
        Syntax_kind.TYPE_ALIAS
        ([ Ceibo.Green.Node left ]
        @ tokens_to_green parser trivia_after_left
        @ [ make_token parser as_kw ]
        @ tokens_to_green parser trivia_after_as
        @ [ Ceibo.Green.Node alias ])
  | _ ->
      Token_cursor.set_position parser.cursor saved_pos;
      left

(** Parse polymorphic type: 'a 'b. type *)
and parse_poly_type = fun ?(stop_before_arrow = false) parser ->
  let type_vars = ref [] in
  let type_vars_trivia = ref [] in
  (* Parse all type variables before the dot *)
  let rec collect_type_vars () =
    match peek_kind parser with
    | Token.Quote ->
        let quote = consume parser in
        let trivia_after_quote = consume_trivia parser in
        (* Expect identifier or underscore *)
        (
          match peek_kind parser with
          | Token.Ident _
          | Token.Underscore ->
              let var_name = consume parser in
              let trivia_after_var = consume_trivia parser in
              (* Build TYPE_VAR node *)
              let var_node = make_node
                Syntax_kind.TYPE_VAR
                [
                  make_token parser quote;
                  Ceibo.Green.Node (make_node Syntax_kind.IDENT_EXPR [ make_token parser var_name ])
                ] in
              type_vars := var_node :: !type_vars;
              type_vars_trivia := (trivia_after_quote @ trivia_after_var) :: !type_vars_trivia;
              collect_type_vars ()
          | _ ->
              (* Error: expected type var name *)
              let found_tok = peek parser in
              let diagnostic = Diagnostic.poly_type_missing_var_name
                ~found:found_tok
                ~text:(token_text parser found_tok)
                ~span:(current_span parser) in
              report_diagnostic parser diagnostic;
              ()
        )
    | _ ->
        (* Done collecting type vars *)
        ()
  in
  collect_type_vars ();
  (* Expect dot *)
  let dot_node, trivia_after_dot =
    if peek_kind parser = Token.Dot then
      let dot = consume parser in
      let trivia = consume_trivia parser in
      ([ make_token parser dot ], trivia)
    else
      (
        let found_tok = peek parser in
        let diagnostic = Diagnostic.poly_type_missing_dot
          ~found:found_tok
          ~text:(token_text parser found_tok)
          ~span:(expected_span parser) in
        report_diagnostic parser diagnostic;
        ([], [])
      )
  in
  (* Parse the actual type *)
  let inner_type = parse_arrow_type ~stop_before_arrow parser in
  (* Build the POLY_TYPE node *)
  let type_var_nodes =
    List.rev !type_vars
    |> List.mapi
      (fun i var ->
        let trivia =
          if i < List.length !type_vars_trivia then
            tokens_to_green parser (List.nth (List.rev !type_vars_trivia) i)
          else
            []
        in
        [ Ceibo.Green.Node var ] @ trivia)
    |> List.flatten
  in
  make_node
    Syntax_kind.POLY_TYPE
    (type_var_nodes
    @ dot_node
    @ tokens_to_green parser trivia_after_dot
    @ [ Ceibo.Green.Node inner_type ])

(** Parse locally abstract type: type a b. type_expr 
    Used in let bindings like: let f : type a. a -> a = ... *)
and parse_locally_abstract_type = fun ?(stop_before_arrow = false) parser ->
  (* Consume 'type' keyword *)
  let type_kw = consume parser in
  let trivia_after_type = consume_trivia parser in
  (* Collect type variable names (identifiers) *)
  let type_vars = ref [] in
  let type_vars_trivia = ref [] in
  let rec collect_vars () =
    match peek_kind parser with
    | Token.Ident _ ->
        let var_name = consume parser in
        type_vars := var_name :: !type_vars;
        let trivia = consume_trivia parser in
        type_vars_trivia := trivia :: !type_vars_trivia;
        collect_vars ()
    | _ -> ()
  in
  collect_vars ();
  (* Expect dot *)
  let dot, dot_trivia =
    if peek_kind parser = Token.Dot then
      let d = consume parser in
      let t = consume_trivia parser in
      ([ make_token parser d ], t)
    else
      (* Missing dot *)
      let found_tok = peek parser in
      let diagnostic = Diagnostic.poly_type_missing_dot
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      report_diagnostic parser diagnostic;
      ([], [])
  in
  (* Parse the actual type *)
  let inner_type = parse_arrow_type ~stop_before_arrow parser in
  (* Build type variable nodes *)
  let var_nodes =
    List.rev !type_vars
    |> List.mapi
      (fun i var ->
        let trivia =
          if i < List.length !type_vars_trivia then
            tokens_to_green parser (List.nth (List.rev !type_vars_trivia) i)
          else
            []
        in
        [ make_token parser var ] @ trivia)
    |> List.flatten
  in
  (* Build POLY_TYPE node *)
  make_node
    Syntax_kind.POLY_TYPE
    ([ make_token parser type_kw ]
    @ tokens_to_green parser trivia_after_type
    @ var_nodes
    @ dot
    @ tokens_to_green parser dot_trivia
    @ [ Ceibo.Green.Node inner_type ])

(** Parse the labeled prefix and domain portion shared by arrow syntaxes. *)
and parse_labeled_arrow_domain = fun parser ~parse_domain ->
  match peek_kind parser with
  | Token.Tilde
  | Token.Question ->
      let saved_pos = Token_cursor.position parser.cursor in
      let prefix_tok = consume parser in
      let trivia_after_prefix = consume_trivia parser in
      (
        match peek_kind parser with
        | Token.Ident _ ->
            let label = consume parser in
            let trivia_after_label = consume_trivia parser in
            (
              match peek_kind parser with
              | Token.Colon ->
                  let colon = consume parser in
                  let trivia_after_colon = consume_trivia parser in
                  let domain = parse_domain parser in
                  (
                    [ make_token parser prefix_tok ]
                    @ tokens_to_green parser trivia_after_prefix
                    @ [ make_token parser label ]
                    @ tokens_to_green parser trivia_after_label
                    @ [ make_token parser colon ]
                    @ tokens_to_green parser trivia_after_colon,
                    domain
                  )
              | _ ->
                  Token_cursor.set_position parser.cursor saved_pos;
                  ([], parse_domain parser)
            )
        | _ ->
            Token_cursor.set_position parser.cursor saved_pos;
            ([], parse_domain parser)
      )
  | Token.Ident _ ->
      let saved_pos = Token_cursor.position parser.cursor in
      let label = consume parser in
      let trivia_after_label = consume_trivia parser in
      (
        match peek_kind parser with
        | Token.Colon ->
            let colon = consume parser in
            let trivia_after_colon = consume_trivia parser in
            if peek_kind parser = Token.Gt then
              (
                Token_cursor.set_position parser.cursor saved_pos;
                ([], parse_domain parser)
              )
            else
              let domain = parse_domain parser in
              (
                [ make_token parser label ]
                @ tokens_to_green parser trivia_after_label
                @ [ make_token parser colon ]
                @ tokens_to_green parser trivia_after_colon,
                domain
              )
        | _ ->
            Token_cursor.set_position parser.cursor saved_pos;
            ([], parse_domain parser)
      )
  | _ ->
      ([], parse_domain parser)

(** Parse arrow type: t1 -> t2 -> t3 (right associative) 
    Also handles labeled params: label:t1 -> t2 and optional: ?label:t1 -> t2 *)
and parse_arrow_type = fun ?(stop_before_arrow = false) parser ->
  let label_nodes, type_to_parse = parse_labeled_arrow_domain parser ~parse_domain:parse_tuple_type in
  let left = type_to_parse in
  (* Speculatively consume trivia to check for -> *)
  let saved_pos = Token_cursor.position parser.cursor in
  let trivia_after_left = consume_trivia parser in
  if peek_kind parser = Token.Arrow && not stop_before_arrow then
    let arrow = consume parser in
    let trivia_after_arrow = consume_trivia parser in
    let right = parse_arrow_type parser in
    (* Right associative *)
    make_node
      Syntax_kind.TYPE_ARROW
      (label_nodes
      @ [ Ceibo.Green.Node left ]
      @ tokens_to_green parser trivia_after_left
      @ [ make_token parser arrow ]
      @ tokens_to_green parser trivia_after_arrow
      @ [ Ceibo.Green.Node right ])
  else
    (
      (* No arrow *)
      if label_nodes = [] then
        (
          (* No label and no arrow - just return the type *)
          Token_cursor.set_position parser.cursor saved_pos;
          left
        )
      else (
        (* We have a label but no arrow - this is an error in types *)
        (* But we'll return what we have *)
        Token_cursor.set_position parser.cursor saved_pos;
        left
      )
    )

(** Parse class type expressions.

    This shares the existing expression grammar for path/object/local-open
    shapes, but recognizes `core_type -> class_type` arrows explicitly. *)
and parse_class_type_expr = fun parser ->
  if can_start_class_type_arrow_parameter parser then
    let saved_pos = Token_cursor.position parser.cursor in
    let label_nodes, parameter_type = parse_labeled_arrow_domain parser ~parse_domain:parse_tuple_type in
    let trivia_after_parameter = consume_trivia parser in
    if peek_kind parser = Token.Arrow then
      let arrow = consume parser in
      let trivia_after_arrow = consume_trivia parser in
      let result_type = parse_class_type_expr parser in
      make_node
        Syntax_kind.TYPE_ARROW
        (label_nodes
        @ [ Ceibo.Green.Node parameter_type ]
        @ tokens_to_green parser trivia_after_parameter
        @ [ make_token parser arrow ]
        @ tokens_to_green parser trivia_after_arrow
        @ [ Ceibo.Green.Node result_type ])
      |> attach_postfix_attributes parser
    else (
      Token_cursor.set_position parser.cursor saved_pos;
      match peek_kind parser with
      | Token.OpenDelim Token.Paren ->
          let saved_pos = Token_cursor.position parser.cursor in
          let lparen = consume parser in
          let trivia_after_lparen = consume_trivia parser in
          let inner = parse_class_type_expr parser in
          let trivia_after_inner = consume_trivia parser in
          (
            match peek_kind parser with
            | Token.CloseDelim Token.Paren ->
                let rparen = consume parser in
                make_node
                  Syntax_kind.PAREN_EXPR
                  ([ make_token parser lparen ]
                  @ tokens_to_green parser trivia_after_lparen
                  @ [ Ceibo.Green.Node inner ]
                  @ tokens_to_green parser trivia_after_inner
                  @ [ make_token parser rparen ])
                |> attach_postfix_attributes parser
            | _ ->
                Token_cursor.set_position parser.cursor saved_pos;
                parse_expr parser
          )
      | _ -> parse_expr parser
    )
  else
    parse_expr parser

(** Parse tuple type: t1 * t2 * t3 *)
and parse_tuple_type = fun parser ->
  let first = parse_parametric_type parser in
  (* Speculatively consume trivia to check for * *)
  let saved_pos = Token_cursor.position parser.cursor in
  let trivia_after_first = consume_trivia parser in
  if peek_kind parser = Token.Star then
    let rec collect_tuple_types acc =
      if peek_kind parser = Token.Star then
        let star = consume parser in
        let trivia_after_star = consume_trivia parser in
        let next_type = parse_parametric_type parser in
        let trivia_after_type = consume_trivia parser in
        collect_tuple_types
          ([ Ceibo.Green.Node next_type ]
          @ tokens_to_green parser trivia_after_type
          @ [ make_token parser star ]
          @ tokens_to_green parser trivia_after_star
          @ acc)
      else
        List.rev acc
    in
    let rest = collect_tuple_types [] in
    make_node
      Syntax_kind.TYPE_TUPLE
      ([ Ceibo.Green.Node first ] @ tokens_to_green parser trivia_after_first @ rest)
  else (
    (* No star - restore position *)
    Token_cursor.set_position parser.cursor saved_pos;
    first
  )

(** Parse parametric type: t list, 'a option, ('a, 'b) map
    Handles chains like: int list option -> ((int list) option) *)
and parse_parametric_type = fun parser ->
  let rec parse_with_arg arg_type =
    let trivia_after_arg_start = position parser in
    let trivia_after_arg = consume_trivia parser in
    match peek_kind parser with
    | Token.Ident _ ->
        (* This might be: 'a list or int Module.t *)
        (* Parse module path for the type constructor *)
        let rec parse_type_path acc =
          match peek_kind parser with
          | Token.Ident _ ->
              let ident = consume parser in
              let trivia = consume_trivia parser in
              let new_acc = acc @ [ make_token parser ident ] @ tokens_to_green parser trivia in
              (* Check for dot (module path) *)
              if peek_kind parser = Token.Dot then
                let dot = consume parser in
                let trivia2 = consume_trivia parser in
                parse_type_path
                  (new_acc @ [ make_token parser dot ] @ tokens_to_green parser trivia2)
              else
                new_acc
          | _ -> acc
        in
        let constr_path = parse_type_path [] in
        let applied_type = make_node
          Syntax_kind.TYPE_CONSTR
          ([ Ceibo.Green.Node arg_type ] @ tokens_to_green parser trivia_after_arg @ constr_path) in
        (* Recursively check for more applications (e.g., int list option) *)
        parse_with_arg applied_type
    | Token.Hash ->
        let hash = consume parser in
        let trivia_after_hash = consume_trivia parser in
        let class_type_name =
          match peek_kind parser with
          | Token.Ident _ ->
              let ident = consume parser in
              [ make_token parser hash ]
              @ tokens_to_green parser trivia_after_hash
              @ [ make_token parser ident ]
          | _ ->
              let found = peek parser in
              let diagnostic = Diagnostic.invalid_type_expression
                ~found
                ~text:(token_text parser found)
                ~span:(expected_span parser) in
              report_diagnostic parser diagnostic;
              [ make_token parser hash ]
        in
        let applied_type = make_node
          Syntax_kind.TYPE_CONSTR
          ([ Ceibo.Green.Node arg_type ] @ tokens_to_green parser trivia_after_arg @ class_type_name) in
        parse_with_arg applied_type
    | _ ->
        (* No more constructors *)
        let () = Token_cursor.set_position parser.cursor trivia_after_arg_start in
        arg_type
  in
  let primary = parse_primary_type parser in
  parse_with_arg primary

(** Parse primary type: type variable, type constructor, or parenthesized type *)
and parse_primary_type = fun parser ->
  match peek_kind parser with
  | Token.Quote ->
      parse_type_variable parser
  | Token.Underscore ->
      (* Wildcard type: _ *)
      let underscore = consume parser in
      make_node Syntax_kind.TYPE_VAR [ make_token parser underscore ]
  | Token.Ident _ ->
      (* Type constructor name: int, string, list, Module.t, Module.Sub.t, etc. *)
      let rec parse_type_path acc =
        match peek_kind parser with
        | Token.Ident _ ->
            let ident = consume parser in
            let trivia = consume_trivia parser in
            let new_acc = acc @ [ make_token parser ident ] @ tokens_to_green parser trivia in
            (* Check for dot (module path) *)
            if peek_kind parser = Token.Dot then
              let dot = consume parser in
              let trivia2 = consume_trivia parser in
              parse_type_path (new_acc @ [ make_token parser dot ] @ tokens_to_green parser trivia2)
            else
              new_acc
        | _ -> acc
      in
      let path = parse_type_path [] in
      make_node Syntax_kind.TYPE_CONSTR path
  | Token.Keyword Keyword.True
  | Token.Keyword Keyword.False ->
      let bool_kw = consume parser in
      make_node Syntax_kind.TYPE_CONSTR [ make_token parser bool_kw ]
  | Token.Hash ->
      let hash = consume parser in
      let trivia_after_hash = consume_trivia parser in
      let path =
        match peek_kind parser with
        | Token.Ident _ ->
            let ident = consume parser in
            [ make_token parser hash ]
            @ tokens_to_green parser trivia_after_hash
            @ [ make_token parser ident ]
        | _ ->
            let found = peek parser in
            let diagnostic = Diagnostic.invalid_type_expression
              ~found
              ~text:(token_text parser found)
              ~span:(expected_span parser) in
            make_error_node parser ~diagnostic ~consumed_tokens:[ hash ]
            |> fun error -> [ Ceibo.Green.Node error ]
      in
      make_node Syntax_kind.TYPE_CONSTR path
  | Token.OpenDelim Token.Bracket ->
      if is_extension_start parser then
        parse_extension parser
      else
        parse_poly_variant_type parser
  | Token.OpenDelim Token.Paren ->
      (* Parenthesized type, tuple type, or first-class module type *)
      let open_paren = consume parser in
      let trivia_after_open = consume_trivia parser in
      (* Check if this is a first-class module type: (module S) or (module S with ...) *)
      (
        match peek_kind parser with
        | Token.Keyword Keyword.Module ->
            let module_kw = consume parser in
            let trivia_after_module = consume_trivia parser in
            (* Parse module type path *)
            let module_type_path = parse_module_type_path parser in
            let trivia_after_path = consume_trivia parser in
            (* Handle optional 'with type ...' constraints *)
            let constraint_children =
              if peek_kind parser = Token.Keyword Keyword.With then
                let with_kw = consume parser in
                let trivia_after_with = consume_trivia parser in
                [ make_token parser with_kw ]
                @ tokens_to_green parser trivia_after_with
                @ parse_module_type_constraints parser
              else if peek_kind parser = Token.Keyword Keyword.Type then
                let found = peek parser in
                let diagnostic = Diagnostic.missing_with_keyword
                  ~found
                  ~text:(token_text parser found)
                  ~span:(point_span found.Token.span.start) in
                report_diagnostic parser diagnostic;
                parse_module_type_constraints parser
              else
                []
            in
            let close_paren =
              expect
                parser
                (Token.CloseDelim Token.Paren)
                (fun found ->
                  Diagnostic.unclosed_delimiter
                    ~opener:"("
                    ~found
                    ~text:(token_text parser found)
                    ~span:(expected_span parser))
            in
            make_node
              Syntax_kind.FIRST_CLASS_MODULE_TYPE
              ([ make_token parser open_paren ]
              @ tokens_to_green parser trivia_after_open
              @ [ make_token parser module_kw ]
              @ tokens_to_green parser trivia_after_module
              @ [ Ceibo.Green.Node module_type_path ]
              @ tokens_to_green parser trivia_after_path
              @ constraint_children
              @ [ make_token parser close_paren ])
        | _ ->
            (* Parse type expression inside - could be single type or tuple of type arguments *)
            let first_type = parse_typexpr parser in
            let trivia_after_first = consume_trivia parser in
            (* Check if this is a tuple of type arguments (comma-separated) *)
            if peek_kind parser = Token.Comma then
              let rec parse_type_args acc =
                if peek_kind parser = Token.Comma then
                  let comma = consume parser in
                  let trivia_after_comma = consume_trivia parser in
                  let next_type = parse_typexpr parser in
                  let trivia_after_type = consume_trivia parser in
                  parse_type_args
                    (acc
                    @ [ make_token parser comma ]
                    @ tokens_to_green parser trivia_after_comma
                    @ [ Ceibo.Green.Node next_type ]
                    @ tokens_to_green parser trivia_after_type)
                else
                  acc
              in
              let rest_args = parse_type_args [] in
              let trivia_before_close = consume_trivia parser in
              let close_paren =
                expect
                  parser
                  (Token.CloseDelim Token.Paren)
                  (fun found ->
                    Diagnostic.unclosed_delimiter
                      ~opener:")"
                      ~found
                      ~text:(token_text parser found)
                      ~span:(expected_span parser))
              in
              (* Return tuple of type arguments *)
              make_node
                Syntax_kind.TYPE_TUPLE
                ([ make_token parser open_paren ]
                @ tokens_to_green parser trivia_after_open
                @ [ Ceibo.Green.Node first_type ]
                @ tokens_to_green parser trivia_after_first
                @ rest_args
                @ tokens_to_green parser trivia_before_close
                @ [ make_token parser close_paren ])
            else
              (* Single parenthesized type *)
              let close_paren =
                expect
                  parser
                  (Token.CloseDelim Token.Paren)
                  (fun found ->
                    Diagnostic.unclosed_delimiter
                      ~opener:")"
                      ~found
                      ~text:(token_text parser found)
                      ~span:(expected_span parser))
              in
              make_node
                Syntax_kind.TYPE_CONSTR
                ([ make_token parser open_paren ]
                @ tokens_to_green parser trivia_after_open
                @ [ Ceibo.Green.Node first_type ]
                @ tokens_to_green parser trivia_after_first
                @ [ make_token parser close_paren ])
      )
  | Token.OpenDelim Token.Brace ->
      if is_brace_extension_start parser then
        parse_extension parser
      else
        parse_record_type parser
  | Token.Lt ->
      parse_object_type parser
  | _ ->
      (* Error: invalid type expression *)
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_type_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

and parse_module_type_constraints = fun parser ->
  let missing_constraint_rhs_boundary parser =
    match peek_kind parser with
    | Token.EOF
    | Token.Keyword Keyword.And
    | Token.CloseDelim Token.Paren -> true
    | _ -> false
  in
  let rec parse_constraints acc =
    match peek_kind parser with
    | Token.Keyword Keyword.Type ->
        let type_kw = consume parser in
        let trivia_after_type = consume_trivia parser in
        let constrained_type_children =
          match peek_kind parser with
          | Token.Eq
          | Token.ColonEq
          | Token.Keyword Keyword.And
          | Token.CloseDelim Token.Paren
          | Token.EOF ->
              let found = peek parser in
              let diagnostic = Diagnostic.missing_type_name
                ~found
                ~text:(token_text parser found)
                ~span:(point_span found.Token.span.start) in
              report_diagnostic parser diagnostic;
              []
          | _ ->
              let constrained_type = parse_typexpr parser in
              [ Ceibo.Green.Node constrained_type ]
        in
        let trivia_after_constrained_type = consume_trivia parser in
        let eq_children =
          if peek_kind parser = Token.Eq || peek_kind parser = Token.ColonEq then
            let eq_tok = consume parser in
            [ make_token parser eq_tok ]
          else
            let found = peek parser in
            let diagnostic = Diagnostic.missing_type_decl_equals
              ~found
              ~text:(token_text parser found)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            []
        in
        let trivia_after_eq = consume_trivia parser in
        let type_children =
          if eq_children = [] && missing_constraint_rhs_boundary parser then
            []
          else
            let typ = parse_typexpr parser in
            let trivia_after_typ = consume_trivia parser in
            [ Ceibo.Green.Node typ ] @ tokens_to_green parser trivia_after_typ
        in
        let constraint_node = make_node
          Syntax_kind.TYPE_CONSTRAINT
          ([ make_token parser type_kw ]
          @ tokens_to_green parser trivia_after_type
          @ constrained_type_children
          @ tokens_to_green parser trivia_after_constrained_type
          @ eq_children
          @ tokens_to_green parser trivia_after_eq
          @ type_children) in
        if peek_kind parser = Token.Keyword Keyword.And then
          let and_kw = consume parser in
          let trivia_after_and = consume_trivia parser in
          parse_constraints
            (acc
            @ [ Ceibo.Green.Node constraint_node ]
            @ [ make_token parser and_kw ]
            @ tokens_to_green parser trivia_after_and)
        else
          acc @ [ Ceibo.Green.Node constraint_node ]
    | _ -> acc
  in
  parse_constraints []

(** Parse polymorphic variant type: [ `A | `B of int ] or [> `A ] or [< `A | `B ] *)
and parse_poly_variant_type = fun parser ->
  match peek_kind parser with
  | Token.OpenDelim Token.Bracket ->
      let open_bracket = consume parser in
      let trivia_after_open = consume_trivia parser in
      (* Check for [> (open) or [< (closed with constraint) *)
      let variant_kind, variant_kind_trivia =
        match peek_kind parser with
        | Token.Gt ->
            (* [> ... ] - open variant (can have more tags) *)
            let gt = consume parser in
            let trivia = consume_trivia parser in
            (Some (`Open, gt), trivia)
        | Token.Lt ->
            (* [< ... ] - closed variant with upper bound *)
            let lt = consume parser in
            let trivia = consume_trivia parser in
            (Some (`Closed, lt), trivia)
        | _ -> (* [ ... ] - exact variant (closed) *)
          (None, [])
      in
      (* Parse variant row fields in source order, preserving explicit separators. *)
      let push_children acc children = List.rev_append children acc in
      let rec parse_fields acc =
        match peek_kind parser with
        | Token.CloseDelim Token.Bracket -> acc
        | _ ->
            let acc =
              match peek_kind parser with
              | Token.Pipe ->
                  let pipe = consume parser in
                  let trivia_after_pipe = consume_trivia parser in
                  push_children
                    acc
                    ([ make_token parser pipe ] @ tokens_to_green parser trivia_after_pipe)
              | _ -> acc
            in
            let acc, consumed_field =
              match peek_kind parser with
              | Token.Backtick ->
                  let tag = parse_poly_variant_tag parser in
                  (push_children acc [ Ceibo.Green.Node tag ], true)
              | Token.Ident _ ->
                  let typ = parse_typexpr parser in
                  (push_children acc [ Ceibo.Green.Node typ ], true)
              | _ ->
                  (acc, false)
            in
            if consumed_field then
              let trivia_after_field = consume_trivia parser in
              parse_fields (push_children acc (tokens_to_green parser trivia_after_field))
            else
              acc
      in
      let field_children = parse_fields [] |> List.rev in
      let close_bracket =
        expect
          parser
          (Token.CloseDelim Token.Bracket)
          (fun found ->
            Diagnostic.unclosed_delimiter
              ~opener:"["
              ~found
              ~text:(token_text parser found)
              ~span:(expected_span parser))
      in
      (* Build children *)
      let variant_kind_children =
        match variant_kind with
        | Some (`Open, tok) -> [ make_token parser tok ] @ tokens_to_green parser variant_kind_trivia
        | Some (`Closed, tok) -> [ make_token parser tok ] @ tokens_to_green parser variant_kind_trivia
        | None -> []
      in
      make_node
        Syntax_kind.TYPE_POLY_VARIANT
        ([ make_token parser open_bracket ]
        @ tokens_to_green parser trivia_after_open
        @ variant_kind_children
        @ field_children
        @ [ make_token parser close_bracket ])
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_type_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse a single polymorphic variant tag: `Tag or `Tag of type *)
and parse_poly_variant_tag = fun parser ->
  match peek_kind parser with
  | Token.Backtick ->
      let backtick = consume parser in
      let trivia_after_backtick = consume_trivia parser in
      (* Expect tag name (identifier) *)
      let tag_name =
        match peek_kind parser with
        | Token.Ident _ ->
            let ident = consume parser in
            make_token parser ident
        | _ ->
            let found_tok = peek parser in
            let diagnostic = Diagnostic.invalid_type_expression
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            make_token parser found_tok
      in
      let trivia_after_name = consume_trivia parser in
      (* Check for 'of' and type *)
      let of_parts =
        match peek_kind parser with
        | Token.Keyword Keyword.Of ->
            let of_kw = consume parser in
            let trivia_after_of = consume_trivia parser in
            let typ = parse_typexpr parser in
            [ make_token parser of_kw ]
            @ tokens_to_green parser trivia_after_of
            @ [ Ceibo.Green.Node typ ]
        | _ -> []
      in
      make_node
        Syntax_kind.POLY_VARIANT_TAG
        ([ make_token parser backtick ]
        @ tokens_to_green parser trivia_after_backtick
        @ [ tag_name ]
        @ tokens_to_green parser trivia_after_name
        @ of_parts)
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_type_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse record type: { field1: type1; field2: type2 } *)
and parse_record_type = fun parser ->
  match peek_kind parser with
  | Token.OpenDelim Token.Brace ->
      let open_brace = consume parser in
      let trivia_after_open = consume_trivia parser in
      let field_children = ref [] in
      let rec parse_fields () =
        match peek_kind parser with
        | Token.CloseDelim Token.Brace ->
            (* End of record *)
            ()
        | Token.Keyword Keyword.Mutable
        | Token.Ident _ ->
            (* Parse field: [mutable] name : type *)
            (* Check for optional mutable keyword *)
            let mutable_kw, trivia_after_mutable =
              match peek_kind parser with
              | Token.Keyword Keyword.Mutable ->
                  let mutable_tok = consume parser in
                  let trivia = consume_trivia parser in
                  (Some mutable_tok, trivia)
              | _ -> (None, [])
            in
            (* Parse field name *)
            let field_name, field_name_text =
              match peek_kind parser with
              | Token.Ident _ ->
                  let tok = consume parser in
                  let text = token_text parser tok in
                  (tok, text)
              | _ ->
                  (* Error: expected field name (possibly after mutable) *)
                  let found_tok = peek parser in
                  let diagnostic =
                    if Option.is_some mutable_kw then
                      Diagnostic.mutable_field_missing_name
                        ~found:found_tok
                        ~text:(token_text parser found_tok)
                        ~span:(expected_span parser)
                    else
                      Diagnostic.invalid_type_expression
                        ~found:found_tok
                        ~text:(token_text parser found_tok)
                        ~span:(expected_span parser)
                  in
                  report_diagnostic parser diagnostic;
                  (found_tok, token_text parser found_tok)
            in
            let trivia_after_name = consume_trivia parser in
            (* Expect colon *)
            let colon =
              expect
                parser
                Token.Colon
                (fun found ->
                  Diagnostic.record_field_missing_colon
                    ~field_name:field_name_text
                    ~found
                    ~text:(token_text parser found)
                    ~span:(expected_span parser))
            in
            let trivia_after_colon = consume_trivia parser in
            (* Parse field type *)
            let field_type = parse_typexpr parser in
            let trivia_after_type = consume_trivia parser in
            (* Build field node *)
            let mutable_parts =
              match mutable_kw with
              | Some tok -> [ make_token parser tok ] @ tokens_to_green parser trivia_after_mutable
              | None -> []
            in
            let field_node = make_node
              Syntax_kind.TYPE_RECORD_FIELD
              (mutable_parts
              @ [ make_token parser field_name ]
              @ tokens_to_green parser trivia_after_name
              @ [ make_token parser colon ]
              @ tokens_to_green parser trivia_after_colon
              @ [ Ceibo.Green.Node field_type ]
              @ tokens_to_green parser trivia_after_type) in
            field_children := !field_children @ [ Ceibo.Green.Node field_node ];
            (* Check for semicolon separator *)
            (
              match peek_kind parser with
              | Token.Semi ->
                  let semi = consume parser in
                  let trivia_after_semi = consume_trivia parser in
                  field_children := !field_children
                  @ [ make_token parser semi ]
                  @ tokens_to_green parser trivia_after_semi;
                  parse_fields ()
              | Token.CloseDelim Token.Brace ->
                  (* End of record, no more fields *)
                  ()
              | _ ->
                  (* Missing semicolon or closing brace *)
                  let found_tok = peek parser in
                  let diagnostic = Diagnostic.invalid_type_expression
                    ~found:found_tok
                    ~text:(token_text parser found_tok)
                    ~span:(expected_span parser) in
                  report_diagnostic parser diagnostic;
                  (* Try to continue parsing *)
                  parse_fields ()
            )
        | Token.Semi ->
            (* Extra semicolon, consume and continue *)
            let semi = consume parser in
            let trivia_after_semi = consume_trivia parser in
            field_children := !field_children @ [ make_token parser semi ] @ tokens_to_green parser trivia_after_semi;
            parse_fields ()
        | _ ->
            (* Error or end *)
            ()
      in
      parse_fields ();
      (* Expect closing brace *)
      let close_brace =
        expect
          parser
          (Token.CloseDelim Token.Brace)
          (fun found ->
            Diagnostic.unclosed_delimiter
              ~opener:"}"
              ~found
              ~text:(token_text parser found)
              ~span:(expected_span parser))
      in
      make_node
        Syntax_kind.TYPE_RECORD
        ([ make_token parser open_brace ]
        @ tokens_to_green parser trivia_after_open
        @ !field_children
        @ [ make_token parser close_brace ])
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_type_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** *
    ============================================================================
    * GRAMMAR SECTION 6: PATTERNS *
    ============================================================================
*)
(** Parse pattern - for now just identifiers *)
and parse_pattern = fun parser ->
  let base = parse_or_pattern parser in
  attach_postfix_attributes parser base

(** Parse a pattern while reserving top-level commas for surrounding syntax. *)
and parse_pattern_no_tuple = fun parser ->
  let base = parse_or_pattern_no_tuple parser in
  attach_postfix_attributes parser base

(** Parse or-pattern: p1 | p2 | p3 *)
and parse_or_pattern = fun parser ->
  let first_pat = parse_tuple_pattern_or_as parser in
  (* Check for | to continue or-pattern *)
  if peek_kind parser = Token.Pipe then
    let trivia_after_first = consume_trivia parser in
    (* Parse remaining patterns after | *)
    let rec parse_pipe_patterns acc =
      let trivia_before_pipe = consume_trivia parser in
      if peek_kind parser = Token.Pipe then
        let pipe = consume parser in
        let trivia_after_pipe = consume_trivia parser in
        (* Check if we have a valid pattern after | *)
        let extra_children, pat =
          if can_start_pattern parser then
            ([], parse_tuple_pattern_or_as parser)
          else if peek_kind parser = Token.Pipe then
            (
              (* Double pipe || - missing pattern between pipes *)
              let extra_pipe = consume parser in
              let trivia_after_extra_pipe = consume_trivia parser in
              let diagnostic = Diagnostic.or_pattern_double
                ~found:extra_pipe
                ~text:(token_text parser extra_pipe)
                ~span:extra_pipe.Token.span in
              let error_node = make_error_node parser ~diagnostic ~consumed_tokens:[ extra_pipe ] in
              let extra_children = [ Ceibo.Green.Node error_node ] @ tokens_to_green parser trivia_after_extra_pipe in
              let pat =
                if can_start_pattern parser then
                  parse_tuple_pattern_or_as parser
                else
                  make_node Syntax_kind.ERROR []
              in
              (extra_children, pat)
            )
          else
            (* Some other invalid token - missing pattern after pipe *)
            let found_tok = peek parser in
            let diagnostic = Diagnostic.or_pattern_missing
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            ([], make_node Syntax_kind.ERROR [])
        in
        (* Collect trivia before pipe, pipe token, and trivia after pipe as green elements *)
        let pipe_trivia_green = tokens_to_green parser trivia_before_pipe
        @ [ make_token parser pipe ]
        @ tokens_to_green parser trivia_after_pipe in
        parse_pipe_patterns (acc @ pipe_trivia_green @ extra_children @ [ Ceibo.Green.Node pat ])
      else
        acc @ tokens_to_green parser trivia_before_pipe
    in
    let rest_children = parse_pipe_patterns [] in
    (* Build OR_PATTERN node with all patterns interleaved with pipes *)
    let children = [ Ceibo.Green.Node first_pat ] @ tokens_to_green parser trivia_after_first @ rest_children in
    make_node Syntax_kind.OR_PATTERN children
  else
    first_pat

(** Parse or-patterns without allowing top-level tuple commas. *)
and parse_or_pattern_no_tuple = fun parser ->
  let first_pat = parse_cons_pattern parser in
  if peek_kind parser = Token.Pipe then
    let trivia_after_first = consume_trivia parser in
    let rec parse_pipe_patterns acc =
      let trivia_before_pipe = consume_trivia parser in
      if peek_kind parser = Token.Pipe then
        let pipe = consume parser in
        let trivia_after_pipe = consume_trivia parser in
        let pat =
          if can_start_pattern parser then
            parse_cons_pattern parser
          else
            let found_tok = peek parser in
            let diagnostic = Diagnostic.or_pattern_missing
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            make_node Syntax_kind.ERROR []
        in
        let pipe_trivia_green = tokens_to_green parser trivia_before_pipe
        @ [ make_token parser pipe ]
        @ tokens_to_green parser trivia_after_pipe in
        parse_pipe_patterns (acc @ pipe_trivia_green @ [ Ceibo.Green.Node pat ])
      else
        acc @ tokens_to_green parser trivia_before_pipe
    in
    let rest_children = parse_pipe_patterns [] in
    make_node
      Syntax_kind.OR_PATTERN
      ([ Ceibo.Green.Node first_pat ] @ tokens_to_green parser trivia_after_first @ rest_children)
  else
    first_pat

(** Parse a labeled tuple pattern element such as `~x`, `~x:pat`, or `~(x : t)`. *)
and parse_labeled_tuple_pattern_element = fun parser ->
  let invalid_pattern found = Diagnostic.invalid_pattern
    ~found
    ~text:(token_text parser found)
    ~span:(current_span parser) in
  let tilde =
    expect parser Token.Tilde (fun found -> invalid_pattern found)
  in
  let trivia_after_tilde = consume_trivia parser in
  match peek_kind parser with
  | Token.OpenDelim Token.Paren ->
      let open_paren = consume parser in
      let trivia_after_open = consume_trivia parser in
      let label =
        parse_ident parser (fun parser found -> invalid_pattern found)
      in
      let label_pattern = make_node Syntax_kind.IDENT_PATTERN [ make_token parser label ] in
      let trivia_after_label = consume_trivia parser in
      let colon =
        expect parser Token.Colon (fun found -> invalid_pattern found)
      in
      let trivia_after_colon = consume_trivia parser in
      let type_expr = parse_typexpr parser in
      let trivia_after_type = consume_trivia parser in
      let close_paren =
        expect
          parser
          (Token.CloseDelim Token.Paren)
          (fun found ->
            Diagnostic.unclosed_delimiter
              ~opener:")"
              ~found
              ~text:(token_text parser found)
              ~span:(expected_span parser))
      in
      let typed_pattern =
        make_node
          Syntax_kind.TYPED_PATTERN
          ([ make_token parser open_paren ]
          @ tokens_to_green parser trivia_after_open
          @ [ Ceibo.Green.Node label_pattern ]
          @ tokens_to_green parser trivia_after_label
          @ [ make_token parser colon ]
          @ tokens_to_green parser trivia_after_colon
          @ [ Ceibo.Green.Node type_expr ]
          @ tokens_to_green parser trivia_after_type
          @ [ make_token parser close_paren ])
      in
      (
        [ make_token parser tilde ]
        @ tokens_to_green parser trivia_after_tilde
        @ [ Ceibo.Green.Node typed_pattern ],
        None,
        true
      )
  | _ ->
      let label =
        parse_ident parser (fun parser found -> invalid_pattern found)
      in
      let label_pattern = make_node Syntax_kind.IDENT_PATTERN [ make_token parser label ] in
      let trivia_after_label = consume_trivia parser in
      if peek_kind parser = Token.Colon then
        let colon = consume parser in
        let trivia_after_colon = consume_trivia parser in
        let payload_pattern = parse_primary_pattern parser in
        (
          [ make_token parser tilde ]
          @ tokens_to_green parser trivia_after_tilde
          @ [ Ceibo.Green.Node label_pattern ]
          @ tokens_to_green parser trivia_after_label
          @ [ make_token parser colon ]
          @ tokens_to_green parser trivia_after_colon
          @ [ Ceibo.Green.Node payload_pattern ],
          None,
          true
        )
      else
        (
          [ make_token parser tilde ]
          @ tokens_to_green parser trivia_after_tilde
          @ [ Ceibo.Green.Node label_pattern ]
          @ tokens_to_green parser trivia_after_label,
          None,
          true
        )

(** Parse one tuple pattern element while keeping enough information to
    distinguish labeled tuple syntax from ordinary standalone patterns. *)
and parse_tuple_pattern_element = fun parser ->
  match peek_kind parser with
  | Token.Tilde -> parse_labeled_tuple_pattern_element parser
  | _ ->
      let pattern = parse_cons_pattern parser in
      ([ Ceibo.Green.Node pattern ], Some pattern, false)

(** Parse tuple pattern without parentheses: a, b, c *)
and parse_tuple_pattern_or_as = fun parser ->
  let first_children, first_pattern_opt, first_requires_tuple = parse_tuple_pattern_element parser in
  (* Check if followed by comma (tuple without parens) *)
  if peek_kind parser = Token.Comma then
    let trivia_after_first = consume_trivia parser in
    (* Parse rest of tuple elements *)
    let rec parse_tuple_rest acc =
      match peek_kind parser with
      | Token.Comma ->
          let comma = consume parser in
          let trivia_after_comma = consume_trivia parser in
          if peek_kind parser = Token.DotDot then
            let dotdot = consume parser in
            let trivia_after_dotdot = consume_trivia parser in
            acc
            @ [ make_token parser comma ]
            @ tokens_to_green parser trivia_after_comma
            @ [ make_token parser dotdot ]
            @ tokens_to_green parser trivia_after_dotdot
          else
            let element_children, _, _ = parse_tuple_pattern_element parser in
            let trivia_after_element = consume_trivia parser in
            parse_tuple_rest
              (acc
              @ [ make_token parser comma ]
              @ tokens_to_green parser trivia_after_comma
              @ element_children
              @ tokens_to_green parser trivia_after_element)
      | _ -> acc
    in
    let rest = parse_tuple_rest [] in
    make_node
      Syntax_kind.TUPLE_PATTERN
      (first_children @ tokens_to_green parser trivia_after_first @ rest)
  else
    match first_pattern_opt with
    | Some first_pattern -> first_pattern
    | None ->
        if first_requires_tuple then
          (
            let found_tok = peek parser in
            let diagnostic = Diagnostic.invalid_pattern
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            make_node Syntax_kind.ERROR []
          )
        else
          make_node Syntax_kind.ERROR []

(** Parse as-pattern: p as x *)
and parse_as_pattern = fun parser ->
  let left = parse_primary_pattern parser in
  (* Check for 'as' keyword *)
  match peek_kind parser with
  | Token.Keyword Keyword.As ->
      let trivia_after_left = consume_trivia parser in
      (* Found 'as', consume it and parse identifier *)
      let as_keyword = consume parser in
      let trivia_after_as = consume_trivia parser in
      (* Parse the identifier after 'as' *)
      let ident =
        match peek_kind parser with
        | Token.Ident _ -> consume parser
        | _ ->
            let found = peek parser in
            let diagnostic = Diagnostic.invalid_pattern
              ~found
              ~text:(token_text parser found)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            found
      in
      make_node
        Syntax_kind.AS_PATTERN
        ([ Ceibo.Green.Node left ]
        @ tokens_to_green parser trivia_after_left
        @ [ make_token parser as_keyword ]
        @ tokens_to_green parser trivia_after_as
        @ [ make_token parser ident ])
  | _ -> left

(** Parse cons pattern: x :: xs *)
and parse_cons_pattern = fun parser ->
  let left = parse_as_pattern parser in
  if peek_kind parser = Token.ColonColon then
    let trivia_after_left = consume_trivia parser in
    (* Found ::, consume it and parse right side *)
    let cons_op = consume parser in
    let trivia_after_cons = consume_trivia parser in
    (* Check if we have a valid pattern for the tail *)
    let right =
      if can_start_pattern parser then
        parse_cons_pattern parser
      else
        (* Missing tail pattern *)
        let found_tok = peek parser in
        let diagnostic = Diagnostic.cons_pattern_missing_tail
          ~found:found_tok
          ~text:(token_text parser found_tok)
          ~span:(expected_span parser) in
        report_diagnostic parser diagnostic;
        make_node Syntax_kind.ERROR []
    in
    make_node
      Syntax_kind.CONS_PATTERN
      ([ Ceibo.Green.Node left ]
      @ tokens_to_green parser trivia_after_left
      @ [ make_token parser cons_op ]
      @ tokens_to_green parser trivia_after_cons
      @ [ Ceibo.Green.Node right ])
  else
    left

(** Parse primary pattern: literals, identifiers, constructors, tuples, lists,
    etc. *)
and parse_effect_pattern = fun parser ->
  let saved_pos = Token_cursor.position parser.cursor in
  match peek_kind parser with
  | Token.Ident _ when String.equal (token_text parser (peek parser)) "effect" ->
      let effect_ident = consume parser in
      let trivia_after_effect = consume_trivia parser in
      if not (can_start_pattern parser) then
        (
          Token_cursor.set_position parser.cursor saved_pos;
          None
        )
      else
        let effect_pattern = parse_pattern_no_tuple parser in
        let trivia_after_payload = consume_trivia parser in
        if not (peek_kind parser = Token.Comma) then
          (
            Token_cursor.set_position parser.cursor saved_pos;
            None
          )
        else
          let comma = consume parser in
          let trivia_after_comma = consume_trivia parser in
          let continuation_pattern =
            if can_start_pattern parser then
              parse_pattern parser
            else
              let found_tok = peek parser in
              let diagnostic = Diagnostic.invalid_pattern
                ~found:found_tok
                ~text:(token_text parser found_tok)
                ~span:(expected_span parser) in
              report_diagnostic parser diagnostic;
              make_node Syntax_kind.ERROR []
          in
          Some (make_node
            Syntax_kind.EFFECT_PATTERN
            ([ make_token parser effect_ident ]
            @ tokens_to_green parser trivia_after_effect
            @ [ Ceibo.Green.Node effect_pattern ]
            @ tokens_to_green parser trivia_after_payload
            @ [ make_token parser comma ]
            @ tokens_to_green parser trivia_after_comma
            @ [ Ceibo.Green.Node continuation_pattern ]))
  | _ -> None

and parse_ident_or_constructor_pattern = fun parser ->
  let ident = consume parser in
  let text = token_text parser ident in
  if ident_starts_uppercase text then
    let make_constructor_pattern path_segments trivia_after_path =
      let existential_nodes =
        match parse_locally_abstract_types parser with
        | Some existentials ->
            let trivia_after_existentials = consume_trivia parser in
            [ Ceibo.Green.Node existentials ] @ tokens_to_green parser trivia_after_existentials
        | None -> []
      in
      let argument_nodes =
        if can_start_pattern_arg parser then
          let arg = parse_primary_pattern parser in
          [ Ceibo.Green.Node arg ]
        else
          []
      in
      make_node
        Syntax_kind.CONSTRUCTOR_PATTERN
        (path_segments @ tokens_to_green parser trivia_after_path @ existential_nodes @ argument_nodes)
    in
    let rec collect_path_segments acc =
      match peek_kind parser with
      | Token.Dot -> (
          match (peek_n parser 1).Token.kind with
          | Token.Ident _ ->
              let trivia_after = consume_trivia parser in
              let dot = consume parser in
              let trivia_after_dot = consume_trivia parser in
              let next_ident = consume parser in
              collect_path_segments
                (acc
                @ tokens_to_green parser trivia_after
                @ [ make_token parser dot ]
                @ tokens_to_green parser trivia_after_dot
                @ [ make_token parser next_ident ])
          | Token.OpenDelim Token.Paren
          | Token.OpenDelim Token.Brace ->
              (acc, consume_trivia parser)
          | _ ->
              (acc, consume_trivia parser)
        )
      | _ -> (acc, consume_trivia parser)
    in
    let path_segments, trivia_after_path = collect_path_segments [ make_token parser ident ] in
    (* Local open pattern: Module.(pattern) *)
    match peek_kind parser, (peek_n parser 1).Token.kind with
    | Token.Dot, Token.OpenDelim Token.Paren ->
        let dot = consume parser in
        let trivia_after_dot = consume_trivia parser in
        let open_paren = consume parser in
        let trivia_after_open = consume_trivia parser in
        let inner_pattern = parse_pattern parser in
        let trivia_before_close = consume_trivia parser in
        let close_paren =
          expect
            parser
            (Token.CloseDelim Token.Paren)
            (fun found ->
              Diagnostic.unclosed_delimiter
                ~opener:")"
                ~found
                ~text:(token_text parser found)
                ~span:(expected_span parser))
        in
        let module_path = make_node Syntax_kind.MODULE_PATH path_segments in
        make_node
          Syntax_kind.LOCAL_OPEN_PATTERN
          ([ Ceibo.Green.Node module_path ]
          @ tokens_to_green parser trivia_after_path
          @ [ make_token parser dot ]
          @ tokens_to_green parser trivia_after_dot
          @ [ make_token parser open_paren ]
          @ tokens_to_green parser trivia_after_open
          @ [ Ceibo.Green.Node inner_pattern ]
          @ tokens_to_green parser trivia_before_close
          @ [ make_token parser close_paren ])
    | Token.Dot, Token.OpenDelim Token.Brace ->
        let dot = consume parser in
        let trivia_after_dot = consume_trivia parser in
        let inner_pattern = parse_record_pattern parser in
        let module_path = make_node Syntax_kind.MODULE_PATH path_segments in
        make_node
          Syntax_kind.LOCAL_OPEN_PATTERN
          ([ Ceibo.Green.Node module_path ]
          @ tokens_to_green parser trivia_after_path
          @ [ make_token parser dot ]
          @ tokens_to_green parser trivia_after_dot
          @ [ Ceibo.Green.Node inner_pattern ])
    | _ ->
        make_constructor_pattern path_segments trivia_after_path
  else
    make_node Syntax_kind.IDENT_PATTERN [ make_token parser ident ]

and parse_primary_pattern = fun parser ->
  match peek_kind parser with
  | Token.Minus
  | Token.Plus
  | Token.MinusDot
  | Token.PlusDot ->
      let sign = consume parser in
      let trivia_after_sign = consume_trivia parser in
      let signed_literal =
        match (sign.Token.kind, peek_kind parser) with
        | ((Token.Minus | Token.Plus), Token.Literal (Token.Int _)) ->
            let literal = consume parser in
            make_node
              Syntax_kind.INT_LITERAL
              ([ make_token parser sign ]
              @ tokens_to_green parser trivia_after_sign
              @ [ make_token parser literal ])
        | ((Token.Minus | Token.Plus | Token.MinusDot | Token.PlusDot), Token.Literal (Token.Float _)) ->
            let literal = consume parser in
            make_node
              Syntax_kind.FLOAT_LITERAL
              ([ make_token parser sign ]
              @ tokens_to_green parser trivia_after_sign
              @ [ make_token parser literal ])
        | _ ->
            let found_tok = peek parser in
            let diagnostic = Diagnostic.invalid_pattern
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser) in
            make_error_node parser ~diagnostic ~consumed_tokens:[ sign ]
      in
      signed_literal
  | Token.Underscore ->
      let underscore = consume parser in
      make_node Syntax_kind.WILDCARD_PATTERN [ make_token parser underscore ]
  | Token.Literal (Token.Int _) ->
      let tok = consume parser in
      (* Check for range pattern: 0 .. 9 *)
      if peek_kind parser = Token.DotDot then
        let trivia1 = consume_trivia parser in
        let dotdot = consume parser in
        let trivia2 = consume_trivia parser in
        match peek_kind parser with
        | Token.Literal (Token.Int _) ->
            let end_tok = consume parser in
            make_node
              Syntax_kind.RANGE_PATTERN
              ([ make_token parser tok ]
              @ tokens_to_green parser trivia1
              @ [ make_token parser dotdot ]
              @ tokens_to_green parser trivia2
              @ [ make_token parser end_tok ])
        | _ -> make_node Syntax_kind.INT_LITERAL [ make_token parser tok ]
      else
        make_node Syntax_kind.INT_LITERAL [ make_token parser tok ]
  | Token.Literal (Token.Float _) ->
      let tok = consume parser in
      make_node Syntax_kind.FLOAT_LITERAL [ make_token parser tok ]
  | Token.Literal (Token.String _) ->
      let tok = consume parser in
      make_node Syntax_kind.STRING_LITERAL [ make_token parser tok ]
  | Token.Literal (Token.Char _) ->
      let tok = consume parser in
      (* Check for range pattern: 'a' .. 'z' *)
      if peek_kind parser = Token.DotDot then
        let trivia1 = consume_trivia parser in
        let dotdot = consume parser in
        let trivia2 = consume_trivia parser in
        match peek_kind parser with
        | Token.Literal (Token.Char _) ->
            let end_tok = consume parser in
            make_node
              Syntax_kind.RANGE_PATTERN
              ([ make_token parser tok ]
              @ tokens_to_green parser trivia1
              @ [ make_token parser dotdot ]
              @ tokens_to_green parser trivia2
              @ [ make_token parser end_tok ])
        | _ -> make_node Syntax_kind.CHAR_LITERAL [ make_token parser tok ]
      else
        make_node Syntax_kind.CHAR_LITERAL [ make_token parser tok ]
  | Token.Keyword Keyword.True ->
      let tok = consume parser in
      make_node Syntax_kind.BOOL_LITERAL [ make_token parser tok ]
  | Token.Keyword Keyword.False ->
      let tok = consume parser in
      make_node Syntax_kind.BOOL_LITERAL [ make_token parser tok ]
  | Token.Ident _ when String.equal (token_text parser (peek parser)) "effect" -> (
      match parse_effect_pattern parser with
      | Some pattern -> pattern
      | None -> parse_ident_or_constructor_pattern parser
    )
  | Token.Ident _ ->
      parse_ident_or_constructor_pattern parser
  | Token.OpenDelim Token.Paren ->
      parse_paren_pattern parser
  | Token.OpenDelim Token.Bracket ->
      if is_extension_start parser then
        parse_extension parser
      else
        parse_list_pattern parser
  | Token.OpenDelim Token.Array ->
      parse_array_pattern parser
  | Token.OpenDelim Token.Brace ->
      if is_brace_extension_start parser then
        parse_extension parser
      else
        parse_record_pattern parser
  | Token.Backtick ->
      (* Polymorphic variant pattern: `Tag or `Tag pattern *)
      let backtick = consume parser in
      let trivia_after_backtick = consume_trivia parser in
      (* Expect tag name *)
      let tag_name =
        match peek_kind parser with
        | Token.Ident _ ->
            let ident = consume parser in
            make_token parser ident
        | _ ->
            let found_tok = peek parser in
            let diagnostic = Diagnostic.invalid_pattern
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            make_token parser found_tok
      in
      let trivia_after_name = consume_trivia parser in
      (* Check if followed by argument pattern *)
      let arg_pattern =
        if can_start_pattern_arg parser then
          let arg = parse_primary_pattern parser in
          [ Ceibo.Green.Node arg ]
        else
          []
      in
      make_node
        Syntax_kind.POLY_VARIANT_PATTERN
        ([ make_token parser backtick ]
        @ tokens_to_green parser trivia_after_backtick
        @ [ tag_name ]
        @ tokens_to_green parser trivia_after_name
        @ arg_pattern)
  | Token.Hash ->
      let hash = consume parser in
      let trivia_after_hash = consume_trivia parser in
      let type_name =
        match peek_kind parser with
        | Token.Ident _ ->
            let ident = consume parser in
            make_token parser ident
        | _ ->
            let found_tok = peek parser in
            let diagnostic = Diagnostic.invalid_pattern
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            make_token parser found_tok
      in
      let trivia_after_name = consume_trivia parser in
      make_node
        Syntax_kind.POLY_VARIANT_TYPE_PATTERN
        ([ make_token parser hash ]
        @ tokens_to_green parser trivia_after_hash
        @ [ type_name ]
        @ tokens_to_green parser trivia_after_name)
  | Token.Keyword Keyword.Lazy ->
      (* Lazy pattern: lazy p *)
      let lazy_kw = consume parser in
      let trivia_after_lazy = consume_trivia parser in
      let pattern = parse_primary_pattern parser in
      make_node
        Syntax_kind.LAZY_PATTERN
        ([ make_token parser lazy_kw ]
        @ tokens_to_green parser trivia_after_lazy
        @ [ Ceibo.Green.Node pattern ])
  | Token.Keyword Keyword.Exception ->
      (* Exception pattern: exception E or exception E x *)
      let exception_kw = consume parser in
      let trivia_after_exception = consume_trivia parser in
      (* Parse the exception pattern (constructor name + optional args) *)
      let exception_pattern = parse_primary_pattern parser in
      make_node
        Syntax_kind.EXCEPTION_PATTERN
        ([ make_token parser exception_kw ]
        @ tokens_to_green parser trivia_after_exception
        @ [ Ceibo.Green.Node exception_pattern ])
  | Token.Keyword _ ->
      (* Keywords cannot be used as identifiers in patterns *)
      let kw_tok = consume parser in
      let diagnostic = Diagnostic.invalid_pattern
        ~found:kw_tok
        ~text:(token_text parser kw_tok)
        ~span:(current_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[ kw_tok ]
  | _ ->
      let found_tok = peek parser in
      let exp_span = expected_span parser in
      let diagnostic = Diagnostic.invalid_pattern
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:exp_span in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Check if token can start a pattern *)
and can_start_pattern = fun parser ->
  match peek_kind parser with
  | Token.Underscore
  | Token.Literal _
  | Token.Ident _
  | Token.OpenDelim Token.Paren
  | Token.OpenDelim Token.Bracket
  | Token.OpenDelim Token.Array
  | Token.OpenDelim Token.Brace
  | Token.Plus
  | Token.Minus
  | Token.PlusDot
  | Token.MinusDot
  | Token.Keyword Keyword.True
  | Token.Keyword Keyword.False
  | Token.Keyword Keyword.Lazy
  | Token.Keyword Keyword.Exception
  | Token.Tilde
  | Token.Backtick -> true
  | _ -> false

(** Check if token can start a pattern argument (for constructor patterns) *)
and can_start_pattern_arg = fun parser ->
  match peek_kind parser with
  | Token.Tilde -> false
  | _ -> can_start_pattern parser

(** Check if token can start an expression *)
and can_start_expr = fun parser ->
  match peek_kind parser with
  | Token.Literal _
  | Token.Ident _
  | Token.OpenDelim Token.Paren
  | Token.OpenDelim Token.Bracket
  | Token.OpenDelim Token.Brace
  | Token.OpenDelim Token.BeginEnd
  | Token.Keyword Keyword.True
  | Token.Keyword Keyword.False
  | Token.Keyword Keyword.If
  | Token.Keyword Keyword.Match
  | Token.Keyword Keyword.Fun
  | Token.Keyword Keyword.Function
  | Token.Keyword Keyword.Let
  | Token.Keyword Keyword.Try
  | Token.Keyword Keyword.While
  | Token.Keyword Keyword.For
  | Token.Keyword Keyword.Assert
  | Token.Keyword Keyword.Lazy
  | Token.Keyword Keyword.New
  | Token.OpenDelim Token.ObjectEnd
  | Token.Backtick
  | Token.Plus
  | Token.PlusDot
  | Token.Minus
  | Token.MinusDot
  | Token.Bang
  | Token.Tilde
  | Token.Question -> true
  | _ -> false

(** Parse tuple pattern: (p1, p2, p3)
    Called after parsing first pattern and finding a comma *)
and parse_tuple_pattern = fun parser open_paren trivia_after_open first_pat trivia_after_first ->
  (* Parse remaining tuple elements after comma *)
  let rec parse_rest_elements acc_patterns acc_commas_trivia =
    if peek_kind parser = Token.Comma then
      let comma = consume parser in
      let trivia_after_comma = consume_trivia parser in
      (* Check for trailing comma before close paren *)
      if peek_kind parser = Token.CloseDelim Token.Paren then
        (List.rev acc_patterns, List.rev acc_commas_trivia)
      else
        let pat = parse_pattern parser in
        let trivia_after_pat = consume_trivia parser in
        (* Collect comma and trivia as green elements *)
        let comma_trivia_green = [ make_token parser comma ] @ tokens_to_green parser trivia_after_comma in
        parse_rest_elements
          (pat :: acc_patterns)
          ((comma_trivia_green @ tokens_to_green parser trivia_after_pat) @ acc_commas_trivia)
    else
      (* No more commas - return accumulated *)
      (List.rev acc_patterns, List.rev acc_commas_trivia)
  in
  let rest_patterns, commas_trivia = parse_rest_elements [] [] in
  let trivia_before_close = consume_trivia parser in
  let close_paren =
    expect
      parser
      (Token.CloseDelim Token.Paren)
      (fun found ->
        Diagnostic.unclosed_delimiter
          ~opener:")"
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser))
  in
  (* Build TUPLE_PATTERN with first pattern and rest *)
  make_node
    Syntax_kind.TUPLE_PATTERN
    ([ make_token parser open_paren ]
    @ tokens_to_green parser trivia_after_open
    @ [ Ceibo.Green.Node first_pat ]
    @ tokens_to_green parser trivia_after_first
    @ commas_trivia
    @ List.concat_map (fun p -> [ Ceibo.Green.Node p ]) rest_patterns
    @ tokens_to_green parser trivia_before_close
    @ [ make_token parser close_paren ])

(** Check if a token can be part of an operator name *)
and is_operator_token = function
  | Token.Plus
  | Token.Minus
  | Token.Star
  | Token.Slash
  | Token.Percent
  | Token.Caret
  | Token.Eq
  | Token.Lt
  | Token.Gt
  | Token.LtEq
  | Token.GtEq
  | Token.Ne
  | Token.Bang
  | Token.And
  | Token.Or
  | Token.Pipe
  | Token.Ampersand
  | Token.At
  | Token.Hash
  | Token.Tilde
  | Token.Question
  | Token.Dollar
  | Token.ColonColon
  | Token.ColonEq
  | Token.Arrow
  | Token.LeftArrow
  | Token.StarStar
  | Token.EqEq
  | Token.BangEq
  | Token.AtAt
  | Token.PipeGt
  | Token.PlusDot
  | Token.MinusDot
  | Token.StarDot
  | Token.SlashDot
  | Token.Dot
  | Token.Colon
  | Token.Keyword Keyword.Mod
  | Token.Keyword Keyword.Land
  | Token.Keyword Keyword.Lor
  | Token.Keyword Keyword.Lxor
  | Token.Keyword Keyword.Lsl
  | Token.Keyword Keyword.Lsr
  | Token.Keyword Keyword.Asr
  | Token.Keyword Keyword.Or
  | Token.Keyword Keyword.Let
  | Token.Keyword Keyword.And -> true
  | _ -> false

(** Check if identifier text matches a keyword operator name *)
and is_keyword_operator_name = fun text ->
  match text with
  | "mod"
  | "land"
  | "lor"
  | "lxor"
  | "lsl"
  | "lsr"
  | "asr"
  | "or" -> true
  | _ -> false

(** Parse operator pattern like ( + ), ( let* ), ( mod ) *)
and parse_operator_pattern = fun parser open_paren trivia_after_open ->
  (* Collect all tokens that form the operator name *)
  let operator_tokens = ref [] in
  let rec collect_nested close_kind =
    match peek_kind parser with
    | kind when kind = close_kind ->
        let close_tok = consume parser in
        operator_tokens := close_tok :: !operator_tokens;
        collect_operator_tokens ()
    | Token.EOF ->
        ()
    | Token.OpenDelim Token.Bracket ->
        let tok = consume parser in
        operator_tokens := tok :: !operator_tokens;
        collect_nested (Token.CloseDelim Token.Bracket)
    | Token.OpenDelim Token.Brace ->
        let tok = consume parser in
        operator_tokens := tok :: !operator_tokens;
        collect_nested (Token.CloseDelim Token.Brace)
    | Token.OpenDelim Token.Paren ->
        let tok = consume parser in
        operator_tokens := tok :: !operator_tokens;
        collect_nested (Token.CloseDelim Token.Paren)
    | _ ->
        let tok = consume parser in
        operator_tokens := tok :: !operator_tokens;
        collect_nested close_kind
  and collect_operator_tokens () =
    match peek_kind parser with
    | Token.OpenDelim Token.Bracket ->
        let open_bracket = consume parser in
        operator_tokens := open_bracket :: !operator_tokens;
        collect_nested (Token.CloseDelim Token.Bracket)
    | Token.OpenDelim Token.Brace ->
        let open_brace = consume parser in
        operator_tokens := open_brace :: !operator_tokens;
        collect_nested (Token.CloseDelim Token.Brace)
    | Token.OpenDelim Token.Paren ->
        let nested_open = consume parser in
        operator_tokens := nested_open :: !operator_tokens;
        collect_nested (Token.CloseDelim Token.Paren)
    | Token.CloseDelim Token.Paren ->
        (* End of operator name *)
        ()
    | tok when is_operator_token tok ->
        let t = consume parser in
        operator_tokens := t :: !operator_tokens;
        collect_operator_tokens ()
    | Token.Ident _ ->
        (* Check if this identifier is a keyword operator *)
        let text = token_text parser (peek parser) in
        if is_keyword_operator_name text then
          (
            let t = consume parser in
            operator_tokens := t :: !operator_tokens;
            collect_operator_tokens ()
          )
        else
          (* Not a keyword operator - stop collecting *)
          ()
    | _ ->
        (* Unexpected token - stop collecting *)
        ()
  in
  collect_operator_tokens ();
  let trivia_before_close = consume_trivia parser in
  let close_paren =
    expect
      parser
      (Token.CloseDelim Token.Paren)
      (fun found ->
        Diagnostic.unclosed_delimiter
          ~opener:")"
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser))
  in
  make_node
    Syntax_kind.OPERATOR_PATTERN
    ([ make_token parser open_paren ]
    @ tokens_to_green parser trivia_after_open
    @ List.map (fun t -> make_token parser t) (List.rev !operator_tokens)
    @ tokens_to_green parser trivia_before_close
    @ [ make_token parser close_paren ])

(** Parse parenthesized pattern or tuple pattern *)
and parse_paren_pattern = fun parser ->
  let open_paren =
    expect
      parser
      (Token.OpenDelim Token.Paren)
      (fun found ->
        Diagnostic.invalid_pattern ~found ~text:(token_text parser found) ~span:(current_span parser))
  in
  let trivia_after_open = consume_trivia parser in
  (* Check for empty tuple () *)
  if peek_kind parser = Token.CloseDelim Token.Paren then
    let close_paren = consume parser in
    make_node
      Syntax_kind.UNIT_LITERAL
      ([ make_token parser open_paren ]
      @ tokens_to_green parser trivia_after_open
      @ [ make_token parser close_paren ])
  else if let next_kind = (Token_cursor.peek_n parser.cursor 1).Token.kind in
    match (peek_kind parser, next_kind) with
    | ((Token.Plus | Token.Minus), Token.Literal (Token.Int _)) -> true
    | ((Token.Plus | Token.Minus | Token.PlusDot | Token.MinusDot), Token.Literal (Token.Float _)) -> true
    | _ -> false then
    let first_pat = parse_pattern parser in
    let trivia_before_close = consume_trivia parser in
    let close_paren =
      expect
        parser
        (Token.CloseDelim Token.Paren)
        (fun found ->
          Diagnostic.unclosed_delimiter
            ~opener:")"
            ~found
            ~text:(token_text parser found)
            ~span:(expected_span parser))
    in
    make_node
      Syntax_kind.PAREN_PATTERN
      ([ make_token parser open_paren ]
      @ tokens_to_green parser trivia_after_open
      @ [ Ceibo.Green.Node first_pat ]
      @ tokens_to_green parser trivia_before_close
      @ [ make_token parser close_paren ])
    (* Check for operator pattern: ( + ), ( let* ), etc. *)
  else if is_operator_token (peek_kind parser) then
    parse_operator_pattern parser open_paren trivia_after_open
    (* Check for first-class module pattern:
     (module M), (module _), (module M : S), or (module _ : S) *)
  else if peek_kind parser = Token.Keyword Keyword.Module then
    let module_kw = consume parser in
    let trivia_after_module = consume_trivia parser in
    (* Expect module identifier (uppercase) or wildcard binder *)
    let module_ident =
      match peek_kind parser with
      | Token.Ident _
      | Token.Underscore ->
          let ident = consume parser in
          make_token parser ident
      | _ ->
          let found_tok = peek parser in
          let diagnostic = Diagnostic.invalid_pattern
            ~found:found_tok
            ~text:(token_text parser found_tok)
            ~span:(expected_span parser) in
          report_diagnostic parser diagnostic;
          make_token parser found_tok
    in
    let trivia_after_ident = consume_trivia parser in
    (* Check for optional type constraint: (module M : S) *)
    let constraint_children =
      if peek_kind parser = Token.Colon then
        let colon = consume parser in
        let trivia_after_colon = consume_trivia parser in
        let sig_type = parse_module_type_expr parser in
        let trivia_after_type = consume_trivia parser in
        [ make_token parser colon ]
        @ tokens_to_green parser trivia_after_colon
        @ [ Ceibo.Green.Node sig_type ]
        @ tokens_to_green parser trivia_after_type
      else
        []
    in
    (* Expect closing paren *)
    let close_paren =
      expect
        parser
        (Token.CloseDelim Token.Paren)
        (fun found ->
          Diagnostic.unclosed_delimiter
            ~opener:")"
            ~found
            ~text:(token_text parser found)
            ~span:(expected_span parser))
    in
    make_node
      Syntax_kind.FIRST_CLASS_MODULE_PATTERN
      ([ make_token parser open_paren ]
      @ tokens_to_green parser trivia_after_open
      @ [ make_token parser module_kw ]
      @ tokens_to_green parser trivia_after_module
      @ [ module_ident ]
      @ tokens_to_green parser trivia_after_ident
      @ constraint_children
      @ [ make_token parser close_paren ])
  else if peek_kind parser = Token.Comma then
    let comma = consume parser in
    let trivia_after_comma = consume_trivia parser in
    let diagnostic = Diagnostic.tuple_pattern_extra_comma
      ~found:comma
      ~text:(token_text parser comma)
      ~span:comma.Token.span in
    let close_paren =
      expect
        parser
        (Token.CloseDelim Token.Paren)
        (fun found ->
          Diagnostic.unclosed_delimiter
            ~opener:")"
            ~found
            ~text:(token_text parser found)
            ~span:(expected_span parser))
    in
    make_node
      Syntax_kind.TUPLE_PATTERN
      ([ make_token parser open_paren ]
      @ tokens_to_green parser trivia_after_open
      @ [ Ceibo.Green.Node (make_error_node parser ~diagnostic ~consumed_tokens:[ comma ]) ]
      @ tokens_to_green parser trivia_after_comma
      @ [ make_token parser close_paren ])
  else
    (* Parse first pattern *)
    let first_pat = parse_pattern parser in
    let trivia_after_first = consume_trivia parser in
    (* Check if this is a tuple (has comma), typed pattern (has colon), or single pattern *)
    if peek_kind parser = Token.Comma then
      parse_tuple_pattern parser open_paren trivia_after_open first_pat trivia_after_first
    else if peek_kind parser = Token.Colon then
      let colon = consume parser in
      let trivia_after_colon = consume_trivia parser in
      (* Parse type expression *)
      let type_expr = parse_typexpr parser in
      let trivia_after_type = consume_trivia parser in
      let close_paren =
        expect
          parser
          (Token.CloseDelim Token.Paren)
          (fun found ->
            Diagnostic.unclosed_delimiter
              ~opener:")"
              ~found
              ~text:(token_text parser found)
              ~span:(expected_span parser))
      in
      make_node
        Syntax_kind.TYPED_PATTERN
        ([ make_token parser open_paren ]
        @ tokens_to_green parser trivia_after_open
        @ [ Ceibo.Green.Node first_pat ]
        @ tokens_to_green parser trivia_after_first
        @ [ make_token parser colon ]
        @ tokens_to_green parser trivia_after_colon
        @ [ Ceibo.Green.Node type_expr ]
        @ tokens_to_green parser trivia_after_type
        @ [ make_token parser close_paren ])
    else
      (* Single pattern in parens *)
      let trivia_before_close = consume_trivia parser in
      let close_paren =
        expect
          parser
          (Token.CloseDelim Token.Paren)
          (fun found ->
            Diagnostic.unclosed_delimiter
              ~opener:")"
              ~found
              ~text:(token_text parser found)
              ~span:(expected_span parser))
      in
      make_node
        Syntax_kind.PAREN_PATTERN
        ([ make_token parser open_paren ]
        @ tokens_to_green parser trivia_after_open
        @ [ Ceibo.Green.Node first_pat ]
        @ tokens_to_green parser trivia_after_first
        @ tokens_to_green parser trivia_before_close
        @ [ make_token parser close_paren ])

(** Parse list pattern: [] or [x; y; z] *)
and parse_list_pattern = fun parser ->
  match peek_kind parser with
  | Token.OpenDelim Token.Bracket ->
      let open_bracket = consume parser in
      let trivia_after_open = consume_trivia parser in
      (* Check for empty list [] *)
      if peek_kind parser = Token.CloseDelim Token.Bracket then
        let close_bracket = consume parser in
        make_node
          Syntax_kind.LIST_PATTERN
          ([ make_token parser open_bracket ]
          @ tokens_to_green parser trivia_after_open
          @ [ make_token parser close_bracket ])
      else
        (* Parse list elements *)
        let first_pat = parse_pattern parser in
        let trivia_after_first = consume_trivia parser in
        let content_parts = ref
          ([ Ceibo.Green.Node first_pat ] @ tokens_to_green parser trivia_after_first) in
        while peek_kind parser = Token.Semi do
          let semi = consume parser in
          let trivia_after_semi = consume_trivia parser in
          content_parts := !content_parts @ [ make_token parser semi ] @ tokens_to_green parser trivia_after_semi;
          (* Check for trailing semicolon or end *)
          if peek_kind parser = Token.CloseDelim Token.Bracket then
            ()
          else
            (
              let pat = parse_pattern parser in
              let trivia_after_pat = consume_trivia parser in
              content_parts := !content_parts @ [ Ceibo.Green.Node pat ] @ tokens_to_green parser trivia_after_pat
            )
        done;
        let trivia_before_close = consume_trivia parser in
        let close_bracket =
          if peek_kind parser = Token.CloseDelim Token.Bracket then
            consume parser
          else
            peek parser
        in
        make_node
          Syntax_kind.LIST_PATTERN
          ([ make_token parser open_bracket ]
          @ tokens_to_green parser trivia_after_open
          @ !content_parts
          @ tokens_to_green parser trivia_before_close
          @ [ make_token parser close_bracket ])
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_pattern
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse array pattern: [||] or [|x; y; z|] *)
and parse_array_pattern = fun parser ->
  match peek_kind parser with
  | Token.OpenDelim Token.Array ->
      let open_array = consume parser in
      let trivia_after_open = consume_trivia parser in
      (* Check for empty array [||] *)
      if peek_kind parser = Token.CloseDelim Token.Array then
        let close_array = consume parser in
        make_node
          Syntax_kind.ARRAY_PATTERN
          ([ make_token parser open_array ]
          @ tokens_to_green parser trivia_after_open
          @ [ make_token parser close_array ])
      else
        (* Parse array elements *)
        let first_pat = parse_pattern parser in
        let trivia_after_first = consume_trivia parser in
        let content_parts = ref
          ([ Ceibo.Green.Node first_pat ] @ tokens_to_green parser trivia_after_first) in
        while peek_kind parser = Token.Semi do
          let semi = consume parser in
          let trivia_after_semi = consume_trivia parser in
          content_parts := !content_parts @ [ make_token parser semi ] @ tokens_to_green parser trivia_after_semi;
          (* Check for trailing semicolon or end *)
          if peek_kind parser = Token.CloseDelim Token.Array then
            ()
          else
            (
              let pat = parse_pattern parser in
              let trivia_after_pat = consume_trivia parser in
              content_parts := !content_parts @ [ Ceibo.Green.Node pat ] @ tokens_to_green parser trivia_after_pat
            )
        done;
        let trivia_before_close = consume_trivia parser in
        let close_array =
          if peek_kind parser = Token.CloseDelim Token.Array then
            consume parser
          else
            peek parser
        in
        make_node
          Syntax_kind.ARRAY_PATTERN
          ([ make_token parser open_array ]
          @ tokens_to_green parser trivia_after_open
          @ !content_parts
          @ tokens_to_green parser trivia_before_close
          @ [ make_token parser close_array ])
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_pattern
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse record pattern: { field1; field2 = pat } or {} *)
and parse_record_pattern = fun parser ->
  let open_brace, content, close_brace =
    parse_braces parser
      (fun parser ->
        (* Check for empty record {} *)
        if peek_kind parser = Token.CloseDelim Token.Brace then
          []
        else
          let first_field = parse_record_field_pattern parser in
          let content_parts = ref [ Ceibo.Green.Node first_field ] in
          let trivia_after_first = consume_trivia parser in
          content_parts := !content_parts @ tokens_to_green parser trivia_after_first;
          while peek_kind parser = Token.Semi do
            let semi = consume parser in
            let trivia_after_semi = consume_trivia parser in
            content_parts := !content_parts @ [ make_token parser semi ] @ tokens_to_green parser trivia_after_semi;
            (* Check for trailing semicolon, wildcard, or another field *)
            if peek_kind parser = Token.CloseDelim Token.Brace then
              ()
            else if peek_kind parser = Token.Underscore then
              (
                (* Record wildcard pattern: { field; _ } *)
                let underscore = consume parser in
                content_parts := !content_parts @ [ make_token parser underscore ];
                let trivia_after_wildcard = consume_trivia parser in
                content_parts := !content_parts @ tokens_to_green parser trivia_after_wildcard
              )
            else
              (
                let field = parse_record_field_pattern parser in
                content_parts := !content_parts @ [ Ceibo.Green.Node field ];
                let trivia_after_field = consume_trivia parser in
                content_parts := !content_parts @ tokens_to_green parser trivia_after_field
              )
          done;
          !content_parts)
  in
  make_node
    Syntax_kind.RECORD_PATTERN
    ([ make_token parser open_brace ] @ content @ [ make_token parser close_brace ])

(** Parse a qualified field name: [module-path "."] field-name 
    Returns the field path as a list of green elements *)
and parse_qualified_field = fun parser ->
  (* Parse first identifier *)
  let first_ident =
    parse_ident
      parser
      (fun parser found ->
        Diagnostic.invalid_pattern ~found ~text:(token_text parser found) ~span:(current_span parser))
  in
  (* Collect module path segments if present: Ident . Ident . ... *)
  let rec collect_path_segments acc =
    let trivia_after = consume_trivia parser in
    if has_dot_ident_continuation parser then
      let dot = consume parser in
      let trivia_after_dot = consume_trivia parser in
      let next_ident = consume parser in
      (* Add the accumulated elements plus dot and next identifier *)
      collect_path_segments
        (acc
        @ tokens_to_green parser trivia_after
        @ [ make_token parser dot ]
        @ tokens_to_green parser trivia_after_dot
        @ [ make_token parser next_ident ])
    else
      (* No more path segments *)
      (acc, trivia_after)
  in
  collect_path_segments [ make_token parser first_ident ]

(** Parse a single record field pattern: field or field = pattern 
    Field can be qualified: Module.SubModule.field *)
and parse_record_field_pattern = fun parser ->
  (* Parse field name, which may include module path: Module.field *)
  let field_path, trivia_after_field = parse_qualified_field parser in
  (* Check for = pattern *)
  if peek_kind parser = Token.Eq then
    let eq = consume parser in
    let trivia_after_eq = consume_trivia parser in
    let pattern = parse_pattern parser in
    make_node
      Syntax_kind.RECORD_FIELD_PATTERN
      (field_path
      @ tokens_to_green parser trivia_after_field
      @ [ make_token parser eq ]
      @ tokens_to_green parser trivia_after_eq
      @ [ Ceibo.Green.Node pattern ])
  else
    (* Shorthand: just field name, equivalent to field = field *)
    make_node
      Syntax_kind.RECORD_FIELD_PATTERN
      (field_path @ tokens_to_green parser trivia_after_field)

(** *
    ============================================================================
    * GRAMMAR SECTION 7: EXPRESSIONS *
    ============================================================================
*)
(** Parse constant expression: literals, unit, true, false, etc. *)
and parse_constant = fun parser ->
  match peek_kind parser with
  | Token.Literal (Token.Int _) ->
      let tok = consume parser in
      make_node Syntax_kind.INT_LITERAL [ make_token parser tok ]
  | Token.Literal (Token.Float _) ->
      let tok = consume parser in
      make_node Syntax_kind.FLOAT_LITERAL [ make_token parser tok ]
  | Token.Literal (Token.String _) ->
      let tok = consume parser in
      make_node Syntax_kind.STRING_LITERAL [ make_token parser tok ]
  | Token.Literal (Token.Char _) ->
      let tok = consume parser in
      make_node Syntax_kind.CHAR_LITERAL [ make_token parser tok ]
  | Token.Keyword Keyword.True ->
      let tok = consume parser in
      make_node Syntax_kind.BOOL_LITERAL [ make_token parser tok ]
  | Token.Keyword Keyword.False ->
      let tok = consume parser in
      make_node Syntax_kind.BOOL_LITERAL [ make_token parser tok ]
  | _ ->
      let found_tok = peek parser in
      let found_text = token_text parser found_tok in
      (* Check if it's an operator (missing left operand) *)
      let diagnostic =
        if Option.is_some (operator_info (peek_kind parser)) then
          Diagnostic.missing_binary_operand
            ~operator:found_text
            ~side:"left"
            ~found:found_tok
            ~text:found_text
            ~span:(expected_span parser)
        else
          Diagnostic.invalid_expression
            ~found:found_tok
            ~text:found_text
            ~span:(expected_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse character literal from quote tokens.

    Handles malformed char literals like:
    - 'a (unclosed)
    - '' (empty)
    - 'abc (multiple chars) *)
and parse_char_literal = fun parser ->
  match peek_kind parser with
  | Token.Quote -> (
      let quote = consume parser in
      (* Check what follows the quote *)
      match peek_kind parser with
      | Token.Quote ->
          (* Empty char literal: '' *)
          let closing_quote = consume parser in
          let span = Ceibo.Span.make ~start:quote.Token.span.start ~end_:closing_quote.Token.span.end_ in
          let diagnostic = Diagnostic.empty_char_literal ~span in
          make_error_node parser ~diagnostic ~consumed_tokens:[ quote; closing_quote ]
      | Token.Ident _ -> (
          (* Could be 'a (unclosed char) or 'a (type var used in expr - wrong) *)
          let ident = consume parser in
          let ident_text = token_text parser ident in
          (* Check if there's a closing quote *)
          match peek_kind parser with
          | Token.Quote when String.length ident_text = 1 ->
              (* 'a' - should have been tokenized as Literal (Char) but wasn't *)
              let closing_quote = consume parser in
              make_node
                Syntax_kind.CHAR_LITERAL
                [
                  make_token parser quote;
                  make_token parser ident;
                  make_token parser closing_quote;
                ]
          | Token.Quote ->
              (* 'abc' - multiple characters *)
              let closing_quote = consume parser in
              let span = Ceibo.Span.make
                ~start:quote.Token.span.start
                ~end_:closing_quote.Token.span.end_ in
              let diagnostic = Diagnostic.multi_char_literal ~text:ident_text ~span in
              make_error_node parser ~diagnostic ~consumed_tokens:[ quote; ident; closing_quote ]
          | _ ->
              (* 'a (unclosed) - missing closing quote after the character *)
              let pos = ident.Token.span.end_ in
              let span = point_span pos in
              let diagnostic = Diagnostic.unclosed_char_literal ~text:(("'" ^ ident_text)) ~span in
              make_error_node parser ~diagnostic ~consumed_tokens:[ quote; ident ]
        )
      | Token.EOF ->
          (* ' at EOF *)
          let diagnostic = Diagnostic.unclosed_char_literal ~text:"'" ~span:(expected_span parser) in
          make_error_node parser ~diagnostic ~consumed_tokens:[ quote ]
      | _ ->
          (* Some other token - unexpected *)
          let diagnostic = Diagnostic.unclosed_char_literal ~text:"'" ~span:(current_span parser) in
          make_error_node parser ~diagnostic ~consumed_tokens:[ quote ]
    )
  | _ ->
      (* Not a quote - shouldn't be called *)
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse a single attribute: [@attr] or [@@attr] or [@attr payload] *)
and parse_attribute = fun parser ->
  match peek_kind parser with
  | Token.OpenDelim Token.Bracket ->
      let open_bracket = consume parser in
      let trivia_after_open = consume_trivia parser in
      (* Expect @, @@, or @@@ *)
      let at_tokens =
        match peek_kind parser with
        | Token.AtAt ->
            let atat = consume parser in
            let trivia_after_atat = consume_trivia parser in
            if peek_kind parser = Token.At then
              let at = consume parser in
              [ make_token parser atat ]
              @ tokens_to_green parser trivia_after_atat
              @ [ make_token parser at ]
            else
              [ make_token parser atat ]
        | Token.At ->
            [ make_token parser (consume parser) ]
        | _ ->
            let found = peek parser in
            let diagnostic = Diagnostic.invalid_expression
              ~found
              ~text:(token_text parser found)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            []
      in
      let trivia_after_at = consume_trivia parser in
      (* Parse attribute name (identifier) *)
      let attr_name =
        match peek_kind parser with
        | Token.Ident _ -> consume parser
        | _ ->
            let found = peek parser in
            let diagnostic = Diagnostic.invalid_expression
              ~found
              ~text:(token_text parser found)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            found
      in
      let trivia_after_name = consume_trivia parser in
      (* Parse optional attribute payload, respecting nested delimiters. *)
      let payload_tokens = ref [] in
      let nested_brackets = ref 0 in
      let nested_delims = ref 0 in
      let should_stop () =
        peek_kind parser = Token.CloseDelim Token.Bracket
        && !nested_brackets = 0
        && !nested_delims = 0 in
      while (not (should_stop ())) && peek_kind parser != Token.EOF do
        let tok = consume parser in
        (
          match tok.Token.kind with
          | Token.OpenDelim Token.Bracket -> nested_brackets := !nested_brackets + 1
          | Token.CloseDelim Token.Bracket when !nested_brackets > 0 -> nested_brackets := !nested_brackets
          - 1
          | Token.OpenDelim _ -> nested_delims := !nested_delims + 1
          | Token.CloseDelim _ when !nested_delims > 0 -> nested_delims := !nested_delims - 1
          | _ -> ()
        );
        payload_tokens := tok :: !payload_tokens;
      done;
      (* Expect ] *)
      let close_bracket =
        if peek_kind parser = Token.CloseDelim Token.Bracket then
          consume parser
        else
          let found = peek parser in
          let diagnostic = Diagnostic.invalid_expression
            ~found
            ~text:(token_text parser found)
            ~span:(expected_span parser) in
          report_diagnostic parser diagnostic;
          found
      in
      (* Build ATTRIBUTE node *)
      make_node
        Syntax_kind.ATTRIBUTE_EXPR
        ([ make_token parser open_bracket ]
        @ tokens_to_green parser trivia_after_open
        @ at_tokens
        @ tokens_to_green parser trivia_after_at
        @ [ make_token parser attr_name ]
        @ tokens_to_green parser trivia_after_name
        @ List.rev_map (make_token parser) !payload_tokens
        @ [ make_token parser close_bracket ])
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Check whether the current position starts a bracket attribute. *)
and is_attribute_start = fun parser ->
  match peek_kind parser with
  | Token.OpenDelim Token.Bracket ->
      let saved_pos = Token_cursor.position parser.cursor in
      let _ = consume parser in
      let is_attr =
        match peek_kind parser with
        | Token.At
        | Token.AtAt -> true
        | _ -> false
      in
      Token_cursor.set_position parser.cursor saved_pos;
      is_attr
  | _ -> false

(** Check whether the current position starts a bracket extension. *)
and is_extension_start = fun parser ->
  match peek_kind parser with
  | Token.OpenDelim Token.Bracket ->
      let saved_pos = Token_cursor.position parser.cursor in
      let _ = consume parser in
      let is_ext = peek_kind parser = Token.Percent in
      Token_cursor.set_position parser.cursor saved_pos;
      is_ext
  | _ -> false

(** Check whether the current position starts a brace extension. *)
and is_brace_extension_start = fun parser ->
  match peek_kind parser with
  | Token.OpenDelim Token.Brace ->
      let saved_pos = Token_cursor.position parser.cursor in
      let _ = consume parser in
      let is_ext = peek_kind parser = Token.Percent in
      Token_cursor.set_position parser.cursor saved_pos;
      is_ext
  | _ -> false

(** Parse zero or more attributes: [@attr1] [@attr2] ... *)
and parse_attributes = fun parser ->
  let attrs = ref [] in
  let attrs_trivia = ref [] in
  while is_attribute_start parser do
    let attr = parse_attribute parser in
    attrs := attr :: !attrs;
    let trivia = consume_trivia parser in
    attrs_trivia := trivia :: !attrs_trivia;
  done;
  (* Return attributes and trivia as green nodes *)
  let attr_list = List.rev !attrs in
  let trivia_list = List.rev !attrs_trivia in
  List.mapi
    (fun i attr ->
      let trivia =
        if i < List.length trivia_list then
          List.nth trivia_list i
        else
          []
      in
      [ Ceibo.Green.Node attr ] @ tokens_to_green parser trivia)
    attr_list |> List.flatten

(** Parse optional extension name after keyword: %identifier
    Used for: let%foo, match%bar, etc.
    Returns green nodes for the extension if present, otherwise empty list *)
and parse_extension_name = fun parser ->
  let next_kind = (peek_n parser 1).Token.kind in
  if peek_kind parser = Token.Percent && match next_kind with
    | Token.Ident _ -> true
    | _ -> false then
    let percent = consume parser in
    let trivia_after_percent = consume_trivia parser in
    match peek_kind parser with
    | Token.Ident _ ->
        let ext_name = consume parser in
        [ make_token parser percent ]
        @ tokens_to_green parser trivia_after_percent
        @ [ make_token parser ext_name ]
    | _ ->
        (* % without identifier - just return the % token *)
        [ make_token parser percent ] @ tokens_to_green parser trivia_after_percent
  else
    []

and parse_extension_and_attributes = fun parser ->
  let ext_nodes = parse_extension_name parser in
  let trivia_after_ext =
    if ext_nodes != [] then
      consume_trivia parser
    else
      []
  in
  let attr_nodes = parse_attributes parser in
  (ext_nodes, trivia_after_ext, attr_nodes)

and parse_keyword_extension_and_attributes = fun parser -> parse_extension_and_attributes parser

and attach_postfix_attributes = fun parser node ->
  let saved_pos = Token_cursor.position parser.cursor in
  let trivia_before_attr = consume_trivia parser in
  if is_attribute_start parser then
    let attr_nodes = parse_attributes parser in
    let wrapped = make_node
      Syntax_kind.ATTRIBUTE_EXPR
      ([ Ceibo.Green.Node node ] @ tokens_to_green parser trivia_before_attr @ attr_nodes) in
    attach_postfix_attributes parser wrapped
  else (
    Token_cursor.set_position parser.cursor saved_pos;
    node
  )

(** Parse a single extension: [%ext] or [%%ext] or [%ext payload] *)
and parse_extension = fun parser ->
  match peek_kind parser with
  | Token.OpenDelim Token.Bracket ->
      let open_bracket = consume parser in
      let trivia_after_open = consume_trivia parser in
      (* Expect % or %% (two % tokens) *)
      let percent_tokens, is_floating =
        match peek_kind parser with
        | Token.Percent -> (
            let first_percent = consume parser in
            (* Check for second % *)
            match peek_kind parser with
            | Token.Percent ->
                let second_percent = consume parser in
                ([ make_token parser first_percent; make_token parser second_percent ], true)
            | _ -> ([ make_token parser first_percent ], false)
          )
        | _ ->
            let found = peek parser in
            let diagnostic = Diagnostic.invalid_expression
              ~found
              ~text:(token_text parser found)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            ([], false)
      in
      let trivia_after_percent = consume_trivia parser in
      (* Parse extension name (identifier) *)
      let ext_name =
        match peek_kind parser with
        | Token.Ident _ -> consume parser
        | _ ->
            let found = peek parser in
            let diagnostic = Diagnostic.invalid_expression
              ~found
              ~text:(token_text parser found)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            found
      in
      let trivia_after_name = consume_trivia parser in
      (* Parse optional extension payload, respecting nested delimiters. *)
      let payload_tokens = ref [] in
      let nested_brackets = ref 0 in
      let nested_delims = ref 0 in
      let should_stop () =
        peek_kind parser = Token.CloseDelim Token.Bracket
        && !nested_brackets = 0
        && !nested_delims = 0 in
      while (not (should_stop ())) && peek_kind parser != Token.EOF do
        let tok = consume parser in
        (
          match tok.Token.kind with
          | Token.OpenDelim Token.Bracket -> nested_brackets := !nested_brackets + 1
          | Token.CloseDelim Token.Bracket when !nested_brackets > 0 -> nested_brackets := !nested_brackets
          - 1
          | Token.OpenDelim _ -> nested_delims := !nested_delims + 1
          | Token.CloseDelim _ when !nested_delims > 0 -> nested_delims := !nested_delims - 1
          | _ -> ()
        );
        payload_tokens := tok :: !payload_tokens;
      done;
      (* Expect ] *)
      let close_bracket =
        if peek_kind parser = Token.CloseDelim Token.Bracket then
          consume parser
        else
          let found = peek parser in
          let diagnostic = Diagnostic.invalid_expression
            ~found
            ~text:(token_text parser found)
            ~span:(expected_span parser) in
          report_diagnostic parser diagnostic;
          found
      in
      (* Build EXTENSION node *)
      make_node
        Syntax_kind.EXTENSION_EXPR
        ([ make_token parser open_bracket ]
        @ tokens_to_green parser trivia_after_open
        @ percent_tokens
        @ tokens_to_green parser trivia_after_percent
        @ [ make_token parser ext_name ]
        @ tokens_to_green parser trivia_after_name
        @ List.rev_map (make_token parser) !payload_tokens
        @ [ make_token parser close_bracket ])
  | Token.OpenDelim Token.Brace ->
      let open_brace = consume parser in
      let trivia_after_open = consume_trivia parser in
      let percent_tokens =
        match peek_kind parser with
        | Token.Percent ->
            let first_percent = consume parser in
            (
              match peek_kind parser with
              | Token.Percent ->
                  let second_percent = consume parser in
                  [ make_token parser first_percent; make_token parser second_percent ]
              | _ -> [ make_token parser first_percent ]
            )
        | _ ->
            let found = peek parser in
            let diagnostic = Diagnostic.invalid_expression
              ~found
              ~text:(token_text parser found)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            []
      in
      let trivia_after_percent = consume_trivia parser in
      let ext_name_tokens = ref [] in
      let rec parse_ext_name () =
        match peek_kind parser with
        | Token.Ident _
        | Token.Dot ->
            let tok = consume parser in
            ext_name_tokens := !ext_name_tokens @ [ make_token parser tok ];
            let trivia = consume_trivia parser in
            ext_name_tokens := !ext_name_tokens @ tokens_to_green parser trivia;
            parse_ext_name ()
        | _ -> ()
      in
      parse_ext_name ();
      let payload_tokens = ref [] in
      let nested_braces = ref 0 in
      let nested_delims = ref 0 in
      let should_stop () =
        peek_kind parser = Token.CloseDelim Token.Brace && !nested_braces = 0 && !nested_delims = 0 in
      while (not (should_stop ())) && peek_kind parser != Token.EOF do
        let tok = consume parser in
        (
          match tok.Token.kind with
          | Token.OpenDelim Token.Brace -> nested_braces := !nested_braces + 1
          | Token.CloseDelim Token.Brace when !nested_braces > 0 -> nested_braces := !nested_braces
          - 1
          | Token.OpenDelim _ -> nested_delims := !nested_delims + 1
          | Token.CloseDelim _ when !nested_delims > 0 -> nested_delims := !nested_delims - 1
          | _ -> ()
        );
        payload_tokens := tok :: !payload_tokens
      done;
      let close_brace =
        if peek_kind parser = Token.CloseDelim Token.Brace then
          consume parser
        else
          let found = peek parser in
          let diagnostic = Diagnostic.invalid_expression
            ~found
            ~text:(token_text parser found)
            ~span:(expected_span parser) in
          report_diagnostic parser diagnostic;
          found
      in
      make_node
        Syntax_kind.EXTENSION_EXPR
        ([ make_token parser open_brace ]
        @ tokens_to_green parser trivia_after_open
        @ percent_tokens
        @ tokens_to_green parser trivia_after_percent
        @ !ext_name_tokens
        @ List.rev_map (make_token parser) !payload_tokens
        @ [ make_token parser close_brace ])
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse labeled parameter: ~label or ~label:pattern or ~(label) or ~(label:pattern)

    Non-parenthesized renames must stay tight (`~label:name`). A spaced form such
    as `~label: name` is ambiguous with a whole-binding type annotation and is
    therefore rejected with a parse diagnostic. *)
and parse_labeled_param = fun parser ->
  (* MUST start with ~ *)
  let tilde =
    expect
      parser
      Token.Tilde
      (fun found ->
        Diagnostic.invalid_expression
          ~found
          ~text:(token_text parser found)
          ~span:(current_span parser))
  in
  let trivia_after_tilde = consume_trivia parser in
  (* Define the core parser for label and optional :pattern *)
  let parse_label_and_pattern parser =
    let label =
      parse_ident
        parser
        (fun parser found ->
          Diagnostic.invalid_expression
            ~found
            ~text:(token_text parser found)
            ~span:(current_span parser))
    in
    let trivia_after_label = consume_trivia parser in
    (* Check for optional :pattern *)
    let colon_pattern_parts =
      if peek_kind parser = Token.Colon && trivia_after_label = [] then
        let colon = consume parser in
        let trivia_after_colon = consume_trivia parser in
        if not (List.is_empty trivia_after_colon) then
          (
            let found = peek parser in
            let diagnostic = Diagnostic.invalid_pattern
              ~found
              ~text:(token_text parser found)
              ~span:(current_span parser) in
            report_diagnostic parser diagnostic
          );
          let pattern = parse_pattern parser in
          [ make_token parser colon ]
          @ tokens_to_green parser trivia_after_colon
          @ [ Ceibo.Green.Node pattern ]
        else
          []
    in
    ([ make_token parser label ] @ tokens_to_green parser trivia_after_label @ colon_pattern_parts)
  in
  (* Now decide: parenthesized or not? *)
  match peek_kind parser with
  | Token.OpenDelim Token.Paren ->
      (* In parentheses: ~(label : TYPE) - parse type annotation, not pattern *)
      let open_p, content, close_p =
        parse_parens parser
          (fun parser ->
            let label =
              parse_ident
                parser
                (fun parser found ->
                  Diagnostic.invalid_expression
                    ~found
                    ~text:(token_text parser found)
                    ~span:(current_span parser))
            in
            let trivia_after_label = consume_trivia parser in
            (* Check for optional :type *)
            let colon_type_parts =
              if peek_kind parser = Token.Colon then
                let colon = consume parser in
                let trivia_after_colon = consume_trivia parser in
                (* Parse TYPE expression (not pattern!) *)
                let type_expr = parse_typexpr parser in
                [ make_token parser colon ]
                @ tokens_to_green parser trivia_after_colon
                @ [ Ceibo.Green.Node type_expr ]
              else
                []
            in
            ([ make_token parser label ] @ tokens_to_green parser trivia_after_label @ colon_type_parts))
      in
      make_node
        Syntax_kind.LABELED_PARAM
        ([ make_token parser tilde ]
        @ tokens_to_green parser trivia_after_tilde
        @ [ make_token parser open_p ]
        @ content
        @ [ make_token parser close_p ])
  | Token.Ident _ ->
      (* Not in parentheses: ~label:pattern - parse pattern *)
      let content = parse_label_and_pattern parser in
      make_node
        Syntax_kind.LABELED_PARAM
        ([ make_token parser tilde ] @ tokens_to_green parser trivia_after_tilde @ content)
  | _ ->
      let found = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found
        ~text:(token_text parser found)
        ~span:(expected_span parser) in
      report_diagnostic parser diagnostic;
      make_error_node parser ~diagnostic ~consumed_tokens:[ tilde ]

(** Parse optional parameter: ?label, ?label:pattern, or ?(label = expr)

    Like labeled renames, non-parenthesized optional renames stay tight
    (`?label:name`). The spaced form `?label: name` is rejected so it cannot be
    confused with a whole-binding type annotation. *)
and parse_optional_param = fun parser ->
  let parse_default_param_content parser =
    let binding = parse_pattern parser in
    let trivia_after_binding = consume_trivia parser in
    let type_parts =
      if peek_kind parser = Token.Colon then
        let colon = consume parser in
        let trivia_after_colon = consume_trivia parser in
        let type_expr = parse_typexpr parser in
        [ make_token parser colon ]
        @ tokens_to_green parser trivia_after_colon
        @ [ Ceibo.Green.Node type_expr ]
      else
        []
    in
    let trivia_before_eq = consume_trivia parser in
    let eq =
      expect
        parser
        Token.Eq
        (fun found ->
          Diagnostic.invalid_expression
            ~found
            ~text:(token_text parser found)
            ~span:(expected_span parser))
    in
    let trivia_after_eq = consume_trivia parser in
    let default_expr = parse_expr parser in
    [ Ceibo.Green.Node binding ]
    @ tokens_to_green parser trivia_after_binding
    @ type_parts
    @ tokens_to_green parser trivia_before_eq
    @ [ make_token parser eq ]
    @ tokens_to_green parser trivia_after_eq
    @ [ Ceibo.Green.Node default_expr ]
  in
  (* MUST start with ? *)
  let question =
    expect
      parser
      Token.Question
      (fun found ->
        Diagnostic.invalid_expression
          ~found
          ~text:(token_text parser found)
          ~span:(current_span parser))
  in
  let trivia_after_q = consume_trivia parser in
  match peek_kind parser with
  | Token.OpenDelim Token.Paren ->
      (* ?(label = expr) - with default value *)
      let open_p, content, close_p = parse_parens parser parse_default_param_content in
      make_node
        Syntax_kind.OPTIONAL_PARAM_DEFAULT
        ([ make_token parser question ]
        @ tokens_to_green parser trivia_after_q
        @ [ make_token parser open_p ]
        @ content
        @ [ make_token parser close_p ])
  | Token.Ident _ ->
      (* ?label, ?label:pattern, or ?label:(binding = expr) *)
      let parse_label_and_pattern parser =
        let label =
          parse_ident
            parser
            (fun parser found ->
              Diagnostic.invalid_expression
                ~found
                ~text:(token_text parser found)
                ~span:(current_span parser))
        in
        let trivia_after_label = consume_trivia parser in
        (* Check for optional :pattern *)
        let colon_pattern_parts =
          if peek_kind parser = Token.Colon && trivia_after_label = [] then
            let colon = consume parser in
            let trivia_after_colon = consume_trivia parser in
            if not (List.is_empty trivia_after_colon) then
              (
                let found = peek parser in
                let diagnostic = Diagnostic.invalid_pattern
                  ~found
                  ~text:(token_text parser found)
                  ~span:(current_span parser) in
                report_diagnostic parser diagnostic
              );
              if peek_kind parser = Token.OpenDelim Token.Paren then
                let open_p, content, close_p = parse_parens parser parse_default_param_content in
                [ make_token parser colon ]
                @ tokens_to_green parser trivia_after_colon
                @ [ make_token parser open_p ]
                @ content
                @ [ make_token parser close_p ]
              else
                let pattern = parse_pattern parser in
                [ make_token parser colon ]
                @ tokens_to_green parser trivia_after_colon
                @ [ Ceibo.Green.Node pattern ]
            else
              []
        in
        ([ make_token parser label ] @ tokens_to_green parser trivia_after_label @ colon_pattern_parts)
      in
      let content = parse_label_and_pattern parser in
      make_node
        Syntax_kind.OPTIONAL_PARAM
        ([ make_token parser question ] @ tokens_to_green parser trivia_after_q @ content)
  | _ ->
      let found = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found
        ~text:(token_text parser found)
        ~span:(expected_span parser) in
      report_diagnostic parser diagnostic;
      make_error_node parser ~diagnostic ~consumed_tokens:[ question ]

(** Parse function parameter - dispatches to specific parser based on prefix *)
and parse_fun_param = fun parser ->
  match parse_locally_abstract_types parser with
  | Some lat -> lat
  | None -> (
      match peek_kind parser with
      | Token.Question -> parse_optional_param parser
      | Token.Tilde -> parse_labeled_param parser
      | _ -> parse_pattern parser
    )

(** Parse function argument: expr | ~label | ~label:expr | ?label | ?label:expr 
    Grammar: argument ::= expr | "~" label-name | "~" label-name ":" expr 
                               | "?" label-name | "?" label-name ":" expr *)
and parse_argument = fun parser ->
  match peek_kind parser with
  | Token.Tilde ->
      (* Labeled argument: ~label or ~label:expr *)
      let tilde = consume parser in
      let trivia_after_tilde = consume_trivia parser in
      (* Must have a label name *)
      let label =
        parse_ident
          parser
          (fun parser found ->
            Diagnostic.invalid_expression
              ~found
              ~text:(token_text parser found)
              ~span:(current_span parser))
      in
      let trivia_after_label = consume_trivia parser in
      (* Check for :expr *)
      if peek_kind parser = Token.Colon then
        let colon = consume parser in
        let trivia_after_colon = consume_trivia parser in
        let value_expr = parse_assign_expr_no_comma parser in
        make_node
          Syntax_kind.LABELED_ARG
          ([ make_token parser tilde ]
          @ tokens_to_green parser trivia_after_tilde
          @ [ make_token parser label ]
          @ tokens_to_green parser trivia_after_label
          @ [ make_token parser colon ]
          @ tokens_to_green parser trivia_after_colon
          @ [ Ceibo.Green.Node value_expr ])
      else
        (* Just ~label (punning) *)
        make_node
          Syntax_kind.LABELED_ARG
          ([ make_token parser tilde ]
          @ tokens_to_green parser trivia_after_tilde
          @ [ make_token parser label ]
          @ tokens_to_green parser trivia_after_label)
  | Token.Question ->
      (* Optional argument: ?label or ?label:expr *)
      let question = consume parser in
      let trivia_after_question = consume_trivia parser in
      (* Must have a label name *)
      let label =
        parse_ident
          parser
          (fun parser found ->
            Diagnostic.invalid_expression
              ~found
              ~text:(token_text parser found)
              ~span:(current_span parser))
      in
      let trivia_after_label = consume_trivia parser in
      (* Check for :expr *)
      if peek_kind parser = Token.Colon then
        let colon = consume parser in
        let trivia_after_colon = consume_trivia parser in
        let value_expr = parse_assign_expr_no_comma parser in
        make_node
          Syntax_kind.OPTIONAL_ARG
          ([ make_token parser question ]
          @ tokens_to_green parser trivia_after_question
          @ [ make_token parser label ]
          @ tokens_to_green parser trivia_after_label
          @ [ make_token parser colon ]
          @ tokens_to_green parser trivia_after_colon
          @ [ Ceibo.Green.Node value_expr ])
      else
        (* Just ?label (punning) *)
        make_node
          Syntax_kind.OPTIONAL_ARG
          ([ make_token parser question ]
          @ tokens_to_green parser trivia_after_question
          @ [ make_token parser label ]
          @ tokens_to_green parser trivia_after_label)
  | _ ->
      (* Regular expression argument - use postfix to avoid infinite recursion *)
      parse_postfix_expr parser

(** Parse function expression: fun p1 p2 ... pn [: type] -> expr 
    Grammar: fun { parameter }+ [":" typexpr] "->" expr *)
and parse_fun_expr = fun parser ->
  match peek_kind parser with
  | Token.Keyword Keyword.Fun ->
      let fun_keyword = consume parser in
      let trivia_after_fun = consume_trivia parser in
      let ext_nodes, trivia_after_ext, attr_nodes = parse_keyword_extension_and_attributes parser in
      (* Parse parameters until we hit -> *)
      let rec collect_params acc depth =
        if depth > 100 then
          (
            let found = peek parser in
            let diagnostic = Diagnostic.invalid_expression
              ~found
              ~text:(token_text parser found)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            List.rev acc
          )
        else
          let trivia = consume_trivia parser in
          match peek_kind parser with
          | Token.Arrow ->
              (* Done collecting params *)
              List.rev (tokens_to_green parser trivia @ acc)
          | Token.Colon ->
              (* Return-type annotation starts here. *)
              List.rev (tokens_to_green parser trivia @ acc)
          | Token.EOF ->
              (* Reached EOF without finding -> *)
              List.rev (tokens_to_green parser trivia @ acc)
          | _ ->
              (* Parse one parameter - save position to detect infinite loops *)
              let pos_before = position parser in
              let param = parse_fun_param parser in
              let pos_after = position parser in
              (* If we didn't make progress, stop to prevent infinite loop *)
              if pos_before = pos_after then
                List.rev (tokens_to_green parser trivia @ acc)
              else
                collect_params
                  ([ Ceibo.Green.Node param ] @ tokens_to_green parser trivia @ acc)
                  (depth + 1)
      in
      let params = collect_params [] 0 in
      let type_annotation_nodes =
        if peek_kind parser = Token.Colon then
          let colon = consume parser in
          let trivia_after_colon = consume_trivia parser in
          let type_expr = parse_typexpr ~stop_before_arrow:true parser in
          let trivia_after_type = consume_trivia parser in
          [ make_token parser colon ]
          @ tokens_to_green parser trivia_after_colon
          @ [ Ceibo.Green.Node type_expr ]
          @ tokens_to_green parser trivia_after_type
        else
          []
      in
      (* Expect -> *)
      let arrow =
        expect
          parser
          Token.Arrow
          (fun found ->
            Diagnostic.invalid_expression
              ~found
              ~text:(token_text parser found)
              ~span:(expected_span parser))
      in
      let trivia_after_arrow = consume_trivia parser in
      (* Parse body expression *)
      let body = parse_expr parser in
      make_node
        Syntax_kind.FUN_EXPR
        ([ make_token parser fun_keyword ]
        @ tokens_to_green parser trivia_after_fun
        @ ext_nodes
        @ tokens_to_green parser trivia_after_ext
        @ attr_nodes
        @ params
        @ type_annotation_nodes
        @ [ make_token parser arrow ]
        @ tokens_to_green parser trivia_after_arrow
        @ [ Ceibo.Green.Node body ])
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse object type field: m : int *)
and parse_object_type_field = fun parser ->
  let name =
    match peek_kind parser with
    | Token.Ident _ -> consume parser
    | _ ->
        let found = peek parser in
        let diagnostic = Diagnostic.invalid_type_expression
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser) in
        report_diagnostic parser diagnostic;
        found
  in
  let trivia_after_name = consume_trivia parser in
  let colon =
    expect
      parser
      Token.Colon
      (fun found ->
        Diagnostic.invalid_type_expression
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser))
  in
  let trivia_after_colon = consume_trivia parser in
  let typ = parse_typexpr parser in
  make_node
    Syntax_kind.OBJECT_TYPE_FIELD
    ([ make_token parser name ]
    @ tokens_to_green parser trivia_after_name
    @ [ make_token parser colon ]
    @ tokens_to_green parser trivia_after_colon
    @ [ Ceibo.Green.Node typ ])

(** Parse object type: < m : int; n : string > *)
and parse_object_type = fun parser ->
  let open_angle = consume parser in
  let trivia_after_open = consume_trivia parser in
  let rec parse_fields acc =
    match peek_kind parser with
    | Token.Gt ->
        acc
    | Token.Semi ->
        let semi = consume parser in
        let trivia_after_semi = consume_trivia parser in
        parse_fields (acc @ [ make_token parser semi ] @ tokens_to_green parser trivia_after_semi)
    | Token.Ident _ ->
        let field = parse_object_type_field parser in
        let trivia_after_field = consume_trivia parser in
        parse_fields (acc @ [ Ceibo.Green.Node field ] @ tokens_to_green parser trivia_after_field)
    | _ ->
        let found = peek parser in
        let diagnostic = Diagnostic.invalid_type_expression
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser) in
        report_diagnostic parser diagnostic;
        acc
  in
  let fields = parse_fields [] in
  let close_angle =
    expect
      parser
      Token.Gt
      (fun found ->
        Diagnostic.invalid_type_expression
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser))
  in
  make_node
    Syntax_kind.OBJECT_TYPE
    ([ make_token parser open_angle ]
    @ tokens_to_green parser trivia_after_open
    @ fields
    @ [ make_token parser close_angle ])

(** Parse object method: method [private] name = expr *)
and parse_object_method = fun parser ->
  let method_kw = consume parser in
  let trivia_after_method = consume_trivia parser in
  let bang_children, trivia_after_bang =
    match peek_kind parser with
    | Token.Bang ->
        let bang = consume parser in
        let trivia = consume_trivia parser in
        ([ make_token parser bang ], trivia)
    | _ -> ([], [])
  in
  let private_children, trivia_after_private =
    match peek_kind parser with
    | Token.Keyword Keyword.Private ->
        let private_kw = consume parser in
        let trivia = consume_trivia parser in
        ([ make_token parser private_kw ], trivia)
    | _ -> ([], [])
  in
  let virtual_children, trivia_after_virtual =
    match peek_kind parser with
    | Token.Keyword Keyword.Virtual ->
        let virtual_kw = consume parser in
        let trivia = consume_trivia parser in
        ([ make_token parser virtual_kw ], trivia)
    | _ -> ([], [])
  in
  let name =
    match parse_ident_like parser with
    | node -> node
  in
  let trivia_after_name = consume_trivia parser in
  let rec parse_params acc =
    match peek_kind parser with
    | Token.Eq
    | Token.Colon
    | Token.EOF
    | Token.CloseDelim Token.ObjectEnd -> acc
    | _ ->
        let param = parse_pattern parser in
        let trivia_after_param = consume_trivia parser in
        parse_params (acc @ [ Ceibo.Green.Node param ] @ tokens_to_green parser trivia_after_param)
  in
  let params = parse_params [] in
  let type_children =
    match peek_kind parser with
    | Token.Colon ->
        let colon = consume parser in
        let trivia_after_colon = consume_trivia parser in
        let typ = parse_typexpr parser in
        let trivia_after_type = consume_trivia parser in
        [ make_token parser colon ]
        @ tokens_to_green parser trivia_after_colon
        @ [ Ceibo.Green.Node typ ]
        @ tokens_to_green parser trivia_after_type
    | _ -> []
  in
  let body_children =
    match peek_kind parser with
    | Token.Eq ->
        let eq = consume parser in
        let trivia_after_eq = consume_trivia parser in
        let body = parse_expr parser in
        let trivia_after_body = consume_trivia parser in
        [ make_token parser eq ]
        @ tokens_to_green parser trivia_after_eq
        @ [ Ceibo.Green.Node body ]
        @ tokens_to_green parser trivia_after_body
    | _ -> []
  in
  let attr_nodes = parse_attributes parser in
  make_node
    Syntax_kind.OBJECT_METHOD
    ([ make_token parser method_kw ]
    @ tokens_to_green parser trivia_after_method
    @ bang_children
    @ tokens_to_green parser trivia_after_bang
    @ private_children
    @ tokens_to_green parser trivia_after_private
    @ virtual_children
    @ tokens_to_green parser trivia_after_virtual
    @ [ Ceibo.Green.Node name ]
    @ tokens_to_green parser trivia_after_name
    @ params
    @ type_children
    @ body_children
    @ attr_nodes)

(** Parse object val: val [mutable] name = expr *)
and parse_object_val = fun parser ->
  let val_kw = consume parser in
  let trivia_after_val = consume_trivia parser in
  let bang_children, trivia_after_bang =
    match peek_kind parser with
    | Token.Bang ->
        let bang = consume parser in
        let trivia = consume_trivia parser in
        ([ make_token parser bang ], trivia)
    | _ -> ([], [])
  in
  let virtual_children, trivia_after_virtual =
    match peek_kind parser with
    | Token.Keyword Keyword.Virtual ->
        let virtual_kw = consume parser in
        let trivia = consume_trivia parser in
        ([ make_token parser virtual_kw ], trivia)
    | _ -> ([], [])
  in
  let mutable_children, trivia_after_mutable =
    match peek_kind parser with
    | Token.Keyword Keyword.Mutable ->
        let mutable_kw = consume parser in
        let trivia = consume_trivia parser in
        ([ make_token parser mutable_kw ], trivia)
    | _ -> ([], [])
  in
  let name = parse_ident_like parser in
  let trivia_after_name = consume_trivia parser in
  let type_children =
    match peek_kind parser with
    | Token.Colon ->
        let colon = consume parser in
        let trivia_after_colon = consume_trivia parser in
        let typ = parse_typexpr parser in
        let trivia_after_type = consume_trivia parser in
        [ make_token parser colon ]
        @ tokens_to_green parser trivia_after_colon
        @ [ Ceibo.Green.Node typ ]
        @ tokens_to_green parser trivia_after_type
    | _ -> []
  in
  let value_children =
    match peek_kind parser with
    | Token.Eq ->
        let eq = consume parser in
        let trivia_after_eq = consume_trivia parser in
        let value = parse_expr parser in
        let trivia_after_value = consume_trivia parser in
        [ make_token parser eq ]
        @ tokens_to_green parser trivia_after_eq
        @ [ Ceibo.Green.Node value ]
        @ tokens_to_green parser trivia_after_value
    | _ -> []
  in
  let attr_nodes = parse_attributes parser in
  make_node
    Syntax_kind.OBJECT_VAL
    ([ make_token parser val_kw ]
    @ tokens_to_green parser trivia_after_val
    @ bang_children
    @ tokens_to_green parser trivia_after_bang
    @ virtual_children
    @ tokens_to_green parser trivia_after_virtual
    @ mutable_children
    @ tokens_to_green parser trivia_after_mutable
    @ [ Ceibo.Green.Node name ]
    @ tokens_to_green parser trivia_after_name
    @ type_children
    @ value_children
    @ attr_nodes)

(** Parse object initializer: initializer expr *)
and parse_object_initializer = fun parser ->
  let initializer_kw = consume parser in
  let trivia_after_initializer = consume_trivia parser in
  let body = parse_expr parser in
  let trivia_after_body = consume_trivia parser in
  let attr_nodes = parse_attributes parser in
  make_node
    Syntax_kind.IDENT_EXPR
    ([ make_token parser initializer_kw ]
    @ tokens_to_green parser trivia_after_initializer
    @ [ Ceibo.Green.Node body ]
    @ tokens_to_green parser trivia_after_body
    @ attr_nodes)

(** Parse object inherit: inherit expr *)
and parse_object_inherit = fun parser ->
  let inherit_kw = consume parser in
  let trivia_after_inherit = consume_trivia parser in
  let inherited = parse_class_type_expr parser in
  let trivia_after_inherited = consume_trivia parser in
  let attr_nodes = parse_attributes parser in
  make_node
    Syntax_kind.OBJECT_INHERIT
    ([ make_token parser inherit_kw ]
    @ tokens_to_green parser trivia_after_inherit
    @ [ Ceibo.Green.Node inherited ]
    @ tokens_to_green parser trivia_after_inherited
    @ attr_nodes)

and parse_object_constraint = fun parser ->
  let constraint_kw = consume parser in
  let trivia_after_constraint = consume_trivia parser in
  let left_type = parse_typexpr parser in
  let trivia_after_left = consume_trivia parser in
  let eq_children =
    match peek_kind parser with
    | Token.Eq ->
        let eq = consume parser in
        [ make_token parser eq ]
    | _ ->
        let found = peek parser in
        let diagnostic = Diagnostic.invalid_type_expression
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser) in
        report_diagnostic parser diagnostic;
        []
  in
  let trivia_after_eq = consume_trivia parser in
  let right_type = parse_typexpr parser in
  let trivia_after_right = consume_trivia parser in
  let attr_nodes = parse_attributes parser in
  make_node
    Syntax_kind.TYPE_CONSTRAINT
    ([ make_token parser constraint_kw ]
    @ tokens_to_green parser trivia_after_constraint
    @ [ Ceibo.Green.Node left_type ]
    @ tokens_to_green parser trivia_after_left
    @ eq_children
    @ tokens_to_green parser trivia_after_eq
    @ [ Ceibo.Green.Node right_type ]
    @ tokens_to_green parser trivia_after_right
    @ attr_nodes)

(** Parse object expression: object [(self)] members end *)
and parse_object_expr = fun parser ->
  let object_kw = consume parser in
  let trivia_after_object = consume_trivia parser in
  let ext_nodes, trivia_after_ext, attr_nodes = parse_keyword_extension_and_attributes parser in
  let self_binding, trivia_after_self =
    match peek_kind parser with
    | Token.OpenDelim Token.Paren ->
        let open_paren = consume parser in
        let trivia_after_open = consume_trivia parser in
        let self_pattern = parse_pattern parser in
        let trivia_after_pattern = consume_trivia parser in
        let close_paren =
          expect
            parser
            (Token.CloseDelim Token.Paren)
            (fun found ->
              Diagnostic.unclosed_delimiter
                ~opener:"("
                ~found
                ~text:(token_text parser found)
                ~span:(expected_span parser))
        in
        let trivia = consume_trivia parser in
        (
          Some (make_node
            Syntax_kind.OBJECT_SELF
            ([ make_token parser open_paren ]
            @ tokens_to_green parser trivia_after_open
            @ [ Ceibo.Green.Node self_pattern ]
            @ tokens_to_green parser trivia_after_pattern
            @ [ make_token parser close_paren ])),
          trivia
        )
    | _ -> (None, [])
  in
  let rec parse_members acc =
    match peek_kind parser with
    | Token.CloseDelim Token.ObjectEnd
    | Token.EOF ->
        acc
    | Token.Keyword Keyword.Method ->
        let member = parse_object_method parser in
        let trivia = consume_trivia parser in
        parse_members (acc @ [ Ceibo.Green.Node member ] @ tokens_to_green parser trivia)
    | Token.Keyword Keyword.Val ->
        let member = parse_object_val parser in
        let trivia = consume_trivia parser in
        parse_members (acc @ [ Ceibo.Green.Node member ] @ tokens_to_green parser trivia)
    | Token.Keyword Keyword.Inherit ->
        let member = parse_object_inherit parser in
        let trivia = consume_trivia parser in
        parse_members (acc @ [ Ceibo.Green.Node member ] @ tokens_to_green parser trivia)
    | Token.Keyword Keyword.Constraint ->
        let member = parse_object_constraint parser in
        let trivia = consume_trivia parser in
        parse_members (acc @ [ Ceibo.Green.Node member ] @ tokens_to_green parser trivia)
    | Token.OpenDelim Token.Bracket when is_extension_start parser ->
        let member = parse_extension parser in
        let trivia = consume_trivia parser in
        parse_members (acc @ [ Ceibo.Green.Node member ] @ tokens_to_green parser trivia)
    | Token.OpenDelim Token.Brace when is_brace_extension_start parser ->
        let member = parse_extension parser in
        let trivia = consume_trivia parser in
        parse_members (acc @ [ Ceibo.Green.Node member ] @ tokens_to_green parser trivia)
    | Token.Keyword Keyword.Initializer ->
        let member = parse_object_initializer parser in
        let trivia = consume_trivia parser in
        parse_members (acc @ [ Ceibo.Green.Node member ] @ tokens_to_green parser trivia)
    | _ ->
        let found = consume parser in
        let diagnostic = Diagnostic.invalid_expression
          ~found
          ~text:(token_text parser found)
          ~span:(current_span parser) in
        let error = make_error_node parser ~diagnostic ~consumed_tokens:[ found ] in
        let trivia = consume_trivia parser in
        parse_members (acc @ [ Ceibo.Green.Node error ] @ tokens_to_green parser trivia)
  in
  let members = parse_members [] in
  let close_object =
    expect
      parser
      (Token.CloseDelim Token.ObjectEnd)
      (fun found ->
        Diagnostic.unclosed_delimiter
          ~opener:"object"
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser))
  in
  make_node Syntax_kind.OBJECT_EXPR
    (
      [ make_token parser object_kw ]
      @ tokens_to_green parser trivia_after_object
      @ ext_nodes
      @ tokens_to_green parser trivia_after_ext
      @ attr_nodes
      @ (
        match self_binding with
        | Some self_node -> [ Ceibo.Green.Node self_node ] @ tokens_to_green parser trivia_after_self
        | None -> []
      )
      @ members
      @ [ make_token parser close_object ]
    )

(** Parse primary expression (no operators, no function application) *)
and parse_primary_expr = fun parser ->
  match peek_kind parser with
  | Token.Minus
  | Token.MinusDot
  | Token.Bang
  | Token.Plus
  | Token.PlusDot
  | Token.Tilde
  | Token.Question ->
      let op = consume parser in
      let trivia_after_op = consume_trivia parser in
      let operand = parse_primary_expr parser in
      make_node
        Syntax_kind.PREFIX_EXPR
        ([ make_token parser op ]
        @ tokens_to_green parser trivia_after_op
        @ [ Ceibo.Green.Node operand ])
  | Token.Dot ->
      let dot = consume parser in
      make_node Syntax_kind.UNREACHABLE_EXPR [ make_token parser dot ]
  | Token.Ident _ ->
      let ident = consume parser in
      make_node Syntax_kind.IDENT_EXPR [ make_token parser ident ]
  | Token.Literal _ ->
      parse_constant parser
  | Token.Keyword Keyword.True ->
      parse_constant parser
  | Token.Keyword Keyword.False ->
      parse_constant parser
  | Token.Keyword Keyword.Fun ->
      parse_fun_expr parser
  | Token.Keyword Keyword.Function ->
      parse_function_expr parser
  | Token.Keyword Keyword.If ->
      parse_if_expr parser
  | Token.Keyword Keyword.Match ->
      parse_match_expr parser
  | Token.Keyword Keyword.Let ->
      parse_let_in_expr parser
  | Token.Keyword Keyword.Assert ->
      parse_assert_expr parser
  | Token.Keyword Keyword.Lazy ->
      parse_lazy_expr parser
  | Token.Keyword Keyword.New ->
      let new_kw = consume parser in
      let trivia_after_new = consume_trivia parser in
      let ext_nodes, trivia_after_ext, attr_nodes = parse_keyword_extension_and_attributes parser in
      let class_path = parse_module_path_or_expr parser in
      make_node
        Syntax_kind.NEW_EXPR
        ([ make_token parser new_kw ]
        @ tokens_to_green parser trivia_after_new
        @ ext_nodes
        @ tokens_to_green parser trivia_after_ext
        @ attr_nodes
        @ [ Ceibo.Green.Node class_path ])
  | Token.OpenDelim Token.ObjectEnd ->
      parse_object_expr parser
  | Token.Hash ->
      parse_hash_ident_expr parser
  | Token.Keyword Keyword.Try ->
      parse_try_expr parser
  | Token.Keyword Keyword.While ->
      parse_while_expr parser
  | Token.Keyword Keyword.For ->
      parse_for_expr parser
  | Token.OpenDelim Token.Paren when (peek_n parser 1).Token.kind = Token.Keyword Keyword.Let ->
      parse_parenthesized_let_expr parser
  | Token.OpenDelim Token.Paren ->
      parse_paren_expr parser
  | Token.OpenDelim Token.BeginEnd ->
      parse_begin_end_expr parser
  | Token.OpenDelim Token.Bracket ->
      (* Check if this is an extension [%...] or a list [...] *)
      if (peek_n parser 1).Token.kind = Token.Percent then
        parse_extension parser
      else
        parse_list_expr parser
  | Token.OpenDelim Token.Array ->
      parse_array_expr parser
  | Token.OpenDelim Token.Brace ->
      if is_brace_extension_start parser then
        parse_extension parser
      else
        parse_record_expr parser
  | Token.Backtick ->
      (* Polymorphic variant expression: `Tag or `Tag expr *)
      let backtick = consume parser in
      let trivia_after_backtick = consume_trivia parser in
      (* Expect tag name *)
      let tag_name =
        match peek_kind parser with
        | Token.Ident _ ->
            let ident = consume parser in
            make_token parser ident
        | _ ->
            let found_tok = peek parser in
            let diagnostic = Diagnostic.invalid_expression
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            make_token parser found_tok
      in
      let trivia_after_name = consume_trivia parser in
      (* Check if followed by argument expression *)
      let arg_expr =
        if can_start_poly_variant_payload_expr parser then
          let arg = parse_primary_expr parser in
          [ Ceibo.Green.Node arg ]
        else
          []
      in
      make_node
        Syntax_kind.POLY_VARIANT_EXPR
        ([ make_token parser backtick ]
        @ tokens_to_green parser trivia_after_backtick
        @ [ tag_name ]
        @ tokens_to_green parser trivia_after_name
        @ arg_expr)
  | Token.Quote ->
      parse_char_literal parser
  | Token.Unknown '\'' ->
      (* Malformed char literal from lexer (e.g., '', 'a) *)
      let tok = consume parser in
      let span = tok.Token.span in
      let text = token_text parser tok in
      let diagnostic =
        if text = "''" then
          Diagnostic.empty_char_literal ~span
        else
          Diagnostic.unclosed_char_literal ~text ~span
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[ tok ]
  | _ ->
      (* Unexpected token - consume it to avoid infinite loops *)
      let found_tok = consume parser in
      let found_text = token_text parser found_tok in
      (* Check if it's an operator (missing left operand) *)
      let diagnostic =
        if Option.is_some (operator_info found_tok.Token.kind) then
          Diagnostic.missing_binary_operand
            ~operator:found_text
            ~side:"left"
            ~found:found_tok
            ~text:found_text
            ~span:(current_span parser)
        else
          Diagnostic.invalid_expression ~found:found_tok ~text:found_text ~span:(current_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[ found_tok ]

(** Check if current token can be a prefix operator (unary operator).
    These tokens can appear after a binary operator and start a new expression. *)
and can_be_prefix_operator = fun token_kind ->
  match token_kind with
  | Token.Minus
  | Token.MinusDot
  | Token.Bang
  | Token.Plus
  | Token.PlusDot
  | Token.Tilde
  | Token.Question -> true
  | _ -> false

(** Check whether an opening paren followed by an operator starts an operator
    pattern like [(+)] or [( .%{;..} )], rather than a parenthesized unary
    expression like [(-1)] or [(!x)]. *)
and is_operator_pattern_paren = fun parser ->
  let saved_pos = Token_cursor.position parser.cursor in
  let result =
    match peek_kind parser with
    | tok when is_operator_token tok ->
        let first = consume parser in
        (
          match first.Token.kind with
          | Token.Keyword Keyword.Let
          | Token.Keyword Keyword.And -> is_operator_token (peek_kind parser)
          | _ -> not (can_be_prefix_operator first.Token.kind && can_start_arg_expr parser)
        )
    | _ -> false
  in
  Token_cursor.set_position parser.cursor saved_pos;
  result

(** Check if current token can start an argument expression.

    This is intentionally narrower than "can start any expression". In
    particular, prefix operators like [-] and [+] are excluded here so
    application parsing does not steal infix expressions such as [1 + 2] and
    reinterpret them as [1 (+2)]. Unary operators are still handled by
    [parse_primary_expr] when they appear in positions that genuinely start an
    expression. *)
and can_start_arg_expr = fun parser ->
  match peek_kind parser with
  | Token.Ident _
  | Token.Literal _
  | Token.Keyword Keyword.True
  | Token.Keyword Keyword.False
  | Token.Keyword Keyword.Fun
  | Token.Keyword Keyword.Function
  | Token.Keyword Keyword.If
  | Token.Keyword Keyword.Match
  | Token.OpenDelim Token.Paren
  | Token.OpenDelim Token.BeginEnd
  | Token.OpenDelim Token.Bracket
  | Token.OpenDelim Token.Array
  | Token.OpenDelim Token.Brace
  | Token.Dot
  | Token.Keyword Keyword.New
  | Token.OpenDelim Token.ObjectEnd
  | Token.Bang
  | Token.Quote
  | Token.Backtick
  | Token.Tilde
  | Token.Question -> true
  | _ -> false

and can_start_poly_variant_payload_expr = fun parser ->
  match peek_kind parser with
  | Token.Tilde
  | Token.Question -> false
  | _ -> can_start_arg_expr parser

(** Parse postfix expressions: field access (.), array indexing (.), etc.
    This has higher precedence than application.
    Grammar: expr ::= primary_expr { '.' ident | '.' '(' expr ')' | '.' '[' expr ']' }* *)
and parse_postfix_expr = fun parser ->
  let base = parse_primary_expr parser in
  let rec parse_postfix expr =
    match peek_kind parser with
    | Token.Hash ->
        let next_kind = (peek_n parser 1).Token.kind in
        (
          match next_kind with
          | Token.Ident _ ->
              let hash = consume parser in
              let trivia_after_hash = consume_trivia parser in
              let method_name = [ make_token parser (consume parser) ] in
              let trivia_after_method = consume_trivia parser in
              let method_call = make_node
                Syntax_kind.METHOD_CALL_EXPR
                ([ Ceibo.Green.Node expr ]
                @ [ make_token parser hash ]
                @ tokens_to_green parser trivia_after_hash
                @ method_name
                @ tokens_to_green parser trivia_after_method) in
              parse_postfix method_call
          | tok when is_operator_token tok ->
              expr
          | _ ->
              let hash = consume parser in
              let found = peek parser in
              let diagnostic = Diagnostic.invalid_expression
                ~found
                ~text:(token_text parser found)
                ~span:hash.Token.span in
              report_diagnostic parser diagnostic;
              expr
        )
    | Token.OpenDelim Token.Bracket ->
        (* Check if this is an attribute [@...] or array/string indexing with dot *)
        let next_kind = (peek_n parser 1).Token.kind in
        if next_kind = Token.At || next_kind = Token.AtAt then
          let attr = parse_attribute parser in
          let attr_expr = make_node
            Syntax_kind.ATTRIBUTE_EXPR
            [ Ceibo.Green.Node expr; Ceibo.Green.Node attr ] in
          parse_postfix attr_expr
        else
          expr
    | Token.Dot -> (
        let dot = consume parser in
        match peek_kind parser with
        | kind when is_custom_index_operator_token kind ->
            if not (has_custom_index_payload_after_dot parser) then
              expr
            else
              let trivia_after_dot = consume_trivia parser in
              let rec collect_op_tokens acc =
                match peek_kind parser with
                | kind when is_custom_index_operator_token kind ->
                    let tok = consume parser in
                    collect_op_tokens (acc @ [ make_token parser tok ])
                | _ -> acc
              in
              let op_tokens = collect_op_tokens [] in
              let trivia_after_op = consume_trivia parser in
              let open_delim =
                match peek_kind parser with
                | Token.OpenDelim Token.Paren
                | Token.OpenDelim Token.Bracket
                | Token.OpenDelim Token.Brace -> Some (consume parser)
                | _ -> None
              in
              (
                match open_delim with
                | Some open_tok ->
                    let trivia_after_open = consume_trivia parser in
                    let index_expr = parse_expr parser in
                    let trivia_after_expr = consume_trivia parser in
                    let close_delim =
                      match open_tok.Token.kind with
                      | Token.OpenDelim Token.Paren -> Token.CloseDelim Token.Paren
                      | Token.OpenDelim Token.Bracket -> Token.CloseDelim Token.Bracket
                      | Token.OpenDelim Token.Brace -> Token.CloseDelim Token.Brace
                      | _ -> Token.CloseDelim Token.Paren
                    in
                    let close_tok =
                      expect
                        parser
                        close_delim
                        (fun found ->
                          Diagnostic.unclosed_delimiter
                            ~opener:".("
                            ~found
                            ~text:(token_text parser found)
                            ~span:(expected_span parser))
                    in
                    let trivia_after_close = consume_trivia parser in
                    let custom_index =
                      make_node
                        Syntax_kind.ARRAY_INDEX_EXPR
                        ([ Ceibo.Green.Node expr ]
                        @ [ make_token parser dot ]
                        @ tokens_to_green parser trivia_after_dot
                        @ op_tokens
                        @ tokens_to_green parser trivia_after_op
                        @ [ make_token parser open_tok ]
                        @ tokens_to_green parser trivia_after_open
                        @ [ Ceibo.Green.Node index_expr ]
                        @ tokens_to_green parser trivia_after_expr
                        @ [ make_token parser close_tok ]
                        @ tokens_to_green parser trivia_after_close)
                    in
                    parse_postfix custom_index
                | None -> expr
              )
        | Token.Ident _ ->
            let trivia_after_dot = consume_trivia parser in
            let field = consume parser in
            let trivia_after_field = consume_trivia parser in
            let field_access = make_node
              Syntax_kind.FIELD_ACCESS_EXPR
              ([ Ceibo.Green.Node expr ]
              @ [ make_token parser dot ]
              @ tokens_to_green parser trivia_after_dot
              @ [ make_token parser field ]
              @ tokens_to_green parser trivia_after_field) in
            parse_postfix field_access
        | Token.OpenDelim Token.Paren -> (
            let trivia_after_dot = consume_trivia parser in
            let open_paren = consume parser in
            let trivia_after_open = consume_trivia parser in
            if green_expr_is_module_path_like expr then
              let starts_operator_expr =
                is_operator_token (peek_kind parser)
                || match peek_kind parser with
                | Token.Ident _ ->
                    let text = token_text parser (peek parser) in
                    is_keyword_operator_name text
                | _ -> false
              in
              if starts_operator_expr then
                let operator_children =
                  match peek_kind parser with
                  | Token.Ident _ -> [ make_token parser (consume parser) ]
                  | _ ->
                      let rec collect acc =
                        match peek_kind parser with
                        | tok when is_operator_token tok ->
                            let op = consume parser in
                            collect (acc @ [ make_token parser op ])
                        | _ -> acc
                      in
                      collect []
                in
                let trivia_after_operator = consume_trivia parser in
                match peek_kind parser with
                | Token.CloseDelim Token.Paren ->
                    let close_paren = consume parser in
                    let trivia_after_close = consume_trivia parser in
                    let operator_expr = make_node Syntax_kind.IDENT_EXPR operator_children in
                    let local_open =
                      make_node
                        Syntax_kind.LOCAL_OPEN_EXPR
                        ([ Ceibo.Green.Node expr ]
                        @ [ make_token parser dot ]
                        @ tokens_to_green parser trivia_after_dot
                        @ [ make_token parser open_paren ]
                        @ tokens_to_green parser trivia_after_open
                        @ [ Ceibo.Green.Node operator_expr ]
                        @ tokens_to_green parser trivia_after_operator
                        @ [ make_token parser close_paren ]
                        @ tokens_to_green parser trivia_after_close)
                    in
                    parse_postfix local_open
                | _ -> expr
              else
                let body_expr = parse_expr parser in
                let trivia_after_body = consume_trivia parser in
                match peek_kind parser with
                | Token.CloseDelim Token.Paren ->
                    let close_paren = consume parser in
                    let trivia_after_close = consume_trivia parser in
                    let local_open =
                      make_node
                        Syntax_kind.LOCAL_OPEN_EXPR
                        ([ Ceibo.Green.Node expr ]
                        @ [ make_token parser dot ]
                        @ tokens_to_green parser trivia_after_dot
                        @ [ make_token parser open_paren ]
                        @ tokens_to_green parser trivia_after_open
                        @ [ Ceibo.Green.Node body_expr ]
                        @ tokens_to_green parser trivia_after_body
                        @ [ make_token parser close_paren ]
                        @ tokens_to_green parser trivia_after_close)
                    in
                    parse_postfix local_open
                | _ -> expr
            else
              let index_expr = parse_expr parser in
              let trivia_after_expr = consume_trivia parser in
              match peek_kind parser with
              | Token.CloseDelim Token.Paren ->
                  let close_paren = consume parser in
                  let trivia_after_close = consume_trivia parser in
                  let array_index =
                    make_node
                      Syntax_kind.ARRAY_INDEX_EXPR
                      ([ Ceibo.Green.Node expr ]
                      @ [ make_token parser dot ]
                      @ tokens_to_green parser trivia_after_dot
                      @ [ make_token parser open_paren ]
                      @ tokens_to_green parser trivia_after_open
                      @ [ Ceibo.Green.Node index_expr ]
                      @ tokens_to_green parser trivia_after_expr
                      @ [ make_token parser close_paren ]
                      @ tokens_to_green parser trivia_after_close)
                  in
                  parse_postfix array_index
              | _ -> expr
          )
        | Token.OpenDelim Token.Bracket -> (
            let trivia_after_dot = consume_trivia parser in
            if green_expr_is_module_path_like expr then
              let list_expr = parse_list_expr parser in
              let local_open = make_node
                Syntax_kind.LOCAL_OPEN_EXPR
                ([ Ceibo.Green.Node expr ]
                @ [ make_token parser dot ]
                @ tokens_to_green parser trivia_after_dot
                @ [ Ceibo.Green.Node list_expr ]) in
              parse_postfix local_open
            else
              let open_bracket = consume parser in
              let trivia_after_open = consume_trivia parser in
              let index_expr = parse_expr parser in
              let trivia_after_expr = consume_trivia parser in
              match peek_kind parser with
              | Token.CloseDelim Token.Bracket ->
                  let close_bracket = consume parser in
                  let trivia_after_close = consume_trivia parser in
                  let string_index =
                    make_node
                      Syntax_kind.STRING_INDEX_EXPR
                      ([ Ceibo.Green.Node expr ]
                      @ [ make_token parser dot ]
                      @ tokens_to_green parser trivia_after_dot
                      @ [ make_token parser open_bracket ]
                      @ tokens_to_green parser trivia_after_open
                      @ [ Ceibo.Green.Node index_expr ]
                      @ tokens_to_green parser trivia_after_expr
                      @ [ make_token parser close_bracket ]
                      @ tokens_to_green parser trivia_after_close)
                  in
                  parse_postfix string_index
              | _ -> expr
          )
        | Token.OpenDelim Token.Brace ->
            let trivia_after_dot = consume_trivia parser in
            let record_expr = parse_record_expr parser in
            let local_open = make_node
              Syntax_kind.LOCAL_OPEN_EXPR
              ([ Ceibo.Green.Node expr ]
              @ [ make_token parser dot ]
              @ tokens_to_green parser trivia_after_dot
              @ [ Ceibo.Green.Node record_expr ]) in
            parse_postfix local_open
        | Token.OpenDelim Token.Array ->
            let trivia_after_dot = consume_trivia parser in
            let array_expr = parse_array_expr parser in
            let local_open = make_node
              Syntax_kind.LOCAL_OPEN_EXPR
              ([ Ceibo.Green.Node expr ]
              @ [ make_token parser dot ]
              @ tokens_to_green parser trivia_after_dot
              @ [ Ceibo.Green.Node array_expr ]) in
            parse_postfix local_open
        | _ ->
            expr
      )
    | _ ->
        expr
  in
  parse_postfix base

(** Parse function application: f x y z
    This has higher precedence than binary operators.
    Grammar: expr ::= expr { argument }+ *)
and parse_application_expr = fun parser ->
  let func = parse_postfix_expr parser in
  (* Keep parsing arguments while we can *)
  let rec parse_args func_expr =
    if can_start_arg_expr parser then
      let func_trivia = peek_trivia parser in
      let has_newline =
        List.exists
          (fun tok ->
            match tok.Token.kind with
            | Token.Whitespace ->
                let text = token_text parser tok in
                String.contains text "\n"
            | _ -> false)
          func_trivia
      in
      (* Check if next token is a structure-level keyword *)
      let is_structure_keyword =
        match peek_kind parser with
        | Token.Keyword Keyword.Let
        | Token.Keyword Keyword.Type
        | Token.Keyword Keyword.Module
        | Token.Keyword Keyword.Exception
        | Token.Keyword Keyword.Open
        | Token.Keyword Keyword.Include -> true
        | _ -> false
      in
      (* Don't parse as argument if we have newline + structure keyword *)
      if has_newline && is_structure_keyword then
        func_expr
      else
        let func_trivia = consume_trivia parser in
        let arg = parse_argument parser in
        (* Build application node: (f arg) *)
        let app_expr = make_node
          Syntax_kind.APPLY_EXPR
          ([ Ceibo.Green.Node func_expr ]
          @ tokens_to_green parser func_trivia
          @ [ Ceibo.Green.Node arg ]) in
        (* Continue parsing more arguments: ((f arg1) arg2) ... *)
        parse_args app_expr
    else
      func_expr
  in
  parse_args func

(** Parse binary expression with precedence climbing. This handles expressions
    like: 1 + 2 * 3, x || y && z, etc. *)
and parse_binary_expr = fun parser min_prec ->
  (* Parse left side (which may include function application) *)
  let left = parse_application_expr parser in
  (* Keep parsing operators while they have higher precedence *)
  let rec climb left =
    let current_operator =
      match peek_kind parser with
      | Token.Gt when parser.object_update_depth > 0 -> None
      | kind -> operator_info kind
    in
    match current_operator with
    | Some (prec, is_right_assoc) when prec >= min_prec ->
        let left_trivia = consume_trivia parser in
        let op = consume parser in
        let operator_children = ref [ make_token parser op ] in
        let operator_text_parts = ref [ token_text parser op ] in
        let rec collect_operator_suffix () =
          match op.Token.kind, peek_kind parser with
          | Token.Hash, tok when is_operator_token tok ->
              let trivia = consume_trivia parser in
              let next_op = consume parser in
              operator_children := !operator_children
              @ tokens_to_green parser trivia
              @ [ make_token parser next_op ];
              operator_text_parts := !operator_text_parts @ [ token_text parser next_op ];
              collect_operator_suffix ()
          | _ -> ()
        in
        collect_operator_suffix ();
        let op_text = String.concat "" !operator_text_parts in
        let trivia_after_op = consume_trivia parser in
        (* Check for consecutive operators or missing right operand *)
        let next_token_kind = peek_kind parser in
        let next_is_operator = Option.is_some (operator_info next_token_kind) in
        let next_can_be_prefix = can_be_prefix_operator next_token_kind in
        let at_end = next_token_kind = Token.EOF in
        if next_is_operator && not next_can_be_prefix then
          let next_op = peek parser in
          let next_op_text = token_text parser next_op in
          let operators = op_text ^ " " ^ next_op_text in
          let diagnostic = Diagnostic.consecutive_binary_operators
            ~operators
            ~found:next_op
            ~text:next_op_text
            ~span:(current_span parser) in
          (* Skip tokens until recovery point to avoid cascading errors *)
          let rec skip_to_recovery consumed_tokens =
            match peek_kind parser with
            | Token.Keyword Keyword.In
            | Token.EOF -> consumed_tokens
            | _ ->
                let tok = consume parser in
                skip_to_recovery (tok :: consumed_tokens)
          in
          let consumed = skip_to_recovery [ next_op ] in
          let error_node = make_error_node parser ~diagnostic ~consumed_tokens:(List.rev consumed) in
          (* Build partial binary expression with error on right *)
          let bin_expr = make_node
            Syntax_kind.INFIX_EXPR
            ([ Ceibo.Green.Node left ]
            @ tokens_to_green parser left_trivia
            @ !operator_children
            @ tokens_to_green parser trivia_after_op
            @ [ Ceibo.Green.Node error_node ]) in
          bin_expr
        else if at_end then
          let diagnostic = Diagnostic.missing_binary_operand
            ~operator:op_text
            ~side:"right"
            ~found:(peek parser)
            ~text:""
            ~span:(expected_span parser) in
          let error_node = make_error_node parser ~diagnostic ~consumed_tokens:[] in
          (* Build partial binary expression with error on right *)
          let bin_expr = make_node
            Syntax_kind.INFIX_EXPR
            ([ Ceibo.Green.Node left ]
            @ tokens_to_green parser left_trivia
            @ !operator_children
            @ tokens_to_green parser trivia_after_op
            @ [ Ceibo.Green.Node error_node ]) in
          bin_expr
        else
          (* Normal case: parse right side *)
          let next_min_prec =
            if is_right_assoc then
              prec
            else
              prec + 1
          in
          let right = parse_binary_expr parser next_min_prec in
          (* Build binary expression node *)
          let bin_expr = make_node
            Syntax_kind.INFIX_EXPR
            ([ Ceibo.Green.Node left ]
            @ tokens_to_green parser left_trivia
            @ !operator_children
            @ tokens_to_green parser trivia_after_op
            @ [ Ceibo.Green.Node right ]) in
          (* Continue climbing with the new left side *)
          climb bin_expr
    | _ ->
        (* No more operators, return current expression *)
        left
  in
  climb left

(** Parse tuple expression: e1, e2, e3
    This is lower precedence than binary operators.
    Grammar: expr ::= expr { "," expr }+ *)
and parse_tuple_expr = fun parser ->
  let first = parse_binary_expr parser 0 in
  (* Check if we have a comma (tuple) *)
  match peek_kind parser with
  | Token.Comma ->
      let trivia_after_first = consume_trivia parser in
      (* Parse comma-separated elements *)
      let rec parse_elements acc =
        match peek_kind parser with
        | Token.Comma ->
            let comma = consume parser in
            let trivia_after_comma = consume_trivia parser in
            let elem = parse_binary_expr parser 0 in
            let trivia_after_elem = consume_trivia parser in
            parse_elements
              (tokens_to_green parser trivia_after_elem
              @ [ Ceibo.Green.Node elem ]
              @ tokens_to_green parser trivia_after_comma
              @ [ make_token parser comma ]
              @ acc)
        | _ ->
            (* No more commas *)
            List.rev acc
      in
      let elements = parse_elements
        (tokens_to_green parser trivia_after_first @ [ Ceibo.Green.Node first ]) in
      make_node Syntax_kind.TUPLE_EXPR elements
  | _ -> first

(** Parse expression (top-level entry point) *)
and parse_expr = fun parser -> parse_sequence_expr parser

(** Parse assignment expression while reserving top-level commas for surrounding syntax. *)
and parse_assign_expr_no_comma = fun parser ->
  let left = parse_binary_expr parser 0 in
  (* Check for assignment operator *)
  match peek_kind parser with
  | Token.LeftArrow
  | Token.ColonEq ->
      let trivia_after_left = consume_trivia parser in
      let arrow = consume parser in
      let trivia_after_arrow = consume_trivia parser in
      let right =
        match peek_kind parser with
        | Token.EOF
        | Token.Keyword Keyword.In
        | Token.Keyword Keyword.Done
        | Token.Keyword Keyword.End
        | Token.CloseDelim Token.Paren
        | Token.CloseDelim Token.Bracket
        | Token.CloseDelim Token.Array
        | Token.CloseDelim Token.Brace
        | Token.CloseDelim Token.ObjectEnd ->
            let diagnostic = Diagnostic.missing_binary_operand
              ~operator:(token_text parser arrow)
              ~side:"right"
              ~found:(peek parser)
              ~text:(token_text parser (peek parser))
              ~span:(expected_span parser) in
            make_error_node parser ~diagnostic ~consumed_tokens:[]
        | _ -> parse_assign_expr parser
      in
      make_node
        Syntax_kind.ASSIGN_EXPR
        ([ Ceibo.Green.Node left ]
        @ tokens_to_green parser trivia_after_left
        @ [ make_token parser arrow ]
        @ tokens_to_green parser trivia_after_arrow
        @ [ Ceibo.Green.Node right ])
  | _ -> left

(** Parse assignment expression: lvalue <- expr or ref := expr *)
and parse_assign_expr = fun parser ->
  let left = parse_tuple_expr parser in
  (* Check for assignment operator *)
  match peek_kind parser with
  | Token.LeftArrow
  | Token.ColonEq ->
      let trivia_after_left = consume_trivia parser in
      let arrow = consume parser in
      let trivia_after_arrow = consume_trivia parser in
      let right =
        match peek_kind parser with
        | Token.EOF
        | Token.Keyword Keyword.In
        | Token.Keyword Keyword.Done
        | Token.Keyword Keyword.End
        | Token.CloseDelim Token.Paren
        | Token.CloseDelim Token.Bracket
        | Token.CloseDelim Token.Array
        | Token.CloseDelim Token.Brace
        | Token.CloseDelim Token.ObjectEnd ->
            let diagnostic = Diagnostic.missing_binary_operand
              ~operator:(token_text parser arrow)
              ~side:"right"
              ~found:(peek parser)
              ~text:(token_text parser (peek parser))
              ~span:(expected_span parser) in
            make_error_node parser ~diagnostic ~consumed_tokens:[]
        | _ -> parse_assign_expr parser
      in
      (* Right associative *)
      make_node
        Syntax_kind.ASSIGN_EXPR
        ([ Ceibo.Green.Node left ]
        @ tokens_to_green parser trivia_after_left
        @ [ make_token parser arrow ]
        @ tokens_to_green parser trivia_after_arrow
        @ [ Ceibo.Green.Node right ])
  | _ -> left

(** Parse sequence expression: expr1; expr2; expr3 *)
and parse_sequence_expr = fun parser ->
  let first = parse_assign_expr parser in
  (* Check if we have semicolons to make a sequence *)
  if
    peek_kind parser = Token.Semi
    && not (parser.record_field_depth > 0 && semicolon_belongs_to_record_field parser)
  then
    (
      let trivia_before_first_semi = consume_trivia parser in
      let parts = ref ([ Ceibo.Green.Node first ] @ tokens_to_green parser trivia_before_first_semi) in
      let rec parse_rest () =
        if peek_kind parser = Token.Semi then
          (
            let semi = consume parser in
            let trivia_after_semi = consume_trivia parser in
            parts := !parts @ [ make_token parser semi ] @ tokens_to_green parser trivia_after_semi;
            if
              peek_kind parser = Token.EOF
              || peek_kind parser = Token.Semi
              || peek_kind parser = Token.CloseDelim Token.Paren
              || peek_kind parser = Token.CloseDelim Token.BeginEnd
              || peek_kind parser = Token.CloseDelim Token.Bracket
              || peek_kind parser = Token.CloseDelim Token.Array
              || peek_kind parser = Token.CloseDelim Token.Brace
              || peek_kind parser = Token.Keyword Keyword.And
              || peek_kind parser = Token.Keyword Keyword.In
              || peek_kind parser = Token.Keyword Keyword.Done
              || peek_kind parser = Token.Keyword Keyword.End
              || peek_kind parser = Token.CloseDelim Token.ObjectEnd
              || (parser.case_expr_depth > 0 && peek_kind parser = Token.Pipe)
            then
              ()
            else
              let ext_nodes = parse_extension_name parser in
              let trivia_after_ext =
                if ext_nodes != [] then
                  consume_trivia parser
                else
                  []
              in
              let expr = parse_assign_expr parser in
              let expr =
                if ext_nodes = [] then
                  expr
                else
                  make_node
                    Syntax_kind.EXTENSION_EXPR
                    (ext_nodes @ tokens_to_green parser trivia_after_ext @ [ Ceibo.Green.Node expr ])
              in
              let trivia_after_expr = consume_trivia parser in
              parts := !parts @ [ Ceibo.Green.Node expr ] @ tokens_to_green parser trivia_after_expr;
              parse_rest ()
          )
      in
      parse_rest ();
      make_node Syntax_kind.SEQUENCE_EXPR !parts
    )
  else
    first

(** Parse parenthesized expression or unit: (expr) or () *)
and parse_parenthesized_let_expr = fun parser ->
  match peek_kind parser with
  | Token.OpenDelim Token.Paren -> (
      let lparen = consume parser in
      let trivia_after_lparen = consume_trivia parser in
      let expr = parse_let_in_expr parser in
      let trivia_after_expr = consume_trivia parser in
      let rparen_children =
        match peek_kind parser with
        | Token.CloseDelim Token.Paren ->
            let rparen = consume parser in
            [ make_token parser rparen ]
        | _ ->
            let found_tok = peek parser in
            let diagnostic = Diagnostic.unclosed_delimiter
              ~opener:"("
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            []
      in
      make_node
        Syntax_kind.PAREN_EXPR
        ([ make_token parser lparen ]
        @ tokens_to_green parser trivia_after_lparen
        @ [ Ceibo.Green.Node expr ]
        @ tokens_to_green parser trivia_after_expr
        @ rparen_children)
    )
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

and parse_paren_expr = fun parser ->
  match peek_kind parser with
  | Token.OpenDelim Token.Paren -> (
      let lparen = consume parser in
      let trivia_after_lparen = consume_trivia parser in
      (* Check if this is unit (), first-class module (module M), or a parenthesized expression *)
      match peek_kind parser with
      | Token.CloseDelim Token.Paren ->
          (* This is unit: () *)
          let rparen = consume parser in
          make_node
            Syntax_kind.UNIT_LITERAL
            ([ make_token parser lparen ]
            @ tokens_to_green parser trivia_after_lparen
            @ [ make_token parser rparen ])
      | tok when is_operator_token tok ->
          if is_operator_pattern_paren parser then
            parse_operator_pattern parser lparen trivia_after_lparen
          else
            let expr = parse_expr parser in
            let trivia_after_expr = consume_trivia parser in
            let rparen_children =
              match peek_kind parser with
              | Token.CloseDelim Token.Paren ->
                  let rparen = consume parser in
                  [ make_token parser rparen ]
              | _ ->
                  let found_tok = peek parser in
                  let diagnostic = Diagnostic.unclosed_delimiter
                    ~opener:"("
                    ~found:found_tok
                    ~text:(token_text parser found_tok)
                    ~span:(expected_span parser) in
                  report_diagnostic parser diagnostic;
                  []
            in
            make_node
              Syntax_kind.PAREN_EXPR
              ([ make_token parser lparen ]
              @ tokens_to_green parser trivia_after_lparen
              @ [ Ceibo.Green.Node expr ]
              @ tokens_to_green parser trivia_after_expr
              @ rparen_children)
      | Token.Keyword Keyword.Module ->
          (* This is a first-class module: (module M) or (module M : S) *)
          let module_kw = consume parser in
          let trivia_after_module = consume_trivia parser in
          (* Parse full module expression, including struct...end *)
          let module_expr =
            match peek_kind parser with
            | Token.Ident name when not (ident_starts_uppercase name) ->
                let ident = consume parser in
                let diagnostic = Diagnostic.invalid_module_name
                  ~found:ident
                  ~text:(token_text parser ident)
                  ~span:ident.Token.span in
                make_error_node parser ~diagnostic ~consumed_tokens:[ ident ]
            | _ when can_start_module_expr parser ->
                parse_module_expr parser
            | _ ->
                let diagnostic = Diagnostic.missing_module_expr
                  ~found:(peek parser)
                  ~text:(token_text parser (peek parser))
                  ~span:(expected_span parser) in
                make_error_node parser ~diagnostic ~consumed_tokens:[]
          in
          let trivia_after_expr = consume_trivia parser in
          (* Check for optional type constraint: (module M : S) *)
          let constraint_children =
            if peek_kind parser = Token.Colon then
              let colon = consume parser in
              let trivia_after_colon = consume_trivia parser in
              let sig_type = parse_module_type_expr parser in
              let trivia_after_type = consume_trivia parser in
              [ make_token parser colon ]
              @ tokens_to_green parser trivia_after_colon
              @ [ Ceibo.Green.Node sig_type ]
              @ tokens_to_green parser trivia_after_type
            else
              []
          in
          (* Expect closing paren *)
          let rparen_children =
            match peek_kind parser with
            | Token.CloseDelim Token.Paren ->
                let rparen = consume parser in
                [ make_token parser rparen ]
            | _ ->
                let found_tok = peek parser in
                let diagnostic = Diagnostic.unclosed_delimiter
                  ~opener:"("
                  ~found:found_tok
                  ~text:(token_text parser found_tok)
                  ~span:(expected_span parser) in
                report_diagnostic parser diagnostic;
                []
          in
          make_node
            Syntax_kind.FIRST_CLASS_MODULE_EXPR
            ([ make_token parser lparen ]
            @ tokens_to_green parser trivia_after_lparen
            @ [ make_token parser module_kw ]
            @ tokens_to_green parser trivia_after_module
            @ [ Ceibo.Green.Node module_expr ]
            @ tokens_to_green parser trivia_after_expr
            @ constraint_children
            @ rparen_children)
      | _ ->
          (* This is a parenthesized expression, typed expression, or coercion *)
          let expr = parse_expr parser in
          let trivia_after_expr = consume_trivia parser in
          (* Check what comes after the expression *)
          (
            match peek_kind parser with
            | Token.Colon -> (
                (* Could be typed expression (e : t) or coercion (e : t :> t2) or (e :> t) *)
                let colon = consume parser in
                let trivia_after_colon = consume_trivia parser in
                (* Check if this is :> (coercion without constraint) *)
                match peek_kind parser with
                | Token.Gt ->
                    (* This is (e :> t) - simple coercion *)
                    let gt = consume parser in
                    let trivia_after_gt = consume_trivia parser in
                    let target_type = parse_typexpr parser in
                    let trivia_after_type = consume_trivia parser in
                    let rparen_children =
                      match peek_kind parser with
                      | Token.CloseDelim Token.Paren ->
                          let rparen = consume parser in
                          [ make_token parser rparen ]
                      | _ ->
                          let found_tok = peek parser in
                          let diagnostic = Diagnostic.unclosed_delimiter
                            ~opener:"("
                            ~found:found_tok
                            ~text:(token_text parser found_tok)
                            ~span:(expected_span parser) in
                          report_diagnostic parser diagnostic;
                          []
                    in
                    make_node
                      Syntax_kind.COERCE_EXPR
                      ([ make_token parser lparen ]
                      @ tokens_to_green parser trivia_after_lparen
                      @ [ Ceibo.Green.Node expr ]
                      @ tokens_to_green parser trivia_after_expr
                      @ [ make_token parser colon ]
                      @ [ make_token parser gt ]
                      @ tokens_to_green parser trivia_after_gt
                      @ [ Ceibo.Green.Node target_type ]
                      @ tokens_to_green parser trivia_after_type
                      @ rparen_children)
                | _ ->
                    (* Parse the type expression *)
                    let type_expr = parse_typexpr parser in
                    let trivia_after_type = consume_trivia parser in
                    (* Check if followed by :> (constrained coercion) *)
                    (
                      match peek_kind parser with
                      | Token.Colon -> (
                          let colon2 = consume parser in
                          let trivia_after_colon2 = consume_trivia parser in
                          match peek_kind parser with
                          | Token.Gt ->
                              (* This is (e : t1 :> t2) - constrained coercion *)
                              let gt = consume parser in
                              let trivia_after_gt = consume_trivia parser in
                              let target_type = parse_typexpr parser in
                              let trivia_after_target = consume_trivia parser in
                              let rparen_children =
                                match peek_kind parser with
                                | Token.CloseDelim Token.Paren ->
                                    let rparen = consume parser in
                                    [ make_token parser rparen ]
                                | _ ->
                                    let found_tok = peek parser in
                                    let diagnostic = Diagnostic.unclosed_delimiter
                                      ~opener:"("
                                      ~found:found_tok
                                      ~text:(token_text parser found_tok)
                                      ~span:(expected_span parser) in
                                    report_diagnostic parser diagnostic;
                                    []
                              in
                              make_node
                                Syntax_kind.COERCE_EXPR
                                ([ make_token parser lparen ]
                                @ tokens_to_green parser trivia_after_lparen
                                @ [ Ceibo.Green.Node expr ]
                                @ tokens_to_green parser trivia_after_expr
                                @ [ make_token parser colon ]
                                @ tokens_to_green parser trivia_after_colon
                                @ [ Ceibo.Green.Node type_expr ]
                                @ tokens_to_green parser trivia_after_type
                                @ [ make_token parser colon2 ]
                                @ tokens_to_green parser trivia_after_colon2
                                @ [ make_token parser gt ]
                                @ tokens_to_green parser trivia_after_gt
                                @ [ Ceibo.Green.Node target_type ]
                                @ tokens_to_green parser trivia_after_target
                                @ rparen_children)
                          | _ ->
                              (* Not a coercion, just typed expression with extra colon - error *)
                              let found_tok = peek parser in
                              let diagnostic = Diagnostic.invalid_expression
                                ~found:found_tok
                                ~text:(token_text parser found_tok)
                                ~span:(expected_span parser) in
                              make_error_node parser ~diagnostic ~consumed_tokens:[]
                        )
                      | Token.CloseDelim Token.Paren ->
                          (* This is (e : t) - typed expression *)
                          let rparen = consume parser in
                          make_node
                            Syntax_kind.TYPED_EXPR
                            ([ make_token parser lparen ]
                            @ tokens_to_green parser trivia_after_lparen
                            @ [ Ceibo.Green.Node expr ]
                            @ tokens_to_green parser trivia_after_expr
                            @ [ make_token parser colon ]
                            @ tokens_to_green parser trivia_after_colon
                            @ [ Ceibo.Green.Node type_expr ]
                            @ tokens_to_green parser trivia_after_type
                            @ [ make_token parser rparen ])
                      | _ ->
                          (* Expected ) after type *)
                          let found_tok = peek parser in
                          let diagnostic = Diagnostic.unclosed_delimiter
                            ~opener:"("
                            ~found:found_tok
                            ~text:(token_text parser found_tok)
                            ~span:(expected_span parser) in
                          report_diagnostic parser diagnostic;
                          make_node
                            Syntax_kind.TYPED_EXPR
                            ([ make_token parser lparen ]
                            @ tokens_to_green parser trivia_after_lparen
                            @ [ Ceibo.Green.Node expr ]
                            @ tokens_to_green parser trivia_after_expr
                            @ [ make_token parser colon ]
                            @ tokens_to_green parser trivia_after_colon
                            @ [ Ceibo.Green.Node type_expr ]
                            @ tokens_to_green parser trivia_after_type)
                    )
              )
            | Token.CloseDelim Token.Paren ->
                (* Plain parenthesized expression (e) *)
                let rparen = consume parser in
                make_node
                  Syntax_kind.PAREN_EXPR
                  ([ make_token parser lparen ]
                  @ tokens_to_green parser trivia_after_lparen
                  @ [ Ceibo.Green.Node expr ]
                  @ tokens_to_green parser trivia_after_expr
                  @ [ make_token parser rparen ])
            | _ ->
                (* Expected ) but found something else *)
                let found_tok = peek parser in
                let diagnostic = Diagnostic.unclosed_delimiter
                  ~opener:"("
                  ~found:found_tok
                  ~text:(token_text parser found_tok)
                  ~span:(expected_span parser) in
                report_diagnostic parser diagnostic;
                make_node
                  Syntax_kind.PAREN_EXPR
                  ([ make_token parser lparen ]
                  @ tokens_to_green parser trivia_after_lparen
                  @ [ Ceibo.Green.Node expr ]
                  @ tokens_to_green parser trivia_after_expr)
          )
    )
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse begin...end expression: begin expr end *)
and parse_begin_end_expr = fun parser ->
  match peek_kind parser with
  | Token.OpenDelim Token.BeginEnd ->
      let begin_delim = consume parser in
      let trivia_after_begin = consume_trivia parser in
      let ext_nodes, trivia_after_ext, attr_nodes = parse_keyword_extension_and_attributes parser in
      let expr = parse_expr parser in
      let trivia_after_expr = consume_trivia parser in
      (* Expect 'end' (which is CloseDelim BeginEnd) *)
      let end_children =
        match peek_kind parser with
        | Token.CloseDelim Token.BeginEnd ->
            let end_delim = consume parser in
            [ make_token parser end_delim ]
        | _ ->
            let found_tok = peek parser in
            (* Point to end of last token for better error location *)
            let diagnostic = Diagnostic.unclosed_delimiter
              ~opener:"begin"
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            []
      in
      make_node
        Syntax_kind.PAREN_EXPR
        ([ make_token parser begin_delim ]
        @ tokens_to_green parser trivia_after_begin
        @ ext_nodes
        @ tokens_to_green parser trivia_after_ext
        @ attr_nodes
        @ [ Ceibo.Green.Node expr ]
        @ tokens_to_green parser trivia_after_expr
        @ end_children)
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse list expression: [1; 2; 3] or [] *)
and parse_list_expr = fun parser ->
  match peek_kind parser with
  | Token.OpenDelim Token.Bracket ->
      (* Check if this is actually an extension [%...] or attribute [@...] *)
      let saved_pos = Token_cursor.position parser.cursor in
      let _ = consume parser in
      (* consume [ *)
      let is_extension = peek_kind parser = Token.Percent in
      let is_attribute = peek_kind parser = Token.At || peek_kind parser = Token.AtAt in
      Token_cursor.set_position parser.cursor saved_pos;
      if is_extension then
        parse_extension parser
      else if is_attribute then
        parse_attribute parser
      else
        (* It's a regular list *)
        let open_bracket = consume parser in
        let trivia_after_open = consume_trivia parser in
        let elements = ref [] in
        let rec parse_elements () =
          match peek_kind parser with
          | Token.CloseDelim Token.Bracket
          | Token.EOF ->
              ()
          | Token.Semi ->
              let semi = consume parser in
              let trivia_after_semi = consume_trivia parser in
              if peek_kind parser = Token.CloseDelim Token.Bracket || peek_kind parser = Token.EOF then
                elements := !elements @ [ make_token parser semi ] @ tokens_to_green parser trivia_after_semi
              else
                let diagnostic = Diagnostic.list_double_semicolon
                  ~found:semi
                  ~text:(token_text parser semi)
                  ~span:semi.Token.span in
                let error_node = make_error_node parser ~diagnostic ~consumed_tokens:[ semi ] in
                elements := !elements @ [ Ceibo.Green.Node error_node ] @ tokens_to_green parser trivia_after_semi;
                parse_elements ()
          | _ ->
              let elem = parse_assign_expr parser in
              let trivia_after_elem = consume_trivia parser in
              elements := !elements @ [ Ceibo.Green.Node elem ] @ tokens_to_green parser trivia_after_elem;
              if peek_kind parser = Token.Semi then
                (
                  let semi = consume parser in
                  let trivia_after_semi = consume_trivia parser in
                  elements := !elements @ [ make_token parser semi ] @ tokens_to_green parser trivia_after_semi;
                  parse_elements ()
                )
        in
        parse_elements ();
        (* Expect closing bracket *)
        let close_children =
          match peek_kind parser with
          | Token.CloseDelim Token.Bracket ->
              let close_bracket = consume parser in
              [ make_token parser close_bracket ]
          | _ ->
              let found_tok = peek parser in
              let diagnostic = Diagnostic.unclosed_delimiter
                ~opener:"["
                ~found:found_tok
                ~text:(token_text parser found_tok)
                ~span:(expected_span parser) in
              report_diagnostic parser diagnostic;
              []
        in
        make_node
          Syntax_kind.LIST_EXPR
          ([ make_token parser open_bracket ]
          @ tokens_to_green parser trivia_after_open
          @ !elements
          @ close_children)
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse array expression: [|1; 2; 3|] or [||] *)
and parse_array_expr = fun parser ->
  match peek_kind parser with
  | Token.OpenDelim Token.Array ->
      let open_array = consume parser in
      let trivia_after_open = consume_trivia parser in
      let elements = ref [] in
      let rec parse_elements () =
        match peek_kind parser with
        | Token.CloseDelim Token.Array
        | Token.EOF -> ()
        | _ ->
            let elem = parse_assign_expr parser in
            let trivia_after_elem = consume_trivia parser in
            elements := !elements @ [ Ceibo.Green.Node elem ] @ tokens_to_green parser trivia_after_elem;
            if peek_kind parser = Token.Semi then
              (
                let semi = consume parser in
                let trivia_after_semi = consume_trivia parser in
                elements := !elements @ [ make_token parser semi ] @ tokens_to_green parser trivia_after_semi;
                parse_elements ()
              )
      in
      parse_elements ();
      (* Parse closing |] *)
      let close_children =
        match peek_kind parser with
        | Token.CloseDelim Token.Array ->
            let close_array = consume parser in
            [ make_token parser close_array ]
        | _ -> []
      in
      make_node
        Syntax_kind.ARRAY_EXPR
        ([ make_token parser open_array ] @ tokens_to_green parser trivia_after_open @ !elements @ close_children)
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse record expression: { field = value } or { base with field = value } *)
and parse_record_expr = fun parser ->
  match peek_kind parser with
  | Token.OpenDelim Token.Brace ->
      let open_brace = consume parser in
      let trivia_after_open = consume_trivia parser in
      (* Check for empty record {} *)
      if peek_kind parser = Token.CloseDelim Token.Brace then
        let close_brace = consume parser in
        make_node
          Syntax_kind.RECORD_EXPR
          ([ make_token parser open_brace ]
          @ tokens_to_green parser trivia_after_open
          @ [ make_token parser close_brace ])
      else if peek_kind parser = Token.Lt then
        let open_angle = consume parser in
        let trivia_after_angle = consume_trivia parser in
        let content_parts = ref [] in
        let previous_object_update_depth = parser.object_update_depth in
        parser.object_update_depth <- previous_object_update_depth + 1;
        let rec parse_fields () =
          if peek_kind parser = Token.Gt || peek_kind parser = Token.EOF then
            ()
          else
            (
              let field = parse_record_field parser in
              content_parts := !content_parts @ [ Ceibo.Green.Node field ];
              let trivia_after_field = consume_trivia parser in
              content_parts := !content_parts @ tokens_to_green parser trivia_after_field;
              if peek_kind parser = Token.Semi then
                (
                  let semi = consume parser in
                  let trivia_after_semi = consume_trivia parser in
                  content_parts := !content_parts
                  @ [ make_token parser semi ]
                  @ tokens_to_green parser trivia_after_semi;
                  parse_fields ()
                )
            )
        in
        parse_fields ();
        let close_angle =
          if peek_kind parser = Token.Gt then
            consume parser
          else
            peek parser
        in
        let trivia_before_close_brace = consume_trivia parser in
        let close_brace =
          if peek_kind parser = Token.CloseDelim Token.Brace then
            consume parser
          else
            peek parser
        in
        parser.object_update_depth <- previous_object_update_depth;
        make_node
          Syntax_kind.OBJECT_UPDATE_EXPR
          ([ make_token parser open_brace ]
          @ tokens_to_green parser trivia_after_open
          @ [ make_token parser open_angle ]
          @ tokens_to_green parser trivia_after_angle
          @ !content_parts
          @ [ make_token parser close_angle ]
          @ tokens_to_green parser trivia_before_close_brace
          @ [ make_token parser close_brace ])
      else
        (* Parse first identifier/expression to determine if it's update syntax *)
        (* { expr with ... } vs { field = ... } *)
        let first_expr =
          match peek_kind parser with
          | Token.OpenDelim Token.Brace
          | Token.OpenDelim Token.Paren -> parse_expr parser
          | _ -> parse_record_field parser
        in
        let trivia_after_first = consume_trivia parser in
        (* Check if this is record update: { expr with ... } *)
        if peek_kind parser = Token.Keyword Keyword.With then
          (
            let with_kw = consume parser in
            let trivia_after_with = consume_trivia parser in
            let content_parts = ref
              ([ Ceibo.Green.Node first_expr ]
              @ tokens_to_green parser trivia_after_first
              @ [ make_token parser with_kw ]
              @ tokens_to_green parser trivia_after_with) in
            let rec parse_fields () =
              if peek_kind parser = Token.CloseDelim Token.Brace || peek_kind parser = Token.EOF then
                ()
              else
                (
                  let field = parse_record_field parser in
                  content_parts := !content_parts @ [ Ceibo.Green.Node field ];
                  let trivia_after_field = consume_trivia parser in
                  content_parts := !content_parts @ tokens_to_green parser trivia_after_field;
                  if peek_kind parser = Token.Semi then
                    (
                      let semi = consume parser in
                      let trivia_after_semi = consume_trivia parser in
                      content_parts := !content_parts
                      @ [ make_token parser semi ]
                      @ tokens_to_green parser trivia_after_semi;
                      if peek_kind parser != Token.CloseDelim Token.Brace then
                        parse_fields ()
                    )
                )
            in
            parse_fields ();
            let trivia_before_close = consume_trivia parser in
            let close_brace =
              if peek_kind parser = Token.CloseDelim Token.Brace then
                consume parser
              else
                peek parser
            in
            make_node
              Syntax_kind.RECORD_UPDATE_EXPR
              ([ make_token parser open_brace ]
              @ tokens_to_green parser trivia_after_open
              @ !content_parts
              @ tokens_to_green parser trivia_before_close
              @ [ make_token parser close_brace ])
          )
        else
          (* Regular record: { field = value; ... } *)
          (* The first_expr is actually a field *)
          let content_parts = ref
            ([ Ceibo.Green.Node first_expr ] @ tokens_to_green parser trivia_after_first) in
          let rec parse_fields () =
            if peek_kind parser = Token.Semi then
              (
                let semi = consume parser in
                let trivia_after_semi = consume_trivia parser in
                content_parts := !content_parts
                @ [ make_token parser semi ]
                @ tokens_to_green parser trivia_after_semi;
                if peek_kind parser = Token.CloseDelim Token.Brace then
                  ()
                else
                  (
                    let field = parse_record_field parser in
                    content_parts := !content_parts @ [ Ceibo.Green.Node field ];
                    let trivia_after_field = consume_trivia parser in
                    content_parts := !content_parts @ tokens_to_green parser trivia_after_field;
                    parse_fields ()
                  )
              )
          in
          parse_fields ();
          let trivia_before_close = consume_trivia parser in
          let close_brace =
            if peek_kind parser = Token.CloseDelim Token.Brace then
              consume parser
            else
              peek parser
          in
          make_node
            Syntax_kind.RECORD_EXPR
            ([ make_token parser open_brace ]
            @ tokens_to_green parser trivia_after_open
            @ !content_parts
            @ tokens_to_green parser trivia_before_close
            @ [ make_token parser close_brace ])
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse a single record field: field = expr or field (shorthand) *)
and parse_record_field = fun parser ->
  (* Parse field name - can be qualified like Module.field *)
  let field_name_parts =
    match peek_kind parser with
    | Token.Ident _ ->
        let first = consume parser in
        (* Check for module path: Module.field *)
        let rec parse_path acc =
          let trivia = consume_trivia parser in
          if has_dot_ident_continuation parser then
            let dot = consume parser in
            let trivia2 = consume_trivia parser in
            let next = consume parser in
            parse_path
              (acc
              @ tokens_to_green parser trivia
              @ [ make_token parser dot ]
              @ tokens_to_green parser trivia2
              @ [ make_token parser next ])
          else
            acc @ tokens_to_green parser trivia
        in
        [ make_token parser first ] @ parse_path []
    | _ -> [ make_token parser (peek parser) ]
  in
  let trivia_after_name = consume_trivia parser in
  (* Check for = *)
  if peek_kind parser = Token.Eq then
    let eq = consume parser in
    let trivia_after_eq = consume_trivia parser in
    let previous_record_field_depth = parser.record_field_depth in
    parser.record_field_depth <- previous_record_field_depth + 1;
    let value_expr = parse_assign_expr parser in
    parser.record_field_depth <- previous_record_field_depth;
    make_node
      Syntax_kind.RECORD_FIELD
      (field_name_parts
      @ tokens_to_green parser trivia_after_name
      @ [ make_token parser eq ]
      @ tokens_to_green parser trivia_after_eq
      @ [ Ceibo.Green.Node value_expr ])
  else
    (* Shorthand: just field name, equivalent to field = field *)
    make_node Syntax_kind.RECORD_FIELD (field_name_parts @ tokens_to_green parser trivia_after_name)

(** Parse if-then-else expression: if cond then e1 else e2 *)
and parse_if_expr = fun parser ->
  match peek_kind parser with
  | Token.Keyword Keyword.If ->
      let if_kw = consume parser in
      let trivia_after_if = consume_trivia parser in
      let ext_nodes, trivia_after_ext, attr_nodes = parse_keyword_extension_and_attributes parser in
      (* Parse condition *)
      let cond = parse_expr parser in
      let trivia_after_cond = consume_trivia parser in
      (* Expect 'then' keyword *)
      let has_then = peek_kind parser = Token.Keyword Keyword.Then in
      let then_children, trivia_after_then, then_expr, trivia_after_then_expr =
        if has_then then
          let then_kw = consume parser in
          let trivia_after_then = consume_trivia parser in
          let then_expr = parse_expr parser in
          let trivia_after_then_expr = consume_trivia parser in
          ([ make_token parser then_kw ], trivia_after_then, then_expr, trivia_after_then_expr)
        else
          (* Missing 'then' - report error and skip to 'else' *)
          let found_tok = peek parser in
          let diagnostic = Diagnostic.if_missing_then
            ~found:found_tok
            ~text:(token_text parser found_tok)
            ~span:(expected_span parser) in
          report_diagnostic parser diagnostic;
          (* Skip tokens until we find 'else' or a stopping point *)
          let error_tokens = ref [] in
          while not
            (peek_kind parser = Token.Keyword Keyword.Else
            || peek_kind parser = Token.Keyword Keyword.In
            || peek_kind parser = Token.Semi
            || peek_kind parser = Token.EOF) do
            error_tokens := consume parser :: !error_tokens
          done;
          (* Wrap consumed tokens in ERROR node *)
          let error_children = tokens_to_green parser (List.rev !error_tokens) in
          let error_node = make_node Syntax_kind.ERROR error_children in
          ([], [], error_node, [])
      in
      (* Optional 'else' keyword and branch *)
      let else_parts =
        match peek_kind parser with
        | Token.Keyword Keyword.Else ->
            let else_kw = consume parser in
            let trivia_after_else = consume_trivia parser in
            let else_expr = parse_expr parser in
            [ make_token parser else_kw ]
            @ tokens_to_green parser trivia_after_else
            @ [ Ceibo.Green.Node else_expr ]
        | _ -> []
      in
      make_node
        Syntax_kind.IF_EXPR
        ([ make_token parser if_kw ]
        @ tokens_to_green parser trivia_after_if
        @ ext_nodes
        @ tokens_to_green parser trivia_after_ext
        @ attr_nodes
        @ [ Ceibo.Green.Node cond ]
        @ tokens_to_green parser trivia_after_cond
        @ then_children
        @ tokens_to_green parser trivia_after_then
        @ [ Ceibo.Green.Node then_expr ]
        @ tokens_to_green parser trivia_after_then_expr
        @ else_parts)
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse function expression: function | pattern -> expr | ... *)
and parse_function_expr = fun parser ->
  match peek_kind parser with
  | Token.Keyword Keyword.Function ->
      let function_kw = consume parser in
      let trivia_after_function = consume_trivia parser in
      let ext_nodes, trivia_after_ext, attr_nodes = parse_keyword_extension_and_attributes parser in
      (* Parse match cases: | pattern -> expr *)
      let cases = ref [] in
      let trivia_parts = ref [] in
      let rec parse_cases () =
        match peek_kind parser with
        | Token.Pipe ->
            let case = parse_match_case parser in
            let trivia = consume_trivia parser in
            trivia_parts := trivia :: !trivia_parts;
            cases := case :: !cases;
            parse_cases ()
        | _ ->
            (* No more cases *)
            ()
      in
      (* Check if first case has optional leading pipe *)
      let first_pipe_optional =
        if peek_kind parser = Token.Pipe then
          (
            let pipe = consume parser in
            let trivia = consume_trivia parser in
            Some (pipe, trivia)
          )
        else
          None
      in
      (* Parse first case (required) *)
      let first_case =
        if can_start_pattern parser then
          parse_match_case parser
        else
          (* Missing pattern - create error node *)
          let found_tok = peek parser in
          let diagnostic = Diagnostic.match_missing_pattern
            ~found:found_tok
            ~text:(token_text parser found_tok)
            ~span:(expected_span parser) in
          make_error_node parser ~diagnostic ~consumed_tokens:[]
      in
      let trivia_after_first = consume_trivia parser in
      (* Parse remaining cases *)
      parse_cases ();
      (* Build children list - interleave cases with trivia *)
      let case_list = List.rev !cases in
      let trivia_list = List.rev !trivia_parts in
      let rec interleave cases trivias acc =
        match (cases, trivias) with
        | [], [] -> List.rev acc
        | c :: cs, [] -> interleave cs [] (Ceibo.Green.Node c :: acc)
        | c :: cs, t :: ts -> interleave
          cs
          ts
          (List.rev_append (tokens_to_green parser t) (Ceibo.Green.Node c :: acc))
        | [], _ :: _ -> List.rev acc
      in
      let case_elements = interleave case_list trivia_list [] in
      let children =
        [ make_token parser function_kw ]
        @ tokens_to_green parser trivia_after_function
        @ ext_nodes
        @ tokens_to_green parser trivia_after_ext
        @ attr_nodes
        @ (
          match first_pipe_optional with
          | Some (pipe, trivia) -> [ make_token parser pipe ] @ tokens_to_green parser trivia
          | None -> []
        )
        @ [ Ceibo.Green.Node first_case ]
        @ tokens_to_green parser trivia_after_first
        @ case_elements
      in
      make_node Syntax_kind.FUNCTION_EXPR children
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse match expression: match expr with | pattern -> expr | ... *)
and parse_match_expr = fun parser ->
  match peek_kind parser with
  | Token.Keyword Keyword.Match ->
      let match_kw = consume parser in
      let trivia_after_match = consume_trivia parser in
      let ext_nodes, trivia_after_ext, attr_nodes = parse_keyword_extension_and_attributes parser in
      (* Parse scrutinee expression - check if 'with' comes immediately *)
      let scrutinee, trivia_after_scrutinee =
        if peek_kind parser = Token.Keyword Keyword.With then
          (
            (* Missing scrutinee *)
            let found_tok = peek parser in
            let diagnostic = Diagnostic.match_missing_scrutinee
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            let error_node = make_node Syntax_kind.ERROR [] in
            (error_node, [])
          )
        else
          let scrut = parse_expr parser in
          let trivia = consume_trivia parser in
          (scrut, trivia)
      in
      (* Expect 'with' keyword *)
      let with_children =
        match peek_kind parser with
        | Token.Keyword Keyword.With ->
            let with_kw = consume parser in
            [ make_token parser with_kw ]
        | _ ->
            let found_tok = peek parser in
            let diagnostic = Diagnostic.match_missing_with
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            []
      in
      let trivia_after_with = consume_trivia parser in
      (* Parse match cases: | pattern -> expr *)
      let cases = ref [] in
      let trivia_parts = ref [] in
      let rec parse_cases () =
        match peek_kind parser with
        | Token.Pipe ->
            let case = parse_match_case parser in
            cases := case :: !cases;
            let trivia = consume_trivia parser in
            trivia_parts := trivia :: !trivia_parts;
            parse_cases ()
        | _ -> ()
      in
      (* First case might not have leading pipe *)
      (
        match peek_kind parser with
        | Token.Pipe -> parse_cases ()
        | _ ->
            (* Try to parse a case without pipe *)
            if
              peek_kind parser != Token.EOF
              && peek_kind parser != Token.Keyword Keyword.In
              && peek_kind parser != Token.Semi
            then
              (
                let case = parse_match_case parser in
                cases := [ case ];
                let trivia = consume_trivia parser in
                trivia_parts := [ trivia ];
                parse_cases ()
              )
      );
      (* Interleave cases with trivia *)
      let case_list = List.rev !cases in
      let trivia_list = List.rev !trivia_parts in
      let rec interleave cases trivias acc =
        match (cases, trivias) with
        | [], [] -> List.rev acc
        | c :: cs, [] -> interleave cs [] (Ceibo.Green.Node c :: acc)
        | c :: cs, t :: ts -> interleave
          cs
          ts
          (List.rev_append (tokens_to_green parser t) (Ceibo.Green.Node c :: acc))
        | [], _ :: _ -> List.rev acc
      in
      let case_elements = interleave case_list trivia_list [] in
      make_node
        Syntax_kind.MATCH_EXPR
        ([ make_token parser match_kw ]
        @ tokens_to_green parser trivia_after_match
        @ ext_nodes
        @ tokens_to_green parser trivia_after_ext
        @ attr_nodes
        @ [ Ceibo.Green.Node scrutinee ]
        @ tokens_to_green parser trivia_after_scrutinee
        @ with_children
        @ tokens_to_green parser trivia_after_with
        @ case_elements)
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse a match case: | pattern -> expr or | pattern when guard -> expr *)
and parse_match_case = fun parser ->
  (* Optional leading pipe *)
  let pipe_tok =
    if peek_kind parser = Token.Pipe then
      let tok = consume parser in
      [ make_token parser tok ]
    else
      []
  in
  let trivia_after_pipe = consume_trivia parser in
  (* Parse pattern - check if arrow comes immediately *)
  let pattern, trivia_after_pattern =
    if peek_kind parser = Token.Arrow then
      (
        (* Missing pattern *)
        let found_tok = peek parser in
        let diagnostic = Diagnostic.match_missing_pattern
          ~found:found_tok
          ~text:(token_text parser found_tok)
          ~span:(expected_span parser) in
        report_diagnostic parser diagnostic;
        let error_node = make_node Syntax_kind.ERROR [] in
        (error_node, [])
      )
    else
      let pat = parse_pattern parser in
      let trivia = consume_trivia parser in
      (pat, trivia)
  in
  (* Optional 'when' guard *)
  let guard_parts =
    match peek_kind parser with
    | Token.Keyword Keyword.When ->
        let when_kw = consume parser in
        let trivia_after_when = consume_trivia parser in
        (* Check if arrow comes immediately after when *)
        let guard_expr, trivia_after_guard =
          if peek_kind parser = Token.Arrow then
            (
              (* Missing guard expression *)
              let found_tok = peek parser in
              let diagnostic = Diagnostic.match_guard_missing_expr
                ~found:found_tok
                ~text:(token_text parser found_tok)
                ~span:(expected_span parser) in
              report_diagnostic parser diagnostic;
              let error_node = make_node Syntax_kind.ERROR [] in
              (error_node, [])
            )
          else
            let expr = parse_expr parser in
            let trivia = consume_trivia parser in
            (expr, trivia)
        in
        [ make_token parser when_kw ]
        @ tokens_to_green parser trivia_after_when
        @ [ Ceibo.Green.Node guard_expr ]
        @ tokens_to_green parser trivia_after_guard
    | _ -> []
  in
  (* Expect '->' *)
  let arrow_children =
    match peek_kind parser with
    | Token.Arrow ->
        let arrow = consume parser in
        [ make_token parser arrow ]
    | _ ->
        let found_tok = peek parser in
        let diagnostic = Diagnostic.invalid_expression
          ~found:found_tok
          ~text:(token_text parser found_tok)
          ~span:(expected_span parser) in
        report_diagnostic parser diagnostic;
        []
  in
  let trivia_after_arrow = consume_trivia parser in
  (* Parse case expression *)
  let previous_case_expr_depth = parser.case_expr_depth in
  parser.case_expr_depth <- previous_case_expr_depth + 1;
  let case_expr = parse_expr parser in
  parser.case_expr_depth <- previous_case_expr_depth;
  make_node
    Syntax_kind.MATCH_CASE
    (pipe_tok
    @ tokens_to_green parser trivia_after_pipe
    @ [ Ceibo.Green.Node pattern ]
    @ tokens_to_green parser trivia_after_pattern
    @ guard_parts
    @ arrow_children
    @ tokens_to_green parser trivia_after_arrow
    @ [ Ceibo.Green.Node case_expr ])

(** Parse assert expression: assert expr *)
and parse_assert_expr = fun parser ->
  match peek_kind parser with
  | Token.Keyword Keyword.Assert ->
      let assert_kw = consume parser in
      let trivia_after_assert = consume_trivia parser in
      let ext_nodes, trivia_after_ext, attr_nodes = parse_keyword_extension_and_attributes parser in
      let expr = parse_expr parser in
      make_node
        Syntax_kind.ASSERT_EXPR
        ([ make_token parser assert_kw ]
        @ tokens_to_green parser trivia_after_assert
        @ ext_nodes
        @ tokens_to_green parser trivia_after_ext
        @ attr_nodes
        @ [ Ceibo.Green.Node expr ])
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse lazy expression: lazy expr *)
and parse_lazy_expr = fun parser ->
  match peek_kind parser with
  | Token.Keyword Keyword.Lazy ->
      let lazy_kw = consume parser in
      let trivia_after_lazy = consume_trivia parser in
      let ext_nodes, trivia_after_ext, attr_nodes = parse_keyword_extension_and_attributes parser in
      let expr = parse_expr parser in
      make_node
        Syntax_kind.LAZY_EXPR
        ([ make_token parser lazy_kw ]
        @ tokens_to_green parser trivia_after_lazy
        @ ext_nodes
        @ tokens_to_green parser trivia_after_ext
        @ attr_nodes
        @ [ Ceibo.Green.Node expr ])
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse try expression: try expr with | pattern -> expr | ... *)
and parse_try_expr = fun parser ->
  match peek_kind parser with
  | Token.Keyword Keyword.Try ->
      let try_kw = consume parser in
      let trivia_after_try = consume_trivia parser in
      let ext_nodes, trivia_after_ext, attr_nodes = parse_keyword_extension_and_attributes parser in
      (* Parse try body expression *)
      let body = parse_expr parser in
      let trivia_after_body = consume_trivia parser in
      (* Expect 'with' keyword *)
      let with_children =
        match peek_kind parser with
        | Token.Keyword Keyword.With ->
            let with_kw = consume parser in
            [ make_token parser with_kw ]
        | _ ->
            let found_tok = peek parser in
            let diagnostic = Diagnostic.invalid_expression
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            []
      in
      let trivia_after_with = consume_trivia parser in
      (* Parse exception cases: | pattern -> expr *)
      let cases = ref [] in
      let trivia_parts = ref [] in
      let rec parse_cases () =
        match peek_kind parser with
        | Token.Pipe ->
            let case = parse_match_case parser in
            cases := case :: !cases;
            let trivia = consume_trivia parser in
            trivia_parts := trivia :: !trivia_parts;
            parse_cases ()
        | _ -> ()
      in
      (* First case might not have leading pipe *)
      (
        match peek_kind parser with
        | Token.Pipe -> parse_cases ()
        | _ ->
            (* Try to parse a case without pipe *)
            if peek_kind parser != Token.EOF then
              (
                let case = parse_match_case parser in
                cases := [ case ];
                let trivia = consume_trivia parser in
                trivia_parts := [ trivia ];
                parse_cases ()
              )
      );
      (* Interleave cases with trivia *)
      let case_list = List.rev !cases in
      let trivia_list = List.rev !trivia_parts in
      let rec interleave cases trivias acc =
        match (cases, trivias) with
        | [], [] -> List.rev acc
        | c :: cs, [] -> interleave cs [] (Ceibo.Green.Node c :: acc)
        | c :: cs, t :: ts -> interleave
          cs
          ts
          (List.rev_append (tokens_to_green parser t) (Ceibo.Green.Node c :: acc))
        | [], _ :: _ -> List.rev acc
      in
      let case_elements = interleave case_list trivia_list [] in
      make_node
        Syntax_kind.TRY_EXPR
        ([ make_token parser try_kw ]
        @ tokens_to_green parser trivia_after_try
        @ ext_nodes
        @ tokens_to_green parser trivia_after_ext
        @ attr_nodes
        @ [ Ceibo.Green.Node body ]
        @ tokens_to_green parser trivia_after_body
        @ with_children
        @ tokens_to_green parser trivia_after_with
        @ case_elements)
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse while expression: while cond do body done *)
and parse_while_expr = fun parser ->
  match peek_kind parser with
  | Token.Keyword Keyword.While ->
      let while_kw = consume parser in
      let trivia_after_while = consume_trivia parser in
      let ext_nodes, trivia_after_ext, attr_nodes = parse_keyword_extension_and_attributes parser in
      (* Parse condition expression *)
      let cond = parse_expr parser in
      let trivia_after_cond = consume_trivia parser in
      (* Expect 'do' keyword *)
      let do_children =
        match peek_kind parser with
        | Token.Keyword Keyword.Do ->
            let do_kw = consume parser in
            [ make_token parser do_kw ]
        | _ ->
            let found_tok = peek parser in
            let diagnostic = Diagnostic.invalid_expression
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            []
      in
      let trivia_after_do = consume_trivia parser in
      (* Parse body expression *)
      let body = parse_expr parser in
      let trivia_after_body = consume_trivia parser in
      (* Expect 'done' keyword *)
      let done_children =
        match peek_kind parser with
        | Token.Keyword Keyword.Done ->
            let done_kw = consume parser in
            [ make_token parser done_kw ]
        | _ ->
            let found_tok = peek parser in
            let diagnostic = Diagnostic.invalid_expression
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            []
      in
      make_node
        Syntax_kind.WHILE_EXPR
        ([ make_token parser while_kw ]
        @ tokens_to_green parser trivia_after_while
        @ ext_nodes
        @ tokens_to_green parser trivia_after_ext
        @ attr_nodes
        @ [ Ceibo.Green.Node cond ]
        @ tokens_to_green parser trivia_after_cond
        @ do_children
        @ tokens_to_green parser trivia_after_do
        @ [ Ceibo.Green.Node body ]
        @ tokens_to_green parser trivia_after_body
        @ done_children)
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse for expression: for x = e1 to/downto e2 do body done *)
and parse_for_expr = fun parser ->
  match peek_kind parser with
  | Token.Keyword Keyword.For ->
      let for_kw = consume parser in
      let trivia_after_for = consume_trivia parser in
      let ext_nodes, trivia_after_ext, attr_nodes = parse_keyword_extension_and_attributes parser in
      (* Parse loop variable (identifier) *)
      let var_ident = consume parser in
      let trivia_after_var = consume_trivia parser in
      (* Expect '=' *)
      let eq_children =
        match peek_kind parser with
        | Token.Eq ->
            let eq = consume parser in
            [ make_token parser eq ]
        | _ ->
            let found_tok = peek parser in
            let diagnostic = Diagnostic.invalid_expression
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            []
      in
      let trivia_after_eq = consume_trivia parser in
      (* Parse start expression *)
      let start_expr = parse_expr parser in
      let trivia_after_start = consume_trivia parser in
      (* Expect 'to' or 'downto' keyword *)
      let direction_children =
        match peek_kind parser with
        | Token.Keyword Keyword.To ->
            let to_kw = consume parser in
            [ make_token parser to_kw ]
        | Token.Keyword Keyword.Downto ->
            let downto_kw = consume parser in
            [ make_token parser downto_kw ]
        | _ ->
            let found_tok = peek parser in
            let diagnostic = Diagnostic.invalid_expression
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            []
      in
      let trivia_after_direction = consume_trivia parser in
      (* Parse end expression *)
      let end_expr = parse_expr parser in
      let trivia_after_end = consume_trivia parser in
      (* Expect 'do' keyword *)
      let do_children =
        match peek_kind parser with
        | Token.Keyword Keyword.Do ->
            let do_kw = consume parser in
            [ make_token parser do_kw ]
        | _ ->
            let found_tok = peek parser in
            let diagnostic = Diagnostic.invalid_expression
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            []
      in
      let trivia_after_do = consume_trivia parser in
      (* Parse body expression *)
      let body = parse_expr parser in
      let trivia_after_body = consume_trivia parser in
      (* Expect 'done' keyword *)
      let done_children =
        match peek_kind parser with
        | Token.Keyword Keyword.Done ->
            let done_kw = consume parser in
            [ make_token parser done_kw ]
        | _ ->
            let found_tok = peek parser in
            let diagnostic = Diagnostic.invalid_expression
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            []
      in
      make_node
        Syntax_kind.FOR_EXPR
        ([ make_token parser for_kw ]
        @ tokens_to_green parser trivia_after_for
        @ ext_nodes
        @ tokens_to_green parser trivia_after_ext
        @ attr_nodes
        @ [ make_token parser var_ident ]
        @ tokens_to_green parser trivia_after_var
        @ eq_children
        @ tokens_to_green parser trivia_after_eq
        @ [ Ceibo.Green.Node start_expr ]
        @ tokens_to_green parser trivia_after_start
        @ direction_children
        @ tokens_to_green parser trivia_after_direction
        @ [ Ceibo.Green.Node end_expr ]
        @ tokens_to_green parser trivia_after_end
        @ do_children
        @ tokens_to_green parser trivia_after_do
        @ [ Ceibo.Green.Node body ]
        @ tokens_to_green parser trivia_after_body
        @ done_children)
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse parallel bindings for binding operators: and* x = e and* y = e *)
and parse_parallel_bindings = fun parser ->
  let bindings = ref [] in
  let rec loop () =
    let trivia_before = consume_trivia parser in
    match peek_kind parser with
    | Token.Keyword Keyword.And ->
        let saved_pos = Token_cursor.position parser.cursor in
        let and_kw = consume parser in
        let trivia_after_and = consume_trivia parser in
        (* Check if followed by operator (and* or and+) *)
        (
          match peek_kind parser with
          | Token.Star
          | Token.Plus
          | Token.Minus
          | Token.Slash
          | Token.Percent
          | Token.Caret
          | Token.Eq
          | Token.Lt
          | Token.Gt
          | Token.And
          | Token.Or
          | Token.Ampersand
          | Token.Pipe
          | Token.Bang
          | Token.Question
          | Token.At
          | Token.Hash
          | Token.Tilde
          | Token.Dollar ->
              let op_token = consume parser in
              let trivia_after_op = consume_trivia parser in
              (* Parse pattern = expr *)
              let pattern = parse_pattern parser in
              let trivia_after_pat = consume_trivia parser in
              let eq =
                expect
                  parser
                  Token.Eq
                  (fun found ->
                    Diagnostic.invalid_expression
                      ~found
                      ~text:(token_text parser found)
                      ~span:(expected_span parser))
              in
              let trivia_after_eq = consume_trivia parser in
              let expr = parse_expr parser in
              let trivia_after_expr = consume_trivia parser in
              (* Collect this binding *)
              let binding_nodes =
                tokens_to_green parser trivia_before
                @ [ make_token parser and_kw ]
                @ tokens_to_green parser trivia_after_and
                @ [ make_token parser op_token ]
                @ tokens_to_green parser trivia_after_op
                @ [ Ceibo.Green.Node pattern ]
                @ tokens_to_green parser trivia_after_pat
                @ [ make_token parser eq ]
                @ tokens_to_green parser trivia_after_eq
                @ [ Ceibo.Green.Node expr ]
                @ tokens_to_green parser trivia_after_expr
              in
              bindings := binding_nodes :: !bindings;
              loop ()
          | _ ->
              (* Not a binding operator, restore position *)
              (Token_cursor.set_position parser.cursor saved_pos);
              ()
        )
    | _ -> ()
  in
  loop ();
  List.concat (List.rev !bindings)

(** Parse binding operator expression: let* x = e1 in e2 *)
and parse_binding_operator_expr = fun parser let_kw trivia_after_let op_token ->
  let trivia_after_op = consume_trivia parser in
  (* Parse pattern (binding) *)
  let pattern = parse_pattern parser in
  let trivia_after_pattern = consume_trivia parser in
  (* Expect = *)
  let eq_token =
    expect
      parser
      Token.Eq
      (fun found ->
        Diagnostic.invalid_expression
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser))
  in
  let trivia_after_eq = consume_trivia parser in
  (* Parse bound expression *)
  let bound_expr = parse_expr parser in
  let trivia_after_bound = consume_trivia parser in
  (* Check for 'and*' or 'and+' parallel bindings *)
  let parallel_bindings = parse_parallel_bindings parser in
  (* Expect 'in' keyword *)
  let in_kw =
    expect
      parser
      (Token.Keyword Keyword.In)
      (fun found ->
        Diagnostic.invalid_expression
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser))
  in
  let trivia_after_in = consume_trivia parser in
  (* Parse body expression *)
  let body_expr = parse_expr parser in
  (* Build LET_EXPR node with binding operator *)
  make_node
    Syntax_kind.LET_EXPR
    ([ make_token parser let_kw ]
    @ tokens_to_green parser trivia_after_let
    @ [ make_token parser op_token ]
    @ tokens_to_green parser trivia_after_op
    @ [ Ceibo.Green.Node pattern ]
    @ tokens_to_green parser trivia_after_pattern
    @ [ make_token parser eq_token ]
    @ tokens_to_green parser trivia_after_eq
    @ [ Ceibo.Green.Node bound_expr ]
    @ tokens_to_green parser trivia_after_bound
    @ parallel_bindings
    @ [ make_token parser in_kw ]
    @ tokens_to_green parser trivia_after_in
    @ [ Ceibo.Green.Node body_expr ])

(** Parse let-in expression: let x = expr1 in expr2 *)
and parse_let_in_expr = fun parser ->
  match peek_kind parser with
  | Token.Keyword Keyword.Let ->
      let parse_missing_in_body parser =
        match peek_kind parser with
        | Token.EOF
        | Token.Keyword Keyword.End
        | Token.Keyword Keyword.In
        | Token.CloseDelim _
        | Token.OpenDelim Token.StructEnd
        | Token.OpenDelim Token.SigEnd -> make_node Syntax_kind.ERROR []
        | _ -> parse_expr parser
      in
      let let_kw = consume parser in
      let trivia_after_let = consume_trivia parser in
      let ext_nodes = parse_extension_name parser in
      let trivia_after_ext =
        if ext_nodes != [] then
          consume_trivia parser
        else
          []
      in
      let attr_nodes = parse_attributes parser in
      (* Check for binding operator suffix: let*, let+, etc. *)
      let binding_op_suffix =
        if ext_nodes != [] then
          None
        else
          match peek_kind parser with
          | Token.Star
          | Token.Plus
          | Token.Minus
          | Token.Slash
          | Token.Percent
          | Token.Caret
          | Token.Eq
          | Token.Lt
          | Token.Gt
          | Token.And
          | Token.Or
          | Token.Ampersand
          | Token.Pipe
          | Token.Bang
          | Token.Question
          | Token.At
          | Token.Hash
          | Token.Tilde
          | Token.Dollar -> Some (consume parser)
          | _ -> None
      in
      (
        match binding_op_suffix with
        | Some op_token ->
            (* This is a binding operator: let* x = e in body *)
            parse_binding_operator_expr parser let_kw trivia_after_let op_token
        | None -> (* Check for 'open' keyword - let open Module in expr *)
          (
            match peek_kind parser with
            | Token.Keyword Keyword.Open ->
                let open_kw = consume parser in
                let trivia_after_open = consume_trivia parser in
                let ext_nodes, trivia_after_ext, attr_nodes = parse_keyword_extension_and_attributes
                  parser in
                (* Parse module path *)
                let rec parse_module_path acc =
                  match peek_kind parser with
                  | Token.Ident _ ->
                      let ident = consume parser in
                      let new_acc = acc @ [ make_token parser ident ] in
                      let trivia_after_ident = consume_trivia parser in
                      (* Check for dot continuation *)
                      (
                        match peek_kind parser with
                        | Token.Dot ->
                            let dot = consume parser in
                            let trivia_after_dot = consume_trivia parser in
                            parse_module_path
                              (new_acc @ [ make_token parser dot ] @ tokens_to_green parser trivia_after_dot)
                        | _ -> (new_acc, trivia_after_ident)
                      )
                  | _ ->
                      let found_tok = peek parser in
                      let diagnostic = Diagnostic.invalid_expression
                        ~found:found_tok
                        ~text:(token_text parser found_tok)
                        ~span:(expected_span parser) in
                      report_diagnostic parser diagnostic;
                      ([], [])
                in
                let module_path, trivia_after_module = parse_module_path [] in
                (* Expect 'in' keyword *)
                let in_kw, trivia_after_in =
                  match peek_kind parser with
                  | Token.Keyword Keyword.In ->
                      let in_tok = consume parser in
                      let trivia = consume_trivia parser in
                      (Some in_tok, trivia)
                  | _ ->
                      let found_tok = peek parser in
                      let diagnostic = Diagnostic.invalid_expression
                        ~found:found_tok
                        ~text:(token_text parser found_tok)
                        ~span:(expected_span parser) in
                      report_diagnostic parser diagnostic;
                      (None, [])
                in
                (* Parse body expression *)
                let body_expr =
                  if Option.is_some in_kw then
                    parse_expr parser
                  else
                    parse_missing_in_body parser
                in
                let in_children =
                  match in_kw with
                  | Some tok -> [ make_token parser tok ] @ tokens_to_green parser trivia_after_in
                  | None -> []
                in
                make_node
                  Syntax_kind.LOCAL_OPEN_EXPR
                  ([ make_token parser let_kw ]
                  @ tokens_to_green parser trivia_after_let
                  @ [ make_token parser open_kw ]
                  @ tokens_to_green parser trivia_after_open
                  @ ext_nodes
                  @ tokens_to_green parser trivia_after_ext
                  @ attr_nodes
                  @ module_path
                  @ tokens_to_green parser trivia_after_module
                  @ in_children
                  @ [ Ceibo.Green.Node body_expr ])
            | Token.Keyword Keyword.Module ->
                (* let module M = module_expr in expr *)
                let module_kw = consume parser in
                let trivia_after_module_kw = consume_trivia parser in
                let ext_nodes, trivia_after_ext, attr_nodes = parse_keyword_extension_and_attributes
                  parser in
                (* Parse module name (must be capitalized) *)
                let module_name =
                  match peek_kind parser with
                  | Token.Ident _ ->
                      let ident = consume parser in
                      make_token parser ident
                  | _ ->
                      let found_tok = peek parser in
                      let diagnostic = Diagnostic.invalid_expression
                        ~found:found_tok
                        ~text:(token_text parser found_tok)
                        ~span:(expected_span parser) in
                      report_diagnostic parser diagnostic;
                      make_token parser found_tok
                in
                let trivia_after_name = consume_trivia parser in
                (* Expect '=' *)
                let eq_tok =
                  match peek_kind parser with
                  | Token.Eq ->
                      let eq = consume parser in
                      Some (make_token parser eq)
                  | _ ->
                      let found_tok = peek parser in
                      let diagnostic = Diagnostic.invalid_expression
                        ~found:found_tok
                        ~text:(token_text parser found_tok)
                        ~span:(expected_span parser) in
                      report_diagnostic parser diagnostic;
                      None
                in
                let trivia_after_eq = consume_trivia parser in
                (* Parse module expression - check for (val expr : SIG) syntax *)
                let module_expr =
                  match peek_kind parser with
                  | Token.OpenDelim Token.Paren ->
                      (* Check if this is (val ...) unpacking *)
                      if (peek_n parser 1).Token.kind = Token.Keyword Keyword.Val then
                        let lparen = consume parser in
                        let trivia1 = consume_trivia parser in
                        let val_kw = consume parser in
                        let trivia2 = consume_trivia parser in
                        let inner_expr = parse_expr parser in
                        let trivia3 = consume_trivia parser in
                        (* Optional type constraint: : SIG *)
                        let type_constraint =
                          if peek_kind parser = Token.Colon then
                            let colon = consume parser in
                            let trivia4 = consume_trivia parser in
                            let sig_type = parse_module_type_expr parser in
                            let trivia_after_sig_type = consume_trivia parser in
                            Some (colon, trivia4, sig_type, trivia_after_sig_type)
                          else
                            None
                        in
                        let rparen =
                          if peek_kind parser = Token.CloseDelim Token.Paren then
                            consume parser
                          else
                            peek parser
                        in
                        (* Build FIRST_CLASS_MODULE_EXPR *)
                        let constraint_children =
                          match type_constraint with
                          | Some (colon, trivia, sig_type, trivia_after_sig_type) -> [
                            make_token parser colon
                          ]
                          @ tokens_to_green parser trivia
                          @ [ Ceibo.Green.Node sig_type ]
                          @ tokens_to_green parser trivia_after_sig_type
                          | None -> []
                        in
                        make_node
                          Syntax_kind.FIRST_CLASS_MODULE_EXPR
                          ([ make_token parser lparen ]
                          @ tokens_to_green parser trivia1
                          @ [ make_token parser val_kw ]
                          @ tokens_to_green parser trivia2
                          @ [ Ceibo.Green.Node inner_expr ]
                          @ tokens_to_green parser trivia3
                          @ constraint_children
                          @ [ make_token parser rparen ])
                      else
                        (* Regular module expression *)
                        parse_module_expr parser
                  | _ ->
                      (* Direct module name or other module expression *)
                      parse_module_expr parser
                in
                let trivia_after_module_expr = consume_trivia parser in
                (* Expect 'in' keyword *)
                let in_kw =
                  match peek_kind parser with
                  | Token.Keyword Keyword.In ->
                      let in_tok = consume parser in
                      Some (make_token parser in_tok)
                  | _ ->
                      let found_tok = peek parser in
                      let diagnostic = Diagnostic.invalid_expression
                        ~found:found_tok
                        ~text:(token_text parser found_tok)
                        ~span:(expected_span parser) in
                      report_diagnostic parser diagnostic;
                      None
                in
                let trivia_after_in = consume_trivia parser in
                (* Parse body expression *)
                let body_expr =
                  match in_kw with
                  | Some _ -> parse_expr parser
                  | None -> parse_missing_in_body parser
                in
                let eq_children =
                  match eq_tok with
                  | Some t -> [ t ]
                  | None -> []
                in
                let in_children =
                  match in_kw with
                  | Some t -> [ t ]
                  | None -> []
                in
                make_node
                  Syntax_kind.LET_MODULE_EXPR
                  ([ make_token parser let_kw ]
                  @ tokens_to_green parser trivia_after_let
                  @ [ make_token parser module_kw ]
                  @ tokens_to_green parser trivia_after_module_kw
                  @ ext_nodes
                  @ tokens_to_green parser trivia_after_ext
                  @ attr_nodes
                  @ [ module_name ]
                  @ tokens_to_green parser trivia_after_name
                  @ eq_children
                  @ tokens_to_green parser trivia_after_eq
                  @ [ Ceibo.Green.Node module_expr ]
                  @ tokens_to_green parser trivia_after_module_expr
                  @ in_children
                  @ tokens_to_green parser trivia_after_in
                  @ [ Ceibo.Green.Node body_expr ])
            | Token.Keyword Keyword.Exception ->
                let exception_decl = parse_exception_decl parser in
                let trivia_after_exception_decl = consume_trivia parser in
                let in_children =
                  match peek_kind parser with
                  | Token.Keyword Keyword.In ->
                      let in_kw = consume parser in
                      ([ make_token parser in_kw ], true)
                  | _ ->
                      let found_tok = peek parser in
                      let diagnostic = Diagnostic.invalid_expression
                        ~found:found_tok
                        ~text:(token_text parser found_tok)
                        ~span:(expected_span parser) in
                      report_diagnostic parser diagnostic;
                      ([], false)
                in
                let trivia_after_in = consume_trivia parser in
                let body_expr =
                  match in_children with
                  | _, true -> parse_expr parser
                  | _ -> parse_missing_in_body parser
                in
                make_node
                  Syntax_kind.LET_EXPR
                  ([ make_token parser let_kw ]
                  @ tokens_to_green parser trivia_after_let
                  @ ext_nodes
                  @ tokens_to_green parser trivia_after_ext
                  @ attr_nodes
                  @ [ Ceibo.Green.Node exception_decl ]
                  @ tokens_to_green parser trivia_after_exception_decl
                  @ (fst in_children)
                  @ tokens_to_green parser trivia_after_in
                  @ [ Ceibo.Green.Node body_expr ])
            | _ ->
                (* Regular let binding: let [rec] pattern params = expr in body *)
                (* Check for optional 'rec' keyword *)
                let rec_kw, trivia_after_rec, is_recursive =
                  match peek_kind parser with
                  | Token.Keyword Keyword.Rec ->
                      let rec_tok = consume parser in
                      let trivia = consume_trivia parser in
                      (Some rec_tok, trivia, true)
                  | _ -> (None, [], false)
                in
                (* Parse pattern *)
                let pattern = parse_pattern parser in
                let trivia_after_pattern = consume_trivia parser in
                (* Check if this is function syntax: let f x y = expr or let rec f x y = expr *)
                (* If next token is not = and can start a pattern, collect parameters *)
                let params = ref [] in
                let params_trivia = ref [] in
                let first_param_start = ref None in
                let rec collect_params () =
                  match parse_locally_abstract_types parser with
                  | Some lat ->
                      if Option.is_none !first_param_start then
                        first_param_start := Some (peek parser).Token.span.start;
                      params := lat :: !params;
                      let trivia = consume_trivia parser in
                      params_trivia := trivia :: !params_trivia;
                      collect_params ()
                  | None -> (
                      match peek_kind parser with
                      | Token.Tilde ->
                          if Option.is_none !first_param_start then
                            first_param_start := Some (peek parser).Token.span.start;
                          let param = parse_labeled_param parser in
                          params := param :: !params;
                          let trivia = consume_trivia parser in
                          params_trivia := trivia :: !params_trivia;
                          collect_params ()
                      | Token.Question ->
                          if Option.is_none !first_param_start then
                            first_param_start := Some (peek parser).Token.span.start;
                          let param = parse_optional_param parser in
                          params := param :: !params;
                          let trivia = consume_trivia parser in
                          params_trivia := trivia :: !params_trivia;
                          collect_params ()
                      | Token.Eq -> ()
                      | _ when can_start_pattern parser ->
                          if Option.is_none !first_param_start then
                            first_param_start := Some (peek parser).Token.span.start;
                          let param = parse_pattern parser in
                          params := param :: !params;
                          let trivia = consume_trivia parser in
                          params_trivia := trivia :: !params_trivia;
                          collect_params ()
                      | _ -> ()
                    )
                in
                collect_params ();
                (* Build parameter nodes *)
                let param_nodes =
                  List.rev !params
                  |> List.mapi
                    (fun i param ->
                      let trivia =
                        if i < List.length !params_trivia then
                          List.nth (List.rev !params_trivia) i
                        else
                          []
                      in
                      tokens_to_green parser trivia @ [ Ceibo.Green.Node param ])
                  |> List.flatten
                in
                let type_annotation_nodes =
                  if peek_kind parser = Token.Colon then
                    let colon = consume parser in
                    let trivia_after_colon = consume_trivia parser in
                    let type_expr = parse_typexpr parser in
                    let trivia_after_type = consume_trivia parser in
                    [ make_token parser colon ]
                    @ tokens_to_green parser trivia_after_colon
                    @ [ Ceibo.Green.Node type_expr ]
                    @ tokens_to_green parser trivia_after_type
                  else
                    []
                in
                (* Expect '=' *)
                let eq_children =
                  match peek_kind parser with
                  | Token.Eq ->
                      let eq = consume parser in
                      [ make_token parser eq ]
                  | _ ->
                      let found_tok = peek parser in
                      let span =
                        match !first_param_start with
                        | Some start -> point_span start
                        | None -> expected_span parser
                      in
                      let diagnostic = Diagnostic.missing_let_binding_equals
                        ~found:found_tok
                        ~text:(token_text parser found_tok)
                        ~span in
                      report_diagnostic parser diagnostic;
                      []
                in
                let trivia_after_eq = consume_trivia parser in
                (* Parse bound expression *)
                let bound_expr =
                  if missing_let_in_expr_boundary parser then
                    let found_tok = peek parser in
                    let diagnostic = Diagnostic.missing_let_binding_expr
                      ~found:found_tok
                      ~text:(token_text parser found_tok)
                      ~span:(expected_span parser) in
                    make_error_node parser ~diagnostic ~consumed_tokens:[]
                  else
                    parse_expr parser
                in
                let trivia_after_bound = consume_trivia parser in
                let rec parse_and_bindings acc =
                  match peek_kind parser with
                  | Token.Keyword Keyword.And ->
                      let and_kw = consume parser in
                      let trivia_after_and = consume_trivia parser in
                      let ext_nodes2 = parse_extension_name parser in
                      let trivia_after_ext2 =
                        if ext_nodes2 != [] then
                          consume_trivia parser
                        else
                          []
                      in
                      let attr_nodes2 = parse_attributes parser in
                      let pat2 = parse_pattern parser in
                      let trivia_after_pat2 = consume_trivia parser in
                      let params2 = ref [] in
                      let params_trivia2 = ref [] in
                      let first_param_start2 = ref None in
                      let rec collect_params2 () =
                        match parse_locally_abstract_types parser with
                        | Some lat ->
                            if Option.is_none !first_param_start2 then
                              first_param_start2 := Some (peek parser).Token.span.start;
                            params2 := lat :: !params2;
                            let trivia = consume_trivia parser in
                            params_trivia2 := trivia :: !params_trivia2;
                            collect_params2 ()
                        | None -> (
                            match peek_kind parser with
                            | Token.Tilde ->
                                if Option.is_none !first_param_start2 then
                                  first_param_start2 := Some (peek parser).Token.span.start;
                                let param = parse_labeled_param parser in
                                params2 := param :: !params2;
                                let trivia = consume_trivia parser in
                                params_trivia2 := trivia :: !params_trivia2;
                                collect_params2 ()
                            | Token.Question ->
                                if Option.is_none !first_param_start2 then
                                  first_param_start2 := Some (peek parser).Token.span.start;
                                let param = parse_optional_param parser in
                                params2 := param :: !params2;
                                let trivia = consume_trivia parser in
                                params_trivia2 := trivia :: !params_trivia2;
                                collect_params2 ()
                            | Token.Eq -> ()
                            | _ when can_start_pattern parser ->
                                if Option.is_none !first_param_start2 then
                                  first_param_start2 := Some (peek parser).Token.span.start;
                                let param = parse_pattern parser in
                                params2 := param :: !params2;
                                let trivia = consume_trivia parser in
                                params_trivia2 := trivia :: !params_trivia2;
                                collect_params2 ()
                            | _ -> ()
                          )
                      in
                      collect_params2 ();
                      let param_nodes2 =
                        List.rev !params2
                        |> List.mapi
                          (fun i param ->
                            let trivia =
                              if i < List.length !params_trivia2 then
                                List.nth (List.rev !params_trivia2) i
                              else
                                []
                            in
                            tokens_to_green parser trivia @ [ Ceibo.Green.Node param ])
                        |> List.flatten
                      in
                      let eq_children2 =
                        match peek_kind parser with
                        | Token.Eq -> [ make_token parser (consume parser) ]
                        | _ ->
                            let found_tok = peek parser in
                            let span =
                              match !first_param_start2 with
                              | Some start -> point_span start
                              | None -> expected_span parser
                            in
                            let diagnostic = Diagnostic.missing_let_binding_equals
                              ~found:found_tok
                              ~text:(token_text parser found_tok)
                              ~span in
                            report_diagnostic parser diagnostic;
                            []
                      in
                      let trivia_after_eq2 = consume_trivia parser in
                      let bound_expr2 =
                        if missing_let_in_expr_boundary parser then
                          let found_tok = peek parser in
                          let diagnostic = Diagnostic.missing_let_binding_expr
                            ~found:found_tok
                            ~text:(token_text parser found_tok)
                            ~span:(expected_span parser) in
                          make_error_node parser ~diagnostic ~consumed_tokens:[]
                        else
                          parse_expr parser
                      in
                      let trivia_after_bound2 = consume_trivia parser in
                      let binding2 =
                        make_node
                          Syntax_kind.LET_BINDING
                          (ext_nodes2
                          @ tokens_to_green parser trivia_after_ext2
                          @ attr_nodes2
                          @ [ Ceibo.Green.Node pat2 ]
                          @ tokens_to_green parser trivia_after_pat2
                          @ param_nodes2
                          @ eq_children2
                          @ tokens_to_green parser trivia_after_eq2
                          @ [ Ceibo.Green.Node bound_expr2 ])
                      in
                      parse_and_bindings
                        (acc
                        @ [ make_token parser and_kw ]
                        @ tokens_to_green parser trivia_after_and
                        @ [ Ceibo.Green.Node binding2 ]
                        @ tokens_to_green parser trivia_after_bound2)
                  | _ -> acc
                in
                let and_children = parse_and_bindings [] in
                (* Expect 'in' keyword *)
                let in_children =
                  match peek_kind parser with
                  | Token.Keyword Keyword.In ->
                      let in_kw = consume parser in
                      ([ make_token parser in_kw ], true)
                  | _ ->
                      let found_tok = peek parser in
                      let diagnostic = Diagnostic.invalid_expression
                        ~found:found_tok
                        ~text:(token_text parser found_tok)
                        ~span:(expected_span parser) in
                      report_diagnostic parser diagnostic;
                      ([], false)
                in
                let trivia_after_in = consume_trivia parser in
                (* Parse body expression *)
                let body_expr =
                  match in_children with
                  | _, true -> parse_expr parser
                  | _ -> parse_missing_in_body parser
                in
                (* Build rec keyword children if present *)
                let rec_children =
                  match rec_kw with
                  | Some tok -> [ make_token parser tok ] @ tokens_to_green parser trivia_after_rec
                  | None -> []
                in
                (* Choose syntax kind based on whether it's recursive *)
                let syntax_kind =
                  if is_recursive then
                    Syntax_kind.LET_REC_EXPR
                  else
                    Syntax_kind.LET_EXPR
                in
                make_node
                  syntax_kind
                  ([ make_token parser let_kw ]
                  @ tokens_to_green parser trivia_after_let
                  @ ext_nodes
                  @ tokens_to_green parser trivia_after_ext
                  @ attr_nodes
                  @ rec_children
                  @ [ Ceibo.Green.Node pattern ]
                  @ tokens_to_green parser trivia_after_pattern
                  @ param_nodes
                  @ type_annotation_nodes
                  @ eq_children
                  @ tokens_to_green parser trivia_after_eq
                  @ [ Ceibo.Green.Node bound_expr ]
                  @ tokens_to_green parser trivia_after_bound
                  @ and_children
                  @ (fst in_children)
                  @ tokens_to_green parser trivia_after_in
                  @ [ Ceibo.Green.Node body_expr ])
          )
      )
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse let binding: let pattern = expr *)
(** Parse locally abstract type parameters: (type a b c) *)
and parse_locally_abstract_types = fun parser ->
  match peek_kind parser with
  | Token.OpenDelim Token.Paren ->
      let saved_pos = Token_cursor.position parser.cursor in
      let lparen = consume parser in
      let trivia1 = consume_trivia parser in
      (* Check for 'type' keyword *)
      if peek_kind parser = Token.Keyword Keyword.Type then
        let type_kw = consume parser in
        let trivia2 = consume_trivia parser in
        (* Collect type variable names: a b c *)
        let type_vars = ref [] in
        let type_vars_trivia = ref [] in
        let rec collect_types () =
          match peek_kind parser with
          | Token.Ident name when String.length name > 0 && name.[0] >= 'a' && name.[0] <= 'z' ->
              (* Lowercase identifier - type variable *)
              let var = consume parser in
              type_vars := var :: !type_vars;
              let trivia = consume_trivia parser in
              type_vars_trivia := trivia :: !type_vars_trivia;
              collect_types ()
          | _ ->
              (* Done - stop at ), EOF, or unexpected token *)
              ()
        in
        collect_types ();
        (* Expect ) *)
        let rparen =
          if peek_kind parser = Token.CloseDelim Token.Paren then
            consume parser
          else
            peek parser
        in
        (* Build type variable nodes *)
        let type_var_nodes =
          List.rev !type_vars
          |> List.mapi
            (fun i var ->
              let trivia =
                if i < List.length !type_vars_trivia then
                  List.nth (List.rev !type_vars_trivia) i
                else
                  []
              in
              [ make_token parser var ] @ tokens_to_green parser trivia)
          |> List.flatten
        in
        Some (make_node
          Syntax_kind.LOCALLY_ABSTRACT_TYPE_PARAM
          ([ make_token parser lparen ]
          @ tokens_to_green parser trivia1
          @ [ make_token parser type_kw ]
          @ tokens_to_green parser trivia2
          @ type_var_nodes
          @ [ make_token parser rparen ]))
      else (
        (* Not a locally abstract type, restore position *)
        Token_cursor.set_position parser.cursor saved_pos;
        None
      )
  | _ -> None

and parse_let_binding = fun parser ->
  match peek_kind parser with
  | Token.Keyword Keyword.Let ->
      let let_kw = consume parser in
      let trivia_after_let = consume_trivia parser in
      (* Parse optional extension: let%foo *)
      let ext_nodes = parse_extension_name parser in
      let trivia_after_ext =
        if ext_nodes != [] then
          consume_trivia parser
        else
          []
      in
      (* Parse optional attributes: let[@inline] f x = ... *)
      let attr_nodes = parse_attributes parser in
      (* Check for optional 'rec' keyword *)
      let rec_nodes =
        if peek_kind parser = Token.Keyword Keyword.Rec then
          let rec_kw = consume parser in
          let trivia_after_rec = consume_trivia parser in
          [ make_token parser rec_kw ] @ tokens_to_green parser trivia_after_rec
        else
          []
      in
      let pat = parse_pattern parser in
      let trivia_after_pat = consume_trivia parser in
      (* Check for locally abstract types: (type a b) *)
      let locally_abstract_types = parse_locally_abstract_types parser in
      let trivia_after_lat = consume_trivia parser in
      (* Helper to build LAT nodes *)
      let lat_nodes =
        match locally_abstract_types with
        | Some lat -> [ Ceibo.Green.Node lat ] @ tokens_to_green parser trivia_after_lat
        | None -> []
      in
      (* Check if this is function syntax: let f x y = expr *)
      (* If next token is not = and can start a pattern, collect parameters *)
      let params = ref [] in
      let params_trivia = ref [] in
      let first_param_start = ref None in
      let rec collect_params () =
        match parse_locally_abstract_types parser with
        | Some lat ->
            if Option.is_none !first_param_start then
              first_param_start := Some (peek parser).Token.span.start;
            params := lat :: !params;
            let trivia = consume_trivia parser in
            params_trivia := trivia :: !params_trivia;
            collect_params ()
        | None -> (
            match peek_kind parser with
            | Token.Tilde ->
                (* Labeled parameter: ~label or ~label:pattern *)
                if Option.is_none !first_param_start then
                  first_param_start := Some (peek parser).Token.span.start;
                let param = parse_labeled_param parser in
                params := param :: !params;
                let trivia = consume_trivia parser in
                params_trivia := trivia :: !params_trivia;
                collect_params ()
            | Token.Question ->
                (* Optional parameter: ?label or ?label:pattern or ?(label = default) *)
                if Option.is_none !first_param_start then
                  first_param_start := Some (peek parser).Token.span.start;
                let param = parse_optional_param parser in
                params := param :: !params;
                let trivia = consume_trivia parser in
                params_trivia := trivia :: !params_trivia;
                collect_params ()
            | Token.Eq ->
                (* End of parameters *)
                ()
            | _ when can_start_pattern parser ->
                (* Regular pattern parameter *)
                if Option.is_none !first_param_start then
                  first_param_start := Some (peek parser).Token.span.start;
                let param = parse_pattern parser in
                params := param :: !params;
                let trivia = consume_trivia parser in
                params_trivia := trivia :: !params_trivia;
                collect_params ()
            | _ ->
                (* End of parameters *)
                ()
          )
      in
      collect_params ();
      (* Build parameter nodes *)
      let param_nodes =
        List.rev !params
        |> List.mapi
          (fun i param ->
            let trivia =
              if i < List.length !params_trivia then
                List.nth (List.rev !params_trivia) i
              else
                []
            in
            tokens_to_green parser trivia @ [ Ceibo.Green.Node param ])
        |> List.flatten
      in
      (* Check for optional type annotation: : type *)
      let type_annotation_nodes =
        if peek_kind parser = Token.Colon then
          let colon = consume parser in
          let trivia_after_colon = consume_trivia parser in
          let type_expr = parse_typexpr parser in
          let trivia_after_type = consume_trivia parser in
          [ make_token parser colon ]
          @ tokens_to_green parser trivia_after_colon
          @ [ Ceibo.Green.Node type_expr ]
          @ tokens_to_green parser trivia_after_type
        else
          []
      in
      (* Expect '=' *)
      let has_eq = peek_kind parser = Token.Eq in
      let eq_tok =
        if has_eq then
          consume parser
        else
          let found_tok = peek parser in
          let span =
            match !first_param_start with
            | Some start -> point_span start
            | None -> expected_span parser
          in
          let diag = Diagnostic.missing_let_binding_equals
            ~found:found_tok
            ~text:(token_text parser found_tok)
            ~span in
          report_diagnostic parser diag;
          peek parser
      in
      (* If no =, skip to = or next keyword and return *)
      if not has_eq then
        let skipped_tokens = error_recover_until
          parser
          ~sync_tokens:[ Token.Eq; Token.Keyword Keyword.Let; Token.Keyword Keyword.Type; ] in
        let trivia_after_skip = consume_trivia parser in
        (* Check if we found = *)
        match peek_kind parser with
        | Token.Eq ->
            (* Found =, continue parsing *)
            let eq = consume parser in
            let trivia_after_eq = consume_trivia parser in
            let expr =
              if missing_let_binding_boundary parser then
                let found_tok = peek parser in
                let diagnostic = Diagnostic.missing_let_binding_expr
                  ~found:found_tok
                  ~text:(token_text parser found_tok)
                  ~span:(expected_span parser) in
                make_error_node parser ~diagnostic ~consumed_tokens:[]
              else
                parse_expr parser
            in
            make_node
              Syntax_kind.LET_BINDING
              ([ make_token parser let_kw ]
              @ tokens_to_green parser trivia_after_let
              @ ext_nodes
              @ tokens_to_green parser trivia_after_ext
              @ attr_nodes
              @ rec_nodes
              @ [ Ceibo.Green.Node pat ]
              @ tokens_to_green parser trivia_after_pat
              @ lat_nodes
              @ param_nodes
              @ type_annotation_nodes
              @ tokens_to_green parser skipped_tokens
              @ tokens_to_green parser trivia_after_skip
              @ [ make_token parser eq ]
              @ tokens_to_green parser trivia_after_eq
              @ [ Ceibo.Green.Node expr ])
        | _ ->
            (* Didn't find =, stop here *)
            make_node
              Syntax_kind.LET_BINDING
              ([ make_token parser let_kw ]
              @ tokens_to_green parser trivia_after_let
              @ ext_nodes
              @ tokens_to_green parser trivia_after_ext
              @ attr_nodes
              @ rec_nodes
              @ [ Ceibo.Green.Node pat ]
              @ tokens_to_green parser trivia_after_pat
              @ lat_nodes
              @ param_nodes
              @ type_annotation_nodes
              @ tokens_to_green parser skipped_tokens
              @ tokens_to_green parser trivia_after_skip)
      else
        (* Have =, continue normally *)
        let trivia_after_eq = consume_trivia parser in
        let expr =
          if missing_let_binding_boundary parser then
            let found_tok = peek parser in
            let diagnostic = Diagnostic.missing_let_binding_expr
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser) in
            make_error_node parser ~diagnostic ~consumed_tokens:[]
          else
            parse_expr parser
        in
        (* Check if expr had error - if so, skip to sync point and return early *)
        if Ceibo.Green.kind (Ceibo.Green.Node expr) = Syntax_kind.ERROR then
          let skipped_tokens = error_recover_until
            parser
            ~sync_tokens:[ Token.Keyword Keyword.Let; Token.Keyword Keyword.Type ] in
          let trivia_after_skip = consume_trivia parser in
          make_node
            Syntax_kind.LET_BINDING
            ([ make_token parser let_kw ]
            @ tokens_to_green parser trivia_after_let
            @ ext_nodes
            @ tokens_to_green parser trivia_after_ext
            @ attr_nodes
            @ rec_nodes
            @ [ Ceibo.Green.Node pat ]
            @ tokens_to_green parser trivia_after_pat
            @ lat_nodes
            @ param_nodes
            @ type_annotation_nodes
            @ [ make_token parser eq_tok ]
            @ tokens_to_green parser trivia_after_eq
            @ [ Ceibo.Green.Node expr ]
            @ tokens_to_green parser skipped_tokens
            @ tokens_to_green parser trivia_after_skip)
        else
          (* No error in expression - continue normally *)
          let first_binding =
            make_node
              Syntax_kind.LET_BINDING
              ([ make_token parser let_kw ]
              @ tokens_to_green parser trivia_after_let
              @ ext_nodes
              @ tokens_to_green parser trivia_after_ext
              @ attr_nodes
              @ rec_nodes
              @ [ Ceibo.Green.Node pat ]
              @ tokens_to_green parser trivia_after_pat
              @ lat_nodes
              @ param_nodes
              @ type_annotation_nodes
              @ [ make_token parser eq_tok ]
              @ tokens_to_green parser trivia_after_eq
              @ [ Ceibo.Green.Node expr ])
          in
          (* Check for 'and' to form mutually recursive/parallel bindings *)
          let trivia_start = position parser in
          let trivia_after_first = consume_trivia parser in
          let rec parse_and_bindings acc =
            match peek_kind parser with
            | Token.Keyword Keyword.And ->
                let and_kw = consume parser in
                let trivia_after_and = consume_trivia parser in
                (* Parse optional extension: and%foo *)
                let ext_nodes2 = parse_extension_name parser in
                let trivia_after_ext2 =
                  if ext_nodes2 != [] then
                    consume_trivia parser
                  else
                    []
                in
                (* Parse optional attributes for this binding *)
                let attr_nodes2 = parse_attributes parser in
                (* Check for optional 'rec' keyword (though unusual after 'and') *)
                let rec_nodes2 =
                  if peek_kind parser = Token.Keyword Keyword.Rec then
                    let rec_kw = consume parser in
                    let trivia_after_rec = consume_trivia parser in
                    [ make_token parser rec_kw ] @ tokens_to_green parser trivia_after_rec
                  else
                    []
                in
                let pat2 = parse_pattern parser in
                let trivia_after_pat2 = consume_trivia parser in
                (* Check for locally abstract types *)
                let locally_abstract_types2 = parse_locally_abstract_types parser in
                let trivia_after_lat2 = consume_trivia parser in
                let lat_nodes2 =
                  match locally_abstract_types2 with
                  | Some lat -> [ Ceibo.Green.Node lat ] @ tokens_to_green parser trivia_after_lat2
                  | None -> []
                in
                (* Collect parameters *)
                let params2 = ref [] in
                let params_trivia2 = ref [] in
                let rec collect_params2 () =
                  match parse_locally_abstract_types parser with
                  | Some lat ->
                      params2 := lat :: !params2;
                      let trivia = consume_trivia parser in
                      params_trivia2 := trivia :: !params_trivia2;
                      collect_params2 ()
                  | None -> (
                      match peek_kind parser with
                      | Token.Tilde ->
                          let param = parse_labeled_param parser in
                          params2 := param :: !params2;
                          let trivia = consume_trivia parser in
                          params_trivia2 := trivia :: !params_trivia2;
                          collect_params2 ()
                      | Token.Question ->
                          let param = parse_optional_param parser in
                          params2 := param :: !params2;
                          let trivia = consume_trivia parser in
                          params_trivia2 := trivia :: !params_trivia2;
                          collect_params2 ()
                      | Token.Eq ->
                          ()
                      | _ when can_start_pattern parser ->
                          let param = parse_pattern parser in
                          params2 := param :: !params2;
                          let trivia = consume_trivia parser in
                          params_trivia2 := trivia :: !params_trivia2;
                          collect_params2 ()
                      | _ ->
                          ()
                    )
                in
                collect_params2 ();
                let param_nodes2 =
                  List.rev !params2
                  |> List.mapi
                    (fun i param ->
                      let trivia =
                        if i < List.length !params_trivia2 then
                          List.nth (List.rev !params_trivia2) i
                        else
                          []
                      in
                      tokens_to_green parser trivia @ [ Ceibo.Green.Node param ])
                  |> List.flatten
                in
                (* Check for optional type annotation *)
                let type_annotation_nodes2 =
                  if peek_kind parser = Token.Colon then
                    let colon = consume parser in
                    let trivia_after_colon = consume_trivia parser in
                    let type_expr = parse_typexpr parser in
                    let trivia_after_type = consume_trivia parser in
                    [ make_token parser colon ]
                    @ tokens_to_green parser trivia_after_colon
                    @ [ Ceibo.Green.Node type_expr ]
                    @ tokens_to_green parser trivia_after_type
                  else
                    []
                in
                (* Expect '=' *)
                let eq2 =
                  if peek_kind parser = Token.Eq then
                    consume parser
                  else
                    peek parser
                in
                let trivia_after_eq2 = consume_trivia parser in
                (* Parse expression *)
                let expr2 = parse_expr parser in
                let next_binding =
                  make_node
                    Syntax_kind.LET_BINDING
                    (ext_nodes2
                    @ tokens_to_green parser trivia_after_ext2
                    @ attr_nodes2
                    @ rec_nodes2
                    @ [ Ceibo.Green.Node pat2 ]
                    @ tokens_to_green parser trivia_after_pat2
                    @ lat_nodes2
                    @ param_nodes2
                    @ type_annotation_nodes2
                    @ [ make_token parser eq2 ]
                    @ tokens_to_green parser trivia_after_eq2
                    @ [ Ceibo.Green.Node expr2 ])
                in
                let trivia_after_next = consume_trivia parser in
                parse_and_bindings
                  (tokens_to_green parser trivia_after_next
                  @ [ Ceibo.Green.Node next_binding ]
                  @ tokens_to_green parser trivia_after_and
                  @ [ make_token parser and_kw ]
                  @ acc)
            | _ -> List.rev acc
          in
          let and_bindings = parse_and_bindings [] in
          (* If we found 'and' bindings, wrap in MUTUAL, else return single *)
          if and_bindings = [] then
            let () = Token_cursor.set_position parser.cursor trivia_start in
            first_binding
          else
            make_node
              Syntax_kind.LET_MUTUAL_DECL
              ([ Ceibo.Green.Node first_binding ] @ tokens_to_green parser trivia_after_first @ and_bindings)
  | _ ->
      let found_tok = peek parser in
      let found_text = token_text parser found_tok in
      (* Check if it's an operator (missing left operand) *)
      let diagnostic =
        if Option.is_some (operator_info (peek_kind parser)) then
          Diagnostic.missing_binary_operand
            ~operator:found_text
            ~side:"left"
            ~found:found_tok
            ~text:found_text
            ~span:(expected_span parser)
        else
          Diagnostic.invalid_expression
            ~found:found_tok
            ~text:found_text
            ~span:(expected_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** *
    ============================================================================
    * GRAMMAR SECTION 6: TOP-LEVEL DECLARATIONS *
    ============================================================================
*)
(** Parse variant constructor declaration: A | B of int * string
    
    constr-decl ::= (constr-name | "[]" | "(:)") ["of" constr-args]
    constr-args ::= typexpr { "*" typexpr }
*)
and parse_variant_constr = fun parser ->
  (* Parse constructor name *)
  let constr_name =
    match peek_kind parser with
    | Token.Ident name when String.length name > 0
    && (String.get name 0 >= 'A' && String.get name 0 <= 'Z') ->
        let ident = consume parser in
        make_node Syntax_kind.IDENT_EXPR [ make_token parser ident ]
    | Token.OpenDelim Token.Bracket ->
        (* Special constructor: [] *)
        let open_bracket = consume parser in
        let close_bracket =
          expect
            parser
            (Token.CloseDelim Token.Bracket)
            (fun found ->
              Diagnostic.unclosed_delimiter
                ~opener:"]"
                ~found
                ~text:(token_text parser found)
                ~span:(expected_span parser))
        in
        make_node
          Syntax_kind.IDENT_EXPR
          [ make_token parser open_bracket; make_token parser close_bracket ]
    | Token.OpenDelim Token.Paren ->
        (* Special constructor: (:) - parse manually since it's unusual *)
        let open_paren = consume parser in
        (
          match peek_kind parser with
          | Token.Colon ->
              let colon = consume parser in
              (
                match peek_kind parser with
                | Token.CloseDelim Token.Paren ->
                    let close_paren = consume parser in
                    make_node
                      Syntax_kind.IDENT_EXPR
                      [
                        make_token parser open_paren;
                        make_token parser colon;
                        make_token parser close_paren
                      ]
                | _ ->
                    let found_tok = peek parser in
                    let diagnostic = Diagnostic.unclosed_delimiter
                      ~opener:")"
                      ~found:found_tok
                      ~text:(token_text parser found_tok)
                      ~span:(expected_span parser) in
                    make_error_node parser ~diagnostic ~consumed_tokens:[ open_paren ]
              )
          | _ ->
              let found_tok = peek parser in
              let diagnostic = Diagnostic.invalid_expression
                ~found:found_tok
                ~text:(token_text parser found_tok)
                ~span:(current_span parser) in
              make_error_node parser ~diagnostic ~consumed_tokens:[ open_paren ]
        )
    | _ ->
        let found_tok = peek parser in
        let diagnostic = Diagnostic.invalid_expression
          ~found:found_tok
          ~text:(token_text parser found_tok)
          ~span:(current_span parser) in
        make_error_node parser ~diagnostic ~consumed_tokens:[]
  in
  let trivia_after_name = consume_trivia parser in
  (* Check for GADT syntax (: type) or regular "of" clause *)
  let type_children =
    match peek_kind parser with
    | Token.Colon ->
        (* GADT constructor: Name : type_expr *)
        let colon = consume parser in
        let trivia_after_colon = consume_trivia parser in
        (* Parse the full GADT type (can include existentials like 'a. ...) *)
        let gadt_type = parse_typexpr parser in
        [ make_token parser colon ]
        @ tokens_to_green parser trivia_after_colon
        @ [ Ceibo.Green.Node gadt_type ]
    | Token.Keyword Keyword.Of ->
        (* Regular constructor: Name of args *)
        let of_kw = consume parser in
        let trivia_after_of = consume_trivia parser in
        (* Parse constructor arguments: type1 * type2 * type3 *)
        let first_arg = parse_typexpr parser in
        let trivia_after_first = consume_trivia parser in
        (* Collect additional arguments separated by * *)
        let rec parse_args acc =
          match peek_kind parser with
          | Token.Star ->
              let star = consume parser in
              let trivia_after_star = consume_trivia parser in
              let arg = parse_typexpr parser in
              let trivia_after_arg = consume_trivia parser in
              parse_args
                (tokens_to_green parser trivia_after_arg
                @ [ Ceibo.Green.Node arg ]
                @ tokens_to_green parser trivia_after_star
                @ [ make_token parser star ]
                @ acc)
          | _ -> List.rev acc
        in
        let additional_args = parse_args [] in
        [ make_token parser of_kw ]
        @ tokens_to_green parser trivia_after_of
        @ [ Ceibo.Green.Node first_arg ]
        @ tokens_to_green parser trivia_after_first
        @ additional_args
    | _ ->
        []
  in
  make_node
    Syntax_kind.TYPE_VARIANT_CONSTR
    ([ Ceibo.Green.Node constr_name ] @ tokens_to_green parser trivia_after_name @ type_children)

(** Parse variant type representation: ["|"] A | B of int | C
    
    type-representation ::= "=" ["|"] constr-decl { "|" constr-decl }
    
    Returns list of green children (including | tokens and constructors)
*)
and parse_variant_representation = fun parser ->
  (* Optional leading | *)
  let leading_pipe_children =
    match peek_kind parser with
    | Token.Pipe ->
        let pipe = consume parser in
        let trivia = consume_trivia parser in
        [ make_token parser pipe ] @ tokens_to_green parser trivia
    | _ -> []
  in
  (* Parse first constructor *)
  let first_constr = parse_variant_constr parser in
  let trivia_after_first = consume_trivia parser in
  (* Parse additional constructors: | A | B | C *)
  let rec parse_constrs acc =
    match peek_kind parser with
    | Token.Pipe ->
        let pipe = consume parser in
        let trivia_after_pipe = consume_trivia parser in
        let constr = parse_variant_constr parser in
        let trivia_after_constr = consume_trivia parser in
        parse_constrs
          (tokens_to_green parser trivia_after_constr
          @ [ Ceibo.Green.Node constr ]
          @ tokens_to_green parser trivia_after_pipe
          @ [ make_token parser pipe ]
          @ acc)
    | _ -> List.rev acc
  in
  let additional_constrs = parse_constrs [] in
  leading_pipe_children
  @ [ Ceibo.Green.Node first_constr ]
  @ tokens_to_green parser trivia_after_first
  @ additional_constrs

(** Parse type declaration: type [type_params] name = typexpr *)
(** Parse type constraints: constraint 'a = int constraint 'b = string *)
and parse_type_constraints = fun parser ->
  let rec parse_constraints acc =
    let trivia = consume_trivia parser in
    match peek_kind parser with
    | Token.Keyword Keyword.Constraint ->
        let constraint_kw = consume parser in
        let trivia_after_kw = consume_trivia parser in
        (* Parse type variable (left side of =) *)
        let type_var = parse_typexpr parser in
        let trivia_after_var = consume_trivia parser in
        (* Expect = *)
        let eq =
          if peek_kind parser = Token.Eq then
            consume parser
          else
            peek parser
        in
        let trivia_after_eq = consume_trivia parser in
        (* Parse concrete type (right side of =) *)
        let concrete_type = parse_typexpr parser in
        let constraint_node = make_node
          Syntax_kind.TYPE_CONSTRAINT
          ([ make_token parser constraint_kw ]
          @ tokens_to_green parser trivia_after_kw
          @ [ Ceibo.Green.Node type_var ]
          @ tokens_to_green parser trivia_after_var
          @ [ make_token parser eq ]
          @ tokens_to_green parser trivia_after_eq
          @ [ Ceibo.Green.Node concrete_type ]) in
        parse_constraints
          ([ Ceibo.Green.Node constraint_node ] @ tokens_to_green parser trivia @ acc)
    | _ -> (List.rev acc, trivia)
  in
  parse_constraints []

and parse_type_decl = fun parser ->
  if peek_kind parser = Token.Keyword Keyword.Type then
    (
      let type_kw = consume parser in
      let trivia_after_type = consume_trivia parser in
      let ext_nodes = parse_extension_name parser in
      let trivia_after_ext =
        if ext_nodes != [] then
          consume_trivia parser
        else
          []
      in
      let attr_nodes = parse_attributes parser in
      (* Check for optional 'nonrec' keyword: type nonrec t = ... *)
      let nonrec_children =
        match peek_kind parser with
        | Token.Keyword Keyword.Nonrec ->
            let nonrec_kw = consume parser in
            let trivia_after_nonrec = consume_trivia parser in
            [ make_token parser nonrec_kw ] @ tokens_to_green parser trivia_after_nonrec
        | _ -> []
      in
      (* Check for common mistake: bracketed type params like type foo<A, B> *)
      (* This check must come BEFORE type name parsing *)
      let bracket_error_early =
        match peek_kind parser with
        | Token.Ident _ ->
            (* Peek ahead to see if there's a < after the identifier *)
            let next_tok = Token_cursor.peek_n parser.cursor 1 in
            if next_tok.Token.kind = Token.Lt then
              let name_tok = consume parser in
              let lt_tok = consume parser in
              let type_name = token_text parser name_tok in
              let diagnostic = Diagnostic.bracketed_type_parameters
                ~type_name
                ~found:lt_tok
                ~text:"<"
                ~span:(Ceibo.Span.make ~start:lt_tok.Token.span.start ~end_:lt_tok.Token.span.end_) in
              (* Skip until > or = or EOF *)
              let rec skip_bracketed acc =
                match peek_kind parser with
                | Token.Gt ->
                    let gt_tok = consume parser in
                    List.rev (gt_tok :: acc)
                | Token.Eq
                | Token.EOF ->
                    List.rev acc
                | _ ->
                    let tok = consume parser in
                    skip_bracketed (tok :: acc)
              in
              let consumed = skip_bracketed [ lt_tok ] in
              let error_node = make_error_node parser ~diagnostic ~consumed_tokens:consumed in
              Some (name_tok, error_node)
            else
              None
        | _ -> None
      in
      (* Try to parse type parameters (e.g., 'a or ('a, 'b)) - optional *)
      let type_params, trivia_after_params =
        match bracket_error_early with
        | Some _ -> (* Already consumed name and brackets, no type params *)
          ([], [])
        | None -> (
            match peek_kind parser with
            | Token.Unknown '\'' ->
                (* Malformed type variable like ' a or '' *)
                let tok = consume parser in
                let diagnostic = Diagnostic.malformed_type_variable
                  ~found:tok
                  ~text:(token_text parser tok)
                  ~span:tok.Token.span in
                let error_node = make_error_node parser ~diagnostic ~consumed_tokens:[ tok ] in
                let trivia = consume_trivia parser in
                ([ Ceibo.Green.Node error_node ], trivia)
            | Token.Underscore ->
                (* Check if next token is also underscore (__ is invalid) *)
                let next_tok = Token_cursor.peek_n parser.cursor 1 in
                if next_tok.Token.kind = Token.Underscore then
                  let tok1 = consume parser in
                  let tok2 = consume parser in
                  let diagnostic = Diagnostic.invalid_type_parameter
                    ~text:"__"
                    ~found:tok1
                    ~text_found:"__"
                    ~span:(Ceibo.Span.make ~start:tok1.Token.span.start ~end_:tok2.Token.span.end_) in
                  let error_node = make_error_node parser ~diagnostic ~consumed_tokens:[ tok1; tok2 ] in
                  let trivia = consume_trivia parser in
                  ([ Ceibo.Green.Node error_node ], trivia)
                else
                  (* Single _ is a wildcard type param for GADTs: type _ t *)
                  parse_type_params parser
            | Token.Ident name when String.length name > 0 && String.get name 0 = '_' ->
                let tok = consume parser in
                let diagnostic = Diagnostic.invalid_type_parameter
                  ~text:name
                  ~found:tok
                  ~text_found:name
                  ~span:(current_span parser) in
                let error_node = make_error_node parser ~diagnostic ~consumed_tokens:[ tok ] in
                let trivia = consume_trivia parser in
                ([ Ceibo.Green.Node error_node ], trivia)
            | Token.Bang ->
                let next_tok = Token_cursor.peek_n parser.cursor 1 in
                let next_kind = next_tok.Token.kind in
                let valid_after_bang =
                  next_kind = Token.Quote
                  || next_kind = Token.Underscore
                  || next_kind = Token.Plus
                  || next_kind = Token.Minus in
                if valid_after_bang then
                  parse_type_params parser
                else
                  let tok = consume parser in
                  let text = token_text parser tok in
                  let diagnostic = Diagnostic.invalid_type_parameter
                    ~text
                    ~found:tok
                    ~text_found:text
                    ~span:(current_span parser) in
                  let error_node = make_error_node parser ~diagnostic ~consumed_tokens:[ tok ] in
                  let trivia = consume_trivia parser in
                  ([ Ceibo.Green.Node error_node ], trivia)
            | Token.At
            | Token.Caret
            | Token.OpenDelim Token.Bracket ->
                (* Invalid type parameter characters *)
                let tok = consume parser in
                let text = token_text parser tok in
                let diagnostic = Diagnostic.invalid_type_parameter
                  ~text
                  ~found:tok
                  ~text_found:text
                  ~span:(current_span parser) in
                let error_node = make_error_node parser ~diagnostic ~consumed_tokens:[ tok ] in
                let trivia = consume_trivia parser in
                ([ Ceibo.Green.Node error_node ], trivia)
            | _ ->
                parse_type_params parser
          )
      in
      (* Check if type param had error - if so, skip to sync point and return early *)
      match type_params with
      | [ Ceibo.Green.Node node ] when Ceibo.Green.kind (Ceibo.Green.Node node) = Syntax_kind.ERROR ->
          (* Error in type param - skip to next keyword and stop *)
          let skipped_tokens = error_recover_until
            parser
            ~sync_tokens:[ Token.Keyword Keyword.Let; Token.Keyword Keyword.Type ] in
          let trivia_after_skip = consume_trivia parser in
          make_node
            Syntax_kind.TYPE_DECL
            ([ make_token parser type_kw ]
            @ tokens_to_green parser trivia_after_type
            @ ext_nodes
            @ tokens_to_green parser trivia_after_ext
            @ attr_nodes
            @ nonrec_children
            @ type_params
            @ tokens_to_green parser trivia_after_params
            @ tokens_to_green parser skipped_tokens
            @ tokens_to_green parser trivia_after_skip)
      | _ ->
          (* No error in type params - continue normally *)
          (* Check if we already consumed name+brackets in bracket_error_early *)
          let type_name, bracket_error, trivia_after_name, trivia_after_bracket =
            match bracket_error_early with
            | Some (name_tok, error_node) ->
                (* Already consumed name and detected bracket error *)
                let name_node = make_node Syntax_kind.IDENT_EXPR [ make_token parser name_tok ] in
                let trivia = consume_trivia parser in
                (name_node, Some error_node, [], trivia)
            | None ->
                (* Normal case: parse type name (can be Module.t for extensible variants) *)
                let name =
                  match peek_kind parser with
                  | Token.Ident _ ->
                      let first_ident = consume parser in
                      (* Check for module path like Message.t *)
                      let rec parse_path_segments acc =
                        let trivia = consume_trivia parser in
                        if has_dot_ident_continuation parser then
                          let dot = consume parser in
                          let trivia_after_dot = consume_trivia parser in
                          let next_ident = consume parser in
                          parse_path_segments
                            (acc
                            @ tokens_to_green parser trivia
                            @ [ make_token parser dot ]
                            @ tokens_to_green parser trivia_after_dot
                            @ [ make_token parser next_ident ])
                        else
                          acc @ tokens_to_green parser trivia
                      in
                      let path_parts = parse_path_segments [] in
                      if List.length path_parts = 0 then
                        make_node Syntax_kind.IDENT_EXPR [ make_token parser first_ident ]
                      else
                        (* Module path like Message.t *)
                        make_node
                          Syntax_kind.MODULE_PATH
                          ([ make_token parser first_ident ] @ path_parts)
                  | _ ->
                      let found_tok = peek parser in
                      let diagnostic = Diagnostic.missing_type_name
                        ~found:found_tok
                        ~text:(token_text parser found_tok)
                        ~span:(current_span parser) in
                      make_error_node parser ~diagnostic ~consumed_tokens:[]
                in
                let trivia = consume_trivia parser in
                (name, None, trivia, [])
          in
          (* Check if type name had error - if so, skip to = or next keyword *)
          if Ceibo.Green.kind (Ceibo.Green.Node type_name) = Syntax_kind.ERROR then
            let skipped_tokens = error_recover_until
              parser
              ~sync_tokens:[ Token.Eq; Token.Keyword Keyword.Let; Token.Keyword Keyword.Type; ] in
            let trivia_after_skip = consume_trivia parser in
            (* Check if we found = *)
            match peek_kind parser with
            | Token.Eq ->
                (* Found =, continue parsing the type definition *)
                let eq = consume parser in
                let trivia_after_eq = consume_trivia parser in
                let type_expr = parse_typexpr parser in
                make_node
                  Syntax_kind.TYPE_DECL
                  ([ make_token parser type_kw ]
                  @ tokens_to_green parser trivia_after_type
                  @ ext_nodes
                  @ tokens_to_green parser trivia_after_ext
                  @ attr_nodes
                  @ nonrec_children
                  @ type_params
                  @ tokens_to_green parser trivia_after_params
                  @ [ Ceibo.Green.Node type_name ]
                  @ tokens_to_green parser skipped_tokens
                  @ tokens_to_green parser trivia_after_skip
                  @ [ make_token parser eq ]
                  @ tokens_to_green parser trivia_after_eq
                  @ [ Ceibo.Green.Node type_expr ])
            | _ ->
                (* Didn't find =, stop here *)
                make_node
                  Syntax_kind.TYPE_DECL
                  ([ make_token parser type_kw ]
                  @ tokens_to_green parser trivia_after_type
                  @ ext_nodes
                  @ tokens_to_green parser trivia_after_ext
                  @ attr_nodes
                  @ nonrec_children
                  @ type_params
                  @ tokens_to_green parser trivia_after_params
                  @ [ Ceibo.Green.Node type_name ]
                  @ tokens_to_green parser skipped_tokens
                  @ tokens_to_green parser trivia_after_skip)
          else
            (* No error in type name - continue normally *)
            (* trivia_after_name and bracket_error already parsed above *)
            (* Parse '=' or '+=' (optional for abstract types, or after bracket error) *)
            let eq_children, missing_eq_error, is_extensible, is_destructive_subst =
              match peek_kind parser with
              | Token.ColonEq ->
                  let colon_eq = consume parser in
                  ([ make_token parser colon_eq ], None, false, true)
              | Token.Plus ->
                  (* Check if this is += for extensible variant *)
                  let next_tok = Token_cursor.peek_n parser.cursor 1 in
                  if next_tok.Token.kind = Token.Eq then
                    let plus = consume parser in
                    let eq = consume parser in
                    ([ make_token parser plus; make_token parser eq ], None, true, false)
                  else
                    (* Invalid: + without = *)
                    let found_tok = peek parser in
                    let diagnostic = Diagnostic.missing_type_decl_equals
                      ~found:found_tok
                      ~text:(token_text parser found_tok)
                      ~span:(expected_span parser) in
                    ([], Some diagnostic, false, false)
              | Token.Eq ->
                  (* Regular type: type t = ... *)
                  let eq = consume parser in
                  ([ make_token parser eq ], None, false, false)
              | _ when Option.is_some bracket_error -> (* Already reported bracket error, don't complain about missing = *)
                ([], None, false, false)
              | Token.Ident _ ->
                  (* Found identifier after type name without = - this is an error like "type foo int" *)
                  let found_tok = peek parser in
                  let diagnostic = Diagnostic.missing_type_decl_equals
                    ~found:found_tok
                    ~text:(token_text parser found_tok)
                    ~span:(expected_span parser) in
                  ([], Some diagnostic, false, false)
              | _ -> (* No = found and no bracket error - this is abstract type (valid) *)
                ([], None, false, false)
            in
            (* If no =, this is an abstract type (valid in signatures) *)
            if eq_children = [] then
              let children =
                [ make_token parser type_kw ]
                @ tokens_to_green parser trivia_after_type
                @ ext_nodes
                @ tokens_to_green parser trivia_after_ext
                @ attr_nodes
                @ nonrec_children
                @ type_params
                @ tokens_to_green parser trivia_after_params
                @ [ Ceibo.Green.Node type_name ]
                @ tokens_to_green parser trivia_after_name
                @ (
                  match bracket_error with
                  | Some e -> [ Ceibo.Green.Node e ]
                  | None -> []
                )
                @ tokens_to_green parser trivia_after_bracket
                @ (
                  match missing_eq_error with
                  | Some diag ->
                      let error_node = make_error_node parser ~diagnostic:diag ~consumed_tokens:[] in
                      [ Ceibo.Green.Node error_node ]
                  | None -> []
                )
              in
              make_node Syntax_kind.TYPE_DECL children
            else if is_extensible then
              let trivia_after_eq = consume_trivia parser in
              let extensible_children =
                match peek_kind parser with
                | Token.DotDot ->
                    let dotdot = consume parser in
                    [
                      Ceibo.Green.Node (make_node
                        Syntax_kind.TYPE_EXTENSIBLE
                        [ make_token parser dotdot ])
                    ]
                | _ -> parse_variant_representation parser
              in
              make_node Syntax_kind.TYPE_DECL
                (
                  [ make_token parser type_kw ]
                  @ tokens_to_green parser trivia_after_type
                  @ ext_nodes
                  @ tokens_to_green parser trivia_after_ext
                  @ attr_nodes
                  @ nonrec_children
                  @ type_params
                  @ tokens_to_green parser trivia_after_params
                  @ [ Ceibo.Green.Node type_name ]
                  @ tokens_to_green parser trivia_after_name
                  @ (
                    match bracket_error with
                    | Some e -> [ Ceibo.Green.Node e ]
                    | None -> []
                  )
                  @ tokens_to_green parser trivia_after_bracket
                  @ eq_children
                  @ tokens_to_green parser trivia_after_eq
                  @ extensible_children
                )
            else if is_destructive_subst then
              let trivia_after_eq = consume_trivia parser in
              let type_expr = parse_typexpr parser in
              let trivia_after_expr = consume_trivia parser in
              make_node Syntax_kind.TYPE_DECL
                (
                  [ make_token parser type_kw ]
                  @ tokens_to_green parser trivia_after_type
                  @ ext_nodes
                  @ tokens_to_green parser trivia_after_ext
                  @ attr_nodes
                  @ nonrec_children
                  @ type_params
                  @ tokens_to_green parser trivia_after_params
                  @ [ Ceibo.Green.Node type_name ]
                  @ tokens_to_green parser trivia_after_name
                  @ (
                    match bracket_error with
                    | Some e -> [ Ceibo.Green.Node e ]
                    | None -> []
                  )
                  @ tokens_to_green parser trivia_after_bracket
                  @ eq_children
                  @ tokens_to_green parser trivia_after_eq
                  @ [ Ceibo.Green.Node type_expr ]
                  @ tokens_to_green parser trivia_after_expr
                )
            else
              (* Have =, parse type-information (type-equation + optional type-representation) *)
              let trivia_after_eq = consume_trivia parser in
              (* Check for optional 'private' keyword *)
              let private_kw_children, trivia_after_private =
                match peek_kind parser with
                | Token.Keyword Keyword.Private ->
                    let private_kw = consume parser in
                    let trivia = consume_trivia parser in
                    ([ make_token parser private_kw ], trivia)
                | _ -> ([], [])
              in
              (* Check if this is a variant/record definition (starts with | or {) *)
              (* OR if it starts with uppercase Ident (variant constructor) *)
              (* OR if it's a type equation followed by a variant/record *)
              let type_info_children =
                match peek_kind parser with
                | Token.DotDot ->
                    let dotdot = consume parser in
                    [
                      Ceibo.Green.Node (make_node
                        Syntax_kind.TYPE_EXTENSIBLE
                        [ make_token parser dotdot ])
                    ]
                | Token.Pipe ->
                    (* Direct variant: type t = | A | B *)
                    parse_variant_representation parser
                | Token.OpenDelim Token.Brace ->
                    (* Direct record: type t = { ... } *)
                    let record = parse_record_type parser in
                    [ Ceibo.Green.Node record ]
                | Token.Ident name when ident_starts_uppercase name ->
                    (* Uppercase identifier - could be variant constructor OR module path *)
                    (* Check if followed by dot to distinguish:
                       - Path.t -> module path (type alias)
                       - Some -> variant constructor *)
                    if uppercase_ident_starts_variant_representation parser then
                      parse_variant_representation parser
                    else
                      (* Module path like Path.t - parse as type expression *)
                      let type_expr = parse_typexpr parser in
                      let trivia_after_expr = consume_trivia parser in
                      (* Check for optional type-representation after type equation *)
                      (
                        match peek_kind parser with
                        | Token.Eq ->
                            (* Second = means: type t = alias = | A | B *)
                            let eq2 = consume parser in
                            let trivia_after_eq2 = consume_trivia parser in
                            (* Check for 'private' after second = *)
                            let private_kw2_children, trivia_after_private2 =
                              match peek_kind parser with
                              | Token.Keyword Keyword.Private ->
                                  let priv_kw = consume parser in
                                  let trivia = consume_trivia parser in
                                  ([ make_token parser priv_kw ], trivia)
                              | _ -> ([], [])
                            in
                            let representation =
                              match peek_kind parser with
                              | Token.DotDot ->
                                  let dotdot = consume parser in
                                  [
                                    Ceibo.Green.Node (make_node
                                      Syntax_kind.TYPE_EXTENSIBLE
                                      [ make_token parser dotdot ])
                                  ]
                              | Token.Pipe
                              | Token.Ident _ ->
                                  parse_variant_representation parser
                              | Token.OpenDelim Token.Brace ->
                                  let record = parse_record_type parser in
                                  [ Ceibo.Green.Node record ]
                              | _ ->
                                  []
                            in
                            [ Ceibo.Green.Node type_expr ]
                            @ tokens_to_green parser trivia_after_expr
                            @ [ make_token parser eq2 ]
                            @ tokens_to_green parser trivia_after_eq2
                            @ private_kw2_children
                            @ tokens_to_green parser trivia_after_private2
                            @ representation
                        | _ ->
                            (* Just type equation, no representation *)
                            [ Ceibo.Green.Node type_expr ] @ tokens_to_green parser trivia_after_expr
                      )
                | _ -> (* Check if this is an abstract type (private or not) - no representation *)
                  (
                    match peek_kind parser with
                    | Token.EOF ->
                        if List.length private_kw_children > 0 then
                          []
                        else
                          let found_tok = peek parser in
                          let diagnostic = Diagnostic.invalid_type_expression
                            ~found:found_tok
                            ~text:(token_text parser found_tok)
                            ~span:(expected_span parser) in
                          let error_node = make_error_node parser ~diagnostic ~consumed_tokens:[] in
                          [ Ceibo.Green.Node error_node ]
                    | Token.Keyword Keyword.True
                    | Token.Keyword Keyword.False ->
                        let type_expr = parse_typexpr parser in
                        let trivia_after_expr = consume_trivia parser in
                        [ Ceibo.Green.Node type_expr ] @ tokens_to_green parser trivia_after_expr
                    | Token.Keyword _ ->
                        if List.length private_kw_children > 0 then
                          []
                        else
                          let found_tok = peek parser in
                          let diagnostic = Diagnostic.invalid_type_expression
                            ~found:found_tok
                            ~text:(token_text parser found_tok)
                            ~span:(expected_span parser) in
                          let error_node = make_error_node parser ~diagnostic ~consumed_tokens:[] in
                          [ Ceibo.Green.Node error_node ]
                    | _ ->
                        (* Parse type equation first *)
                        let type_expr = parse_typexpr parser in
                        let trivia_after_expr = consume_trivia parser in
                        (* Check for optional type-representation after type equation *)
                        (
                          match peek_kind parser with
                          | Token.Eq ->
                              (* Second = means: type t = alias = | A | B *)
                              let eq2 = consume parser in
                              let trivia_after_eq2 = consume_trivia parser in
                              let representation =
                                match peek_kind parser with
                                | Token.DotDot ->
                                    let dotdot = consume parser in
                                    [
                                      Ceibo.Green.Node (make_node
                                        Syntax_kind.TYPE_EXTENSIBLE
                                        [ make_token parser dotdot ])
                                    ]
                                | Token.Pipe
                                | Token.Ident _ ->
                                    parse_variant_representation parser
                                | Token.OpenDelim Token.Brace ->
                                    let record = parse_record_type parser in
                                    [ Ceibo.Green.Node record ]
                                | _ ->
                                    []
                              in
                              [ Ceibo.Green.Node type_expr ]
                              @ tokens_to_green parser trivia_after_expr
                              @ [ make_token parser eq2 ]
                              @ tokens_to_green parser trivia_after_eq2
                              @ representation
                          | _ ->
                              (* Just type equation, no representation *)
                              [ Ceibo.Green.Node type_expr ] @ tokens_to_green parser trivia_after_expr
                        )
                  )
              in
              (* Parse optional type constraints *)
              let constraint_nodes, trivia_after_constraints = parse_type_constraints parser in
              let first_type_decl =
                make_node Syntax_kind.TYPE_DECL
                  (
                    [ make_token parser type_kw ]
                    @ tokens_to_green parser trivia_after_type
                    @ ext_nodes
                    @ tokens_to_green parser trivia_after_ext
                    @ attr_nodes
                    @ nonrec_children
                    @ type_params
                    @ tokens_to_green parser trivia_after_params
                    @ [ Ceibo.Green.Node type_name ]
                    @ tokens_to_green parser trivia_after_name
                    @ (
                      match bracket_error with
                      | Some e -> [ Ceibo.Green.Node e ]
                      | None -> []
                    )
                    @ tokens_to_green parser trivia_after_bracket
                    @ eq_children
                    @ tokens_to_green parser trivia_after_eq
                    @ private_kw_children
                    @ tokens_to_green parser trivia_after_private
                    @ type_info_children
                    @ tokens_to_green parser trivia_after_constraints
                    @ constraint_nodes
                  )
              in
              (* Check for 'and' to form mutually recursive types *)
              let trivia_after_first_start = position parser in
              let trivia_after_first = consume_trivia parser in
              let rec parse_and_types acc =
                match peek_kind parser with
                | Token.Keyword Keyword.And ->
                    let and_kw = consume parser in
                    let trivia_after_and = consume_trivia parser in
                    let ext_nodes2 = parse_extension_name parser in
                    let trivia_after_ext2 =
                      if ext_nodes2 != [] then
                        consume_trivia parser
                      else
                        []
                    in
                    let attr_nodes2 = parse_attributes parser in
                    (* Parse the next type declaration (without 'type' keyword) *)
                    (* This mirrors the logic above but without the initial 'type' keyword *)
                    let type_params2, trivia_after_params2 = parse_type_params parser in
                    let type_name2 =
                      match peek_kind parser with
                      | Token.Ident _ ->
                          let ident = consume parser in
                          make_node Syntax_kind.IDENT_EXPR [ make_token parser ident ]
                      | _ ->
                          let found_tok = peek parser in
                          let diagnostic = Diagnostic.missing_type_name
                            ~found:found_tok
                            ~text:(token_text parser found_tok)
                            ~span:(current_span parser) in
                          make_error_node parser ~diagnostic ~consumed_tokens:[]
                    in
                    let trivia_after_name2 = consume_trivia parser in
                    let eq2 =
                      if peek_kind parser = Token.Eq then
                        consume parser
                      else
                        peek parser
                    in
                    let trivia_after_eq2 = consume_trivia parser in
                    (* Parse type definition *)
                    let type_info_children2 =
                      match peek_kind parser with
                      | Token.Pipe ->
                          parse_variant_representation parser
                      | Token.OpenDelim Token.Brace ->
                          let record = parse_record_type parser in
                          [ Ceibo.Green.Node record ]
                      | Token.Ident _ when uppercase_ident_starts_variant_representation parser ->
                          parse_variant_representation parser
                      | Token.Ident name when ident_starts_uppercase name ->
                          let type_expr = parse_typexpr parser in
                          [ Ceibo.Green.Node type_expr ]
                      | _ ->
                          let type_expr = parse_typexpr parser in
                          [ Ceibo.Green.Node type_expr ]
                    in
                    (* Parse optional type constraints for 'and' type *)
                    let constraint_nodes2, trivia_after_constraints2 = parse_type_constraints parser in
                    let next_type_decl =
                      make_node
                        Syntax_kind.TYPE_DECL
                        (ext_nodes2
                        @ tokens_to_green parser trivia_after_ext2
                        @ attr_nodes2
                        @ type_params2
                        @ tokens_to_green parser trivia_after_params2
                        @ [ Ceibo.Green.Node type_name2 ]
                        @ tokens_to_green parser trivia_after_name2
                        @ [ make_token parser eq2 ]
                        @ tokens_to_green parser trivia_after_eq2
                        @ type_info_children2
                        @ tokens_to_green parser trivia_after_constraints2
                        @ constraint_nodes2)
                    in
                    let trivia_after_next = consume_trivia parser in
                    parse_and_types
                      (tokens_to_green parser trivia_after_next
                      @ [ Ceibo.Green.Node next_type_decl ]
                      @ tokens_to_green parser trivia_after_and
                      @ [ make_token parser and_kw ]
                      @ acc)
                | _ -> List.rev acc
              in
              let and_decls = parse_and_types [] in
              (* If we found 'and' types, wrap in MUTUAL, else return single *)
              if and_decls = [] then
                let () = Token_cursor.set_position parser.cursor trivia_after_first_start in
                first_type_decl
              else
                make_node
                  Syntax_kind.TYPE_MUTUAL_DECL
                  ([ Ceibo.Green.Node first_type_decl ]
                  @ tokens_to_green parser trivia_after_first
                  @ and_decls)
    )
  else
    let found_tok = peek parser in
    let diagnostic = Diagnostic.missing_type_keyword
      ~found:found_tok
      ~text:(token_text parser found_tok)
      ~span:(current_span parser) in
    make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse module type declaration: module type S = sig ... end *)
and parse_module_type_decl = fun parser ->
  (* Consume 'module' keyword *)
  let module_kw = consume parser in
  let trivia_after_module = consume_trivia parser in
  (* Consume 'type' keyword *)
  let type_kw = consume parser in
  let trivia_after_type = consume_trivia parser in
  let ext_nodes, trivia_after_ext, leading_attr_nodes = parse_extension_and_attributes parser in
  (* Parse module type name (must be uppercase identifier) *)
  let type_name =
    match peek_kind parser with
    | Token.Ident _ -> consume parser
    | _ ->
        let found = peek parser in
        let diagnostic = Diagnostic.missing_module_type_name
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser) in
        report_diagnostic parser diagnostic;
        found
  in
  let trivia_after_name = consume_trivia parser in
  (* Expect = or := *)
  let eq =
    match peek_kind parser with
    | Token.Eq
    | Token.ColonEq -> consume parser
    | _ -> expect
      parser
      Token.Eq
      (fun found ->
        Diagnostic.missing_let_binding_equals
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser))
  in
  let trivia_after_eq = consume_trivia parser in
  let type_expr =
    if can_start_module_type_expr parser then
      parse_module_type_expr parser
    else
      let diagnostic = Diagnostic.missing_module_type_expr
        ~found:(peek parser)
        ~text:(token_text parser (peek parser))
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]
  in
  let trivia_after_expr = consume_trivia parser in
  let trailing_attr_nodes = parse_attributes parser in
  (* Build MODULE_TYPE_DECL node *)
  make_node
    Syntax_kind.MODULE_TYPE_DECL
    ([ make_token parser module_kw ]
    @ tokens_to_green parser trivia_after_module
    @ [ make_token parser type_kw ]
    @ tokens_to_green parser trivia_after_type
    @ ext_nodes
    @ tokens_to_green parser trivia_after_ext
    @ leading_attr_nodes
    @ [ make_token parser type_name ]
    @ tokens_to_green parser trivia_after_name
    @ [ make_token parser eq ]
    @ tokens_to_green parser trivia_after_eq
    @ [ Ceibo.Green.Node type_expr ]
    @ tokens_to_green parser trivia_after_expr
    @ trailing_attr_nodes)

(** Parse module declaration: module M = E *)
and parse_module_decl = fun parser ->
  (* Consume 'module' keyword *)
  let module_kw = consume parser in
  let trivia_after_module = consume_trivia parser in
  let ext_nodes, trivia_after_ext, attr_nodes = parse_extension_and_attributes parser in
  let rec_children, trivia_after_rec =
    match peek_kind parser with
    | Token.Keyword Keyword.Rec ->
        let rec_kw = consume parser in
        let trivia = consume_trivia parser in
        ([ make_token parser rec_kw ], trivia)
    | _ -> ([], [])
  in
  (* Parse module name (must be uppercase identifier) *)
  let module_name =
    match peek_kind parser with
    | Token.Ident name when ident_starts_uppercase name ->
        consume parser
    | Token.Underscore ->
        consume parser
    | _ ->
        let found = peek parser in
        let diagnostic = Diagnostic.invalid_expression
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser) in
        report_diagnostic parser diagnostic;
        found
  in
  let trivia_after_name = consume_trivia parser in
  (* Parse functor parameters: (X : S) (Y : T) ... *)
  let rec parse_functor_params acc =
    if peek_kind parser = Token.OpenDelim Token.Paren then
      let lparen = consume parser in
      let trivia_after_lparen = consume_trivia parser in
      (* Parse parameter name (must be uppercase identifier) *)
      let param_name =
        match peek_kind parser with
        | Token.Ident name when ident_starts_uppercase name -> consume parser
        | _ ->
            let found = peek parser in
            let diagnostic = Diagnostic.invalid_expression
              ~found
              ~text:(token_text parser found)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            found
      in
      let trivia_after_param_name = consume_trivia parser in
      (* Expect colon *)
      let colon =
        expect
          parser
          Token.Colon
          (fun found ->
            Diagnostic.invalid_expression
              ~found
              ~text:(token_text parser found)
              ~span:(expected_span parser))
      in
      let trivia_after_colon = consume_trivia parser in
      (* Parse parameter module type *)
      let param_type = parse_module_type_expr parser in
      let trivia_after_param_type = consume_trivia parser in
      (* Expect closing paren *)
      let rparen =
        expect
          parser
          (Token.CloseDelim Token.Paren)
          (fun found ->
            Diagnostic.unclosed_delimiter
              ~opener:")"
              ~found
              ~text:(token_text parser found)
              ~span:(expected_span parser))
      in
      let trivia_after_rparen = consume_trivia parser in
      (* Build FUNCTOR_PARAM node *)
      let param_node =
        make_node
          Syntax_kind.FUNCTOR_PARAM
          ([ make_token parser lparen ]
          @ tokens_to_green parser trivia_after_lparen
          @ [ make_token parser param_name ]
          @ tokens_to_green parser trivia_after_param_name
          @ [ make_token parser colon ]
          @ tokens_to_green parser trivia_after_colon
          @ [ Ceibo.Green.Node param_type ]
          @ tokens_to_green parser trivia_after_param_type
          @ [ make_token parser rparen ]
          @ tokens_to_green parser trivia_after_rparen)
      in
      (* Continue parsing more functor parameters *)
      parse_functor_params (param_node :: acc)
    else
      List.rev acc
  in
  let functor_params = parse_functor_params [] in
  (* Check for optional signature constraint : S *)
  let signature_constraint =
    if peek_kind parser = Token.Colon then
      let colon = consume parser in
      let trivia_after_colon = consume_trivia parser in
      let sig_expr = parse_module_type_expr parser in
      let trivia_after_sig = consume_trivia parser in
      Some (colon, trivia_after_colon, sig_expr, trivia_after_sig)
    else
      None
  in
  (* Check for optional = (in .mli files, module declarations can end with just : sig) *)
  let module_expr_parts =
    if peek_kind parser = Token.Eq || peek_kind parser = Token.ColonEq then
      let eq = consume parser in
      let trivia_after_eq = consume_trivia parser in
      let module_expr = parse_module_expr parser in
      [ make_token parser eq ]
      @ tokens_to_green parser trivia_after_eq
      @ [ Ceibo.Green.Node module_expr ]
    else if can_start_module_expr parser then
      let found_tok = peek parser in
      let diagnostic = Diagnostic.missing_module_decl_equals
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(expected_span parser) in
      report_diagnostic parser diagnostic;
      let module_expr = parse_module_expr parser in
      [ Ceibo.Green.Node (make_node Syntax_kind.ERROR []) ] @ [ Ceibo.Green.Node module_expr ]
    else
      (* No =, this is a signature-only declaration (common in .mli files) *)
      []
  in
  (* Build MODULE_DECL node *)
  let children =
    [ make_token parser module_kw ]
    @ tokens_to_green parser trivia_after_module
    @ ext_nodes
    @ tokens_to_green parser trivia_after_ext
    @ attr_nodes
    @ rec_children
    @ tokens_to_green parser trivia_after_rec
    @ [ make_token parser module_name ]
    @ tokens_to_green parser trivia_after_name
    @ List.concat_map (fun param -> [ Ceibo.Green.Node param ]) functor_params
    @ (
      match signature_constraint with
      | Some (colon, trivia1, sig_expr, trivia2) -> [ make_token parser colon ]
      @ tokens_to_green parser trivia1
      @ [ Ceibo.Green.Node sig_expr ]
      @ tokens_to_green parser trivia2
      | None -> []
    )
    @ module_expr_parts
  in
  let trivia_after_first_start = position parser in
  let trivia_after_first = consume_trivia parser in
  let rec parse_and_modules acc =
    match peek_kind parser with
    | Token.Keyword Keyword.And ->
        let and_kw = consume parser in
        let trivia_after_and = consume_trivia parser in
        let attr_nodes2 = parse_attributes parser in
        let module_name2 =
          match peek_kind parser with
          | Token.Ident name when ident_starts_uppercase name ->
              consume parser
          | Token.Underscore ->
              consume parser
          | _ ->
              let found = peek parser in
              let diagnostic = Diagnostic.invalid_expression
                ~found
                ~text:(token_text parser found)
                ~span:(expected_span parser) in
              report_diagnostic parser diagnostic;
              found
        in
        let trivia_after_name2 = consume_trivia parser in
        let signature_constraint2 =
          if peek_kind parser = Token.Colon then
            let colon = consume parser in
            let trivia_after_colon = consume_trivia parser in
            let sig_expr = parse_module_type_expr parser in
            let trivia_after_sig = consume_trivia parser in
            Some (colon, trivia_after_colon, sig_expr, trivia_after_sig)
          else
            None
        in
        let module_expr_parts2 =
          if peek_kind parser = Token.Eq || peek_kind parser = Token.ColonEq then
            let eq = consume parser in
            let trivia_after_eq = consume_trivia parser in
            let module_expr = parse_module_expr parser in
            [ make_token parser eq ]
            @ tokens_to_green parser trivia_after_eq
            @ [ Ceibo.Green.Node module_expr ]
          else if can_start_module_expr parser then
            let found_tok = peek parser in
            let diagnostic = Diagnostic.missing_module_decl_equals
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            let module_expr = parse_module_expr parser in
            [ Ceibo.Green.Node (make_node Syntax_kind.ERROR []) ] @ [ Ceibo.Green.Node module_expr ]
          else
            []
        in
        let module_decl2 = make_node Syntax_kind.MODULE_DECL
          (
            attr_nodes2
            @ [ make_token parser module_name2 ]
            @ tokens_to_green parser trivia_after_name2
            @ (
              match signature_constraint2 with
              | Some (colon, trivia1, sig_expr, trivia2) -> [ make_token parser colon ]
              @ tokens_to_green parser trivia1
              @ [ Ceibo.Green.Node sig_expr ]
              @ tokens_to_green parser trivia2
              | None -> []
            )
            @ module_expr_parts2
          )
        in
        let trivia_after_decl2 = consume_trivia parser in
        parse_and_modules
          (acc
          @ [ make_token parser and_kw ]
          @ tokens_to_green parser trivia_after_and
          @ [ Ceibo.Green.Node module_decl2 ]
          @ tokens_to_green parser trivia_after_decl2)
    | _ -> acc
  in
  let and_children = parse_and_modules [] in
  if and_children = [] then
    let () = Token_cursor.set_position parser.cursor trivia_after_first_start in
    make_node Syntax_kind.MODULE_DECL children
  else
    make_node
      Syntax_kind.TYPE_MUTUAL_DECL
      ([ Ceibo.Green.Node (make_node Syntax_kind.MODULE_DECL children) ]
      @ tokens_to_green parser trivia_after_first
      @ and_children)

(** Parse module expression: struct...end, functor application, or identifier *)
(** Parse module path for first-class modules: A.B.C *)
and parse_module_path_or_expr = fun parser ->
  (* Start with first identifier *)
  match peek_kind parser with
  | Token.Ident _ ->
      let first_ident = consume parser in
      let rec parse_path acc =
        let trivia = consume_trivia parser in
        if has_dot_ident_continuation parser then
          let dot = consume parser in
          let trivia_after_dot = consume_trivia parser in
          let ident = consume parser in
          parse_path
            (acc
            @ tokens_to_green parser trivia
            @ [ make_token parser dot ]
            @ tokens_to_green parser trivia_after_dot
            @ [ make_token parser ident ])
        else
          acc @ tokens_to_green parser trivia
      in
      let path_children = [ make_token parser first_ident ] @ parse_path [] in
      make_node Syntax_kind.MODULE_PATH path_children
  | _ ->
      let found = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found
        ~text:(token_text parser found)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[ found ]

(** Parse module type path for type annotations: Transport or A.B.Transport *)
and parse_module_type_path = fun parser ->
  match peek_kind parser with
  | Token.Ident _ ->
      let first_ident = consume parser in
      let rec parse_path acc =
        let trivia = consume_trivia parser in
        if has_dot_ident_continuation parser then
          let dot = consume parser in
          let trivia_after_dot = consume_trivia parser in
          let ident = consume parser in
          parse_path
            (acc
            @ tokens_to_green parser trivia
            @ [ make_token parser dot ]
            @ tokens_to_green parser trivia_after_dot
            @ [ make_token parser ident ])
        else
          acc @ tokens_to_green parser trivia
      in
      let path_children = [ make_token parser first_ident ] @ parse_path [] in
      make_node Syntax_kind.MODULE_TYPE_PATH path_children
  | _ ->
      let found = peek parser in
      let diagnostic = Diagnostic.invalid_type_expression
        ~found
        ~text:(token_text parser found)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[ found ]

(** Parse module type expression for first-class modules: S or S with type t = int *)
and parse_module_type_expr = fun parser ->
  (* Check for "module type of" first *)
  let base =
    match peek_kind parser with
    | Token.OpenDelim Token.Bracket when is_extension_start parser ->
        parse_extension parser
    | Token.OpenDelim Token.Brace when is_brace_extension_start parser ->
        parse_extension parser
    | Token.Keyword Keyword.Module when starts_with_module_type_of_keywords parser ->
        let module_kw = consume parser in
        let trivia_after_module = consume_trivia parser in
        let type_kw = consume parser in
        let trivia_after_type = consume_trivia parser in
        let of_kw = consume parser in
        let trivia_after_of = consume_trivia parser in
        let module_path = parse_module_type_path parser in
        make_node
          Syntax_kind.MODULE_TYPE_OF
          ([ make_token parser module_kw ]
          @ tokens_to_green parser trivia_after_module
          @ [ make_token parser type_kw ]
          @ tokens_to_green parser trivia_after_type
          @ [ make_token parser of_kw ]
          @ tokens_to_green parser trivia_after_of
          @ [ Ceibo.Green.Node module_path ])
    | Token.Keyword Keyword.Module ->
        let found = peek parser in
        let diagnostic = Diagnostic.invalid_expression
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser) in
        make_error_node parser ~diagnostic ~consumed_tokens:[ found ]
    | Token.Keyword Keyword.Functor ->
        (* Functor type: functor (X : S) -> T *)
        let functor_kw = consume parser in
        let trivia_after_functor = consume_trivia parser in
        let attr_nodes = parse_attributes parser in
        (* Parse functor parameter(s) *)
        let rec parse_functor_type_params acc =
          if peek_kind parser = Token.OpenDelim Token.Paren then
            let lparen = consume parser in
            let trivia_after_lparen = consume_trivia parser in
            (* Parse parameter name *)
            let param_name =
              match peek_kind parser with
              | Token.Ident name when ident_starts_uppercase name ->
                  consume parser
              | Token.Underscore ->
                  consume parser
              | _ ->
                  let found = peek parser in
                  let diagnostic = Diagnostic.invalid_expression
                    ~found
                    ~text:(token_text parser found)
                    ~span:(expected_span parser) in
                  report_diagnostic parser diagnostic;
                  found
            in
            let trivia_after_param_name = consume_trivia parser in
            (* Expect colon *)
            let colon =
              expect
                parser
                Token.Colon
                (fun found ->
                  Diagnostic.invalid_expression
                    ~found
                    ~text:(token_text parser found)
                    ~span:(expected_span parser))
            in
            let trivia_after_colon = consume_trivia parser in
            (* Parse parameter module type *)
            let param_type = parse_module_type_expr parser in
            let trivia_after_param_type = consume_trivia parser in
            (* Expect closing paren *)
            let rparen =
              expect
                parser
                (Token.CloseDelim Token.Paren)
                (fun found ->
                  Diagnostic.unclosed_delimiter
                    ~opener:")"
                    ~found
                    ~text:(token_text parser found)
                    ~span:(expected_span parser))
            in
            let trivia_after_rparen = consume_trivia parser in
            (* Build FUNCTOR_PARAM node *)
            let param_node =
              make_node
                Syntax_kind.FUNCTOR_PARAM
                ([ make_token parser lparen ]
                @ tokens_to_green parser trivia_after_lparen
                @ [ make_token parser param_name ]
                @ tokens_to_green parser trivia_after_param_name
                @ [ make_token parser colon ]
                @ tokens_to_green parser trivia_after_colon
                @ [ Ceibo.Green.Node param_type ]
                @ tokens_to_green parser trivia_after_param_type
                @ [ make_token parser rparen ]
                @ tokens_to_green parser trivia_after_rparen)
            in
            parse_functor_type_params (param_node :: acc)
          else
            List.rev acc
        in
        let functor_params = parse_functor_type_params [] in
        (* Expect -> *)
        let arrow =
          expect
            parser
            Token.Arrow
            (fun found ->
              Diagnostic.invalid_expression
                ~found
                ~text:(token_text parser found)
                ~span:(expected_span parser))
        in
        let trivia_after_arrow = consume_trivia parser in
        (* Parse return type (right-associative) *)
        let return_type = parse_module_type_expr parser in
        make_node
          Syntax_kind.FUNCTOR_TYPE
          ([ make_token parser functor_kw ]
          @ tokens_to_green parser trivia_after_functor
          @ attr_nodes
          @ List.concat_map (fun param -> [ Ceibo.Green.Node param ]) functor_params
          @ [ make_token parser arrow ]
          @ tokens_to_green parser trivia_after_arrow
          @ [ Ceibo.Green.Node return_type ])
    | Token.OpenDelim Token.Paren ->
        let lparen = consume parser in
        let trivia_after_lparen = consume_trivia parser in
        if is_parenthesized_functor_type_start parser then
          (
            let param_name = consume parser in
            let trivia_after_param_name = consume_trivia parser in
            let colon =
              expect
                parser
                Token.Colon
                (fun found ->
                  Diagnostic.invalid_expression
                    ~found
                    ~text:(token_text parser found)
                    ~span:(expected_span parser))
            in
            let trivia_after_colon = consume_trivia parser in
            let param_type = parse_module_type_expr parser in
            let trivia_after_param_type = consume_trivia parser in
            let rparen =
              expect
                parser
                (Token.CloseDelim Token.Paren)
                (fun found ->
                  Diagnostic.unclosed_delimiter
                    ~opener:")"
                    ~found
                    ~text:(token_text parser found)
                    ~span:(expected_span parser))
            in
            let trivia_after_rparen = consume_trivia parser in
            let arrow =
              expect
                parser
                Token.Arrow
                (fun found ->
                  Diagnostic.invalid_expression
                    ~found
                    ~text:(token_text parser found)
                    ~span:(expected_span parser))
            in
            let trivia_after_arrow = consume_trivia parser in
            let return_type = parse_module_type_expr parser in
            let param =
              make_node
                Syntax_kind.FUNCTOR_PARAM
                ([ make_token parser lparen ]
                @ tokens_to_green parser trivia_after_lparen
                @ [ make_token parser param_name ]
                @ tokens_to_green parser trivia_after_param_name
                @ [ make_token parser colon ]
                @ tokens_to_green parser trivia_after_colon
                @ [ Ceibo.Green.Node param_type ]
                @ tokens_to_green parser trivia_after_param_type
                @ [ make_token parser rparen ])
            in
            make_node
              Syntax_kind.FUNCTOR_TYPE
              ([ Ceibo.Green.Node param ]
              @ tokens_to_green parser trivia_after_rparen
              @ [ make_token parser arrow ]
              @ tokens_to_green parser trivia_after_arrow
              @ [ Ceibo.Green.Node return_type ])
          )
        else
          let inner = parse_module_type_expr parser in
          let trivia_before_rparen = consume_trivia parser in
          let rparen =
            expect
              parser
              (Token.CloseDelim Token.Paren)
              (fun found ->
                Diagnostic.unclosed_delimiter
                  ~opener:")"
                  ~found
                  ~text:(token_text parser found)
                  ~span:(expected_span parser))
          in
          make_node
            Syntax_kind.PAREN_EXPR
            ([ make_token parser lparen ]
            @ tokens_to_green parser trivia_after_lparen
            @ [ Ceibo.Green.Node inner ]
            @ tokens_to_green parser trivia_before_rparen
            @ [ make_token parser rparen ])
    | Token.Ident _ ->
        parse_module_type_path parser
    | Token.OpenDelim Token.SigEnd ->
        parse_sig_expr parser
    | _ ->
        let found = peek parser in
        let diagnostic = Diagnostic.invalid_expression
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser) in
        make_error_node parser ~diagnostic ~consumed_tokens:[ found ]
  in
  let base_with_attrs = attach_postfix_attributes parser base in
  let with_wrapped =
    let trivia_start = Token_cursor.position parser.cursor in
    let trivia_after_base = consume_trivia parser in
    if peek_kind parser = Token.Keyword Keyword.With then
      let with_kw = consume parser in
      let trivia_after_with = consume_trivia parser in
      let constraint_children = [ make_token parser with_kw ]
      @ tokens_to_green parser trivia_after_with
      @ parse_module_type_constraints parser in
      make_node
        Syntax_kind.MODULE_TYPE_EXPR
        ([ Ceibo.Green.Node base_with_attrs ] @ tokens_to_green parser trivia_after_base @ constraint_children)
    else (
      Token_cursor.set_position parser.cursor trivia_start;
      base_with_attrs
    )
  in
  attach_postfix_attributes parser with_wrapped

and parse_module_expr = fun parser ->
  (* Parse base module expression *)
  let base =
    match peek_kind parser with
    | Token.OpenDelim Token.Bracket when is_extension_start parser ->
        parse_extension parser
    | Token.OpenDelim Token.Brace when is_brace_extension_start parser ->
        parse_extension parser
    | Token.Keyword Keyword.Functor ->
        let functor_kw = consume parser in
        let trivia_after_functor = consume_trivia parser in
        let attr_nodes = parse_attributes parser in
        let rec parse_functor_params acc =
          if peek_kind parser = Token.OpenDelim Token.Paren then
            let lparen = consume parser in
            let trivia_after_lparen = consume_trivia parser in
            let param_name =
              match peek_kind parser with
              | Token.Ident name when ident_starts_uppercase name ->
                  consume parser
              | Token.Underscore ->
                  consume parser
              | _ ->
                  let found = peek parser in
                  let diagnostic = Diagnostic.invalid_expression
                    ~found
                    ~text:(token_text parser found)
                    ~span:(expected_span parser) in
                  report_diagnostic parser diagnostic;
                  found
            in
            let trivia_after_param_name = consume_trivia parser in
            let colon =
              expect
                parser
                Token.Colon
                (fun found ->
                  Diagnostic.invalid_expression
                    ~found
                    ~text:(token_text parser found)
                    ~span:(expected_span parser))
            in
            let trivia_after_colon = consume_trivia parser in
            let param_type = parse_module_type_expr parser in
            let trivia_after_param_type = consume_trivia parser in
            let rparen =
              expect
                parser
                (Token.CloseDelim Token.Paren)
                (fun found ->
                  Diagnostic.unclosed_delimiter
                    ~opener:")"
                    ~found
                    ~text:(token_text parser found)
                    ~span:(expected_span parser))
            in
            let trivia_after_rparen = consume_trivia parser in
            let param =
              make_node
                Syntax_kind.FUNCTOR_PARAM
                ([ make_token parser lparen ]
                @ tokens_to_green parser trivia_after_lparen
                @ [ make_token parser param_name ]
                @ tokens_to_green parser trivia_after_param_name
                @ [ make_token parser colon ]
                @ tokens_to_green parser trivia_after_colon
                @ [ Ceibo.Green.Node param_type ]
                @ tokens_to_green parser trivia_after_param_type
                @ [ make_token parser rparen ]
                @ tokens_to_green parser trivia_after_rparen)
            in
            parse_functor_params (param :: acc)
          else
            List.rev acc
        in
        let functor_params = parse_functor_params [] in
        let arrow =
          expect
            parser
            Token.Arrow
            (fun found ->
              Diagnostic.invalid_expression
                ~found
                ~text:(token_text parser found)
                ~span:(expected_span parser))
        in
        let trivia_after_arrow = consume_trivia parser in
        let body = parse_module_expr parser in
        make_node
          Syntax_kind.FUNCTOR_TYPE
          ([ make_token parser functor_kw ]
          @ tokens_to_green parser trivia_after_functor
          @ attr_nodes
          @ List.concat_map (fun param -> [ Ceibo.Green.Node param ]) functor_params
          @ [ make_token parser arrow ]
          @ tokens_to_green parser trivia_after_arrow
          @ [ Ceibo.Green.Node body ])
    | Token.OpenDelim Token.StructEnd ->
        parse_struct_expr parser
    | Token.OpenDelim Token.Paren ->
        (* Parenthesized module expression *)
        let lparen = consume parser in
        let trivia_after_lparen = consume_trivia parser in
        let inner =
          if peek_kind parser = Token.Keyword Keyword.Val then
            let val_kw = consume parser in
            let trivia_after_val = consume_trivia parser in
            let inner_expr = parse_expr parser in
            let trivia_after_expr = consume_trivia parser in
            let type_constraint =
              if peek_kind parser = Token.Colon then
                let colon = consume parser in
                let trivia_after_colon = consume_trivia parser in
                let sig_type = parse_module_type_expr parser in
                let trivia_after_sig_type = consume_trivia parser in
                Some (colon, trivia_after_colon, sig_type, trivia_after_sig_type)
              else
                None
            in
            let children = [ make_token parser val_kw ]
            @ tokens_to_green parser trivia_after_val
            @ [ Ceibo.Green.Node inner_expr ]
            @ tokens_to_green parser trivia_after_expr
            @ (
              match type_constraint with
              | Some (colon, trivia, sig_type, trivia_after_sig_type) -> [ make_token parser colon ]
              @ tokens_to_green parser trivia
              @ [ Ceibo.Green.Node sig_type ]
              @ tokens_to_green parser trivia_after_sig_type
              | None -> []
            )
            in
            make_node Syntax_kind.FIRST_CLASS_MODULE_EXPR children
          else if is_parenthesized_module_type_expr_start parser then
            parse_module_type_expr parser
          else
            parse_module_expr parser
        in
        let trivia_before_rparen = consume_trivia parser in
        let rparen =
          expect
            parser
            (Token.CloseDelim Token.Paren)
            (fun found ->
              Diagnostic.unclosed_delimiter
                ~opener:")"
                ~found
                ~text:(token_text parser found)
                ~span:(expected_span parser))
        in
        make_node
          Syntax_kind.PAREN_EXPR
          ([ make_token parser lparen ]
          @ tokens_to_green parser trivia_after_lparen
          @ [ Ceibo.Green.Node inner ]
          @ tokens_to_green parser trivia_before_rparen
          @ [ make_token parser rparen ])
    | Token.Ident name ->
        (* Module identifier or module path (A.B.C) *)
        let first_ident = consume parser in
        if not (ident_starts_uppercase name) then
          let diagnostic = Diagnostic.invalid_module_name
            ~found:first_ident
            ~text:(token_text parser first_ident)
            ~span:first_ident.Token.span in
          make_error_node parser ~diagnostic ~consumed_tokens:[ first_ident ]
        else
          (* Check if this is a module path by looking for dots *)
          let rec parse_path_rest acc =
            let trivia = consume_trivia parser in
            if has_dot_ident_continuation parser then
              let dot = consume parser in
              let trivia_after_dot = consume_trivia parser in
              let ident = consume parser in
              parse_path_rest
                (acc
                @ tokens_to_green parser trivia
                @ [ make_token parser dot ]
                @ tokens_to_green parser trivia_after_dot
                @ [ make_token parser ident ])
            else
              acc @ tokens_to_green parser trivia
          in
          let path_parts = parse_path_rest [] in
          if List.length path_parts = 0 then
            make_node Syntax_kind.IDENT_EXPR [ make_token parser first_ident ]
          else
            (* Module path *)
            make_node Syntax_kind.MODULE_PATH ([ make_token parser first_ident ] @ path_parts)
    | _ ->
        let found = peek parser in
        let diagnostic = Diagnostic.invalid_expression
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser) in
        make_error_node parser ~diagnostic ~consumed_tokens:[ found ]
  in
  let base = attach_postfix_attributes parser base in
  (* Parse functor applications: M(X)(Y)(Z) and generative applications M() *)
  let rec parse_applications expr =
    if peek_kind parser = Token.OpenDelim Token.Paren then
      let trivia = consume_trivia parser in
      let lparen = consume parser in
      let trivia_after_lparen = consume_trivia parser in
      let app =
        if peek_kind parser = Token.CloseDelim Token.Paren then
          let rparen = consume parser in
          make_node
            Syntax_kind.MODULE_UNIT_APPLICATION
            (tokens_to_green parser trivia
            @ [ Ceibo.Green.Node expr ]
            @ [ make_token parser lparen ]
            @ tokens_to_green parser trivia_after_lparen
            @ [ make_token parser rparen ])
        else
          let arg = parse_module_expr parser in
          let trivia_before_rparen = consume_trivia parser in
          let rparen =
            expect
              parser
              (Token.CloseDelim Token.Paren)
              (fun found ->
                Diagnostic.unclosed_delimiter
                  ~opener:")"
                  ~found
                  ~text:(token_text parser found)
                  ~span:(expected_span parser))
          in
          make_node
            Syntax_kind.MODULE_APPLICATION
            (tokens_to_green parser trivia
            @ [ Ceibo.Green.Node expr ]
            @ [ make_token parser lparen ]
            @ tokens_to_green parser trivia_after_lparen
            @ [ Ceibo.Green.Node arg ]
            @ tokens_to_green parser trivia_before_rparen
            @ [ make_token parser rparen ])
      in
      parse_applications app
    else
      expr
  in
  parse_applications base

(** Parse struct...end expression *)
and parse_struct_expr = fun parser ->
  (* Consume 'struct' keyword *)
  let struct_kw = consume parser in
  (* Parse structure items until 'end' *)
  let rec parse_items acc =
    match peek_kind parser with
    | Token.CloseDelim Token.StructEnd ->
        List.rev acc
    | Token.EOF ->
        List.rev acc
    | _ ->
        let item = parse_structure_item ~in_block:true parser in
        parse_items ([ Ceibo.Green.Node item ] @ acc)
  in
  let items = parse_items [] in
  (* Consume 'end' keyword *)
  let end_kw =
    expect
      parser
      (Token.CloseDelim Token.StructEnd)
      (fun found ->
        Diagnostic.unclosed_delimiter
          ~opener:"struct"
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser))
  in
  (* Build STRUCT_EXPR node *)
  make_node
    Syntax_kind.STRUCT_EXPR
    ([ make_token parser struct_kw ] @ items @ [ make_token parser end_kw ])

(** Parse sig...end expression *)
and parse_sig_expr = fun parser ->
  (* Consume 'sig' keyword *)
  let sig_kw = consume parser in
  (* Parse signature items until 'end' *)
  let rec parse_items acc =
    match peek_kind parser with
    | Token.CloseDelim Token.SigEnd ->
        List.rev acc
    | Token.EOF ->
        List.rev acc
    | _ ->
        let item = parse_signature_item parser in
        parse_items ([ Ceibo.Green.Node item ] @ acc)
  in
  let items = parse_items [] in
  (* Consume 'end' keyword *)
  let end_kw =
    expect
      parser
      (Token.CloseDelim Token.SigEnd)
      (fun found ->
        Diagnostic.unclosed_delimiter
          ~opener:"sig"
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser))
  in
  (* Build SIG_EXPR node *)
  make_node
    Syntax_kind.SIG_EXPR
    ([ make_token parser sig_kw ] @ items @ [ make_token parser end_kw ])

(** Parse external declaration: external name : type = "primitive" *)
and parse_external_decl = fun parser ->
  (* Consume 'external' keyword *)
  let external_kw = consume parser in
  let trivia_after_external = consume_trivia parser in
  let ext_nodes, trivia_after_ext, leading_attr_nodes = parse_extension_and_attributes parser in
  (* Parse external name (lowercase identifier) *)
  let name =
    match peek_kind parser with
    | Token.Ident _ -> consume parser
    | _ ->
        let found = peek parser in
        let diagnostic = Diagnostic.invalid_expression
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser) in
        report_diagnostic parser diagnostic;
        found
  in
  let trivia_after_name = consume_trivia parser in
  (* Expect : *)
  let colon =
    expect
      parser
      Token.Colon
      (fun found ->
        Diagnostic.missing_external_colon
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser))
  in
  let trivia_after_colon = consume_trivia parser in
  (* Parse type expression *)
  let type_expr = parse_typexpr parser in
  let trivia_after_type = consume_trivia parser in
  (* Expect = *)
  let eq =
    expect
      parser
      Token.Eq
      (fun found ->
        Diagnostic.missing_let_binding_equals
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser))
  in
  let trivia_after_eq = consume_trivia parser in
  (* Parse primitive names (one or more string literals) *)
  (* OCaml allows multiple primitive names: external f : int = "prim1" "prim2" *)
  let primitive_names = ref [] in
  let primitive_trivia = ref [] in
  let rec collect_primitives () =
    match peek_kind parser with
    | Token.Literal (Token.String _) ->
        let prim = consume parser in
        primitive_names := prim :: !primitive_names;
        let trivia = consume_trivia parser in
        primitive_trivia := trivia :: !primitive_trivia;
        collect_primitives ()
    | _ -> ()
  in
  (* Parse first primitive (required) *)
  let first_primitive =
    match peek_kind parser with
    | Token.Literal (Token.String _) -> consume parser
    | _ ->
        let found = peek parser in
        let diagnostic = Diagnostic.invalid_expression
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser) in
        report_diagnostic parser diagnostic;
        found
  in
  let trivia_after_first = consume_trivia parser in
  (* Collect additional primitives *)
  collect_primitives ();
  (* Build primitive nodes *)
  let prim_list = List.rev !primitive_names in
  let trivia_list = List.rev !primitive_trivia in
  let additional_prims =
    List.mapi
      (fun i prim ->
        let trivia =
          if i < List.length trivia_list then
            List.nth trivia_list i
          else
            []
        in
        [ make_token parser prim ] @ tokens_to_green parser trivia)
      prim_list
    |> List.flatten
  in
  (* Parse optional trailing attributes: external f : int = "foo" [@@unboxed] *)
  let attr_nodes = parse_attributes parser in
  (* Build EXTERNAL_DECL node *)
  make_node
    Syntax_kind.EXTERNAL_DECL
    ([ make_token parser external_kw ]
    @ tokens_to_green parser trivia_after_external
    @ ext_nodes
    @ tokens_to_green parser trivia_after_ext
    @ leading_attr_nodes
    @ [ make_token parser name ]
    @ tokens_to_green parser trivia_after_name
    @ [ make_token parser colon ]
    @ tokens_to_green parser trivia_after_colon
    @ [ Ceibo.Green.Node type_expr ]
    @ tokens_to_green parser trivia_after_type
    @ [ make_token parser eq ]
    @ tokens_to_green parser trivia_after_eq
    @ [ make_token parser first_primitive ]
    @ tokens_to_green parser trivia_after_first
    @ additional_prims
    @ attr_nodes)

(** Parse exception declaration: exception Name or exception Name of type *)
and parse_exception_decl = fun parser ->
  (* Consume 'exception' keyword *)
  let exception_kw = consume parser in
  let trivia_after_exception = consume_trivia parser in
  let ext_nodes, trivia_after_ext, attr_nodes = parse_extension_and_attributes parser in
  (* Parse exception name (must be uppercase identifier) *)
  let name =
    match peek_kind parser with
    | Token.Ident name when ident_starts_uppercase name -> consume parser
    | _ ->
        let found = peek parser in
        let diagnostic = Diagnostic.missing_exception_name
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser) in
        report_diagnostic parser diagnostic;
        found
  in
  let trivia_after_name = consume_trivia parser in
  (* Check for optional alias or payload *)
  let payload_children =
    match peek_kind parser with
    | Token.Eq ->
        let eq_tok = consume parser in
        let trivia_after_eq = consume_trivia parser in
        let alias_expr = parse_expr parser in
        [ make_token parser eq_tok ]
        @ tokens_to_green parser trivia_after_eq
        @ [ Ceibo.Green.Node alias_expr ]
    | Token.Keyword Keyword.Of ->
        let of_kw = consume parser in
        let trivia_after_of = consume_trivia parser in
        let type_expr = parse_typexpr parser in
        [ make_token parser of_kw ]
        @ tokens_to_green parser trivia_after_of
        @ [ Ceibo.Green.Node type_expr ]
    | _ ->
        []
  in
  (* Build EXCEPTION_DECL node *)
  make_node
    Syntax_kind.EXCEPTION_DECL
    ([ make_token parser exception_kw ]
    @ tokens_to_green parser trivia_after_exception
    @ ext_nodes
    @ tokens_to_green parser trivia_after_ext
    @ attr_nodes
    @ [ make_token parser name ]
    @ tokens_to_green parser trivia_after_name
    @ payload_children)

(** Parse an identifier-like name, including raw identifiers such as \#and. *)
and parse_ident_like = fun parser ->
  match peek_kind parser with
  | Token.Ident _ ->
      let ident = consume parser in
      make_node Syntax_kind.IDENT_EXPR [ make_token parser ident ]
  | Token.Unknown '\\' ->
      let slash = consume parser in
      let hash_children, name_children =
        match peek_kind parser with
        | Token.Hash ->
            let hash = consume parser in
            let name_children =
              match peek_kind parser with
              | Token.Ident _
              | Token.Keyword _ ->
                  let name = consume parser in
                  [ make_token parser name ]
              | _ ->
                  let found = peek parser in
                  let diagnostic = Diagnostic.invalid_expression
                    ~found
                    ~text:(token_text parser found)
                    ~span:(expected_span parser) in
                  report_diagnostic parser diagnostic;
                  []
            in
            ([ make_token parser hash ], name_children)
        | _ ->
            let found = peek parser in
            let diagnostic = Diagnostic.invalid_expression
              ~found
              ~text:(token_text parser found)
              ~span:(expected_span parser) in
            report_diagnostic parser diagnostic;
            ([], [])
      in
      make_node Syntax_kind.IDENT_EXPR ([ make_token parser slash ] @ hash_children @ name_children)
  | _ ->
      let found = peek parser in
      let diagnostic = Diagnostic.invalid_expression
        ~found
        ~text:(token_text parser found)
        ~span:(expected_span parser) in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse a hash-prefixed identifier/operator sequence like #x, ##x, or #-#x. *)
and parse_hash_ident_expr = fun parser ->
  let hash = consume parser in
  let rec collect acc =
    match peek_kind parser with
    | Token.Ident _ ->
        let ident = consume parser in
        List.rev ([ make_token parser ident ] @ acc)
    | tok when is_operator_token tok ->
        let op = consume parser in
        collect (make_token parser op :: acc)
    | _ ->
        List.rev acc
  in
  let suffix = collect [ make_token parser hash ] in
  make_node Syntax_kind.IDENT_EXPR suffix

(** Parse class declaration: class ['a] c = expr or class c : typ *)
and parse_class_decl = fun parser ->
  let class_kw = consume parser in
  let trivia_after_class = consume_trivia parser in
  let ext_nodes, trivia_after_ext, attr_nodes = parse_extension_and_attributes parser in
  let type_params, trivia_after_params = parse_class_type_params parser in
  let class_name = parse_ident_like parser in
  let trivia_after_name = consume_trivia parser in
  let body_children =
    match peek_kind parser with
    | Token.Eq ->
        let eq = consume parser in
        let trivia_after_eq = consume_trivia parser in
        let body = parse_expr parser in
        let trivia_after_body = consume_trivia parser in
        let trailing_attrs = parse_attributes parser in
        [ make_token parser eq ]
        @ tokens_to_green parser trivia_after_eq
        @ [ Ceibo.Green.Node body ]
        @ tokens_to_green parser trivia_after_body
        @ trailing_attrs
    | Token.Colon ->
        let colon = consume parser in
        let trivia_after_colon = consume_trivia parser in
        let body = parse_class_type_expr parser in
        let trivia_after_body = consume_trivia parser in
        let trailing_attrs = parse_attributes parser in
        [ make_token parser colon ]
        @ tokens_to_green parser trivia_after_colon
        @ [ Ceibo.Green.Node body ]
        @ tokens_to_green parser trivia_after_body
        @ trailing_attrs
    | _ ->
        let found = peek parser in
        let diagnostic = Diagnostic.invalid_expression
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser) in
        [ Ceibo.Green.Node (make_error_node parser ~diagnostic ~consumed_tokens:[]) ]
  in
  make_node
    Syntax_kind.CLASS_DECL
    ([ make_token parser class_kw ]
    @ tokens_to_green parser trivia_after_class
    @ ext_nodes
    @ tokens_to_green parser trivia_after_ext
    @ attr_nodes
    @ type_params
    @ tokens_to_green parser trivia_after_params
    @ [ Ceibo.Green.Node class_name ]
    @ tokens_to_green parser trivia_after_name
    @ body_children)

(** Parse class type declaration: class type c = expr or class type c : typ *)
and parse_class_type_decl = fun parser ->
  let class_kw = consume parser in
  let trivia_after_class = consume_trivia parser in
  let type_kw = consume parser in
  let trivia_after_type = consume_trivia parser in
  let ext_nodes, trivia_after_ext, attr_nodes = parse_extension_and_attributes parser in
  let type_params, trivia_after_params = parse_class_type_params parser in
  let class_name = parse_ident_like parser in
  let trivia_after_name = consume_trivia parser in
  let body_children =
    match peek_kind parser with
    | Token.Eq ->
        let eq = consume parser in
        let trivia_after_eq = consume_trivia parser in
        let body = parse_class_type_expr parser in
        let trivia_after_body = consume_trivia parser in
        let trailing_attrs = parse_attributes parser in
        [ make_token parser eq ]
        @ tokens_to_green parser trivia_after_eq
        @ [ Ceibo.Green.Node body ]
        @ tokens_to_green parser trivia_after_body
        @ trailing_attrs
    | Token.Colon ->
        let colon = consume parser in
        let trivia_after_colon = consume_trivia parser in
        let body = parse_class_type_expr parser in
        let trivia_after_body = consume_trivia parser in
        let trailing_attrs = parse_attributes parser in
        [ make_token parser colon ]
        @ tokens_to_green parser trivia_after_colon
        @ [ Ceibo.Green.Node body ]
        @ tokens_to_green parser trivia_after_body
        @ trailing_attrs
    | _ ->
        let found = peek parser in
        let diagnostic = Diagnostic.invalid_expression
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser) in
        [ Ceibo.Green.Node (make_error_node parser ~diagnostic ~consumed_tokens:[]) ]
  in
  make_node
    Syntax_kind.CLASS_TYPE_DECL
    ([ make_token parser class_kw ]
    @ tokens_to_green parser trivia_after_class
    @ [ make_token parser type_kw ]
    @ tokens_to_green parser trivia_after_type
    @ ext_nodes
    @ tokens_to_green parser trivia_after_ext
    @ attr_nodes
    @ type_params
    @ tokens_to_green parser trivia_after_params
    @ [ Ceibo.Green.Node class_name ]
    @ tokens_to_green parser trivia_after_name
    @ body_children)

(** Parse open statement: `open M` / `open! M` / `open struct ... end` *)
and parse_open_stmt = fun ~signature parser ->
  (* Consume 'open' keyword *)
  let open_kw = consume parser in
  let trivia_after_open = consume_trivia parser in
  let ext_nodes, trivia_after_ext, attr_nodes = parse_extension_and_attributes parser in
  let bang_children, trivia_after_bang =
    match peek_kind parser with
    | Token.Bang ->
        let bang = consume parser in
        let trivia = consume_trivia parser in
        ([ make_token parser bang ], trivia)
    | _ -> ([], [])
  in
  (* Parse module path (e.g., List or Std.List) *)
  let rec parse_module_path () =
    match peek_kind parser with
    | Token.Ident _ ->
        let ident = consume parser in
        let trivia = consume_trivia parser in
        (* Check for dot (qualified path) *)
        if peek_kind parser = Token.Dot then
          let dot = consume parser in
          let trivia2 = consume_trivia parser in
          let rest = parse_module_path () in
          (make_token parser ident :: tokens_to_green parser trivia
          @ [ make_token parser dot ]
          @ tokens_to_green parser trivia2
          @ rest)
        else
          [ make_token parser ident ] @ tokens_to_green parser trivia
    | _ ->
        let found = peek parser in
        let diagnostic = Diagnostic.missing_module_path
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser) in
        report_diagnostic parser diagnostic;
        [ make_token parser found ]
  in
  let can_start_module_expression =
    match peek_kind parser with
    | Token.OpenDelim Token.Bracket when is_extension_start parser -> true
    | Token.OpenDelim Token.Brace when is_brace_extension_start parser -> true
    | Token.Keyword Keyword.Functor -> true
    | Token.OpenDelim Token.StructEnd
    | Token.OpenDelim Token.Paren -> true
    | Token.Ident name when ident_starts_uppercase name -> true
    | _ -> false
  in
  let opened_thing =
    if signature then
      parse_module_path ()
    else if can_start_module_expression then
      [ Ceibo.Green.Node (parse_module_expr parser) ]
    else
      parse_module_path ()
  in
  (* Build OPEN_STMT node *)
  make_node
    Syntax_kind.OPEN_STMT
    ([ make_token parser open_kw ]
    @ tokens_to_green parser trivia_after_open
    @ ext_nodes
    @ tokens_to_green parser trivia_after_ext
    @ attr_nodes
    @ bang_children
    @ tokens_to_green parser trivia_after_bang
    @ opened_thing)

(** Parse include statement: include Module or include Module.Submodule *)
and parse_include_stmt = fun ~signature parser ->
  (* Consume 'include' keyword *)
  let include_kw = consume parser in
  let trivia_after_include = consume_trivia parser in
  let ext_nodes, trivia_after_ext, leading_attr_nodes = parse_extension_and_attributes parser in
  (* Check if this is "module type of" (signature context) or module expression *)
  let included_thing =
    match peek_kind parser with
    | Token.OpenDelim Token.SigEnd -> parse_sig_expr parser
    | Token.Keyword Keyword.Module ->
        if starts_with_module_type_keyword parser then
          parse_module_type_expr parser
        else
          (* Parse as module expression: include M *)
          parse_module_expr parser
    | _ ->
        if signature then
          parse_module_type_expr parser
        else
          (* Default: parse as module expression *)
          (* This handles: include M, include M(X), include (M : S), etc. *)
          parse_module_expr parser
  in
  let trivia_after_included = consume_trivia parser in
  let attr_nodes = parse_attributes parser in
  (* Build INCLUDE_STMT node *)
  make_node
    Syntax_kind.INCLUDE_STMT
    ([ make_token parser include_kw ]
    @ tokens_to_green parser trivia_after_include
    @ ext_nodes
    @ tokens_to_green parser trivia_after_ext
    @ leading_attr_nodes
    @ [ Ceibo.Green.Node included_thing ]
    @ tokens_to_green parser trivia_after_included
    @ attr_nodes)

(** Parse structure item (top-level in .ml files) 
    @param in_block: true if we're already inside a struct/sig block *)
and parse_structure_item = fun ?(in_block = false) parser ->
  match peek_kind parser with
  | Token.Keyword Keyword.Val ->
      parse_val_decl parser
  | Token.Keyword Keyword.Let ->
      (* let open ... in and let module ... in are expression forms. *)
      let saved_pos0 = position parser in
      advance parser;
      let starts_let_expr =
        match peek_kind parser with
        | Token.Keyword Keyword.Open
        | Token.Keyword Keyword.Module
        | Token.Keyword Keyword.Exception -> true
        | _ -> false
      in
      Token_cursor.set_position parser.cursor saved_pos0;
      if starts_let_expr then
        parse_expr parser
      else
        (* Peek ahead to determine if this is a let-binding (structure item)
         or a let-in expression. We do this by saving position and parsing
         ahead to look for 'in' keyword *)
        let saved_pos = position parser in
        let is_let_in_expr = ref false in
        (* Try to scan ahead for 'in' keyword before EOF/next structure item *)
        let rec scan_for_in depth struct_depth delim_depth =
          match peek_kind parser with
          | Token.EOF ->
              ()
          | Token.Keyword Keyword.In when depth = 0 && struct_depth = 0 && delim_depth = 0 ->
              is_let_in_expr := true
          | Token.Keyword Keyword.Let ->
              advance parser;
              (* Only nested let...in pairs affect the in-scan depth. *)
              let new_depth =
                if struct_depth = 0 then
                  depth + 1
                else
                  depth
              in
              scan_for_in new_depth struct_depth delim_depth
          | Token.Keyword Keyword.In ->
              advance parser;
              scan_for_in (depth - 1) struct_depth delim_depth
          | Token.OpenDelim _ ->
              advance parser;
              scan_for_in depth struct_depth (delim_depth + 1)
          | Token.CloseDelim _ when delim_depth > 0 ->
              advance parser;
              scan_for_in depth struct_depth (delim_depth - 1)
          | Token.Keyword Keyword.Struct
          | Token.Keyword Keyword.Sig
          | Token.Keyword Keyword.Begin ->
              advance parser;
              scan_for_in depth (struct_depth + 1) delim_depth
          | Token.Keyword Keyword.End when struct_depth > 0 ->
              advance parser;
              scan_for_in depth (struct_depth - 1) delim_depth
          | Token.Keyword Keyword.End when struct_depth = 0 ->
              (* Hit 'end' at struct_depth 0 - stop searching, this is not a let-in expr *)
              ()
          | Token.Keyword Keyword.Type
          | Token.Keyword Keyword.Module
          | Token.Keyword Keyword.Exception
          | Token.Keyword Keyword.External
          | Token.Keyword Keyword.Open
          | Token.Keyword Keyword.Include when depth = 0 && struct_depth = 0 && delim_depth = 0 ->
              (* Hit next structure item without finding 'in' *)
              ()
          | _ ->
              advance parser;
              scan_for_in depth struct_depth delim_depth
        in
        advance parser;
        (* skip 'let' *)
        (* If we're already in a block, start with struct_depth = 1 to prevent 
           looking past the block's 'end' keyword *)
        let initial_struct_depth =
          if in_block then
            1
          else
            0
        in
        scan_for_in 0 initial_struct_depth 0;
        (* Restore position *)
        Token_cursor.set_position parser.cursor saved_pos;
        (* Parse as expression if we found 'in', otherwise as binding *)
        if !is_let_in_expr then
          parse_expr parser
        else
          parse_let_binding parser
  | Token.Keyword Keyword.Type ->
      parse_type_decl parser
  | Token.Keyword Keyword.Module -> (
      if starts_with_module_type_keyword parser then
        parse_module_type_decl parser
      else
        parse_module_decl parser
    )
  | Token.Keyword Keyword.Class ->
      if starts_with_class_type_keyword parser then
        parse_class_type_decl parser
      else
        parse_class_decl parser
  | Token.Keyword Keyword.External ->
      parse_external_decl parser
  | Token.Keyword Keyword.Exception ->
      parse_exception_decl parser
  | Token.Keyword Keyword.Open ->
      parse_open_stmt ~signature:false parser
  | Token.Keyword Keyword.Include ->
      parse_include_stmt ~signature:false parser
  | _ ->
      (* Try to parse as expression - top-level expressions are allowed in .ml files *)
      (* This includes let...in expressions, function applications, etc. *)
      if can_start_expr parser then
        parse_expr parser
      else
        (* Unknown token - consume it and report error to avoid infinite loop *)
        let tok = consume parser in
        let diagnostic = unexpected_top_level_item_diagnostic parser tok ~signature:false in
        make_error_node parser ~diagnostic ~consumed_tokens:[ tok ]

(** Parse signature item (top-level in .mli files) *)
(** Parse val declaration: val name : type *)
and parse_val_decl = fun parser ->
  (* Consume 'val' keyword *)
  let val_kw = consume parser in
  let trivia_after_val = consume_trivia parser in
  let ext_nodes, trivia_after_ext, attr_nodes = parse_extension_and_attributes parser in
  (* Parse value name - can be identifier OR operator in parens *)
  let value_name =
    match peek_kind parser with
    | Token.OpenDelim Token.Paren ->
        (* Could be operator name: val ( + ) : ... *)
        let open_paren = consume parser in
        let trivia_after_open = consume_trivia parser in
        (* Check if this is an operator (including keyword operators like 'mod') *)
        let is_operator =
          is_operator_token (peek_kind parser)
          || (
            match peek_kind parser with
            | Token.Ident _ ->
                let text = token_text parser (peek parser) in
                is_keyword_operator_name text
            | _ -> false
          )
        in
        if is_operator then
          parse_operator_pattern parser open_paren trivia_after_open
        else
          (
            (* Not an operator - this is an error *)
            let found = peek parser in
            let diagnostic = Diagnostic.invalid_expression
              ~found
              ~text:(token_text parser found)
              ~span:(current_span parser) in
            make_error_node parser ~diagnostic ~consumed_tokens:[ open_paren ]
          )
    | Token.Ident _ ->
        let ident = consume parser in
        make_node Syntax_kind.IDENT_EXPR [ make_token parser ident ]
    | _ ->
        let found = peek parser in
        let diagnostic = Diagnostic.invalid_expression
          ~found
          ~text:(token_text parser found)
          ~span:(current_span parser) in
        make_error_node parser ~diagnostic ~consumed_tokens:[]
  in
  let trivia_after_name = consume_trivia parser in
  (* Expect colon *)
  let colon =
    match peek_kind parser with
    | Token.Colon ->
        let tok = consume parser in
        make_token parser tok
    | _ ->
        let found = peek parser in
        let diagnostic = Diagnostic.invalid_expression
          ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser) in
        report_diagnostic parser diagnostic;
        let missing = make_error_node parser ~diagnostic ~consumed_tokens:[] in
        Ceibo.Green.Node missing
  in
  let trivia_after_colon = consume_trivia parser in
  (* Parse type expression *)
  let type_expr = parse_typexpr parser in
  (* Build VAL_DECL node *)
  make_node
    Syntax_kind.VAL_DECL
    ([ make_token parser val_kw ]
    @ tokens_to_green parser trivia_after_val
    @ ext_nodes
    @ tokens_to_green parser trivia_after_ext
    @ attr_nodes
    @ [ Ceibo.Green.Node value_name ]
    @ tokens_to_green parser trivia_after_name
    @ [ colon ]
    @ tokens_to_green parser trivia_after_colon
    @ [ Ceibo.Green.Node type_expr ])

(** Parse signature item (top-level in .mli files) *)
and parse_signature_item = fun parser ->
  match peek_kind parser with
  | Token.OpenDelim Token.Bracket when is_attribute_start parser ->
      parse_attribute parser
  | Token.OpenDelim Token.Bracket when is_extension_start parser ->
      parse_extension parser
  | Token.OpenDelim Token.Brace when is_brace_extension_start parser ->
      parse_extension parser
  | Token.Keyword Keyword.Val ->
      parse_val_decl parser
  | Token.Keyword Keyword.Type ->
      parse_type_decl parser
  | Token.Keyword Keyword.Module -> (
      if starts_with_module_type_keyword parser then
        parse_module_type_decl parser
      else
        parse_module_decl parser
    )
  | Token.Keyword Keyword.Class ->
      if starts_with_class_type_keyword parser then
        parse_class_type_decl parser
      else
        parse_class_decl parser
  | Token.Keyword Keyword.External ->
      parse_external_decl parser
  | Token.Keyword Keyword.Exception ->
      parse_exception_decl parser
  | Token.Keyword Keyword.Open ->
      parse_open_stmt ~signature:true parser
  | Token.Keyword Keyword.Include ->
      parse_include_stmt ~signature:true parser
  | Token.Keyword Keyword.And ->
      (* Parse 'and' in signature context - always for type declarations *)
      (* In .mli files, 'and' is only used for type ... and ... *)
      let and_kw = consume parser in
      let trivia_after_and = consume_trivia parser in
      let type_params, trivia_after_params = parse_type_params parser in
      let type_name =
        match peek_kind parser with
        | Token.Ident _ ->
            let ident = consume parser in
            make_node Syntax_kind.IDENT_EXPR [ make_token parser ident ]
        | _ ->
            let found_tok = peek parser in
            let diagnostic = Diagnostic.missing_type_name
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(current_span parser) in
            make_error_node parser ~diagnostic ~consumed_tokens:[]
      in
      let trivia_after_name = consume_trivia parser in
      let eq =
        if peek_kind parser = Token.Eq then
          consume parser
        else
          peek parser
      in
      let trivia_after_eq = consume_trivia parser in
      (* Parse type definition - handle variants, records, and type expressions *)
      let type_info_children =
        match peek_kind parser with
        | Token.Pipe ->
            (* Direct variant: type t = | A | B *)
            parse_variant_representation parser
        | Token.OpenDelim Token.Brace ->
            (* Direct record: type t = { ... } *)
            let record = parse_record_type parser in
            [ Ceibo.Green.Node record ]
        | Token.Ident _ when uppercase_ident_starts_variant_representation parser ->
            parse_variant_representation parser
        | Token.Ident name when ident_starts_uppercase name ->
            let type_expr = parse_typexpr parser in
            let trivia_after_expr = consume_trivia parser in
            [ Ceibo.Green.Node type_expr ] @ tokens_to_green parser trivia_after_expr
        | _ ->
            (* Parse type expression *)
            let type_expr = parse_typexpr parser in
            [ Ceibo.Green.Node type_expr ]
      in
      make_node
        Syntax_kind.TYPE_DECL
        ([ make_token parser and_kw ]
        @ tokens_to_green parser trivia_after_and
        @ type_params
        @ tokens_to_green parser trivia_after_params
        @ [ Ceibo.Green.Node type_name ]
        @ tokens_to_green parser trivia_after_name
        @ [ make_token parser eq ]
        @ tokens_to_green parser trivia_after_eq
        @ type_info_children)
  | _ ->
      (* Unknown token - consume it and report error to avoid infinite loop *)
      let tok = consume parser in
      let diagnostic = unexpected_top_level_item_diagnostic parser tok ~signature:true in
      make_error_node parser ~diagnostic ~consumed_tokens:[ tok ]

and parse = fun ~cst_kind ~parse_item ~source ~tokens ->
  let parser = create ~source tokens in
  let same_diagnostic left right =
    let left_found = Diagnostic.found_token left in
    let right_found = Diagnostic.found_token right in
    Diagnostic.id left = Diagnostic.id right
    && left.span.start = right.span.start
    && left.span.end_ = right.span.end_
    && left_found.kind = right_found.kind
    && left_found.text = right_found.text
  in
  let dedupe_diagnostics diagnostics =
    let rec loop = fun prev acc ->
      function
      | [] -> List.rev acc
      | diag :: rest -> (
          match prev with
          | Some previous when same_diagnostic previous diag -> loop prev acc rest
          | _ -> loop (Some diag) (diag :: acc) rest
        )
    in
    loop None [] diagnostics
  in
  (* Parse items until EOF *)
  let rec parse_items acc =
    yield ();
    if is_eof parser then
      List.rev acc
    else if peek_kind parser = Token.EOF then
      List.rev acc
    else if peek_kind parser = Token.Semi then
      let semi = consume parser in
      parse_items ([ make_token parser semi ] @ acc)
    else
      let item = parse_item parser in
      parse_items ([ Ceibo.Green.Node item ] @ acc)
  in
  let items = parse_items [] in
  (* Build SOURCE_FILE with ALL trivia preserved *)
  let children = items in
  let tree = make_node Syntax_kind.SOURCE_FILE children in
  let diagnostics = List.rev (Cell.get parser.diagnostics) |> dedupe_diagnostics in
  {
    source;
    tokens;
    kind = cst_kind;
    tree;
    diagnostics;
  }

(** Parse interface file (.mli) *)
let parse_interface = fun ~source tokens ->
  parse ~cst_kind:`Interface ~parse_item:parse_signature_item ~source ~tokens

(** Parse implementation file (.ml) *)
let parse_implementation = fun ~source tokens ->
  parse ~cst_kind:`Implementation ~parse_item:parse_structure_item ~source ~tokens

open Std
open Std.Collections

module Slice = IO.IoVec.IoSlice

module Event = struct
  module Buffer = Syntax_tree.Builder
end

type file_kind = [ | `Implementation | `Interface]

type parse_result = {
  source: Slice.t;
  kind: file_kind;
  tokens: Raw_token.stream;
  tree: Syntax_tree.t;
  diagnostics: Diagnostic.t Vector.t;
}

type parser = {
  source: Slice.t;
  token_stream: Raw_token.stream;
  raw_tokens: Raw_token.t Vector.t;
  significant_tokens: int Vector.t;
  raw_len: int;
  significant_len: int;
  events: Syntax_tree.Builder.t;
  mutable pos: int;
  mutable last_eof_unclosed_delimiter_offset: int option;
}

type checkpoint = {
  saved_pos: int;
  saved_last_eof_unclosed_delimiter_offset: int option;
  builder_checkpoint: Syntax_tree.Builder.checkpoint;
}

let create = fun source ->
  let token_stream =
    Lexer.tokenize source
    |> Raw_token.from_lexer_tokens ~source
  in
  let raw_tokens = token_stream.Raw_token.raw in
  let significant_tokens = token_stream.Raw_token.significant in
  let raw_len = Vector.length raw_tokens in
  let significant_len = Vector.length significant_tokens in
  let events =
    Syntax_tree.Builder.create
      ~source
      ~token_stream
      ~event_capacity:(Int.max 32 ((significant_len * 3) + (raw_len / 4)))
      ~diagnostic_capacity:8
      ()
  in
  {
    source;
    token_stream;
    raw_tokens;
    significant_tokens;
    raw_len;
    significant_len;
    events;
    pos = 0;
    last_eof_unclosed_delimiter_offset = None;
  }

let checkpoint = fun (p: parser) -> {
  saved_pos = p.pos;
  saved_last_eof_unclosed_delimiter_offset = p.last_eof_unclosed_delimiter_offset;
  builder_checkpoint = Syntax_tree.Builder.checkpoint p.events;
}

let restore = fun (p: parser) (saved: checkpoint) ->
  p.pos <- saved.saved_pos;
  p.last_eof_unclosed_delimiter_offset <- saved.saved_last_eof_unclosed_delimiter_offset;
  Syntax_tree.Builder.restore p.events saved.builder_checkpoint

let significant_count = fun p -> p.significant_len

let raw_count = fun p -> p.raw_len

let raw_at = fun p raw_index -> Vector.get_unchecked p.raw_tokens ~at:raw_index

let significant_raw_at = fun p index ->
  let count = p.significant_len in
  if count = 0 then
    0
  else if index < 0 then
    Vector.get_unchecked p.significant_tokens ~at:0
  else if index >= count then
    Vector.get_unchecked p.significant_tokens ~at:(count - 1)
  else
    Vector.get_unchecked p.significant_tokens ~at:index

let current_raw_index = fun p -> significant_raw_at p p.pos

let peek_raw_index = fun p offset -> significant_raw_at p (p.pos + offset)

let current = fun p -> raw_at p (current_raw_index p)

let peek = fun p offset -> raw_at p (peek_raw_index p offset)

let previous = fun p -> raw_at p (significant_raw_at p (p.pos - 1))

let eof = fun p -> raw_at p (significant_raw_at p (significant_count p - 1))

let current_kind = fun p -> (current p).Raw_token.kind

let peek_kind = fun p offset -> (peek p offset).Raw_token.kind

let kind_is = Syntax_kind.is

let at = fun p kind -> kind_is (current_kind p) kind

let peek_is = fun p offset kind -> kind_is (peek_kind p offset) kind

let is_eof = fun p -> at p Syntax_kind.EOF

let current_offset = fun p -> (current p).Raw_token.span.Span.start

let previous_end_offset = fun p -> (previous p).Raw_token.span.Span.end_

let current_is_tight_after_previous = fun p -> Int.equal (current_offset p) (previous_end_offset p)

let zero_span = fun offset -> Span.make ~start:offset ~end_:offset

let token_text = fun p raw -> Raw_token.text_slice ~source:p.source raw

let text_between = fun p ~start ~end_ ->
  let len = end_ - start in
  if len <= 0 then
    ""
  else
    Slice.sub_unchecked p.source ~off:start ~len
    |> Slice.to_string

let raw_char_is = fun p raw ~offset expected ->
  let index = raw.Raw_token.span.Span.start + offset in
  raw.Raw_token.span.Span.start >= 0
  && index < raw.Raw_token.span.Span.end_
  && index < Slice.length p.source && try Slice.get_unchecked p.source ~at:index = expected with
  | Invalid_argument _ -> false

let raw_starts_with = fun p raw expected -> raw_char_is p raw ~offset:0 expected

let raw_text_is = fun p raw expected ->
  let len = String.length expected in
  let start = raw.Raw_token.span.Span.start in
  let end_ = raw.Raw_token.span.Span.end_ in
  let width = Span.width raw.Raw_token.span in
  width = len && start >= 0 && end_ <= Slice.length p.source && try
    let rec loop index =
      if index >= len then
        true
      else if
        Slice.get_unchecked p.source ~at:(start + index) = String.get_unchecked expected ~at:index
      then
        loop (index + 1)
      else
        false
    in
    loop 0
  with
  | Invalid_argument _ -> false

let current_text_is = fun p expected -> raw_text_is p (current p) expected

let at_end_keyword = fun p -> at p Syntax_kind.END_KW || current_text_is p "end"

let legacy_token = fun raw -> {
  Token.kind = raw.Raw_token.legacy_kind;
  span = raw.Raw_token.span;
  leading_trivia = [];
}

let found_token = fun p raw: Diagnostic.found_token ->
  let token = legacy_token raw in
  { kind = Token.to_string token; text = token_text p raw }

let diagnostic_at_current = fun p kind ->
  let raw = current p in
  Diagnostic.make ~kind ~span:raw.Raw_token.span

let diagnostic_with_current = fun p make ->
  let raw = current p in
  make
    ~found:(legacy_token raw)
    ~text:(token_text p raw)
    ~span:raw.Raw_token.span

let diagnostic_with_current_at = fun p make span ->
  let raw = current p in
  make
    ~found:(legacy_token raw)
    ~text:(token_text p raw)
    ~span

let diagnostic_with_raw_at = fun p raw make span ->
  make
    ~found:(legacy_token raw)
    ~text:(token_text p raw)
    ~span

let diagnostic_with_eof_at = fun p make span -> diagnostic_with_raw_at p (eof p) make span

let diagnostic_with_raw_found_at = fun p raw make span ->
  make
    ~found:(legacy_token raw)
    ~text:(token_text p raw)
    ~span

let invalid_expression = fun p -> diagnostic_with_current p Diagnostic.invalid_expression

let invalid_pattern = fun p -> diagnostic_with_current p Diagnostic.invalid_pattern

let invalid_type_expression = fun p -> diagnostic_with_current p Diagnostic.invalid_type_expression

let missing_let_binding_pattern = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.missing_let_binding_pattern
    (zero_span (previous_end_offset p))

let invalid_pattern_at_previous_end = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.invalid_pattern
    (zero_span (previous_end_offset p))

let invalid_expression_at_previous_end = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.invalid_expression
    (zero_span (previous_end_offset p))

let missing_let_binding_equals = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.missing_let_binding_equals
    (zero_span (current_offset p))

let missing_let_binding_equals_at_eof = fun p ->
  diagnostic_with_eof_at
    p
    Diagnostic.missing_let_binding_equals
    (zero_span (previous_end_offset p))

let missing_let_binding_equals_eof_at_current_offset = fun p ->
  diagnostic_with_eof_at
    p
    Diagnostic.missing_let_binding_equals
    (zero_span (current_offset p))

let missing_let_binding_equals_at_previous_end = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.missing_let_binding_equals
    (zero_span (previous_end_offset p))

let missing_let_binding_expr = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.missing_let_binding_expr
    (zero_span (previous_end_offset p))

let missing_type_name = fun p -> diagnostic_with_current p Diagnostic.missing_type_name

let missing_type_name_at_current_offset = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.missing_type_name
    (zero_span (current_offset p))

let missing_type_decl_equals = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.missing_type_decl_equals
    (zero_span (previous_end_offset p))

let invalid_type_expression_at_previous_end = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.invalid_type_expression
    (zero_span (previous_end_offset p))

let bracketed_type_parameters = fun p ~type_name ->
  diagnostic_with_current
    p
    (Diagnostic.bracketed_type_parameters ~type_name)

let malformed_type_variable_at = fun p raw span ->
  diagnostic_with_raw_found_at
    p
    raw
    Diagnostic.malformed_type_variable
    span

let unclosed_type_params = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.unclosed_type_params
    (zero_span (current_offset p))

let unclosed_type_params_at_previous_end = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.unclosed_type_params
    (zero_span (previous_end_offset p))

let invalid_type_parameter_at = fun p raw span ->
  let text = token_text p raw in
  Diagnostic.invalid_type_parameter ~text ~found:(legacy_token raw) ~text_found:text ~span

let uppercase_type_variable_at = fun p ~quote ~ident ->
  let text = token_text p ident in
  Diagnostic.uppercase_type_variable
    ~text
    ~found:(legacy_token ident)
    ~text_found:text
    ~span:(Span.make ~start:quote.Raw_token.span.Span.start ~end_:ident.Raw_token.span.Span.end_)

let unclosed_delimiter = fun p ~opener ->
  diagnostic_with_current_at
    p
    (Diagnostic.unclosed_delimiter ~opener)
    (
      if is_eof p then
        zero_span (previous_end_offset p)
      else
        (current p).Raw_token.span
    )

let unexpected_closing_delimiter = fun p ~delimiter ->
  diagnostic_with_current
    p
    (Diagnostic.unexpected_closing_delimiter ~delimiter)

let missing_binary_operand = fun p ~operator ~side ->
  diagnostic_with_current_at
    p
    (Diagnostic.missing_binary_operand ~operator ~side)
    (zero_span (current_offset p))

let missing_binary_operand_after_operator = fun p ~operator ~side ->
  diagnostic_with_current_at
    p
    (Diagnostic.missing_binary_operand ~operator ~side)
    (zero_span (previous_end_offset p))

let consecutive_binary_operators = fun p ~operators ->
  diagnostic_with_current
    p
    (Diagnostic.consecutive_binary_operators ~operators)

let list_double_semicolon = fun p -> diagnostic_with_current p Diagnostic.list_double_semicolon

let if_missing_then = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.if_missing_then
    (zero_span (previous_end_offset p))

let match_missing_scrutinee = fun p -> diagnostic_with_current p Diagnostic.match_missing_scrutinee

let match_missing_scrutinee_at_previous_end = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.match_missing_scrutinee
    (zero_span (previous_end_offset p))

let match_missing_with = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.match_missing_with
    (zero_span (previous_end_offset p))

let match_missing_pattern = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.match_missing_pattern
    (zero_span (previous_end_offset p))

let match_guard_missing_expr = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.match_guard_missing_expr
    (zero_span (previous_end_offset p))

let tuple_pattern_extra_comma = fun p ->
  diagnostic_with_current
    p
    Diagnostic.tuple_pattern_extra_comma

let cons_pattern_missing_head = fun p ->
  diagnostic_with_current
    p
    Diagnostic.cons_pattern_missing_head

let cons_pattern_missing_tail = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.cons_pattern_missing_tail
    (zero_span (previous_end_offset p))

let or_pattern_missing = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.or_pattern_missing
    (zero_span (previous_end_offset p))

let or_pattern_double = fun p -> diagnostic_with_current p Diagnostic.or_pattern_double

let mutable_field_missing_name = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.mutable_field_missing_name
    (zero_span (previous_end_offset p))

let record_field_missing_colon = fun p ~field_name ->
  diagnostic_with_current_at
    p
    (Diagnostic.record_field_missing_colon ~field_name)
    (zero_span (previous_end_offset p))

let record_field_missing_type = fun p ~field_name ->
  diagnostic_with_current_at
    p
    (Diagnostic.record_field_missing_type ~field_name)
    (zero_span (previous_end_offset p))

let missing_module_decl_equals = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.missing_module_decl_equals
    (zero_span (previous_end_offset p))

let missing_external_colon = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.missing_external_colon
    (zero_span (previous_end_offset p))

let missing_exception_name = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.missing_exception_name
    (zero_span (previous_end_offset p))

let missing_module_path = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.missing_module_path
    (zero_span (previous_end_offset p))

let missing_module_type_name = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.missing_module_type_name
    (zero_span (previous_end_offset p))

let missing_module_type_expr = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.missing_module_type_expr
    (zero_span (previous_end_offset p))

let missing_module_expr = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.missing_module_expr
    (zero_span (previous_end_offset p))

let missing_with_keyword = fun p ->
  diagnostic_with_current_at
    p
    Diagnostic.missing_with_keyword
    (zero_span (current_offset p))

let invalid_module_name = fun p -> diagnostic_with_current p Diagnostic.invalid_module_name

let unexpected_signature_item = fun p ->
  diagnostic_with_current
    p
    Diagnostic.unexpected_signature_item

let empty_char_literal = fun p -> Diagnostic.empty_char_literal ~span:(current p).Raw_token.span

let unclosed_char_literal = fun p ->
  Diagnostic.unclosed_char_literal
    ~text:(token_text p (current p))
    ~span:(current p).Raw_token.span

let start_node = fun p -> Event.Buffer.start_node p.events

let complete = fun p marker kind -> Event.Buffer.complete p.events marker kind

let precede = fun p completed -> Event.Buffer.precede p.events completed

let bump = fun p ->
  if p.pos < significant_count p then (
    Event.Buffer.token p.events ~raw_index:(current_raw_index p);
    p.pos <- p.pos + 1
  )

let bump_if = fun p kind ->
  if at p kind then (
    bump p;
    true
  ) else
    false

let expect = fun p kind diagnostic ->
  if at p kind then
    bump p
  else (
    Event.Buffer.missing p.events ~kind ~offset:(current_offset p);
    Event.Buffer.error p.events diagnostic
  )

let expect_closer = fun p kind ~opener ->
  if Syntax_kind.(kind = END_KW) && at_end_keyword p || at p kind then
    bump p
  else (
    Event.Buffer.missing p.events ~kind ~offset:(current_offset p);
    let diagnostic = unclosed_delimiter p ~opener in
    if is_eof p then
      let offset = diagnostic.Diagnostic.span.Span.start in
      match p.last_eof_unclosed_delimiter_offset with
      | Some previous when previous = offset -> ()
      | _ ->
          p.last_eof_unclosed_delimiter_offset <- Some offset;
          Event.Buffer.error p.events diagnostic
    else
      Event.Buffer.error p.events diagnostic
  )

let recover_current_as_error = fun p diagnostic ->
  let marker = start_node p in
  Event.Buffer.error p.events diagnostic;
  if is_eof p then
    Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p)
  else
    bump p;
  ignore (complete p marker Syntax_kind.ERROR)

let ensure_progress = fun p before diagnostic ->
  if p.pos = before && not (is_eof p) then
    recover_current_as_error p diagnostic

let starts_structure_item = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.LET_KW
  | Syntax_kind.TYPE_KW
  | Syntax_kind.MODULE_KW
  | Syntax_kind.OPEN_KW
  | Syntax_kind.INCLUDE_KW
  | Syntax_kind.EXTERNAL_KW
  | Syntax_kind.EXCEPTION_KW -> true
  | _ -> false

let kind_at_position = fun p position -> (raw_at p (significant_raw_at p position)).Raw_token.kind

let peek_kind_at_position = fun p position offset -> kind_at_position p (position + offset)

let starts_with_typeof_module_expr_at = fun p position ->
  Syntax_kind.(kind_at_position p position = MODULE_KW
  && peek_kind_at_position p position 1 = TYPE_KW
  && peek_kind_at_position p position 2 = OF_KW)

let starts_with_module_type_decl_at = fun p position ->
  Syntax_kind.(kind_at_position p position = MODULE_KW
  && peek_kind_at_position p position 1 = TYPE_KW
  && peek_kind_at_position p position 2 != OF_KW)

let starts_structure_item_at_position = fun p position ->
  match kind_at_position p position with
  | Syntax_kind.MODULE_KW -> not (starts_with_typeof_module_expr_at p position)
  | kind -> starts_structure_item kind

let starts_signature_item = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.VAL_KW
  | Syntax_kind.TYPE_KW
  | Syntax_kind.MODULE_KW
  | Syntax_kind.OPEN_KW
  | Syntax_kind.INCLUDE_KW
  | Syntax_kind.EXTERNAL_KW
  | Syntax_kind.EXCEPTION_KW -> true
  | _ -> false

let starts_signature_item_at_position = fun p position ->
  match kind_at_position p position with
  | Syntax_kind.MODULE_KW ->
      not (starts_with_typeof_module_expr_at p position)
      && (starts_with_module_type_decl_at p position
      || Syntax_kind.(peek_kind_at_position p position 1 != TYPE_KW))
  | kind -> starts_signature_item kind

let starts_structure_item_at = fun p -> starts_structure_item_at_position p p.pos

let starts_signature_item_at = fun p ->
  starts_signature_item_at_position p p.pos
  || (at p Syntax_kind.LBRACKET
  && (Syntax_kind.(peek_kind p 1 = PERCENT)
  || Syntax_kind.(peek_kind p 1 = AT)
  || Syntax_kind.(peek_kind p 1 = ATAT)))

let raw_contains_newline = fun p raw_index -> Raw_token.has_newline (raw_at p raw_index)

let leading_trivia_contains_newline = fun p ->
  let current_raw = current_raw_index p in
  let previous_raw =
    if p.pos <= 0 then (
      (-1)
    ) else
      significant_raw_at p (p.pos - 1)
  in
  let rec loop index =
    if index >= current_raw || index >= raw_count p then
      false
    else if raw_contains_newline p index then
      true
    else
      loop (index + 1)
  in
  loop (previous_raw + 1)

let leading_trivia_contains_newline_at = fun p position ->
  let current_raw = significant_raw_at p position in
  let previous_raw =
    if position <= 0 then (
      (-1)
    ) else
      significant_raw_at p (position - 1)
  in
  let rec loop index =
    if index >= current_raw || index >= raw_count p then
      false
    else if raw_contains_newline p index then
      true
    else
      loop (index + 1)
  in
  loop (previous_raw + 1)

let leading_trivia_has_post_newline_indent = fun p ->
  let start = previous_end_offset p in
  let end_ = current_offset p in
  let rec loop index after_newline =
    if index >= end_ then
      false
    else
      let char = Slice.get_unchecked p.source ~at:index in
      if char = '\n' then
        loop (index + 1) true
      else if after_newline && (char = ' ' || char = '\t') then
        true
      else
        loop (index + 1) after_newline
  in
  if start < 0 || end_ > Slice.length p.source then
    false
  else
    loop start false

let at_item_boundary_at = fun p position ~signature ->
  let raw = raw_at p (significant_raw_at p position) in
  if Syntax_kind.(raw.Raw_token.kind = EOF) then
    true
  else if Syntax_kind.(raw.Raw_token.kind = END_KW) then
    true
  else if signature && starts_signature_item_at_position p position then
    true
  else if not (leading_trivia_contains_newline_at p position) then
    false
  else if signature then
    starts_signature_item_at_position p position
  else
    starts_structure_item_at_position p position

let has_eq_before_item_boundary = fun p ~signature ->
  let rec loop position =
    let raw = raw_at p (significant_raw_at p position) in
    match raw.Raw_token.kind with
    | Syntax_kind.EOF -> false
    | Syntax_kind.EQ -> true
    | _ when position > p.pos && at_item_boundary_at p position ~signature -> false
    | _ -> loop (position + 1)
  in
  loop p.pos

let at_item_boundary = fun p ~signature ->
  if is_eof p then
    true
  else if at_end_keyword p then
    true
  else if signature && starts_signature_item_at p then
    true
  else if not (leading_trivia_contains_newline p) then
    false
  else if signature then
    starts_signature_item_at p
    || (at p Syntax_kind.LBRACKET
    && (Syntax_kind.(peek_kind p 1 = PERCENT)
    || Syntax_kind.(peek_kind p 1 = AT)
    || Syntax_kind.(peek_kind p 1 = ATAT)))
  else
    starts_structure_item_at p
    || (at p Syntax_kind.LBRACKET
    && (Syntax_kind.(peek_kind p 1 = PERCENT)
    || Syntax_kind.(peek_kind p 1 = AT)
    || Syntax_kind.(peek_kind p 1 = ATAT)))

let expression_boundary = fun p ~stop_at_item ~stop_at_semi ~stop_at_comma ~signature ->
  match current_kind p with
  | Syntax_kind.EOF
  | Syntax_kind.AND_KW
  | Syntax_kind.IN_KW
  | Syntax_kind.THEN_KW
  | Syntax_kind.ELSE_KW
  | Syntax_kind.WITH_KW
  | Syntax_kind.WHEN_KW
  | Syntax_kind.ARROW
  | Syntax_kind.PIPE
  | Syntax_kind.RPAREN
  | Syntax_kind.RBRACKET
  | Syntax_kind.RBRACE
  | Syntax_kind.BAR_RBRACKET
  | Syntax_kind.END_KW
  | Syntax_kind.DONE_KW -> true
  | Syntax_kind.COMMA -> stop_at_comma
  | Syntax_kind.SEMI -> stop_at_semi || (stop_at_item && Syntax_kind.(peek_kind p 1 = SEMI))
  | _ -> stop_at_item && at_item_boundary p ~signature

let trailing_sequence_boundary = fun p ->
  match current_kind p with
  | Syntax_kind.EOF
  | Syntax_kind.AND_KW
  | Syntax_kind.IN_KW
  | Syntax_kind.PIPE
  | Syntax_kind.RPAREN
  | Syntax_kind.RBRACKET
  | Syntax_kind.RBRACE
  | Syntax_kind.BAR_RBRACKET
  | Syntax_kind.END_KW
  | Syntax_kind.DONE_KW -> true
  | _ -> false

let can_start_atom = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.IDENT
  | Syntax_kind.INT
  | Syntax_kind.FLOAT
  | Syntax_kind.STRING
  | Syntax_kind.CHAR
  | Syntax_kind.TRUE_KW
  | Syntax_kind.FALSE_KW
  | Syntax_kind.LPAREN
  | Syntax_kind.BEGIN_KW
  | Syntax_kind.LBRACKET
  | Syntax_kind.LBRACKET_BAR
  | Syntax_kind.LBRACE
  | Syntax_kind.LET_KW
  | Syntax_kind.IF_KW
  | Syntax_kind.MATCH_KW
  | Syntax_kind.FUN_KW
  | Syntax_kind.FUNCTION_KW
  | Syntax_kind.TRY_KW
  | Syntax_kind.WHILE_KW
  | Syntax_kind.FOR_KW
  | Syntax_kind.BACKTICK
  | Syntax_kind.TILDE
  | Syntax_kind.QUESTION
  | Syntax_kind.PLUS
  | Syntax_kind.MINUS
  | Syntax_kind.PLUSDOT
  | Syntax_kind.MINUSDOT
  | Syntax_kind.BANG -> true
  | _ -> false

let missing_binding_expr_boundary = fun p ~signature ~top_level ->
  let let_expr_has_in_before_item_boundary () =
    let next_depth depth kind =
      match kind with
      | Syntax_kind.LPAREN
      | Syntax_kind.LBRACE
      | Syntax_kind.LBRACKET
      | Syntax_kind.LBRACKET_BAR
      | Syntax_kind.BEGIN_KW
      | Syntax_kind.STRUCT_KW
      | Syntax_kind.SIG_KW -> depth + 1
      | Syntax_kind.RPAREN
      | Syntax_kind.RBRACE
      | Syntax_kind.RBRACKET
      | Syntax_kind.BAR_RBRACKET
      | Syntax_kind.END_KW when depth > 0 -> depth - 1
      | _ -> depth
    in
    let rec loop position depth =
      let kind = kind_at_position p position in
      if Syntax_kind.(kind = EOF) then
        false
      else if
        Int.equal depth 0 && position > p.pos && at_item_boundary_at p position ~signature
      then
        false
      else if Int.equal depth 0 && Syntax_kind.(kind = IN_KW) then
        true
      else
        loop (position + 1) (next_depth depth kind)
    in
    Syntax_kind.(current_kind p = LET_KW) && loop p.pos 0
  in
  is_eof p
  || (top_level
  && at_item_boundary p ~signature
  && not
    (can_start_atom (current_kind p)
    && (leading_trivia_has_post_newline_indent p || let_expr_has_in_before_item_boundary ())))

let can_start_pattern_atom = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.IDENT
  | Syntax_kind.UNDERSCORE
  | Syntax_kind.INT
  | Syntax_kind.FLOAT
  | Syntax_kind.STRING
  | Syntax_kind.CHAR
  | Syntax_kind.TRUE_KW
  | Syntax_kind.FALSE_KW
  | Syntax_kind.LPAREN
  | Syntax_kind.LBRACKET
  | Syntax_kind.LBRACKET_BAR
  | Syntax_kind.LBRACE
  | Syntax_kind.BACKTICK
  | Syntax_kind.HASH
  | Syntax_kind.TILDE
  | Syntax_kind.QUESTION
  | Syntax_kind.PLUS
  | Syntax_kind.MINUS
  | Syntax_kind.PLUSDOT
  | Syntax_kind.MINUSDOT
  | Syntax_kind.EXCEPTION_KW -> true
  | _ -> false

let sign_token = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.PLUS
  | Syntax_kind.MINUS
  | Syntax_kind.PLUSDOT
  | Syntax_kind.MINUSDOT -> true
  | _ -> false

let literal_after_sign = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.INT
  | Syntax_kind.FLOAT -> true
  | _ -> false

let prefix_operator = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.PLUS
  | Syntax_kind.MINUS
  | Syntax_kind.PLUSDOT
  | Syntax_kind.MINUSDOT
  | Syntax_kind.BANG -> true
  | _ -> false

let closing_delimiter_text = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.RPAREN -> Some ")"
  | Syntax_kind.RBRACKET -> Some "]"
  | Syntax_kind.RBRACE -> Some "}"
  | Syntax_kind.BAR_RBRACKET -> Some "|]"
  | Syntax_kind.END_KW -> Some "end"
  | _ -> None

let closing_punctuation = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.RPAREN
  | Syntax_kind.RBRACKET
  | Syntax_kind.RBRACE
  | Syntax_kind.BAR_RBRACKET -> true
  | _ -> false

let operator_pattern_token = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.PLUS
  | Syntax_kind.MINUS
  | Syntax_kind.STAR
  | Syntax_kind.SLASH
  | Syntax_kind.PERCENT
  | Syntax_kind.CARET
  | Syntax_kind.EQ
  | Syntax_kind.LT
  | Syntax_kind.GT
  | Syntax_kind.LTE
  | Syntax_kind.GTE
  | Syntax_kind.NE
  | Syntax_kind.BANG
  | Syntax_kind.AMPAMP
  | Syntax_kind.BARBAR
  | Syntax_kind.PIPE
  | Syntax_kind.AMPERSAND
  | Syntax_kind.AT
  | Syntax_kind.HASH
  | Syntax_kind.TILDE
  | Syntax_kind.QUESTION
  | Syntax_kind.DOLLAR
  | Syntax_kind.COLONCOLON
  | Syntax_kind.COLONEQ
  | Syntax_kind.ARROW
  | Syntax_kind.LEFT_ARROW
  | Syntax_kind.STARSTAR
  | Syntax_kind.EQEQ
  | Syntax_kind.BANGEQ
  | Syntax_kind.ATAT
  | Syntax_kind.PIPEGT
  | Syntax_kind.PERCENTGT
  | Syntax_kind.LTPERCENT
  | Syntax_kind.PLUSDOT
  | Syntax_kind.MINUSDOT
  | Syntax_kind.STARDOT
  | Syntax_kind.SLASHDOT -> true
  | _ -> false

let operator_text = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.PLUS -> "+"
  | Syntax_kind.MINUS -> "-"
  | Syntax_kind.STAR -> "*"
  | Syntax_kind.SLASH -> "/"
  | Syntax_kind.PERCENT -> "%"
  | Syntax_kind.CARET -> "^"
  | Syntax_kind.EQ -> "="
  | Syntax_kind.LT -> "<"
  | Syntax_kind.GT -> ">"
  | Syntax_kind.LTE -> "<="
  | Syntax_kind.GTE -> ">="
  | Syntax_kind.NE -> "<>"
  | Syntax_kind.BANG -> "!"
  | Syntax_kind.AMPAMP -> "&&"
  | Syntax_kind.BARBAR -> "||"
  | Syntax_kind.PIPE -> "|"
  | Syntax_kind.AMPERSAND -> "&"
  | Syntax_kind.AT -> "@"
  | Syntax_kind.HASH -> "#"
  | Syntax_kind.TILDE -> "~"
  | Syntax_kind.QUESTION -> "?"
  | Syntax_kind.DOLLAR -> "$"
  | Syntax_kind.COLONCOLON -> "::"
  | Syntax_kind.COLONEQ -> ":="
  | Syntax_kind.ARROW -> "->"
  | Syntax_kind.LEFT_ARROW -> "<-"
  | Syntax_kind.STARSTAR -> "**"
  | Syntax_kind.EQEQ -> "=="
  | Syntax_kind.BANGEQ -> "!="
  | Syntax_kind.ATAT -> "@@"
  | Syntax_kind.PIPEGT -> "|>"
  | Syntax_kind.PERCENTGT -> "%>"
  | Syntax_kind.LTPERCENT -> "<%"
  | Syntax_kind.PLUSDOT -> "+."
  | Syntax_kind.MINUSDOT -> "-."
  | Syntax_kind.STARDOT -> "*."
  | Syntax_kind.SLASHDOT -> "/."
  | kind -> Syntax_kind.to_string kind

let application_binding_power = 60

let attribute_suffix_binding_power = 5

let symbolic_operator_part = fun __tmp1 ->
  match __tmp1 with
  | kind when operator_pattern_token kind -> true
  | _ -> false

let can_start_poly_variant_payload = fun p ->
  match current_kind p with
  | Syntax_kind.TILDE
  | Syntax_kind.QUESTION -> symbolic_operator_part (peek_kind p 1)
  | kind -> can_start_atom kind

let starts_with_typeof_module_expr_keyword = fun p -> starts_with_typeof_module_expr_at p p.pos

let starts_with_module_type_decl_keyword = fun p -> starts_with_module_type_decl_at p p.pos

let identifier_text_starts_uppercase = fun text ->
  String.length text > 0 && match String.get text ~at:0 with
  | Some 'A' .. 'Z' -> true
  | _ -> false

let previous_ident_starts_uppercase = fun p ->
  let raw = previous p in
  Syntax_kind.(raw.Raw_token.kind = IDENT) && identifier_text_starts_uppercase (token_text p raw)

let ident_at_starts_uppercase = fun p offset ->
  Syntax_kind.(peek_kind p offset = IDENT)
  && identifier_text_starts_uppercase (token_text p (peek p offset))

let parenthesized_operator_start = fun p ->
  at p Syntax_kind.DOT
  || (operator_pattern_token (current_kind p)
  && (Syntax_kind.(peek_kind p 1 = RPAREN) || symbolic_operator_part (peek_kind p 1)))

let index_opener = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.LPAREN
  | Syntax_kind.LBRACKET
  | Syntax_kind.LBRACE -> true
  | _ -> false

let symbolic_sequence_followed_by_index_opener = fun p offset ->
  let rec loop offset consumed =
    let kind = peek_kind p offset in
    if symbolic_operator_part kind then
      loop (offset + 1) true
    else
      consumed && index_opener kind
  in
  loop offset false

let symbolic_sequence_followed_by_closer = fun p offset closer ->
  let rec loop offset consumed =
    let kind = peek_kind p offset in
    if symbolic_operator_part kind then
      loop (offset + 1) true
    else
      consumed && Syntax_kind.(kind = closer)
  in
  loop offset false

let binding_operator_suffix = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.STAR
  | Syntax_kind.PLUS -> true
  | _ -> false

let binding_operator_keyword = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.LET_KW
  | Syntax_kind.AND_KW -> true
  | _ -> false

let infix_binding_power = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.BARBAR -> Some 10
  | Syntax_kind.AMPAMP -> Some 15
  | Syntax_kind.EQ
  | Syntax_kind.EQEQ
  | Syntax_kind.BANGEQ
  | Syntax_kind.NE
  | Syntax_kind.LT
  | Syntax_kind.GT
  | Syntax_kind.LTE
  | Syntax_kind.GTE
  | Syntax_kind.LTPERCENT -> Some 20
  | Syntax_kind.COLONCOLON
  | Syntax_kind.AT
  | Syntax_kind.ATAT
  | Syntax_kind.PIPEGT -> Some 30
  | Syntax_kind.DOLLAR -> Some 30
  | Syntax_kind.AMPERSAND -> Some 35
  | Syntax_kind.PLUS
  | Syntax_kind.MINUS
  | Syntax_kind.CARET -> Some 40
  | Syntax_kind.STAR
  | Syntax_kind.SLASH
  | Syntax_kind.PERCENT
  | Syntax_kind.PLUSDOT
  | Syntax_kind.MINUSDOT
  | Syntax_kind.STARDOT
  | Syntax_kind.SLASHDOT
  | Syntax_kind.PERCENTGT
  | Syntax_kind.OPERATOR_KW -> Some 50
  | Syntax_kind.STARSTAR -> Some 60
  | _ -> None

let infix_operator_keyword_binding_power = fun p ->
  if
    current_text_is p "mod"
    || current_text_is p "land"
    || current_text_is p "lor"
    || current_text_is p "lxor"
    || current_text_is p "lsl"
    || current_text_is p "lsr"
    || current_text_is p "asr"
  then
    Some 50
  else
    None

let operator_keyword_at = fun p offset ->
  let raw = peek p offset in
  Syntax_kind.(raw.Raw_token.kind = IDENT)
  && (raw_text_is p raw "mod"
  || raw_text_is p raw "land"
  || raw_text_is p raw "lor"
  || raw_text_is p raw "lxor"
  || raw_text_is p raw "lsl"
  || raw_text_is p raw "lsr"
  || raw_text_is p raw "asr")

let current_infix_binding_power = fun p ->
  match infix_binding_power (current_kind p) with
  | Some _ as bp -> bp
  | None ->
      if at p Syntax_kind.IDENT then
        infix_operator_keyword_binding_power p
      else
        None

let operator_name_start_at = fun p offset ->
  let kind = peek_kind p offset in
  Syntax_kind.(kind = DOT || kind = OPERATOR_KW)
  || symbolic_operator_part kind
  || operator_keyword_at p offset
  || (binding_operator_keyword kind && binding_operator_suffix (peek_kind p (offset + 1)))

let pattern_binding_power = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.PIPE -> Some 10
  | Syntax_kind.COMMA -> Some 15
  | Syntax_kind.AS_KW -> Some 20
  | Syntax_kind.DOTDOT -> Some 25
  | Syntax_kind.COLONCOLON -> Some 30
  | Syntax_kind.COLON -> Some 40
  | _ -> None

let rec consume_path_segments = fun p ->
  if at p Syntax_kind.DOT && Syntax_kind.(peek_kind p 1 = IDENT) then (
    bump p;
    bump p;
    consume_path_segments p
  )

let consume_expr_path_segments = fun p ->
  let rec loop can_consume_value_segment =
    if at p Syntax_kind.DOT && Syntax_kind.(peek_kind p 1 = IDENT) then
      if can_consume_value_segment && ident_at_starts_uppercase p 1 then (
        bump p;
        bump p;
        loop true
      ) else if can_consume_value_segment then (
        bump p;
        bump p
      )
  in
  loop (previous_ident_starts_uppercase p)

let rec consume_balanced_until = fun p ~closer depth ->
  let at_closer = Syntax_kind.(closer = END_KW) && at_end_keyword p || at p closer in
  if not (is_eof p || (depth = 0 && at_closer)) then (
    let depth =
      match current_kind p with
      | Syntax_kind.LPAREN
      | Syntax_kind.LBRACE
      | Syntax_kind.LBRACKET
      | Syntax_kind.LBRACKET_BAR
      | Syntax_kind.BEGIN_KW
      | Syntax_kind.STRUCT_KW
      | Syntax_kind.SIG_KW -> depth + 1
      | Syntax_kind.RPAREN
      | Syntax_kind.RBRACE
      | Syntax_kind.RBRACKET
      | Syntax_kind.BAR_RBRACKET
      | Syntax_kind.END_KW when depth > 0 -> depth - 1
      | _ -> depth
    in
    bump p;
    consume_balanced_until p ~closer depth
  )

let rec consume_angle_balanced_until_gt = fun p depth ->
  if not (is_eof p || (Int.equal depth 0 && at p Syntax_kind.GT)) then (
    let depth =
      match current_kind p with
      | Syntax_kind.LPAREN
      | Syntax_kind.LBRACE
      | Syntax_kind.LBRACKET
      | Syntax_kind.LBRACKET_BAR
      | Syntax_kind.BEGIN_KW
      | Syntax_kind.STRUCT_KW
      | Syntax_kind.SIG_KW
      | Syntax_kind.LT -> Int.add depth 1
      | Syntax_kind.RPAREN
      | Syntax_kind.RBRACE
      | Syntax_kind.RBRACKET
      | Syntax_kind.BAR_RBRACKET
      | Syntax_kind.END_KW
      | Syntax_kind.GT when Int.(depth > 0) -> Int.sub depth 1
      | _ -> depth
    in
    bump p;
    consume_angle_balanced_until_gt p depth
  )

let is_attribute_suffix = fun p ->
  at p Syntax_kind.LBRACKET
  && (Syntax_kind.(peek_kind p 1 = AT) || Syntax_kind.(peek_kind p 1 = ATAT))

let is_extension_shell = fun p -> at p Syntax_kind.LBRACKET && Syntax_kind.(peek_kind p 1 = PERCENT)

let is_attribute_shell = fun p ->
  at p Syntax_kind.LBRACKET
  && (Syntax_kind.(peek_kind p 1 = AT) || Syntax_kind.(peek_kind p 1 = ATAT))

let rec consume_attribute_sigils = fun p ->
  if at p Syntax_kind.AT || at p Syntax_kind.ATAT then (
    bump p;
    consume_attribute_sigils p
  )

let rec consume_extension_sigils = fun p ->
  if at p Syntax_kind.PERCENT then (
    bump p;
    consume_extension_sigils p
  )

let consume_attribute_suffix = fun p ->
  bump p;
  consume_attribute_sigils p;
  consume_balanced_until p ~closer:Syntax_kind.RBRACKET 0;
  expect p Syntax_kind.RBRACKET (invalid_expression p)

let rec consume_same_line_attribute_suffixes = fun p ->
  if is_attribute_suffix p && not (leading_trivia_contains_newline p) then (
    consume_attribute_suffix p;
    consume_same_line_attribute_suffixes p
  )

let rec consume_until = fun p closer ->
  if not (is_eof p || at p closer) then (
    bump p;
    consume_until p closer
  )

let consume_extension_payload = fun p ->
  if at p Syntax_kind.LBRACE && Syntax_kind.(peek_kind p 1 = DOT) then
    consume_until p Syntax_kind.RBRACKET
  else
    consume_balanced_until p ~closer:Syntax_kind.RBRACKET 0

let consume_shortcut_extension_modifier = fun p ->
  if at p Syntax_kind.PERCENT then (
    bump p;
    if at p Syntax_kind.IDENT then
      bump p;
    let rec loop () =
      if at p Syntax_kind.DOT && Syntax_kind.(peek_kind p 1 = IDENT) then (
        bump p;
        bump p;
        loop ()
      )
    in
    loop ()
  )

let rec consume_declaration_attributes = fun p ->
  if is_attribute_shell p then (
    bump p;
    consume_attribute_sigils p;
    consume_balanced_until p ~closer:Syntax_kind.RBRACKET 0;
    expect p Syntax_kind.RBRACKET (invalid_expression p);
    consume_declaration_attributes p
  )

let rec consume_symbolic_operator = fun p ->
  if symbolic_operator_part (current_kind p) then (
    bump p;
    consume_symbolic_operator p
  )

let consume_parenthesized_operator_name = fun p ~signature ->
  if at p Syntax_kind.LPAREN && operator_name_start_at p 1 then (
    bump p;
    let next_depth depth =
      match current_kind p with
      | Syntax_kind.LPAREN
      | Syntax_kind.LBRACE
      | Syntax_kind.LBRACKET
      | Syntax_kind.LBRACKET_BAR -> depth + 1
      | Syntax_kind.RPAREN
      | Syntax_kind.RBRACE
      | Syntax_kind.RBRACKET
      | Syntax_kind.BAR_RBRACKET when depth > 0 -> depth - 1
      | _ -> depth
    in
    let rec loop depth =
      if
        not
          (is_eof p
          || (depth = 0
          && (at p Syntax_kind.RPAREN || at p Syntax_kind.COLON || at_item_boundary p ~signature)))
      then (
        let depth = next_depth depth in
        bump p;
        loop depth
      )
    in
    loop 0;
    expect_closer p Syntax_kind.RPAREN ~opener:"(";
    true
  ) else
    false

let consume_value_name = fun p ~signature ->
  if at p Syntax_kind.IDENT then (
    bump p;
    true
  ) else
    consume_parenthesized_operator_name p ~signature

let consume_first_class_module_shell = fun p ->
  expect p Syntax_kind.MODULE_KW (missing_module_expr p);
  if at p Syntax_kind.RPAREN then
    Event.Buffer.error p.events (missing_module_expr p)
  else if at p Syntax_kind.IDENT then (
    let text = token_text p (current p) in
    if (not (String.equal text "_")) && not (identifier_text_starts_uppercase text) then
      Event.Buffer.error p.events (invalid_module_name p);
    bump p;
    consume_path_segments p
  ) else if
    not (at p Syntax_kind.COLON || at p Syntax_kind.WITH_KW || at p Syntax_kind.TYPE_KW || is_eof p)
  then
    bump p;
  let parse_type_constraint () =
    expect p Syntax_kind.TYPE_KW (missing_type_name p);
    (
      if at p Syntax_kind.IDENT then
        bump p
      else
        Event.Buffer.error p.events (missing_type_name_at_current_offset p)
    );
    if at p Syntax_kind.EQ then (
      bump p;
      consume_balanced_until p ~closer:Syntax_kind.RPAREN 0
    ) else
      Event.Buffer.error p.events (missing_type_decl_equals p)
  in
  if at p Syntax_kind.COLON then (
    bump p;
    consume_balanced_until p ~closer:Syntax_kind.RPAREN 0
  ) else if at p Syntax_kind.TYPE_KW then (
    Event.Buffer.error p.events (missing_with_keyword p);
    parse_type_constraint ()
  ) else if at p Syntax_kind.WITH_KW then (
    bump p;
    parse_type_constraint ()
  ) else
    consume_balanced_until p ~closer:Syntax_kind.RPAREN 0;
  expect_closer p Syntax_kind.RPAREN ~opener:"("

let rec parse_expression = fun
  p ~signature ~stop_at_item ?(stop_at_semi = false) ?(stop_at_comma = false) min_bp ->
  let rec loop lhs =
    if expression_boundary p ~stop_at_item ~stop_at_semi ~stop_at_comma ~signature then
      lhs
    else if at p Syntax_kind.SEMI && not stop_at_semi && min_bp <= 0 then (
      let marker = precede p lhs in
      bump p;
      if not (trailing_sequence_boundary p) then
        ignore (parse_expression p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma 0);
      loop (complete p marker Syntax_kind.SEQUENCE_EXPR)
    ) else if at p Syntax_kind.COMMA && min_bp <= 15 then (
      let marker = precede p lhs in
      bump p;
      ignore (parse_expression p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma 15);
      loop (complete p marker Syntax_kind.TUPLE_EXPR)
    ) else if at p Syntax_kind.DOT then (
      match peek_kind p 1 with
      | Syntax_kind.LPAREN ->
          if previous_ident_starts_uppercase p then
            loop
              (parse_delimited_local_open_expr
                p
                lhs
                ~signature
                ~opener:Syntax_kind.LPAREN
                ~closer:Syntax_kind.RPAREN)
          else
            (
              let marker = precede p lhs in
              bump p;
              bump p;
              if not (at p Syntax_kind.RPAREN || is_eof p) then
                ignore (parse_expression p ~signature ~stop_at_item:false 0);
              expect p Syntax_kind.RPAREN (invalid_expression p);
              loop (complete p marker Syntax_kind.ARRAY_INDEX_EXPR)
            )
      | Syntax_kind.LBRACKET ->
          if previous_ident_starts_uppercase p then
            loop (parse_delimited_local_open_list_expr p lhs ~signature)
          else
            (
              let marker = precede p lhs in
              bump p;
              bump p;
              if not (at p Syntax_kind.RBRACKET || is_eof p) then
                ignore (parse_expression p ~signature ~stop_at_item:false 0);
              expect p Syntax_kind.RBRACKET (invalid_expression p);
              loop (complete p marker Syntax_kind.STRING_INDEX_EXPR)
            )
      | Syntax_kind.LBRACKET_BAR -> loop (parse_delimited_local_open_array_expr p lhs ~signature)
      | Syntax_kind.LBRACE -> loop (parse_delimited_local_open_record_expr p lhs ~signature)
      | Syntax_kind.IDENT ->
          let marker = precede p lhs in
          bump p;
          let field_starts_uppercase = ident_at_starts_uppercase p 0 in
          expect p Syntax_kind.IDENT (invalid_expression p);
          if field_starts_uppercase then
            consume_path_segments p;
          loop (complete p marker Syntax_kind.FIELD_ACCESS_EXPR)
      | Syntax_kind.BANG ->
          loop (parse_dot_bang_expr p lhs ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma)
      | kind when symbolic_operator_part kind && symbolic_sequence_followed_by_index_opener p 1 ->
          loop (parse_extended_index_expr p lhs ~signature ~stop_at_item)
      | _ -> lhs
    ) else if at p Syntax_kind.HASH then
      if min_bp <= 50 then (
        let marker = precede p lhs in
        consume_symbolic_operator p;
        let _rhs = parse_expression p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma 51 in
        loop (complete p marker Syntax_kind.INFIX_EXPR)
      ) else
        lhs
    else if (at p Syntax_kind.LEFT_ARROW || at p Syntax_kind.COLONEQ) && min_bp <= 5 then (
      let marker = precede p lhs in
      let operator = operator_text (current_kind p) in
      bump p;
      if expression_boundary p ~stop_at_item ~stop_at_semi ~stop_at_comma ~signature then (
        Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p);
        Event.Buffer.error
          p.events
          (missing_binary_operand_after_operator p ~operator ~side:"right")
      ) else
        ignore (parse_expression p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma 6);
      loop (complete p marker Syntax_kind.ASSIGN_EXPR)
    ) else if at p Syntax_kind.COLON && min_bp <= 5 then (
      let marker = precede p lhs in
      bump p;
      parse_type_expr p ~allow_leading_poly_type_after_newline:true ~stop_at_arrow:false;
      loop (complete p marker Syntax_kind.TYPED_EXPR)
    ) else
      match current_infix_binding_power p with
      | Some bp when bp >= min_bp ->
          let marker = precede p lhs in
          let operator = token_text p (current p) in
          bump p;
          if expression_boundary p ~stop_at_item ~stop_at_semi ~stop_at_comma ~signature then (
            Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p);
            Event.Buffer.error
              p.events
              (missing_binary_operand_after_operator p ~operator ~side:"right")
          ) else if
            Option.is_some (current_infix_binding_power p) && not (prefix_operator (current_kind p))
          then
            Event.Buffer.error
              p.events
              (consecutive_binary_operators p ~operators:(operator ^ " " ^ token_text p (current p)))
          else
            ignore
              (parse_expression p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma (bp + 1));
          loop (complete p marker Syntax_kind.INFIX_EXPR)
      | Some _ -> lhs
      | _ when is_attribute_suffix p && min_bp <= attribute_suffix_binding_power ->
          let marker = precede p lhs in
          consume_attribute_suffix p;
          loop (complete p marker Syntax_kind.ATTRIBUTE_EXPR)
      | _ when not (is_attribute_suffix p)
      && can_start_atom (current_kind p)
      && min_bp <= application_binding_power ->
          let marker = precede p lhs in
          let _argument =
            parse_expression
              p
              ~signature
              ~stop_at_item
              ~stop_at_semi
              ~stop_at_comma
              (application_binding_power + 1)
          in
          loop (complete p marker Syntax_kind.APPLY_EXPR)
      | _ -> lhs
  in
  loop (parse_prefix_or_atom p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma)

and parse_prefix_or_atom = fun p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma ->
  match current_kind p with
  | kind when prefix_operator kind ->
      if symbolic_operator_part (peek_kind p 1) then
        parse_symbolic_prefix_expr p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma
      else
        (
          let marker = start_node p in
          bump p;
          let _operand =
            parse_expression p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma 70
          in
          complete p marker Syntax_kind.PREFIX_EXPR
        )
  | Syntax_kind.LET_KW -> parse_let_expr p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma
  | Syntax_kind.DOT -> parse_unreachable_expr p
  | Syntax_kind.BACKTICK ->
      parse_poly_variant_expr p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma
  | Syntax_kind.TILDE ->
      if symbolic_operator_part (peek_kind p 1) then
        parse_symbolic_prefix_expr p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma
      else
        parse_label_arg_expr
          p
          ~signature
          ~stop_at_item
          ~stop_at_semi
          ~stop_at_comma
          Syntax_kind.LABELED_ARG
  | Syntax_kind.QUESTION ->
      if symbolic_operator_part (peek_kind p 1) then
        parse_symbolic_prefix_expr p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma
      else
        parse_label_arg_expr
          p
          ~signature
          ~stop_at_item
          ~stop_at_semi
          ~stop_at_comma
          Syntax_kind.OPTIONAL_ARG
  | Syntax_kind.IF_KW -> parse_if_expr p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma
  | Syntax_kind.MATCH_KW -> parse_match_expr p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma
  | Syntax_kind.FUN_KW -> parse_fun_expr p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma
  | Syntax_kind.FUNCTION_KW ->
      parse_function_expr p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma
  | Syntax_kind.TRY_KW -> parse_try_expr p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma
  | Syntax_kind.WHILE_KW -> parse_while_expr p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma
  | Syntax_kind.FOR_KW -> parse_for_expr p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma
  | Syntax_kind.IDENT -> parse_path_expr p
  | Syntax_kind.INT
  | Syntax_kind.FLOAT
  | Syntax_kind.STRING
  | Syntax_kind.CHAR
  | Syntax_kind.TRUE_KW
  | Syntax_kind.FALSE_KW -> parse_literal_expr p
  | Syntax_kind.QUOTE when Syntax_kind.(peek_kind p 1 = IDENT) ->
      let marker = start_node p in
      let quote = current p in
      bump p;
      let ident = current p in
      let text =
        text_between p ~start:quote.Raw_token.span.Span.start ~end_:ident.Raw_token.span.Span.end_
      in
      Event.Buffer.error
        p.events
        (Diagnostic.unclosed_char_literal ~text ~span:(zero_span ident.Raw_token.span.Span.end_));
      bump p;
      complete p marker Syntax_kind.ERROR
  | Syntax_kind.UNKNOWN when raw_starts_with p (current p) '\'' ->
      let marker = start_node p in
      let text = token_text p (current p) in
      Event.Buffer.error
        p.events
        (
          if text = "''" then
            empty_char_literal p
          else
            unclosed_char_literal p
        );
      bump p;
      complete p marker Syntax_kind.ERROR
  | Syntax_kind.LPAREN
  | Syntax_kind.BEGIN_KW -> parse_parenthesized_expr p ~signature ~stop_at_item
  | Syntax_kind.LBRACKET ->
      if is_extension_shell p then
        parse_extension_expr p
      else
        parse_list_expr p ~signature
  | Syntax_kind.LBRACKET_BAR -> parse_array_expr p ~signature
  | Syntax_kind.LBRACE -> parse_record_expr p ~signature
  | kind -> (
      match closing_delimiter_text kind with
      | Some delimiter ->
          let marker = start_node p in
          Event.Buffer.error p.events (unexpected_closing_delimiter p ~delimiter);
          bump p;
          complete p marker Syntax_kind.ERROR
      | None ->
          let marker = start_node p in
          Event.Buffer.error p.events (invalid_expression p);
          if not (is_eof p) then
            bump p
          else
            Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p);
          complete p marker Syntax_kind.ERROR
    )

and parse_symbolic_prefix_expr = fun p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma ->
  let marker = start_node p in
  consume_symbolic_operator p;
  let _operand = parse_expression p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma 70 in
  complete p marker Syntax_kind.PREFIX_EXPR

and parse_unreachable_expr = fun p ->
  let marker = start_node p in
  bump p;
  complete p marker Syntax_kind.UNREACHABLE_EXPR

and parse_operator_value_expr = fun p ->
  let marker = start_node p in
  consume_symbolic_operator p;
  complete p marker Syntax_kind.PATH_EXPR

and parse_delimited_local_open_expr = fun p lhs ~signature ~opener:_ ~closer ->
  let marker = precede p lhs in
  bump p;
  bump p;
  if not (at p closer || is_eof p) then
    if symbolic_sequence_followed_by_closer p 0 closer then
      ignore (parse_operator_value_expr p)
    else
      ignore (parse_expression p ~signature ~stop_at_item:false 0);
  expect p closer (invalid_expression p);
  complete p marker Syntax_kind.LOCAL_OPEN_EXPR

and parse_delimited_local_open_list_expr = fun p lhs ~signature ->
  let marker = precede p lhs in
  bump p;
  ignore (parse_list_expr p ~signature);
  complete p marker Syntax_kind.LOCAL_OPEN_EXPR

and parse_delimited_local_open_array_expr = fun p lhs ~signature ->
  let marker = precede p lhs in
  bump p;
  ignore (parse_array_expr p ~signature);
  complete p marker Syntax_kind.LOCAL_OPEN_EXPR

and parse_delimited_local_open_record_expr = fun p lhs ~signature ->
  let marker = precede p lhs in
  bump p;
  ignore (parse_record_expr p ~signature);
  complete p marker Syntax_kind.LOCAL_OPEN_EXPR

and parse_extended_index_expr = fun p lhs ~signature ~stop_at_item:_ ->
  let marker = precede p lhs in
  bump p;
  consume_symbolic_operator p;
  let closer =
    match current_kind p with
    | Syntax_kind.LPAREN ->
        bump p;
        Syntax_kind.RPAREN
    | Syntax_kind.LBRACKET ->
        bump p;
        Syntax_kind.RBRACKET
    | Syntax_kind.LBRACE ->
        bump p;
        Syntax_kind.RBRACE
    | _ ->
        Event.Buffer.error p.events (invalid_expression p);
        Syntax_kind.RPAREN
  in
  if not (at p closer || is_eof p) then
    ignore (parse_expression p ~signature ~stop_at_item:false 0);
  expect p closer (invalid_expression p);
  complete p marker Syntax_kind.ARRAY_INDEX_EXPR

and parse_path_expr = fun p ->
  let marker = start_node p in
  expect p Syntax_kind.IDENT (invalid_expression p);
  consume_expr_path_segments p;
  complete p marker Syntax_kind.PATH_EXPR

and parse_literal_expr = fun p ->
  let marker = start_node p in
  bump p;
  complete p marker Syntax_kind.LITERAL_EXPR

and parse_poly_variant_expr = fun p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma ->
  let marker = start_node p in
  bump p;
  expect p Syntax_kind.IDENT (invalid_expression p);
  if
    (not (expression_boundary p ~stop_at_item ~stop_at_semi ~stop_at_comma ~signature))
    && can_start_poly_variant_payload p
  then
    ignore
      (parse_expression
        p
        ~signature
        ~stop_at_item
        ~stop_at_semi
        ~stop_at_comma
        (application_binding_power + 1));
  complete p marker Syntax_kind.POLY_VARIANT_EXPR

and parse_label_arg_expr = fun p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma kind ->
  let marker = start_node p in
  bump p;
  expect p Syntax_kind.IDENT (invalid_expression p);
  if at p Syntax_kind.COLON then (
    bump p;
    ignore
      (parse_expression
        p
        ~signature
        ~stop_at_item
        ~stop_at_semi
        ~stop_at_comma
        (application_binding_power + 1))
  );
  complete p marker kind

and parse_parenthesized_expr = fun p ~signature ~stop_at_item ->
  let marker = start_node p in
  let opener = current_kind p in
  bump p;
  let at_closer () = at p Syntax_kind.RPAREN || at p Syntax_kind.END_KW || is_eof p in
  if Syntax_kind.(opener = LPAREN) && at p Syntax_kind.MODULE_KW then (
    consume_first_class_module_shell p;
    complete p marker Syntax_kind.FIRST_CLASS_MODULE_EXPR
  ) else if Syntax_kind.(opener = LPAREN) && parenthesized_operator_start p then (
    consume_balanced_until p ~closer:Syntax_kind.RPAREN 0;
    expect_closer p Syntax_kind.RPAREN ~opener:"(";
    complete p marker Syntax_kind.PATH_EXPR
  ) else (
    if not (at_closer ()) then
      ignore (parse_expression p ~signature ~stop_at_item:false ~stop_at_comma:true 0);
    let rec parse_comma_tail saw_comma =
      if at p Syntax_kind.COMMA then (
        bump p;
        if not (at_closer ()) then
          ignore (parse_expression p ~signature ~stop_at_item:false ~stop_at_comma:true 0);
        parse_comma_tail true
      ) else
        saw_comma
    in
    let saw_comma = parse_comma_tail false in
    (
      match opener with
      | Syntax_kind.BEGIN_KW -> expect_closer p Syntax_kind.END_KW ~opener:"begin"
      | _ -> expect_closer p Syntax_kind.RPAREN ~opener:"("
    );
    complete
      p
      marker
      (
        if saw_comma then
          Syntax_kind.TUPLE_EXPR
        else
          Syntax_kind.PAREN_EXPR
      )
  )

and parse_list_expr = fun p ~signature ->
  let marker = start_node p in
  bump p;
  let rec parse_elements () =
    if not (at p Syntax_kind.RBRACKET || is_eof p) then (
      if at p Syntax_kind.SEMI then (
        if Syntax_kind.(peek_kind p 1 = SEMI) then
          Event.Buffer.error p.events (list_double_semicolon p);
        bump p;
        if at p Syntax_kind.SEMI then
          bump p
      ) else (
        let before = p.pos in
        ignore (parse_expression p ~signature ~stop_at_item:false ~stop_at_semi:true 0);
        ensure_progress p before (invalid_expression p);
        if at p Syntax_kind.SEMI then (
          bump p;
          if at p Syntax_kind.SEMI then (
            if not Syntax_kind.(peek_kind p 1 = RBRACKET) then
              Event.Buffer.error p.events (list_double_semicolon p);
            bump p
          )
        )
      );
      parse_elements ()
    )
  in
  parse_elements ();
  expect_closer p Syntax_kind.RBRACKET ~opener:"[";
  complete p marker Syntax_kind.LIST_EXPR

and parse_extension_expr = fun p ->
  let marker = start_node p in
  bump p;
  consume_extension_sigils p;
  consume_extension_payload p;
  expect p Syntax_kind.RBRACKET (invalid_expression p);
  complete p marker Syntax_kind.EXTENSION_EXPR

and parse_array_expr = fun p ~signature ->
  let marker = start_node p in
  bump p;
  let rec parse_elements () =
    if not (at p Syntax_kind.BAR_RBRACKET || is_eof p) then (
      let before = p.pos in
      ignore (parse_expression p ~signature ~stop_at_item:false ~stop_at_semi:true 0);
      ensure_progress p before (invalid_expression p);
      ignore (bump_if p Syntax_kind.SEMI);
      parse_elements ()
    )
  in
  parse_elements ();
  expect_closer p Syntax_kind.BAR_RBRACKET ~opener:"[|";
  complete p marker Syntax_kind.ARRAY_EXPR

and parse_record_expr = fun p ~signature ->
  let marker = start_node p in
  let rec looks_like_record_field_head offset saw_ident =
    match peek_kind p offset with
    | Syntax_kind.IDENT when not saw_ident -> looks_like_record_field_head (offset + 1) true
    | Syntax_kind.DOT when saw_ident && Syntax_kind.(peek_kind p (offset + 1) = IDENT) ->
        looks_like_record_field_head (offset + 2) true
    | Syntax_kind.EQ
    | Syntax_kind.SEMI
    | Syntax_kind.RBRACE when saw_ident -> true
    | _ -> false
  in
  let rec update_head_ahead depth offset =
    let kind = peek_kind p offset in
    if Syntax_kind.(kind = EOF) then
      false
    else if Int.(depth = 0) && Syntax_kind.(kind = WITH_KW) then
      true
    else if Int.(depth = 0) && Syntax_kind.(kind = SEMI || kind = RBRACE) then
      false
    else
      match kind with
      | Syntax_kind.LPAREN
      | Syntax_kind.LBRACKET
      | Syntax_kind.LBRACE
      | Syntax_kind.LBRACKET_BAR
      | Syntax_kind.BEGIN_KW
      | Syntax_kind.STRUCT_KW
      | Syntax_kind.SIG_KW -> update_head_ahead Int.(depth + 1) (offset + 1)
      | Syntax_kind.RPAREN
      | Syntax_kind.RBRACKET
      | Syntax_kind.RBRACE
      | Syntax_kind.BAR_RBRACKET
      | Syntax_kind.END_KW -> update_head_ahead (Int.max 0 (depth - 1)) (offset + 1)
      | _ -> update_head_ahead depth (offset + 1)
  in
  let parse_record_field () =
    let field_marker = start_node p in
    let before = p.pos in
    if at p Syntax_kind.IDENT then
      ignore (parse_path_expr p)
    else
      ignore (parse_expression p ~signature ~stop_at_item:false ~stop_at_semi:true 0);
    if at p Syntax_kind.EQ then (
      bump p;
      ignore (parse_expression p ~signature ~stop_at_item:false ~stop_at_semi:true 0)
    );
    ensure_progress p before (invalid_expression p);
    ignore (bump_if p Syntax_kind.SEMI);
    complete p field_marker Syntax_kind.RECORD_EXPR_FIELD
  in
  bump p;
  let rec parse_fields () =
    if not (at p Syntax_kind.RBRACE || is_eof p) then (
      ignore (parse_record_field ());
      parse_fields ()
    )
  in
  let kind =
    if at p Syntax_kind.RBRACE || is_eof p then
      Syntax_kind.RECORD_EXPR
    else if looks_like_record_field_head 0 false then (
      parse_fields ();
      Syntax_kind.RECORD_EXPR
    ) else if update_head_ahead 0 0 then (
      ignore (parse_expression p ~signature ~stop_at_item:false ~stop_at_semi:true 0);
      expect p Syntax_kind.WITH_KW (invalid_expression p);
      parse_fields ();
      Syntax_kind.RECORD_UPDATE_EXPR
    ) else (
      parse_fields ();
      Syntax_kind.RECORD_EXPR
    )
  in
  expect p Syntax_kind.RBRACE (invalid_expression p);
  complete p marker kind

and parse_let_expr = fun p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma ->
  let marker = start_node p in
  bump p;
  if binding_operator_suffix (current_kind p) then
    parse_binding_operator_expr p marker ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma
  else if at p Syntax_kind.OPEN_KW then (
    bump p;
    ignore (bump_if p Syntax_kind.BANG);
    ignore (parse_path_expr p);
    expect p Syntax_kind.IN_KW (invalid_expression p);
    ignore (parse_expression p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma 0);
    complete p marker Syntax_kind.LOCAL_OPEN_EXPR
  ) else if at p Syntax_kind.MODULE_KW then (
    let recover_missing_in () =
      Event.Buffer.missing p.events ~kind:Syntax_kind.IN_KW ~offset:(current_offset p);
      Event.Buffer.error
        p.events
        (
          if is_eof p then
            invalid_expression_at_previous_end p
          else
            invalid_expression p
        );
      if not (is_eof p) then
        ignore (parse_expression p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma 0)
    in
    bump p;
    if at p Syntax_kind.IDENT || at p Syntax_kind.PERCENT then
      bump p
    else
      expect p Syntax_kind.IDENT (invalid_expression p);
    consume_balanced_until p ~closer:Syntax_kind.EQ 0;
    expect p Syntax_kind.EQ (invalid_expression p);
    if at p Syntax_kind.IN_KW || is_eof p then
      Event.Buffer.error p.events (missing_module_expr p)
    else
      ignore (parse_module_expr p ~signature);
    if at p Syntax_kind.IN_KW then (
      bump p;
      ignore (parse_expression p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma 0)
    ) else
      recover_missing_in ();
    complete p marker Syntax_kind.LET_MODULE_EXPR
  ) else if at p Syntax_kind.EXCEPTION_KW then (
    bump p;
    consume_balanced_until p ~closer:Syntax_kind.IN_KW 0;
    expect p Syntax_kind.IN_KW (invalid_expression p);
    ignore (parse_expression p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma 0);
    complete p marker Syntax_kind.LET_EXCEPTION_EXPR
  ) else (
    let recover_missing_in () =
      Event.Buffer.missing p.events ~kind:Syntax_kind.IN_KW ~offset:(current_offset p);
      Event.Buffer.error
        p.events
        (
          if is_eof p then
            invalid_expression_at_previous_end p
          else
            invalid_expression p
        );
      if not (is_eof p) then
        ignore (parse_expression p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma 0)
    in
    ignore (bump_if p Syntax_kind.REC_KW);
    parse_let_binding p ~signature ~top_level:false;
    let rec parse_and_bindings () =
      if at p Syntax_kind.AND_KW then (
        bump p;
        parse_let_binding p ~signature ~top_level:false;
        parse_and_bindings ()
      )
    in
    parse_and_bindings ();
    if at p Syntax_kind.IN_KW then (
      bump p;
      ignore (parse_expression p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma 0)
    ) else
      recover_missing_in ();
    complete p marker Syntax_kind.LET_EXPR
  )

and parse_binding_operator_expr = fun
  p marker ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma ->
  bump p;
  parse_let_binding p ~signature ~top_level:false;
  let rec parse_parallel_bindings () =
    if at p Syntax_kind.AND_KW && binding_operator_suffix (peek_kind p 1) then (
      bump p;
      bump p;
      parse_let_binding p ~signature ~top_level:false;
      parse_parallel_bindings ()
    )
  in
  parse_parallel_bindings ();
  expect p Syntax_kind.IN_KW (invalid_expression p);
  ignore (parse_expression p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma 0);
  complete p marker Syntax_kind.BINDING_OPERATOR_EXPR

and parse_dot_bang_expr = fun p lhs ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma ->
  let marker = precede p lhs in
  bump p;
  bump p;
  if at p Syntax_kind.LPAREN then (
    bump p;
    if not (at p Syntax_kind.RPAREN || is_eof p) then
      ignore (parse_expression p ~signature ~stop_at_item:false 0);
    expect p Syntax_kind.RPAREN (invalid_expression p)
  ) else if can_start_atom (current_kind p) then
    ignore (parse_prefix_or_atom p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma)
  else
    Event.Buffer.error p.events (invalid_expression p);
  complete p marker Syntax_kind.LOCAL_OPEN_EXPR

and parse_label_pattern = fun p ~stop_type_at_arrow kind ->
  let marker = start_node p in
  let complete_labeled_pattern has_default =
    let completed_kind =
      if Syntax_kind.(kind = OPTIONAL_PARAM) && has_default then
        Syntax_kind.OPTIONAL_PARAM_DEFAULT
      else
        kind
    in
    complete p marker completed_kind
  in
  let parse_parenthesized_binding () =
    bump p;
    if not (at p Syntax_kind.RPAREN || is_eof p) then
      parse_pattern ~stop_type_at_arrow:false p;
    let has_default =
      if at p Syntax_kind.EQ then (
        bump p;
        ignore (parse_expression p ~signature:false ~stop_at_item:false ~stop_at_semi:true 0);
        true
      ) else
        false
    in
    expect p Syntax_kind.RPAREN (invalid_pattern p);
    complete_labeled_pattern has_default
  in
  bump p;
  if at p Syntax_kind.LPAREN then
    parse_parenthesized_binding ()
  else (
    expect p Syntax_kind.IDENT (invalid_pattern p);
    if at p Syntax_kind.COLON && current_is_tight_after_previous p then (
      bump p;
      if at p Syntax_kind.LPAREN then
        parse_parenthesized_binding ()
      else (
        ignore (parse_pattern_no_apply ~stop_type_at_arrow p);
        complete_labeled_pattern false
      )
    ) else
      complete_labeled_pattern false
  )

and parse_parameter_pattern = fun p ~stop_type_at_arrow ->
  let rec loop lhs =
    if is_attribute_suffix p then (
      let marker = precede p lhs in
      bump p;
      consume_attribute_sigils p;
      consume_balanced_until p ~closer:Syntax_kind.RBRACKET 0;
      expect p Syntax_kind.RBRACKET (invalid_pattern p);
      loop (complete p marker Syntax_kind.ATTRIBUTE_PATTERN)
    ) else
      lhs
  in
  loop (parse_pattern_atom p ~stop_type_at_arrow)

and parse_if_expr = fun p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma ->
  let rec parse_then_branch () =
    let lhs = parse_expression p ~signature ~stop_at_item ~stop_at_semi:true ~stop_at_comma 0 in
    parse_then_branch_sequence_tail lhs
  and parse_then_branch_sequence_tail lhs =
    if not (at p Syntax_kind.SEMI) then
      lhs
    else
      let checkpoint = checkpoint p in
      let marker = precede p lhs in
      bump p;
    if trailing_sequence_boundary p then (
      restore p checkpoint;
      lhs
    ) else
      let _rhs = parse_then_branch () in
      if at p Syntax_kind.ELSE_KW then
        complete p marker Syntax_kind.SEQUENCE_EXPR
      else (
        restore p checkpoint;
        lhs
      )
  in
  let marker = start_node p in
  bump p;
  ignore (parse_expression p ~signature ~stop_at_item:false 0);
  if at p Syntax_kind.THEN_KW then (
    bump p;
    ignore (parse_then_branch ())
  ) else
    Event.Buffer.error p.events (if_missing_then p);
  if at p Syntax_kind.ELSE_KW then (
    bump p;
    ignore (parse_expression p ~signature ~stop_at_item ~stop_at_semi:true ~stop_at_comma 0)
  );
  complete p marker Syntax_kind.IF_EXPR

and parse_match_expr = fun p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma ->
  let marker = start_node p in
  bump p;
  if at p Syntax_kind.WITH_KW then
    Event.Buffer.error p.events (match_missing_scrutinee_at_previous_end p)
  else
    ignore (parse_expression p ~signature ~stop_at_item:false 0);
  if at p Syntax_kind.WITH_KW then
    bump p
  else
    Event.Buffer.error p.events (match_missing_with p);
  parse_match_cases p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma;
  complete p marker Syntax_kind.MATCH_EXPR

and parse_try_expr = fun p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma ->
  let marker = start_node p in
  bump p;
  ignore (parse_expression p ~signature ~stop_at_item:false 0);
  expect p Syntax_kind.WITH_KW (invalid_expression p);
  parse_match_cases p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma;
  complete p marker Syntax_kind.TRY_EXPR

and parse_match_cases = fun p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma ->
  let parse_case () =
    let marker = start_node p in
    ignore (bump_if p Syntax_kind.PIPE);
    if at p Syntax_kind.ARROW then
      Event.Buffer.error p.events (match_missing_pattern p)
    else
      parse_pattern p;
    if at p Syntax_kind.WHEN_KW then (
      bump p;
      if at p Syntax_kind.ARROW then
        Event.Buffer.error p.events (match_guard_missing_expr p)
      else
        ignore (parse_expression p ~signature ~stop_at_item:false 0)
    );
    expect p Syntax_kind.ARROW (invalid_expression p);
    ignore (parse_expression p ~signature ~stop_at_item ~stop_at_semi:false ~stop_at_comma 0);
    ignore (complete p marker Syntax_kind.MATCH_CASE)
  in
  let rec loop first =
    if
      at p Syntax_kind.PIPE
      || (first && (at p Syntax_kind.ARROW || can_start_pattern_atom (current_kind p)))
    then (
      parse_case ();
      loop false
    )
  in
  loop true

and parse_fun_expr = fun p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma ->
  let marker = start_node p in
  bump p;
  let rec parse_params () =
    if not (at p Syntax_kind.ARROW || at p Syntax_kind.COLON || is_eof p) then (
      ignore (parse_parameter_pattern p ~stop_type_at_arrow:true);
      parse_params ()
    )
  in
  parse_params ();
  if at p Syntax_kind.COLON then (
    bump p;
    ignore (parse_type_expr p ~allow_leading_poly_type_after_newline:true ~stop_at_arrow:true)
  );
  expect p Syntax_kind.ARROW (invalid_expression p);
  ignore (parse_expression p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma 0);
  complete p marker Syntax_kind.FUN_EXPR

and parse_function_expr = fun p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma ->
  let marker = start_node p in
  bump p;
  parse_match_cases p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma;
  complete p marker Syntax_kind.FUNCTION_EXPR

and parse_while_expr = fun p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma ->
  let marker = start_node p in
  bump p;
  ignore (parse_expression p ~signature ~stop_at_item:false 0);
  expect p Syntax_kind.DO_KW (invalid_expression p);
  ignore
    (parse_expression p ~signature ~stop_at_item:false ~stop_at_semi:false ~stop_at_comma:false 0);
  expect p Syntax_kind.DONE_KW (invalid_expression p);
  complete p marker Syntax_kind.WHILE_EXPR

and parse_for_expr = fun p ~signature ~stop_at_item ~stop_at_semi ~stop_at_comma ->
  let marker = start_node p in
  bump p;
  parse_pattern ~stop_type_at_arrow:false p;
  expect p Syntax_kind.EQ (invalid_expression p);
  ignore (parse_expression p ~signature ~stop_at_item:false 0);
  if at p Syntax_kind.TO_KW || at p Syntax_kind.DOWNTO_KW then
    bump p
  else
    Event.Buffer.error p.events (invalid_expression p);
  ignore (parse_expression p ~signature ~stop_at_item:false 0);
  expect p Syntax_kind.DO_KW (invalid_expression p);
  ignore
    (parse_expression p ~signature ~stop_at_item:false ~stop_at_semi:false ~stop_at_comma:false 0);
  expect p Syntax_kind.DONE_KW (invalid_expression p);
  complete p marker Syntax_kind.FOR_EXPR

and parse_pattern = fun ?(stop_type_at_arrow = true) p ->
  ignore
    (parse_pattern_bp p ~stop_type_at_arrow 0)

and parse_pattern_no_apply = fun p ~stop_type_at_arrow ->
  let rec loop lhs =
    if is_attribute_suffix p then (
      let marker = precede p lhs in
      bump p;
      consume_attribute_sigils p;
      consume_balanced_until p ~closer:Syntax_kind.RBRACKET 0;
      expect p Syntax_kind.RBRACKET (invalid_pattern p);
      loop (complete p marker Syntax_kind.ATTRIBUTE_PATTERN)
    ) else
      match pattern_binding_power (current_kind p) with
      | Some _ -> (
          match current_kind p with
          | Syntax_kind.COLON ->
              let marker = precede p lhs in
              bump p;
              parse_type_expr
                p
                ~allow_leading_poly_type_after_newline:true
                ~stop_at_arrow:stop_type_at_arrow;
              loop (complete p marker Syntax_kind.CONSTRAINT_PATTERN)
          | Syntax_kind.AS_KW ->
              let marker = precede p lhs in
              bump p;
              if can_start_pattern_atom (current_kind p) then
                ignore (parse_pattern_atom p ~stop_type_at_arrow)
              else
                Event.Buffer.error p.events (invalid_pattern_at_previous_end p);
              loop (complete p marker Syntax_kind.ALIAS_PATTERN)
          | Syntax_kind.COLONCOLON ->
              let marker = precede p lhs in
              bump p;
              if can_start_pattern_atom (current_kind p) then
                ignore (parse_pattern_no_apply p ~stop_type_at_arrow)
              else
                Event.Buffer.error p.events (cons_pattern_missing_tail p);
              loop (complete p marker Syntax_kind.CONS_PATTERN)
          | Syntax_kind.DOTDOT ->
              let marker = precede p lhs in
              bump p;
              ignore (parse_pattern_no_apply p ~stop_type_at_arrow);
              loop (complete p marker Syntax_kind.INTERVAL_PATTERN)
          | Syntax_kind.PIPE ->
              let marker = precede p lhs in
              bump p;
              if at p Syntax_kind.PIPE then
                Event.Buffer.error p.events (or_pattern_double p)
              else if can_start_pattern_atom (current_kind p) then
                ignore (parse_pattern_no_apply p ~stop_type_at_arrow)
              else
                Event.Buffer.error p.events (or_pattern_missing p);
              loop (complete p marker Syntax_kind.OR_PATTERN)
          | Syntax_kind.COMMA ->
              let marker = precede p lhs in
              bump p;
              ignore (parse_pattern_no_apply p ~stop_type_at_arrow);
              loop (complete p marker Syntax_kind.TUPLE_PATTERN)
          | _ -> lhs
        )
      | _ -> lhs
  in
  loop (parse_pattern_atom p ~stop_type_at_arrow)

and parse_pattern_bp = fun p ~stop_type_at_arrow min_bp ->
  let rec loop lhs =
    if is_attribute_suffix p then (
      let marker = precede p lhs in
      bump p;
      consume_attribute_sigils p;
      consume_balanced_until p ~closer:Syntax_kind.RBRACKET 0;
      expect p Syntax_kind.RBRACKET (invalid_pattern p);
      loop (complete p marker Syntax_kind.ATTRIBUTE_PATTERN)
    ) else
      match pattern_binding_power (current_kind p) with
      | Some bp when bp >= min_bp -> (
          match current_kind p with
          | Syntax_kind.COLON ->
              let marker = precede p lhs in
              bump p;
              parse_type_expr
                p
                ~allow_leading_poly_type_after_newline:true
                ~stop_at_arrow:stop_type_at_arrow;
              loop (complete p marker Syntax_kind.CONSTRAINT_PATTERN)
          | Syntax_kind.AS_KW ->
              let marker = precede p lhs in
              bump p;
              if can_start_pattern_atom (current_kind p) then
                ignore (parse_pattern_atom p ~stop_type_at_arrow)
              else
                Event.Buffer.error p.events (invalid_pattern_at_previous_end p);
              loop (complete p marker Syntax_kind.ALIAS_PATTERN)
          | Syntax_kind.COLONCOLON ->
              let marker = precede p lhs in
              bump p;
              if can_start_pattern_atom (current_kind p) then
                ignore (parse_pattern_bp p ~stop_type_at_arrow bp)
              else
                Event.Buffer.error p.events (cons_pattern_missing_tail p);
              loop (complete p marker Syntax_kind.CONS_PATTERN)
          | Syntax_kind.DOTDOT ->
              let marker = precede p lhs in
              bump p;
              ignore (parse_pattern_bp p ~stop_type_at_arrow bp);
              loop (complete p marker Syntax_kind.INTERVAL_PATTERN)
          | Syntax_kind.PIPE ->
              let marker = precede p lhs in
              bump p;
              if at p Syntax_kind.PIPE then
                Event.Buffer.error p.events (or_pattern_double p)
              else if can_start_pattern_atom (current_kind p) then
                ignore (parse_pattern_bp p ~stop_type_at_arrow bp)
              else
                Event.Buffer.error p.events (or_pattern_missing p);
              loop (complete p marker Syntax_kind.OR_PATTERN)
          | Syntax_kind.COMMA ->
              let marker = precede p lhs in
              bump p;
              ignore (parse_pattern_bp p ~stop_type_at_arrow bp);
              loop (complete p marker Syntax_kind.TUPLE_PATTERN)
          | _ -> lhs
        )
      | _ -> lhs
  in
  loop (parse_pattern_apply p ~stop_type_at_arrow)

and parse_pattern_apply = fun p ~stop_type_at_arrow ->
  let rec loop lhs =
    if is_attribute_suffix p then (
      let marker = precede p lhs in
      bump p;
      consume_attribute_sigils p;
      consume_balanced_until p ~closer:Syntax_kind.RBRACKET 0;
      expect p Syntax_kind.RBRACKET (invalid_pattern p);
      loop (complete p marker Syntax_kind.ATTRIBUTE_PATTERN)
    ) else if can_start_pattern_atom (current_kind p) then (
      let marker = precede p lhs in
      ignore (parse_pattern_atom p ~stop_type_at_arrow);
      loop (complete p marker Syntax_kind.CONSTRUCT_PATTERN)
    ) else
      lhs
  in
  loop (parse_pattern_atom p ~stop_type_at_arrow)

and parse_pattern_atom = fun p ~stop_type_at_arrow ->
  match current_kind p with
  | Syntax_kind.UNDERSCORE -> parse_single_token_pattern p Syntax_kind.WILDCARD_PATTERN
  | Syntax_kind.IDENT -> parse_path_pattern p
  | Syntax_kind.INT
  | Syntax_kind.FLOAT
  | Syntax_kind.STRING
  | Syntax_kind.CHAR
  | Syntax_kind.TRUE_KW
  | Syntax_kind.FALSE_KW -> parse_single_token_pattern p Syntax_kind.LITERAL_PATTERN
  | kind when sign_token kind && literal_after_sign (peek_kind p 1) ->
      parse_signed_literal_pattern p
  | Syntax_kind.PERCENT -> parse_single_token_pattern p Syntax_kind.PATH_PATTERN
  | Syntax_kind.LPAREN -> parse_parenthesized_pattern p
  | Syntax_kind.LBRACKET ->
      if is_extension_shell p then
        parse_extension_pattern p
      else
        parse_list_pattern p
  | Syntax_kind.LBRACKET_BAR -> parse_array_pattern p
  | Syntax_kind.LBRACE -> parse_record_pattern p
  | Syntax_kind.TILDE -> parse_label_pattern p ~stop_type_at_arrow Syntax_kind.LABELED_PARAM
  | Syntax_kind.QUESTION -> parse_label_pattern p ~stop_type_at_arrow Syntax_kind.OPTIONAL_PARAM
  | Syntax_kind.BACKTICK -> parse_poly_variant_pattern p ~stop_type_at_arrow
  | Syntax_kind.HASH -> parse_poly_variant_inherit_pattern p
  | Syntax_kind.EXCEPTION_KW ->
      parse_unary_pattern p ~stop_type_at_arrow Syntax_kind.EXCEPTION_PATTERN
  | _ -> parse_error_pattern p

and parse_single_token_pattern = fun p kind ->
  let marker = start_node p in
  if is_eof p then (
    Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p);
    Event.Buffer.error p.events (invalid_pattern p)
  ) else
    bump p;
  complete p marker kind

and parse_signed_literal_pattern = fun p ->
  let marker = start_node p in
  bump p;
  if literal_after_sign (current_kind p) then
    bump p
  else
    Event.Buffer.error p.events (invalid_pattern_at_previous_end p);
  complete p marker Syntax_kind.LITERAL_PATTERN

and parse_path_pattern = fun p ->
  let marker = start_node p in
  expect p Syntax_kind.IDENT (invalid_pattern p);
  consume_path_segments p;
  if
    at p Syntax_kind.DOT
    && (Syntax_kind.(peek_kind p 1 = LPAREN)
    || Syntax_kind.(peek_kind p 1 = LBRACE)
    || Syntax_kind.(peek_kind p 1 = LBRACKET)
    || Syntax_kind.(peek_kind p 1 = LBRACKET_BAR))
  then (
    bump p;
    (
      match current_kind p with
      | Syntax_kind.LPAREN ->
          bump p;
          if not (at p Syntax_kind.RPAREN || is_eof p) then
            parse_pattern ~stop_type_at_arrow:false p;
          expect p Syntax_kind.RPAREN (invalid_pattern p)
      | Syntax_kind.LBRACE -> ignore (parse_record_pattern p)
      | Syntax_kind.LBRACKET ->
          if is_extension_shell p then
            ignore (parse_extension_pattern p)
          else
            ignore (parse_list_pattern p)
      | Syntax_kind.LBRACKET_BAR -> ignore (parse_array_pattern p)
      | _ -> Event.Buffer.error p.events (invalid_pattern p)
    );
    complete p marker Syntax_kind.LOCAL_OPEN_PATTERN
  ) else
    complete p marker Syntax_kind.PATH_PATTERN

and parse_parenthesized_pattern = fun p ->
  let marker = start_node p in
  bump p;
  let at_closer () = at p Syntax_kind.RPAREN || is_eof p in
  if at p Syntax_kind.TYPE_KW then (
    bump p;
    let rec consume_type_names () =
      if at p Syntax_kind.IDENT then (
        bump p;
        consume_type_names ()
      )
    in
    consume_type_names ();
    expect_closer p Syntax_kind.RPAREN ~opener:"(";
    complete p marker Syntax_kind.LOCALLY_ABSTRACT_TYPE_PATTERN
  ) else if at p Syntax_kind.MODULE_KW then (
    bump p;
    if at p Syntax_kind.IDENT || at p Syntax_kind.UNDERSCORE then
      bump p
    else
      expect p Syntax_kind.IDENT (invalid_pattern p);
    consume_balanced_until p ~closer:Syntax_kind.RPAREN 0;
    expect_closer p Syntax_kind.RPAREN ~opener:"(";
    complete p marker Syntax_kind.FIRST_CLASS_MODULE_PATTERN
  ) else if
    binding_operator_keyword (current_kind p)
    && binding_operator_suffix (peek_kind p 1)
    && Syntax_kind.(peek_kind p 2 = RPAREN)
  then (
    bump p;
    bump p;
    expect_closer p Syntax_kind.RPAREN ~opener:"(";
    complete p marker Syntax_kind.PATH_PATTERN
  ) else (
    if not (at_closer ()) then
      if at p Syntax_kind.COMMA then
        Event.Buffer.error p.events (tuple_pattern_extra_comma p)
      else if parenthesized_operator_start p then
        consume_balanced_until p ~closer:Syntax_kind.RPAREN 0
      else if operator_pattern_token (current_kind p) && Syntax_kind.(peek_kind p 1 = RPAREN) then
        ignore (parse_single_token_pattern p Syntax_kind.PATH_PATTERN)
      else
        parse_pattern ~stop_type_at_arrow:false p;
    let rec parse_comma_tail saw_comma =
      if at p Syntax_kind.COMMA then (
        bump p;
        if not (at_closer ()) then
          parse_pattern ~stop_type_at_arrow:false p;
        parse_comma_tail true
      ) else
        saw_comma
    in
    let saw_comma = parse_comma_tail false in
    expect_closer p Syntax_kind.RPAREN ~opener:"(";
    complete
      p
      marker
      (
        if saw_comma then
          Syntax_kind.TUPLE_PATTERN
        else
          Syntax_kind.PAREN_PATTERN
      )
  )

and parse_extension_pattern = fun p ->
  let marker = start_node p in
  bump p;
  consume_extension_sigils p;
  consume_extension_payload p;
  expect p Syntax_kind.RBRACKET (invalid_pattern p);
  complete p marker Syntax_kind.EXTENSION_PATTERN

and parse_list_pattern = fun p ->
  let marker = start_node p in
  bump p;
  let rec parse_elements () =
    if not (at p Syntax_kind.RBRACKET || is_eof p) then (
      let before = p.pos in
      parse_pattern ~stop_type_at_arrow:false p;
      ensure_progress p before (invalid_pattern p);
      ignore (bump_if p Syntax_kind.SEMI);
      parse_elements ()
    )
  in
  parse_elements ();
  expect_closer p Syntax_kind.RBRACKET ~opener:"[";
  complete p marker Syntax_kind.LIST_PATTERN

and parse_array_pattern = fun p ->
  let marker = start_node p in
  bump p;
  let rec parse_elements () =
    if not (at p Syntax_kind.BAR_RBRACKET || is_eof p) then (
      let before = p.pos in
      parse_pattern ~stop_type_at_arrow:false p;
      ensure_progress p before (invalid_pattern p);
      ignore (bump_if p Syntax_kind.SEMI);
      parse_elements ()
    )
  in
  parse_elements ();
  expect_closer p Syntax_kind.BAR_RBRACKET ~opener:"[|";
  complete p marker Syntax_kind.ARRAY_PATTERN

and parse_record_pattern = fun p ->
  let marker = start_node p in
  bump p;
  let rec parse_fields () =
    if not (at p Syntax_kind.RBRACE || is_eof p) then (
      let before = p.pos in
      if at p Syntax_kind.UNDERSCORE then
        ignore (parse_single_token_pattern p Syntax_kind.WILDCARD_PATTERN)
      else (
        ignore (parse_path_pattern p);
        if at p Syntax_kind.EQ then (
          bump p;
          parse_pattern ~stop_type_at_arrow:false p
        )
      );
      ensure_progress p before (invalid_pattern p);
      ignore (bump_if p Syntax_kind.SEMI);
      parse_fields ()
    )
  in
  parse_fields ();
  expect p Syntax_kind.RBRACE (invalid_pattern p);
  complete p marker Syntax_kind.RECORD_PATTERN

and parse_poly_variant_pattern = fun p ~stop_type_at_arrow ->
  let marker = start_node p in
  bump p;
  expect p Syntax_kind.IDENT (invalid_pattern p);
  if can_start_pattern_atom (current_kind p) then
    ignore (parse_pattern_apply p ~stop_type_at_arrow);
  complete p marker Syntax_kind.POLY_VARIANT_PATTERN

and parse_poly_variant_inherit_pattern = fun p ->
  let marker = start_node p in
  bump p;
  ignore (parse_path_pattern p);
  complete p marker Syntax_kind.POLY_VARIANT_PATTERN

and parse_unary_pattern = fun p ~stop_type_at_arrow kind ->
  let marker = start_node p in
  bump p;
  if can_start_pattern_atom (current_kind p) then
    ignore (parse_pattern_atom p ~stop_type_at_arrow)
  else
    Event.Buffer.error p.events (invalid_pattern_at_previous_end p);
  complete p marker kind

and parse_error_pattern = fun p ->
  let marker = start_node p in
  let raw = current p in
  Event.Buffer.error
    p.events
    (
      if at p Syntax_kind.COLONCOLON then
        invalid_pattern_at_previous_end p
      else if Syntax_kind.(peek_kind p 1 = EQ) then
        diagnostic_with_raw_found_at p raw Diagnostic.invalid_pattern (peek p 1).Raw_token.span
      else
        invalid_pattern p
    );
  if is_eof p then
    Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p)
  else
    bump p;
  complete p marker Syntax_kind.ERROR

and type_expr_boundary = fun p ~stop_at_arrow ->
  match current_kind p with
  | Syntax_kind.EOF
  | Syntax_kind.AS_KW
  | Syntax_kind.AND_KW
  | Syntax_kind.CONSTRAINT_KW
  | Syntax_kind.PIPE
  | Syntax_kind.WHEN_KW
  | Syntax_kind.EQ
  | Syntax_kind.COMMA
  | Syntax_kind.RPAREN
  | Syntax_kind.RBRACKET
  | Syntax_kind.RBRACE
  | Syntax_kind.BAR_RBRACKET
  | Syntax_kind.END_KW
  | Syntax_kind.SEMI -> true
  | Syntax_kind.ARROW when stop_at_arrow -> true
  | kind when leading_trivia_contains_newline p
  && (starts_signature_item kind || starts_structure_item kind) -> true
  | _ -> false

and parse_opaque_type_atom = fun p ~stop_at_arrow ->
  let marker = start_node p in
  (
    match current_kind p with
    | Syntax_kind.LBRACKET ->
        let extension_shell = is_extension_shell p in
        bump p;
        if extension_shell then (
          consume_extension_sigils p;
          consume_extension_payload p
        ) else
          consume_balanced_until p ~closer:Syntax_kind.RBRACKET 0;
        expect p Syntax_kind.RBRACKET (invalid_type_expression p)
    | Syntax_kind.LBRACKET_BAR ->
        bump p;
        consume_balanced_until p ~closer:Syntax_kind.BAR_RBRACKET 0;
        expect p Syntax_kind.BAR_RBRACKET (invalid_type_expression p)
    | Syntax_kind.LBRACE ->
        bump p;
        consume_balanced_until p ~closer:Syntax_kind.RBRACE 0;
        expect p Syntax_kind.RBRACE (invalid_type_expression p)
    | Syntax_kind.LT ->
        bump p;
        consume_angle_balanced_until_gt p 0;
        expect p Syntax_kind.GT (invalid_type_expression p)
    | Syntax_kind.SIG_KW ->
        bump p;
        consume_balanced_until p ~closer:Syntax_kind.END_KW 0;
        expect_closer p Syntax_kind.END_KW ~opener:"type"
    | _ ->
        if type_expr_boundary p ~stop_at_arrow then
          Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p)
        else
          bump p
  );
  complete p marker Syntax_kind.OPAQUE_TYPE

and parse_parenthesized_type = fun p ~stop_at_arrow ->
  let marker = start_node p in
  bump p;
  let has_comma = ref false in
  let has_alias = ref false in
  (
    if not (at p Syntax_kind.RPAREN || is_eof p) then
      ignore (parse_type_bp p ~stop_at_arrow:false 0)
  );
  if at p Syntax_kind.AS_KW then (
    has_alias := true;
    bump p;
    if type_expr_boundary p ~stop_at_arrow:false then
      Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p)
    else
      ignore (parse_type_atom p ~stop_at_arrow:false)
  );
  let rec parse_comma_items () =
    if at p Syntax_kind.COMMA then (
      has_comma := true;
      bump p;
      if not (at p Syntax_kind.RPAREN || is_eof p) then
        ignore (parse_type_bp p ~stop_at_arrow:false 0)
      else
        Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p);
      parse_comma_items ()
    )
  in
  parse_comma_items ();
  expect p Syntax_kind.RPAREN (invalid_type_expression p);
  complete
    p
    marker
    (
      if !has_alias then
        Syntax_kind.OPAQUE_TYPE
      else if !has_comma then
        Syntax_kind.TUPLE_TYPE
      else
        Syntax_kind.PAREN_TYPE
    )

and starts_quoted_poly_type = fun p ->
  let rec loop offset consumed =
    if Syntax_kind.(peek_kind p offset = QUOTE && peek_kind p (offset + 1) = IDENT) then
      loop (offset + 2) true
    else
      consumed && Syntax_kind.(peek_kind p offset = DOT)
  in
  loop 0 false

and parse_poly_type = fun p ~stop_at_arrow ->
  let marker = start_node p in
  let rec consume_keyword_type_names consumed =
    if at p Syntax_kind.IDENT then (
      bump p;
      consume_keyword_type_names true
    ) else
      consumed
  in
  let rec consume_quoted_type_names consumed =
    if at p Syntax_kind.QUOTE && Syntax_kind.(peek_kind p 1 = IDENT) then (
      bump p;
      bump p;
      consume_quoted_type_names true
    ) else
      consumed
  in
  let consumed_name =
    match current_kind p with
    | Syntax_kind.TYPE_KW ->
        bump p;
        consume_keyword_type_names false
    | Syntax_kind.QUOTE -> consume_quoted_type_names false
    | _ -> false
  in
  if not consumed_name then
    Event.Buffer.error p.events (missing_type_name_at_current_offset p);
  expect p Syntax_kind.DOT (invalid_type_expression p);
  if type_expr_boundary p ~stop_at_arrow then
    Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p)
  else
    ignore (parse_type_bp p ~stop_at_arrow 0);
  complete p marker Syntax_kind.POLY_TYPE

and parse_labeled_type = fun p ->
  let marker = start_node p in
  ignore (bump_if p Syntax_kind.QUESTION);
  expect p Syntax_kind.IDENT (invalid_type_expression p);
  expect p Syntax_kind.COLON (invalid_type_expression p);
  if type_expr_boundary p ~stop_at_arrow:true then
    Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p)
  else
    ignore (parse_type_bp p ~stop_at_arrow:true 0);
  complete p marker Syntax_kind.LABELED_TYPE

and parse_type_atom = fun p ~stop_at_arrow ->
  match current_kind p with
  | Syntax_kind.TYPE_KW -> parse_poly_type p ~stop_at_arrow
  | Syntax_kind.QUOTE when starts_quoted_poly_type p -> parse_poly_type p ~stop_at_arrow
  | Syntax_kind.IDENT when Syntax_kind.(peek_kind p 1 = COLON) -> parse_labeled_type p
  | Syntax_kind.QUESTION when Syntax_kind.(peek_kind p 1 = IDENT)
  && Syntax_kind.(peek_kind p 2 = COLON) -> parse_labeled_type p
  | Syntax_kind.IDENT ->
      let marker = start_node p in
      bump p;
      consume_path_segments p;
      complete p marker Syntax_kind.PATH_TYPE
  | Syntax_kind.QUOTE ->
      let marker = start_node p in
      bump p;
      if at p Syntax_kind.IDENT then
        bump p
      else
        Event.Buffer.error
          p.events
          (malformed_type_variable_at p (current p) (current p).Raw_token.span);
      complete p marker Syntax_kind.VAR_TYPE
  | Syntax_kind.UNDERSCORE ->
      let marker = start_node p in
      bump p;
      complete p marker Syntax_kind.WILDCARD_TYPE
  | Syntax_kind.LPAREN when Syntax_kind.(peek_kind p 1 = MODULE_KW) ->
      let marker = start_node p in
      bump p;
      consume_first_class_module_shell p;
      complete p marker Syntax_kind.OPAQUE_TYPE
  | Syntax_kind.LPAREN -> parse_parenthesized_type p ~stop_at_arrow
  | _ -> parse_opaque_type_atom p ~stop_at_arrow

and parse_type_bp = fun p ~stop_at_arrow min_bp ->
  let rec loop lhs =
    if is_attribute_suffix p then (
      let marker = precede p lhs in
      consume_attribute_suffix p;
      loop (complete p marker Syntax_kind.OPAQUE_TYPE)
    ) else if type_expr_boundary p ~stop_at_arrow then
      lhs
    else if (not stop_at_arrow) && at p Syntax_kind.ARROW && min_bp <= 5 then (
      let marker = precede p lhs in
      bump p;
      if type_expr_boundary p ~stop_at_arrow then (
        Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p);
        Event.Buffer.error p.events (invalid_type_expression_at_previous_end p)
      ) else
        ignore (parse_type_bp p ~stop_at_arrow 5);
      loop (complete p marker Syntax_kind.ARROW_TYPE)
    ) else if at p Syntax_kind.STAR && min_bp <= 10 then (
      let marker = precede p lhs in
      bump p;
      if type_expr_boundary p ~stop_at_arrow then (
        Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p);
        Event.Buffer.error p.events (invalid_type_expression_at_previous_end p)
      ) else
        ignore (parse_type_bp p ~stop_at_arrow 11);
      loop (complete p marker Syntax_kind.TUPLE_TYPE)
    ) else if at p Syntax_kind.IDENT && min_bp <= 30 then (
      let marker = precede p lhs in
      ignore (parse_type_atom p ~stop_at_arrow);
      loop (complete p marker Syntax_kind.APPLY_TYPE)
    ) else
      lhs
  in
  loop (parse_type_atom p ~stop_at_arrow)

and parse_type_expr = fun p ~allow_leading_poly_type_after_newline ~stop_at_arrow ->
  let marker = start_node p in
  if
    type_expr_boundary p ~stop_at_arrow
    && not (allow_leading_poly_type_after_newline && at p Syntax_kind.TYPE_KW)
  then (
    Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p);
    Event.Buffer.error p.events (invalid_type_expression p)
  ) else (
    ignore (parse_type_bp p ~stop_at_arrow 0);
    let rec recover_trailing () =
      if not (is_eof p || type_expr_boundary p ~stop_at_arrow) then (
        ignore (parse_opaque_type_atom p ~stop_at_arrow);
        recover_trailing ()
      )
    in
    recover_trailing ()
  );
  ignore (complete p marker Syntax_kind.TYPE_EXPR)

and parse_let_binding = fun p ~signature ~top_level ->
  let marker = start_node p in
  let diagnostics_before = Vector.length (Event.Buffer.diagnostics p.events) in
  let missing_pattern = at p Syntax_kind.EQ in
  let starts_parenthesized_binding_name () =
    Syntax_kind.(current_kind p = LPAREN)
    && ((operator_pattern_token (peek_kind p 1) && Syntax_kind.(peek_kind p 2 = RPAREN))
    || (binding_operator_keyword (peek_kind p 1)
    && binding_operator_suffix (peek_kind p 2)
    && Syntax_kind.(peek_kind p 3 = RPAREN)))
  in
  let starts_function_binding_head () =
    match current_kind p with
    | Syntax_kind.IDENT -> not (identifier_text_starts_uppercase (token_text p (current p)))
    | Syntax_kind.LPAREN -> starts_parenthesized_binding_name ()
    | _ -> false
  in
  if missing_pattern then (
    Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p);
    Event.Buffer.error p.events (invalid_pattern_at_previous_end p)
  ) else if starts_function_binding_head () then
    ignore (parse_pattern_no_apply p ~stop_type_at_arrow:false)
  else
    ignore (parse_pattern p ~stop_type_at_arrow:false);
  let rec parse_params () =
    if
      not
        (at p Syntax_kind.EQ
        || is_eof p
        || (top_level && at_item_boundary p ~signature)
        || not (has_eq_before_item_boundary p ~signature))
    then
      if at p Syntax_kind.COLON then (
        bump p;
        parse_type_expr p ~allow_leading_poly_type_after_newline:true ~stop_at_arrow:false
      ) else (
        ignore (parse_parameter_pattern p ~stop_type_at_arrow:false);
        parse_params ()
      )
  in
  parse_params ();
  if at p Syntax_kind.EQ then (
    bump p;
    if missing_binding_expr_boundary p ~signature ~top_level then (
      Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p);
      if not missing_pattern then (
        Event.Buffer.error p.events (missing_let_binding_expr p);
        if is_eof p && diagnostics_before > 0 && current_offset p > previous_end_offset p then
          Event.Buffer.error p.events (invalid_expression_at_previous_end p)
      )
    ) else if (not top_level) && closing_punctuation (current_kind p) then (
      let error_marker = start_node p in
      let raw = current p in
      bump p;
      Event.Buffer.error
        p.events
        (diagnostic_with_raw_found_at
          p
          raw
          Diagnostic.invalid_expression
          (zero_span (current_offset p)));
      ignore (complete p error_marker Syntax_kind.ERROR)
    ) else
      ignore (parse_expression p ~signature ~stop_at_item:top_level 0)
  ) else (
    Event.Buffer.missing p.events ~kind:Syntax_kind.EQ ~offset:(current_offset p);
    Event.Buffer.error
      p.events
      (
        if
          (not (at_item_boundary p ~signature)) && not (has_eq_before_item_boundary p ~signature)
        then
          missing_let_binding_equals_eof_at_current_offset p
        else if at_item_boundary p ~signature then
          missing_let_binding_equals_at_previous_end p
        else
          missing_let_binding_equals p
      );
    if
      not
        (expression_boundary
          p
          ~stop_at_item:top_level
          ~stop_at_semi:false
          ~stop_at_comma:false
          ~signature)
    then
      ignore (parse_expression p ~signature ~stop_at_item:top_level 0)
  );
  ignore (complete p marker Syntax_kind.LET_BINDING)

and consume_until_item_boundary = fun p ~signature ->
  let next_depth depth =
    match current_kind p with
    | Syntax_kind.LPAREN
    | Syntax_kind.LBRACE
    | Syntax_kind.LBRACKET
    | Syntax_kind.LBRACKET_BAR
    | Syntax_kind.BEGIN_KW
    | Syntax_kind.STRUCT_KW
    | Syntax_kind.SIG_KW -> depth + 1
    | Syntax_kind.RPAREN
    | Syntax_kind.RBRACE
    | Syntax_kind.RBRACKET
    | Syntax_kind.BAR_RBRACKET
    | Syntax_kind.END_KW when depth > 0 -> depth - 1
    | _ when depth > 0 && at_end_keyword p -> depth - 1
    | _ -> depth
  in
  let rec loop depth consumed =
    if not (is_eof p || (consumed && depth = 0 && at_item_boundary p ~signature)) then (
      let depth = next_depth depth in
      bump p;
      loop depth true
    )
  in
  loop 0 false

and consume_opaque_until_item_boundary = fun p ~signature kind diagnostic ->
  let marker = start_node p in
  consume_until_item_boundary p ~signature;
  if Event.Buffer.length p.events = 0 then
    Event.Buffer.error p.events diagnostic;
  ignore (complete p marker kind)

and parse_let_decl = fun p ~signature ->
  let marker = start_node p in
  expect p Syntax_kind.LET_KW (invalid_expression p);
  ignore (bump_if p Syntax_kind.REC_KW);
  parse_let_binding p ~signature ~top_level:true;
  let rec parse_and_bindings () =
    if at p Syntax_kind.AND_KW then (
      bump p;
      parse_let_binding p ~signature ~top_level:true;
      parse_and_bindings ()
    )
  in
  parse_and_bindings ();
  let kind =
    if (not signature) && at p Syntax_kind.IN_KW then (
      bump p;
      ignore (parse_expression p ~signature ~stop_at_item:true 0);
      Syntax_kind.LET_EXPR
    ) else
      Syntax_kind.LET_DECL
  in
  ignore (complete p marker kind)

and raw_first_char = fun p raw ->
  let index = raw.Raw_token.span.Span.start in
  if index < 0 || index >= Slice.length p.source then
    None
  else
    try Some (Slice.get_unchecked p.source ~at:index) with
    | Invalid_argument _ -> None

and raw_ident_is_capitalized = fun p raw ->
  Syntax_kind.(raw.Raw_token.kind = IDENT) && match raw_first_char p raw with
  | Some 'A' .. 'Z' -> true
  | _ -> false

and ident_at_is_capitalized = fun p offset ->
  raw_ident_is_capitalized
    p
    (raw_at p (significant_raw_at p (p.pos + offset)))

and starts_bare_variant_constructor = fun p ->
  ident_at_is_capitalized p 0 && not Syntax_kind.(peek_kind p 1 = DOT)

and starts_bare_variant_constructor_at = fun p offset ->
  ident_at_is_capitalized p offset && not Syntax_kind.(peek_kind p (offset + 1) = DOT)

and at_type_decl_member_boundary = fun p ~signature ->
  is_eof p
  || at_item_boundary p ~signature
  || at p Syntax_kind.AND_KW
  || at p Syntax_kind.CONSTRAINT_KW

and at_record_type_field_boundary = fun p ->
  is_eof p || at p Syntax_kind.SEMI || at p Syntax_kind.RBRACE

and parse_record_type_field = fun p ->
  let marker = start_node p in
  let start_pos = p.pos in
  ignore (bump_if p Syntax_kind.MUTABLE_KW);
  let field_name =
    if at p Syntax_kind.IDENT then
      let text = token_text p (current p) in
      bump p;
      Some text
    else (
      if p.pos > start_pos then
        Event.Buffer.error p.events (mutable_field_missing_name p);
      None
    )
  in
  let missing_colon =
    match field_name with
    | Some field_name when not (at p Syntax_kind.COLON) ->
        Event.Buffer.error p.events (record_field_missing_colon p ~field_name);
        true
    | _ -> false
  in
  if at p Syntax_kind.COLON then
    bump p
  else if Option.is_some field_name && not missing_colon then
    ();
  if at_record_type_field_boundary p then (
    match field_name with
    | Some field_name when at p Syntax_kind.SEMI || at p Syntax_kind.RBRACE ->
        Event.Buffer.error p.events (record_field_missing_type p ~field_name)
    | _ -> ()
  ) else
    ignore (parse_type_expr p ~allow_leading_poly_type_after_newline:false ~stop_at_arrow:false);
  if p.pos = start_pos && not (is_eof p || at p Syntax_kind.RBRACE) then
    bump p;
  ignore (bump_if p Syntax_kind.SEMI);
  complete p marker Syntax_kind.RECORD_FIELD

and parse_record_type = fun p ->
  let marker = start_node p in
  ignore (bump_if p Syntax_kind.PRIVATE_KW);
  expect p Syntax_kind.LBRACE (invalid_type_expression p);
  let rec parse_fields () =
    if not (is_eof p || at p Syntax_kind.RBRACE) then (
      ignore (parse_record_type_field p);
      parse_fields ()
    )
  in
  parse_fields ();
  expect_closer p Syntax_kind.RBRACE ~opener:"{";
  complete p marker Syntax_kind.RECORD_TYPE

and parse_variant_constructor = fun p ~signature ->
  let marker = start_node p in
  ignore (bump_if p Syntax_kind.PIPE);
  if at p Syntax_kind.IDENT then (
    bump p;
    consume_path_segments p
  ) else (
    Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p);
    Event.Buffer.error p.events (missing_type_name_at_current_offset p)
  );
  (
    match current_kind p with
    | Syntax_kind.OF_KW ->
        bump p;
        if at_type_decl_member_boundary p ~signature || at p Syntax_kind.PIPE then
          Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p)
        else if at p Syntax_kind.LBRACE then
          ignore (parse_record_type p)
        else
          ignore
            (parse_type_expr p ~allow_leading_poly_type_after_newline:false ~stop_at_arrow:false)
    | Syntax_kind.COLON ->
        bump p;
        if at_type_decl_member_boundary p ~signature || at p Syntax_kind.PIPE then
          Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p)
        else if at p Syntax_kind.LBRACE then (
          ignore (parse_record_type p);
          if at p Syntax_kind.ARROW then (
            bump p;
            if at_type_decl_member_boundary p ~signature || at p Syntax_kind.PIPE then
              Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p)
            else
              ignore
                (parse_type_expr p ~allow_leading_poly_type_after_newline:false ~stop_at_arrow:false)
          )
        ) else
          ignore
            (parse_type_expr p ~allow_leading_poly_type_after_newline:false ~stop_at_arrow:false)
    | _ -> ()
  );
  complete p marker Syntax_kind.VARIANT_CONSTRUCTOR

and parse_variant_type = fun p ~signature ->
  let marker = start_node p in
  ignore (bump_if p Syntax_kind.PRIVATE_KW);
  let rec parse_constructors consumed =
    if at_type_decl_member_boundary p ~signature then
      consumed
    else if at p Syntax_kind.PIPE || starts_bare_variant_constructor p then (
      ignore (parse_variant_constructor p ~signature);
      parse_constructors true
    ) else
      consumed
  in
  ignore (parse_constructors false);
  complete p marker Syntax_kind.VARIANT_TYPE

and consume_type_body = fun p ~signature ->
  match current_kind p with
  | Syntax_kind.LBRACE -> ignore (parse_record_type p)
  | Syntax_kind.LPAREN when Syntax_kind.(peek_kind p 1 = MODULE_KW) ->
      bump p;
      consume_first_class_module_shell p
  | _ -> consume_until_item_boundary p ~signature

and type_decl_body_contains_unsupported_type_syntax = fun p ~signature ->
  let next_depth depth kind =
    match kind with
    | Syntax_kind.LPAREN
    | Syntax_kind.LBRACE
    | Syntax_kind.LBRACKET
    | Syntax_kind.LBRACKET_BAR
    | Syntax_kind.BEGIN_KW
    | Syntax_kind.STRUCT_KW
    | Syntax_kind.SIG_KW -> depth + 1
    | Syntax_kind.RPAREN
    | Syntax_kind.RBRACE
    | Syntax_kind.RBRACKET
    | Syntax_kind.BAR_RBRACKET
    | Syntax_kind.END_KW when depth > 0 -> depth - 1
    | _ -> depth
  in
  let rec loop position depth =
    let kind = (raw_at p (significant_raw_at p position)).Raw_token.kind in
    if Syntax_kind.(kind = EOF) then
      false
    else if position > p.pos && depth = 0 && at_item_boundary_at p position ~signature then
      false
    else if depth = 0 && Syntax_kind.(kind = AND_KW || kind = CONSTRAINT_KW) then
      false
    else if Syntax_kind.(kind = AS_KW || kind = WITH_KW) then
      true
    else
      loop (position + 1) (next_depth depth kind)
  in
  loop p.pos 0

and type_decl_body_needs_opaque_parse = fun p ~signature ->
  match current_kind p with
  | Syntax_kind.PIPE
  | Syntax_kind.LBRACE
  | Syntax_kind.PRIVATE_KW -> true
  | Syntax_kind.LPAREN when Syntax_kind.(peek_kind p 1 = MODULE_KW) -> false
  | _ -> type_decl_body_contains_unsupported_type_syntax p ~signature

and parse_type_decl_representation = fun p ~signature ->
  if
    at p Syntax_kind.LBRACE || (at p Syntax_kind.PRIVATE_KW && Syntax_kind.(peek_kind p 1 = LBRACE))
  then
    ignore (parse_record_type p)
  else if
    at p Syntax_kind.PIPE
    || starts_bare_variant_constructor p
    || (at p Syntax_kind.PRIVATE_KW
    && (Syntax_kind.(peek_kind p 1 = PIPE) || starts_bare_variant_constructor_at p 1))
  then
    ignore (parse_variant_type p ~signature)
  else if at p Syntax_kind.DOTDOT then
    bump p
  else
    consume_until_type_decl_member_boundary p ~signature 0

and parse_type_decl_body = fun p ~signature ->
  if
    at p Syntax_kind.LBRACE || (at p Syntax_kind.PRIVATE_KW && Syntax_kind.(peek_kind p 1 = LBRACE))
  then
    ignore (parse_record_type p)
  else if
    at p Syntax_kind.PIPE
    || starts_bare_variant_constructor p
    || (at p Syntax_kind.PRIVATE_KW
    && (Syntax_kind.(peek_kind p 1 = PIPE) || starts_bare_variant_constructor_at p 1))
  then
    ignore (parse_variant_type p ~signature)
  else if type_decl_body_needs_opaque_parse p ~signature then
    consume_type_body p ~signature
  else (
    parse_type_expr p ~allow_leading_poly_type_after_newline:false ~stop_at_arrow:false;
    if at p Syntax_kind.EQ then (
      bump p;
      parse_type_decl_representation p ~signature
    ) else if
      not
        (is_eof p
        || at_item_boundary p ~signature
        || at p Syntax_kind.AND_KW
        || at p Syntax_kind.CONSTRAINT_KW)
    then
      consume_until_item_boundary p ~signature
  )

and starts_type_extension_body = fun p -> at p Syntax_kind.PIPE || starts_bare_variant_constructor p

and skip_type_extension_param_modifiers = fun p index ->
  match peek_kind p index with
  | Syntax_kind.PLUS
  | Syntax_kind.MINUS
  | Syntax_kind.BANG -> skip_type_extension_param_modifiers p (index + 1)
  | _ -> index

and skip_type_extension_param = fun p index ->
  let index = skip_type_extension_param_modifiers p index in
  match peek_kind p index with
  | Syntax_kind.QUOTE -> (
      match peek_kind p (index + 1) with
      | Syntax_kind.IDENT -> index + 2
      | _ -> index + 1
    )
  | Syntax_kind.UNDERSCORE -> index + 1
  | _ -> index

and skip_parenthesized_type_extension_params = fun p index ->
  match peek_kind p index with
  | Syntax_kind.RPAREN -> index + 1
  | Syntax_kind.EOF -> index
  | Syntax_kind.COMMA -> skip_parenthesized_type_extension_params p (index + 1)
  | _ ->
      let next = skip_type_extension_param p index in
      if next > index then
        skip_parenthesized_type_extension_params p next
      else
        index

and skip_type_extension_params = fun p index ->
  match peek_kind p index with
  | Syntax_kind.NONREC_KW -> skip_type_extension_params p (index + 1)
  | Syntax_kind.PLUS
  | Syntax_kind.MINUS
  | Syntax_kind.BANG
  | Syntax_kind.QUOTE
  | Syntax_kind.UNDERSCORE ->
      let next = skip_type_extension_param p index in
      if next > index then
        skip_type_extension_params p next
      else
        index
  | Syntax_kind.LPAREN ->
      let next = skip_parenthesized_type_extension_params p (index + 1) in
      if next > index then
        skip_type_extension_params p next
      else
        index
  | _ -> index

and skip_type_extension_name_segments = fun p index ->
  if Syntax_kind.(peek_kind p index = DOT) && Syntax_kind.(peek_kind p (index + 1) = IDENT) then
    skip_type_extension_name_segments p (index + 2)
  else
    index

and starts_with_type_extension_decl = fun p ->
  if not (at p Syntax_kind.TYPE_KW) then
    false
  else
    let index = skip_type_extension_params p 1 in
    if Syntax_kind.(peek_kind p index = IDENT) then
      let index = skip_type_extension_name_segments p (index + 1) in
      Syntax_kind.(peek_kind p index = PLUS) && Syntax_kind.(peek_kind p (index + 1) = EQ)
    else
      false

and recover_bad_comment_type_var = fun p raw ->
  if raw_char_is p raw ~offset:1 '(' then (
    let rec consume_until_rparen () =
      if not (is_eof p || at p Syntax_kind.RPAREN) then (
        bump p;
        consume_until_rparen ()
      )
    in
    consume_until_rparen ();
    ignore (bump_if p Syntax_kind.RPAREN)
  )

and consume_type_variable = fun p ->
  let quote = current p in
  bump p;
  if at p Syntax_kind.IDENT then (
    let ident = current p in
    let first =
      try Some (Slice.get_unchecked p.source ~at:ident.Raw_token.span.Span.start) with
      | Invalid_argument _ -> None
    in
    (
      match first with
      | Some first when first >= 'A' && first <= 'Z' ->
          Event.Buffer.error p.events (uppercase_type_variable_at p ~quote ~ident)
      | _ -> ()
    );
    bump p
  ) else
    Event.Buffer.error
      p.events
      (malformed_type_variable_at p (current p) (current p).Raw_token.span)

and consume_type_parameter = fun p ->
  let rec consume_modifiers first =
    match current_kind p with
    | Syntax_kind.PLUS
    | Syntax_kind.MINUS
    | Syntax_kind.BANG ->
        let raw = current p in
        bump p;
        consume_modifiers (Option.or_ first (Some raw))
    | _ -> first
  in
  let first_modifier = consume_modifiers None in
  match current_kind p with
  | Syntax_kind.QUOTE -> consume_type_variable p
  | Syntax_kind.UNDERSCORE -> bump p
  | _ -> (
      match first_modifier with
      | Some raw ->
          Event.Buffer.error p.events (invalid_type_parameter_at p raw (current p).Raw_token.span)
      | None -> ()
    )

and consume_invalid_standalone_type_param = fun p ->
  let raw = current p in
  bump p;
  Event.Buffer.error p.events (invalid_type_parameter_at p raw (current p).Raw_token.span)

and consume_paren_type_params = fun p ->
  bump p;
  let rec consume_items () =
    match current_kind p with
    | Syntax_kind.RPAREN ->
        bump p;
        true
    | Syntax_kind.EOF -> false
    | Syntax_kind.COMMA ->
        bump p;
        consume_items ()
    | Syntax_kind.QUOTE
    | Syntax_kind.PLUS
    | Syntax_kind.MINUS
    | Syntax_kind.BANG
    | Syntax_kind.UNDERSCORE ->
        consume_type_parameter p;
        consume_items ()
    | Syntax_kind.UNKNOWN when raw_starts_with p (current p) '\'' ->
        let raw = current p in
        Event.Buffer.error p.events (malformed_type_variable_at p raw raw.Raw_token.span);
        bump p;
        recover_bad_comment_type_var p raw;
        consume_items ()
    | _ -> false
  in
  if not (consume_items ()) then
    Event.Buffer.error p.events (unclosed_type_params_at_previous_end p)

and consume_type_params = fun p ->
  match current_kind p with
  | Syntax_kind.NONREC_KW ->
      bump p;
      consume_type_params p
  | Syntax_kind.PLUS
  | Syntax_kind.MINUS
  | Syntax_kind.BANG ->
      consume_type_parameter p;
      consume_type_params p
  | Syntax_kind.QUOTE ->
      consume_type_variable p;
      consume_type_params p
  | Syntax_kind.UNDERSCORE ->
      bump p;
      consume_type_params p
  | Syntax_kind.UNKNOWN when raw_starts_with p (current p) '\'' ->
      let raw = current p in
      Event.Buffer.error p.events (malformed_type_variable_at p raw raw.Raw_token.span);
      bump p;
      recover_bad_comment_type_var p raw;
      consume_type_params p
  | Syntax_kind.IDENT when current_text_is p "__" && Syntax_kind.(peek_kind p 1 = IDENT) ->
      consume_invalid_standalone_type_param p;
      consume_type_params p
  | Syntax_kind.AT
  | Syntax_kind.CARET
  | Syntax_kind.LBRACKET when Syntax_kind.(peek_kind p 1 = IDENT) ->
      consume_invalid_standalone_type_param p;
      consume_type_params p
  | Syntax_kind.LPAREN ->
      consume_paren_type_params p;
      consume_type_params p
  | _ -> ()

and parse_type_decl_head = fun p ~signature ->
  consume_type_params p;
  if at p Syntax_kind.IDENT then (
    let text = token_text p (current p) in
    bump p;
    consume_path_segments p;
    Some text
  ) else (
    if not (is_eof p || at_item_boundary p ~signature) then
      Event.Buffer.error p.events (missing_type_name p);
    None
  )

and parse_type_extension_decl = fun p ~signature ->
  let marker = start_node p in
  let head = start_node p in
  expect p Syntax_kind.TYPE_KW (invalid_type_expression p);
  ignore (parse_type_decl_head p ~signature);
  ignore (complete p head Syntax_kind.TYPE_EXTENSION_DECL_HEAD);
  let body = start_node p in
  if at p Syntax_kind.PLUS && Syntax_kind.(peek_kind p 1 = EQ) then (
    bump p;
    bump p;
    if is_eof p || at_item_boundary p ~signature then (
      Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p);
      Event.Buffer.error p.events (invalid_type_expression_at_previous_end p)
    ) else if starts_type_extension_body p then
      ignore (parse_variant_type p ~signature)
    else (
      Event.Buffer.error p.events (invalid_type_expression p);
      consume_until_item_boundary p ~signature
    )
  ) else
    Event.Buffer.error p.events (missing_type_decl_equals p);
  ignore (complete p body Syntax_kind.TYPE_EXTENSION_DECL_BODY);
  if not (is_eof p || at_item_boundary p ~signature) then
    consume_until_item_boundary p ~signature;
  ignore (complete p marker Syntax_kind.TYPE_EXTENSION_DECL)

and type_decl_tail_depth_after = fun depth kind ->
  match kind with
  | Syntax_kind.LPAREN
  | Syntax_kind.LBRACE
  | Syntax_kind.LBRACKET
  | Syntax_kind.LBRACKET_BAR
  | Syntax_kind.BEGIN_KW
  | Syntax_kind.STRUCT_KW
  | Syntax_kind.SIG_KW -> depth + 1
  | Syntax_kind.RPAREN
  | Syntax_kind.RBRACE
  | Syntax_kind.RBRACKET
  | Syntax_kind.BAR_RBRACKET when Int.(depth > 0) -> depth - 1
  | Syntax_kind.END_KW when Int.(depth > 0) -> depth - 1
  | _ -> depth

and consume_until_type_decl_member_boundary = fun p ~signature depth ->
  if is_eof p || at_item_boundary p ~signature || (Int.equal depth 0 && at p Syntax_kind.AND_KW) then
    ()
  else
    (
      let kind = current_kind p in
      bump p;
      consume_until_type_decl_member_boundary
        p
        ~signature
        (type_decl_tail_depth_after depth kind)
    )

and consume_type_decl_constraints = fun p ~signature ->
  if at p Syntax_kind.CONSTRAINT_KW then
    consume_until_type_decl_member_boundary p ~signature 0

and parse_type_decl_member = fun p ~signature ~first ->
  let marker = start_node p in
  if first then
    expect p Syntax_kind.TYPE_KW (invalid_type_expression p)
  else
    expect p Syntax_kind.AND_KW (invalid_type_expression p);
  let type_name = parse_type_decl_head p ~signature in
  (
    match current_kind p with
    | Syntax_kind.LT ->
        let type_name = Option.unwrap_or type_name ~default:"" in
        Event.Buffer.error p.events (bracketed_type_parameters p ~type_name);
        consume_until_type_decl_member_boundary p ~signature 0
    | Syntax_kind.COLONEQ -> consume_until_type_decl_member_boundary p ~signature 0
    | Syntax_kind.EQ ->
        bump p;
        if is_eof p || at_item_boundary p ~signature then (
          Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p);
          Event.Buffer.error p.events (invalid_type_expression_at_previous_end p)
        ) else
          parse_type_decl_body p ~signature;
        consume_type_decl_constraints p ~signature
    | Syntax_kind.EOF -> ()
    | _ when at_item_boundary p ~signature -> ()
    | Syntax_kind.LBRACKET when is_attribute_shell p ->
        consume_until_type_decl_member_boundary p ~signature 0
    | _ ->
        Event.Buffer.missing p.events ~kind:Syntax_kind.EQ ~offset:(current_offset p);
        Event.Buffer.error p.events (missing_type_decl_equals p);
        consume_until_type_decl_member_boundary p ~signature 0
  );
  ignore (complete p marker Syntax_kind.TYPE_DECL_MEMBER)

and parse_type_decl = fun p ~signature ->
  let marker = start_node p in
  ignore (parse_type_decl_member p ~signature ~first:true);
  let rec parse_tail_members () =
    if at p Syntax_kind.AND_KW then (
      ignore (parse_type_decl_member p ~signature ~first:false);
      parse_tail_members ()
    )
  in
  parse_tail_members ();
  ignore (complete p marker Syntax_kind.TYPE_DECL)

and module_expr_boundary = fun p ~signature ->
  is_eof p
  || at_end_keyword p
  || at p Syntax_kind.IN_KW
  || at p Syntax_kind.AND_KW
  || at_item_boundary p ~signature

and module_type_boundary = fun p ~signature ->
  is_eof p
  || at_end_keyword p
  || at p Syntax_kind.IN_KW
  || at p Syntax_kind.AND_KW
  || at p Syntax_kind.EQ
  || at_item_boundary p ~signature

and parse_nested_phrase_separators = fun p ->
  if at p Syntax_kind.SEMI && Syntax_kind.(peek_kind p 1 = SEMI) then (
    bump p;
    bump p;
    parse_nested_phrase_separators p
  )

and module_decl_head_boundary = fun p ~signature depth ->
  is_eof p
  || (Int.equal depth 0
  && (at p Syntax_kind.COLON
  || at p Syntax_kind.EQ
  || at p Syntax_kind.AND_KW
  || at p Syntax_kind.STRUCT_KW
  || at p Syntax_kind.FUNCTOR_KW
  || at p Syntax_kind.IDENT
  || at_end_keyword p
  || at_item_boundary p ~signature))

and consume_module_decl_head_tail = fun p ~signature ->
  let next_depth depth =
    match current_kind p with
    | Syntax_kind.LPAREN
    | Syntax_kind.LBRACE
    | Syntax_kind.LBRACKET
    | Syntax_kind.LBRACKET_BAR
    | Syntax_kind.BEGIN_KW
    | Syntax_kind.STRUCT_KW
    | Syntax_kind.SIG_KW -> depth + 1
    | Syntax_kind.RPAREN
    | Syntax_kind.RBRACE
    | Syntax_kind.RBRACKET
    | Syntax_kind.BAR_RBRACKET
    | Syntax_kind.END_KW when depth > 0 -> depth - 1
    | _ when depth > 0 && at_end_keyword p -> depth - 1
    | _ -> depth
  in
  let rec loop depth =
    if not (module_decl_head_boundary p ~signature depth) then (
      let depth = next_depth depth in
      bump p;
      loop depth
    )
  in
  loop 0

and consume_until_module_expr_boundary = fun p ~signature ->
  let next_depth depth =
    match current_kind p with
    | Syntax_kind.LPAREN
    | Syntax_kind.LBRACE
    | Syntax_kind.LBRACKET
    | Syntax_kind.LBRACKET_BAR
    | Syntax_kind.BEGIN_KW
    | Syntax_kind.STRUCT_KW
    | Syntax_kind.SIG_KW -> depth + 1
    | Syntax_kind.RPAREN
    | Syntax_kind.RBRACE
    | Syntax_kind.RBRACKET
    | Syntax_kind.BAR_RBRACKET
    | Syntax_kind.END_KW when depth > 0 -> depth - 1
    | _ when depth > 0 && at_end_keyword p -> depth - 1
    | _ -> depth
  in
  let rec loop depth consumed =
    if not (module_expr_boundary p ~signature && (consumed || Int.equal depth 0)) then (
      let depth = next_depth depth in
      bump p;
      loop depth true
    )
  in
  loop 0 false

and consume_until_module_type_boundary = fun p ~signature ->
  let next_depth depth =
    match current_kind p with
    | Syntax_kind.LPAREN
    | Syntax_kind.LBRACE
    | Syntax_kind.LBRACKET
    | Syntax_kind.LBRACKET_BAR
    | Syntax_kind.BEGIN_KW
    | Syntax_kind.STRUCT_KW
    | Syntax_kind.SIG_KW -> depth + 1
    | Syntax_kind.RPAREN
    | Syntax_kind.RBRACE
    | Syntax_kind.RBRACKET
    | Syntax_kind.BAR_RBRACKET
    | Syntax_kind.END_KW when depth > 0 -> depth - 1
    | _ when depth > 0 && at_end_keyword p -> depth - 1
    | _ -> depth
  in
  let rec loop depth consumed =
    if not (module_type_boundary p ~signature && (consumed || Int.equal depth 0)) then (
      let depth = next_depth depth in
      bump p;
      loop depth true
    )
  in
  loop 0 false

and parse_struct_module_expr = fun p ->
  let marker = start_node p in
  expect p Syntax_kind.STRUCT_KW (invalid_expression p);
  let rec loop () =
    parse_nested_phrase_separators p;
    if not (is_eof p || at_end_keyword p) then (
      let before = p.pos in
      parse_structure_item p;
      ensure_progress p before (invalid_expression p);
      loop ()
    )
  in
  loop ();
  expect_closer p Syntax_kind.END_KW ~opener:"struct";
  complete p marker Syntax_kind.STRUCT_MODULE_EXPR

and parse_signature_module_type = fun p ->
  let marker = start_node p in
  expect p Syntax_kind.SIG_KW (invalid_type_expression p);
  let rec loop () =
    parse_nested_phrase_separators p;
    if not (is_eof p || at_end_keyword p) then (
      let before = p.pos in
      if starts_signature_item_at p then
        parse_signature_item p
      else
        recover_current_as_error p (unexpected_signature_item p);
      ensure_progress p before (unexpected_signature_item p);
      loop ()
    )
  in
  loop ();
  expect_closer p Syntax_kind.END_KW ~opener:"sig";
  complete p marker Syntax_kind.SIGNATURE_MODULE_TYPE

and parse_module_path_node = fun p kind diagnostic ->
  let marker = start_node p in
  if at p Syntax_kind.IDENT then (
    bump p;
    consume_path_segments p
  ) else (
    Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p);
    Event.Buffer.error p.events diagnostic
  );
  complete p marker kind

and parse_opaque_module_expr = fun p ~signature ->
  let marker = start_node p in
  if module_expr_boundary p ~signature then (
    Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p);
    Event.Buffer.error p.events (missing_module_expr p)
  ) else
    consume_until_module_expr_boundary p ~signature;
  complete p marker Syntax_kind.OPAQUE_MODULE_EXPR

and parse_opaque_module_type = fun p ~signature ->
  let marker = start_node p in
  if module_type_boundary p ~signature then (
    Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p);
    Event.Buffer.error p.events (missing_module_type_expr p)
  ) else
    consume_until_module_type_boundary p ~signature;
  complete p marker Syntax_kind.OPAQUE_MODULE_TYPE

and parse_parenthesized_module_expr = fun p ~signature ->
  let marker = start_node p in
  bump p;
  if at p Syntax_kind.VAL_KW then
    consume_balanced_until p ~closer:Syntax_kind.RPAREN 0
  else if not (at p Syntax_kind.RPAREN || is_eof p) then
    ignore (parse_module_expr p ~signature);
  let has_constraint =
    if at p Syntax_kind.COLON then (
      bump p;
      if not (at p Syntax_kind.RPAREN || is_eof p) then
        ignore (parse_module_type_expr p ~signature);
      true
    ) else
      false
  in
  expect_closer p Syntax_kind.RPAREN ~opener:"(";
  consume_same_line_attribute_suffixes p;
  complete
    p
    marker
    (
      if has_constraint then
        Syntax_kind.CONSTRAINT_MODULE_EXPR
      else
        Syntax_kind.PAREN_MODULE_EXPR
    )

and parse_functor_module_expr = fun p ~signature ->
  let marker = start_node p in
  bump p;
  consume_balanced_until p ~closer:Syntax_kind.ARROW 0;
  expect p Syntax_kind.ARROW (missing_module_expr p);
  if not (module_expr_boundary p ~signature) then
    ignore (parse_module_expr p ~signature)
  else
    Event.Buffer.error p.events (missing_module_expr p);
  complete p marker Syntax_kind.FUNCTOR_MODULE_EXPR

and parse_module_expr_atom = fun p ~signature ->
  match current_kind p with
  | Syntax_kind.STRUCT_KW -> parse_struct_module_expr p
  | Syntax_kind.FUNCTOR_KW -> parse_functor_module_expr p ~signature
  | Syntax_kind.LPAREN -> parse_parenthesized_module_expr p ~signature
  | Syntax_kind.IDENT ->
      parse_module_path_node p Syntax_kind.PATH_MODULE_EXPR (missing_module_expr p)
  | _ -> parse_opaque_module_expr p ~signature

and parse_module_expr_bp = fun p ~signature ->
  let rec loop lhs =
    if at p Syntax_kind.LPAREN then (
      let marker = precede p lhs in
      bump p;
      if not (at p Syntax_kind.RPAREN || is_eof p) then
        ignore (parse_module_expr p ~signature);
      expect_closer p Syntax_kind.RPAREN ~opener:"(";
      loop (complete p marker Syntax_kind.APPLY_MODULE_EXPR)
    ) else
      lhs
  in
  loop (parse_module_expr_atom p ~signature)

and parse_module_expr = fun p ~signature ->
  let marker = start_node p in
  ignore (parse_module_expr_bp p ~signature);
  consume_same_line_attribute_suffixes p;
  complete p marker Syntax_kind.MODULE_EXPR

and parse_parenthesized_module_type = fun p ~signature ->
  let marker = start_node p in
  bump p;
  if not (at p Syntax_kind.RPAREN || is_eof p) then
    ignore (parse_module_type_expr p ~signature);
  expect_closer p Syntax_kind.RPAREN ~opener:"(";
  consume_same_line_attribute_suffixes p;
  complete p marker Syntax_kind.PAREN_MODULE_TYPE

and parse_typeof_module_type = fun p ~signature ->
  let marker = start_node p in
  expect p Syntax_kind.MODULE_KW (invalid_type_expression p);
  expect p Syntax_kind.TYPE_KW (invalid_type_expression p);
  expect p Syntax_kind.OF_KW (invalid_type_expression p);
  if not (module_expr_boundary p ~signature) then
    ignore (parse_module_expr p ~signature)
  else
    Event.Buffer.error p.events (missing_module_expr p);
  consume_same_line_attribute_suffixes p;
  complete p marker Syntax_kind.TYPEOF_MODULE_TYPE

and parse_functor_module_type = fun p ~signature ->
  let marker = start_node p in
  bump p;
  consume_balanced_until p ~closer:Syntax_kind.ARROW 0;
  expect p Syntax_kind.ARROW (missing_module_type_expr p);
  if not (module_type_boundary p ~signature) then
    ignore (parse_module_type_expr p ~signature)
  else
    Event.Buffer.error p.events (missing_module_type_expr p);
  complete p marker Syntax_kind.FUNCTOR_MODULE_TYPE

and parse_arrow_functor_module_type = fun p ~signature ->
  let marker = start_node p in
  consume_balanced_until p ~closer:Syntax_kind.ARROW 0;
  expect p Syntax_kind.ARROW (missing_module_type_expr p);
  if not (module_type_boundary p ~signature) then
    ignore (parse_module_type_expr p ~signature)
  else
    Event.Buffer.error p.events (missing_module_type_expr p);
  complete p marker Syntax_kind.FUNCTOR_MODULE_TYPE

and parse_module_type_atom = fun p ~signature ->
  match current_kind p with
  | Syntax_kind.SIG_KW -> parse_signature_module_type p
  | Syntax_kind.MODULE_KW when starts_with_typeof_module_expr_keyword p ->
      parse_typeof_module_type p ~signature
  | Syntax_kind.FUNCTOR_KW -> parse_functor_module_type p ~signature
  | Syntax_kind.LPAREN when Syntax_kind.(peek_kind p 1 = IDENT)
  && Syntax_kind.(peek_kind p 2 = COLON) -> parse_arrow_functor_module_type p ~signature
  | Syntax_kind.LPAREN -> parse_parenthesized_module_type p ~signature
  | Syntax_kind.IDENT ->
      parse_module_path_node p Syntax_kind.PATH_MODULE_TYPE (missing_module_type_expr p)
  | _ -> parse_opaque_module_type p ~signature

and consume_module_type_with_constraint = fun p ~signature ->
  let parse_type_path () =
    let marker = start_node p in
    if at p Syntax_kind.IDENT then (
      bump p;
      consume_path_segments p
    ) else
      Event.Buffer.error p.events (missing_type_name p);
    complete p marker Syntax_kind.PATH_TYPE
  in
  let parse_type_constraint () =
    let marker = start_node p in
    expect p Syntax_kind.TYPE_KW (invalid_type_expression p);
    ignore (parse_type_path ());
    if at p Syntax_kind.EQ || at p Syntax_kind.COLONEQ then (
      bump p;
      if not (type_expr_boundary p ~stop_at_arrow:false) then
        parse_type_expr p ~allow_leading_poly_type_after_newline:false ~stop_at_arrow:false
    ) else if at p Syntax_kind.PLUS && Syntax_kind.(peek_kind p 1 = EQ) then (
      bump p;
      bump p;
      if not (type_expr_boundary p ~stop_at_arrow:false) then
        parse_type_expr p ~allow_leading_poly_type_after_newline:false ~stop_at_arrow:false
    ) else
      Event.Buffer.error p.events (missing_type_decl_equals p);
    complete p marker Syntax_kind.WITH_TYPE_CONSTRAINT
  in
  let parse_module_constraint () =
    let marker = start_node p in
    expect p Syntax_kind.MODULE_KW (invalid_expression p);
    ignore (parse_module_path_node p Syntax_kind.PATH_MODULE_EXPR (missing_module_expr p));
    if at p Syntax_kind.EQ then (
      bump p;
      if not (module_expr_boundary p ~signature) then
        ignore (parse_module_expr p ~signature)
    ) else
      Event.Buffer.error p.events (missing_module_decl_equals p);
    complete p marker Syntax_kind.WITH_MODULE_CONSTRAINT
  in
  let rec parse_constraint () =
    (
      match current_kind p with
      | Syntax_kind.TYPE_KW -> ignore (parse_type_constraint ())
      | Syntax_kind.MODULE_KW -> ignore (parse_module_constraint ())
      | _ -> consume_until_module_type_boundary p ~signature
    );
    if
      at p Syntax_kind.AND_KW
      && (Syntax_kind.(peek_kind p 1 = TYPE_KW) || Syntax_kind.(peek_kind p 1 = MODULE_KW))
    then (
      bump p;
      parse_constraint ()
    )
  in
  parse_constraint ()

and parse_module_type_bp = fun p ~signature ->
  let rec loop lhs =
    if at p Syntax_kind.WITH_KW then (
      let marker = precede p lhs in
      bump p;
      consume_module_type_with_constraint p ~signature;
      loop (complete p marker Syntax_kind.WITH_MODULE_TYPE)
    ) else
      lhs
  in
  loop (parse_module_type_atom p ~signature)

and parse_module_type_expr = fun p ~signature ->
  let marker = start_node p in
  ignore (parse_module_type_bp p ~signature);
  consume_same_line_attribute_suffixes p;
  complete p marker Syntax_kind.MODULE_TYPE_EXPR

and parse_module_type_decl = fun p ~signature ->
  let marker = start_node p in
  let head = start_node p in
  expect p Syntax_kind.MODULE_KW (invalid_expression p);
  expect p Syntax_kind.TYPE_KW (invalid_type_expression p);
  consume_shortcut_extension_modifier p;
  consume_declaration_attributes p;
  (
    if at p Syntax_kind.IDENT then
      bump p
    else
      Event.Buffer.error p.events (missing_module_type_name p)
  );
  ignore (complete p head Syntax_kind.MODULE_TYPE_DECL_HEAD);
  if at p Syntax_kind.EQ then
    let body = start_node p in
    (
      bump p;
      ignore (parse_module_type_expr p ~signature)
    );
    ignore (complete p body Syntax_kind.MODULE_TYPE_DECL_BODY)
  else if is_eof p || at_item_boundary p ~signature then
    ()
  else
    consume_until_item_boundary p ~signature;
  ignore (complete p marker Syntax_kind.MODULE_TYPE_DECL)

and parse_module_decl_member = fun p ~signature ~first ->
  let marker = start_node p in
  if first then (
    expect p Syntax_kind.MODULE_KW (invalid_expression p);
    consume_shortcut_extension_modifier p;
    ignore (bump_if p Syntax_kind.REC_KW);
    consume_declaration_attributes p
  ) else (
    expect p Syntax_kind.AND_KW (invalid_expression p);
    consume_declaration_attributes p
  );
  (
    if at p Syntax_kind.IDENT || at p Syntax_kind.UNDERSCORE then
      bump p
    else
      Event.Buffer.error p.events (invalid_module_name p)
  );
  consume_module_decl_head_tail p ~signature;
  if at p Syntax_kind.COLON then (
    bump p;
    if is_eof p || at p Syntax_kind.EQ || at_item_boundary p ~signature then
      Event.Buffer.error p.events (missing_module_type_expr p)
    else
      ignore (parse_module_type_expr p ~signature)
  );
  if at p Syntax_kind.EQ then (
    bump p;
    if is_eof p || at_item_boundary p ~signature then
      Event.Buffer.error p.events (missing_module_expr p)
    else
      ignore (parse_module_expr p ~signature)
  ) else if at p Syntax_kind.STRUCT_KW || at p Syntax_kind.IDENT then (
    Event.Buffer.missing p.events ~kind:Syntax_kind.EQ ~offset:(current_offset p);
    Event.Buffer.error p.events (missing_module_decl_equals p);
    ignore (parse_module_expr p ~signature)
  ) else if not (is_eof p || at p Syntax_kind.AND_KW || at_item_boundary p ~signature) then
    consume_until_module_expr_boundary p ~signature;
  ignore (complete p marker Syntax_kind.MODULE_DECL_MEMBER)

and parse_module_decl = fun p ~signature ->
  let marker = start_node p in
  ignore (parse_module_decl_member p ~signature ~first:true);
  let rec parse_tail_members () =
    if at p Syntax_kind.AND_KW then (
      ignore (parse_module_decl_member p ~signature ~first:false);
      parse_tail_members ()
    )
  in
  parse_tail_members ();
  ignore (complete p marker Syntax_kind.MODULE_DECL)

and parse_external_decl = fun p ~signature ->
  let marker = start_node p in
  expect p Syntax_kind.EXTERNAL_KW (invalid_expression p);
  consume_shortcut_extension_modifier p;
  consume_declaration_attributes p;
  ignore (consume_value_name p ~signature);
  if at p Syntax_kind.COLON then (
    bump p;
    parse_type_expr p ~allow_leading_poly_type_after_newline:true ~stop_at_arrow:false
  ) else (
    Event.Buffer.missing p.events ~kind:Syntax_kind.COLON ~offset:(current_offset p);
    Event.Buffer.error p.events (missing_external_colon p)
  );
  if not (is_eof p || at_item_boundary p ~signature) then
    consume_until_item_boundary p ~signature;
  ignore (complete p marker Syntax_kind.EXTERNAL_DECL)

and parse_val_decl = fun p ~signature ->
  let marker = start_node p in
  expect p Syntax_kind.VAL_KW (invalid_type_expression p);
  if not (consume_value_name p ~signature) then
    Event.Buffer.error p.events (missing_type_name p);
  if at p Syntax_kind.COLON then (
    bump p;
    parse_type_expr p ~allow_leading_poly_type_after_newline:true ~stop_at_arrow:false
  ) else (
    Event.Buffer.missing p.events ~kind:Syntax_kind.COLON ~offset:(current_offset p);
    Event.Buffer.error p.events (invalid_type_expression p)
  );
  if not (is_eof p || at_item_boundary p ~signature) then
    consume_until_item_boundary p ~signature;
  ignore (complete p marker Syntax_kind.VAL_DECL)

and parse_exception_alias = fun p ->
  let marker = start_node p in
  expect p Syntax_kind.EQ (invalid_expression p);
  if at p Syntax_kind.IDENT then
    ignore (parse_path_expr p)
  else (
    Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p);
    Event.Buffer.error p.events (missing_module_path p)
  );
  complete p marker Syntax_kind.EXCEPTION_ALIAS

and parse_exception_payload = fun p ~signature ->
  let marker = start_node p in
  expect p Syntax_kind.OF_KW (invalid_type_expression p);
  if is_eof p || at_item_boundary p ~signature then (
    Event.Buffer.missing p.events ~kind:Syntax_kind.IDENT ~offset:(current_offset p);
    Event.Buffer.error p.events (invalid_type_expression_at_previous_end p)
  ) else if at p Syntax_kind.LBRACE then
    ignore (parse_record_type p)
  else
    ignore (parse_type_expr p ~allow_leading_poly_type_after_newline:false ~stop_at_arrow:false);
  complete p marker Syntax_kind.EXCEPTION_PAYLOAD

and parse_exception_decl = fun p ~signature ->
  let marker = start_node p in
  let head = start_node p in
  expect p Syntax_kind.EXCEPTION_KW (invalid_expression p);
  consume_shortcut_extension_modifier p;
  consume_declaration_attributes p;
  if at p Syntax_kind.IDENT then
    bump p
  else
    Event.Buffer.error p.events (missing_exception_name p);
  ignore (complete p head Syntax_kind.EXCEPTION_DECL_HEAD);
  (
    match current_kind p with
    | Syntax_kind.EQ -> ignore (parse_exception_alias p)
    | Syntax_kind.OF_KW -> ignore (parse_exception_payload p ~signature)
    | _ -> ()
  );
  if not (is_eof p || at_item_boundary p ~signature) then
    consume_until_item_boundary p ~signature;
  ignore (complete p marker Syntax_kind.EXCEPTION_DECL)

and parse_open_decl = fun p ~signature ->
  let marker = start_node p in
  expect p Syntax_kind.OPEN_KW (invalid_expression p);
  if is_eof p || at_item_boundary p ~signature then
    Event.Buffer.error p.events (missing_module_path p)
  else
    consume_until_item_boundary p ~signature;
  ignore (complete p marker Syntax_kind.OPEN_DECL)

and parse_include_decl = fun p ~signature ->
  let marker = start_node p in
  expect p Syntax_kind.INCLUDE_KW (invalid_expression p);
  let missing_target =
    is_eof p
    || at_end_keyword p
    || (leading_trivia_contains_newline p && at_item_boundary p ~signature)
  in
  if missing_target then
    Event.Buffer.error p.events (invalid_expression p)
  else if signature then
    ignore (parse_module_type_expr p ~signature)
  else
    ignore (parse_module_expr p ~signature);
  if not (is_eof p || at_item_boundary p ~signature) then
    consume_until_item_boundary p ~signature;
  ignore (complete p marker Syntax_kind.INCLUDE_DECL)

and parse_opaque_decl = fun p ~signature kind diagnostic ->
  consume_opaque_until_item_boundary
    p
    ~signature
    kind
    diagnostic

and parse_expr_item = fun p ~signature ->
  let marker = start_node p in
  ignore (parse_expression p ~signature ~stop_at_item:true 0);
  ignore (complete p marker Syntax_kind.EXPR_ITEM)

and parse_bracketed_item_shell = fun p kind ->
  let marker = start_node p in
  bump p;
  (
    match kind with
    | Syntax_kind.EXTENSION_ITEM ->
        consume_extension_sigils p;
        consume_extension_payload p
    | Syntax_kind.ATTRIBUTE_ITEM ->
        consume_attribute_sigils p;
        consume_balanced_until p ~closer:Syntax_kind.RBRACKET 0
    | _ -> ()
  );
  expect p Syntax_kind.RBRACKET (invalid_expression p);
  ignore (complete p marker kind)

and parse_structure_item = fun p ->
  let marker = start_node p in
  (
    match current_kind p with
    | Syntax_kind.LBRACKET when is_extension_shell p ->
        parse_bracketed_item_shell p Syntax_kind.EXTENSION_ITEM
    | Syntax_kind.LBRACKET when is_attribute_shell p ->
        parse_bracketed_item_shell p Syntax_kind.ATTRIBUTE_ITEM
    | Syntax_kind.LET_KW when binding_operator_suffix (peek_kind p 1) ->
        parse_expr_item p ~signature:false
    | Syntax_kind.LET_KW when Syntax_kind.(peek_kind p 1 = OPEN_KW)
    || Syntax_kind.(peek_kind p 1 = MODULE_KW) -> parse_expr_item p ~signature:false
    | Syntax_kind.LET_KW -> parse_let_decl p ~signature:false
    | Syntax_kind.TYPE_KW when starts_with_type_extension_decl p ->
        parse_type_extension_decl p ~signature:false
    | Syntax_kind.TYPE_KW -> parse_type_decl p ~signature:false
    | Syntax_kind.MODULE_KW when starts_with_module_type_decl_keyword p ->
        parse_module_type_decl p ~signature:false
    | Syntax_kind.MODULE_KW -> parse_module_decl p ~signature:false
    | Syntax_kind.OPEN_KW -> parse_open_decl p ~signature:false
    | Syntax_kind.INCLUDE_KW -> parse_include_decl p ~signature:false
    | Syntax_kind.EXTERNAL_KW -> parse_external_decl p ~signature:false
    | Syntax_kind.EXCEPTION_KW -> parse_exception_decl p ~signature:false
    | _ -> parse_expr_item p ~signature:false
  );
  ignore (complete p marker Syntax_kind.STRUCTURE_ITEM)

and parse_signature_item = fun p ->
  let marker = start_node p in
  (
    match current_kind p with
    | Syntax_kind.LBRACKET when is_extension_shell p ->
        parse_bracketed_item_shell p Syntax_kind.EXTENSION_ITEM
    | Syntax_kind.LBRACKET when is_attribute_shell p ->
        parse_bracketed_item_shell p Syntax_kind.ATTRIBUTE_ITEM
    | Syntax_kind.VAL_KW -> parse_val_decl p ~signature:true
    | Syntax_kind.TYPE_KW when starts_with_type_extension_decl p ->
        parse_type_extension_decl p ~signature:true
    | Syntax_kind.TYPE_KW -> parse_type_decl p ~signature:true
    | Syntax_kind.MODULE_KW when starts_with_module_type_decl_keyword p ->
        parse_module_type_decl p ~signature:true
    | Syntax_kind.MODULE_KW -> parse_module_decl p ~signature:true
    | Syntax_kind.OPEN_KW -> parse_open_decl p ~signature:true
    | Syntax_kind.INCLUDE_KW -> parse_include_decl p ~signature:true
    | Syntax_kind.EXTERNAL_KW -> parse_external_decl p ~signature:true
    | Syntax_kind.EXCEPTION_KW -> parse_exception_decl p ~signature:true
    | _ ->
        Event.Buffer.error p.events (invalid_expression p);
        parse_opaque_decl p ~signature:true Syntax_kind.ERROR (invalid_expression p)
  );
  ignore (complete p marker Syntax_kind.SIGNATURE_ITEM)

and consume_phrase_separators = fun p ->
  if at p Syntax_kind.SEMI && Syntax_kind.(peek_kind p 1 = SEMI) then (
    bump p;
    bump p;
    consume_phrase_separators p
  )

and parse_file = fun kind source ->
  let p = create source in
  let root = start_node p in
  let body = start_node p in
  let rec parse_structure_items () =
    consume_phrase_separators p;
    if not (is_eof p) then (
      let before = p.pos in
      parse_structure_item p;
      ensure_progress p before (invalid_expression p);
      parse_structure_items ()
    )
  in
  let rec parse_signature_items () =
    consume_phrase_separators p;
    if not (is_eof p) then (
      let before = p.pos in
      parse_signature_item p;
      ensure_progress p before (unexpected_signature_item p);
      parse_signature_items ()
    )
  in
  (
    match kind with
    | `Implementation ->
        parse_structure_items ();
        ignore (complete p body Syntax_kind.IMPLEMENTATION)
    | `Interface ->
        parse_signature_items ();
        ignore (complete p body Syntax_kind.INTERFACE)
  );
  if is_eof p then
    bump p;
  ignore (complete p root Syntax_kind.SOURCE_FILE);
  let tree = Syntax_tree.Builder.finish p.events in
  {
    source;
    kind;
    tokens = p.token_stream;
    tree;
    diagnostics = Event.Buffer.diagnostics p.events;
  }

let parse_implementation = fun source -> parse_file `Implementation source

let parse_interface = fun source -> parse_file `Interface source

let parse = fun ~filename source ->
  match Path.extension filename with
  | Some ".mli" -> parse_interface source
  | _ -> parse_implementation source

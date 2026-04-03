open Std
open Std.Sync
open Std.Collections

type error = {
  message: string;
  syntax_kind: Syntax_kind.t;
  span: Ceibo.Span.t;
  context: string list;
}

type record_field_item =
  | RecordField of Cst.RecordField.t
  | Comment of Cst.comment
  | Docstring of Cst.docstring
  | TrailingComment of Cst.comment
  | TrailingDocstring of Cst.docstring

type object_member_item =
  | ObjectMember of Cst.ObjectMember.t
  | Comment of Cst.comment
  | Docstring of Cst.docstring

type class_field_item =
  | ClassField of Cst.ClassField.t
  | Comment of Cst.comment
  | Docstring of Cst.docstring

type class_type_field_item =
  | ClassTypeField of Cst.ClassTypeField.t
  | Comment of Cst.comment
  | Docstring of Cst.docstring

exception Bail of error

let bail = fun ~message ~syntax_node ~context ->
  raise
    (Bail {
      message;
      syntax_kind = Ceibo.Red.SyntaxNode.kind syntax_node;
      span = Ceibo.Red.SyntaxNode.span syntax_node;
      context
    })

let unsupported_parameter = fun node ->
  bail
    ~message:"unsupported parameter shape during Ceibo -> CST lifting"
    ~syntax_node:node
    ~context:[ "parameter" ]

let unsupported_pattern = fun node ->
  bail
    ~message:"unsupported pattern shape during Ceibo -> CST lifting"
    ~syntax_node:node
    ~context:[ "pattern" ]

let unsupported_expression = fun node ->
  bail
    ~message:"unsupported expression shape during Ceibo -> CST lifting"
    ~syntax_node:node
    ~context:[ "expression" ]

let unsupported_module_expression = fun node ->
  bail
    ~message:"unsupported module expression shape during Ceibo -> CST lifting"
    ~syntax_node:node
    ~context:[ "module_expression" ]

let unsupported_class_expression = fun node ->
  bail
    ~message:"unsupported class expression shape during Ceibo -> CST lifting"
    ~syntax_node:node
    ~context:[ "class_expression" ]

let unsupported_class_type = fun node ->
  bail
    ~message:"unsupported class type shape during Ceibo -> CST lifting"
    ~syntax_node:node
    ~context:[ "class_type" ]

let unsupported_item = fun node ->
  bail
    ~message:"unsupported structure item during Ceibo -> CST lifting"
    ~syntax_node:node
    ~context:[ "item" ]

let is_trivia = fun kind -> let open Syntax_kind in kind = WHITESPACE || kind = COMMENT || kind = DOCSTRING

let token = fun syntax_tok -> Cst.Token.{ syntax_token = syntax_tok }

let keyword_let_text = "let"

let keyword_and_text = "and"

let keyword_in_text = "in"

let equals_text = "="

let semicolon_text = ";"

let open_bracket_text = "["

let close_bracket_text = "]"

let close_brace_text = "}"

let dot_text = "."

let colon_text = ":"

let question_mark_text = "?"

let at_text = "@"

let double_at_text = "@@"

let triple_at_text = "@@@"

let percent_text = "%"

let double_percent_text = "%%"

let triple_percent_text = "%%%"

let attribute_sigil_texts = [ at_text; double_at_text; triple_at_text ]

let extension_sigil_texts = [ percent_text; double_percent_text; triple_percent_text ]

let annotation_sigil_texts = attribute_sigil_texts @ extension_sigil_texts

let is_annotation_sigil_text = fun text ->
  List.exists (String.equal text) annotation_sigil_texts

let synthetic_token = fun ~kind ~text ~start_offset ~end_offset ->
  let green_token = Ceibo.Green.make_token ~leading_trivia:[] ~kind ~text ~width:(String.length text) in
  let syntax_token = Ceibo.Red.new_token
    green_token
    (Ceibo.Span.make ~start:start_offset ~end_:end_offset) in
  token syntax_token

let synthetic_syntax_node_wrapping_token = fun docstring_syntax_token ->
  let token_span = Ceibo.Red.SyntaxToken.span docstring_syntax_token in
  let prefix_width = Int.max 0 token_span.start in
  let prefix_token = Ceibo.Green.make_token
    ~leading_trivia:[]
    ~kind:Syntax_kind.WHITESPACE
    ~text:(String.make prefix_width ' ')
    ~width:prefix_width in
  let wrapped_token = Ceibo.Green.Token (Ceibo.Red.SyntaxToken.green docstring_syntax_token) in
  let wrapped_node = Ceibo.Green.make_node
    ~kind:(Ceibo.Red.SyntaxToken.kind docstring_syntax_token)
    ~children:[ wrapped_token ] in
  let root = Ceibo.Green.make_node
    ~kind:Syntax_kind.SOURCE_FILE
    ~children:[ Ceibo.Green.Token prefix_token; Ceibo.Green.Node wrapped_node ] in
  match Ceibo.Red.SyntaxNode.child (Ceibo.Red.new_root root) 1 with
  | Some (Ceibo.Red.Node node) -> node
  | _ -> panic "synthetic_syntax_node_wrapping_token: missing wrapped child node"

let docstring_kind_from_text = fun comment_text ->
  let len = String.length comment_text in
  if len < 5 then
    Cst.Docstring.Ordinary
  else
    let body = String.sub comment_text 3 (len - 5) |> String.trim in
    if String.length body > 0 && (Char.equal body.[0] '{' || Char.equal body.[0] '#') then
      Cst.Docstring.Section
    else
      Cst.Docstring.Ordinary

let docstring_from_token = fun docstring_syntax_token ->
  Cst.Docstring.{
    syntax_node = synthetic_syntax_node_wrapping_token docstring_syntax_token;
    docstring_token =
      Cst.Token.{ syntax_token = docstring_syntax_token };
    kind = docstring_kind_from_text (Ceibo.Red.SyntaxToken.text docstring_syntax_token);
  }

let comment_from_token = fun comment_syntax_token ->
  Cst.Comment.{
    syntax_node = synthetic_syntax_node_wrapping_token comment_syntax_token;
    comment_token =
      Cst.Token.{ syntax_token = comment_syntax_token };
  }

let syntax_token_from_trivia = fun trivia ->
  let span = Ceibo.Red.SyntaxTrivia.span trivia in
  synthetic_token
    ~kind:(Ceibo.Red.SyntaxTrivia.kind trivia)
    ~text:(Ceibo.Red.SyntaxTrivia.text trivia)
    ~start_offset:span.start
    ~end_offset:span.end_
  |> Cst.Token.syntax_token

let trivia_from_token = fun syntax_token ->
  match Ceibo.Red.SyntaxToken.kind syntax_token with
  | Syntax_kind.COMMENT -> Some (Cst.Trivia.Comment (comment_from_token syntax_token))
  | Syntax_kind.DOCSTRING -> Some (Cst.Trivia.Docstring (docstring_from_token syntax_token))
  | _ -> None

let trivia_from_syntax_trivia = fun trivia ->
  match Ceibo.Red.SyntaxTrivia.kind trivia with
  | Syntax_kind.COMMENT -> Some (Cst.Trivia.Comment (comment_from_token
    (syntax_token_from_trivia trivia)))
  | Syntax_kind.DOCSTRING -> Some (Cst.Trivia.Docstring (docstring_from_token
    (syntax_token_from_trivia trivia)))
  | _ -> None

let leading_trivia_from_syntax_token = fun syntax_token ->
  Ceibo.Red.SyntaxToken.leading_trivia syntax_token |> List.filter_map trivia_from_syntax_trivia

let span_contains = fun (outer: Ceibo.Span.t) (inner: Ceibo.Span.t) ->
  inner.start >= outer.start && inner.end_ <= outer.end_

let standalone_trivia_items_from_node = fun ~comment_item_of_comment ~docstring_item_of_docstring ~after_offset ~excluded_spans node ->
  Ceibo.Red.SyntaxNode.tokens node |> List.filter_map
    (fun syntax_token ->
      let token_span = Ceibo.Red.SyntaxToken.span syntax_token in
      if
        token_span.start < after_offset
        || List.exists (fun excluded_span -> span_contains excluded_span token_span) excluded_spans
      then
        None
      else
        match Ceibo.Red.SyntaxToken.kind syntax_token with
        | Syntax_kind.COMMENT -> Some (comment_item_of_comment (comment_from_token syntax_token))
        | Syntax_kind.DOCSTRING -> Some (docstring_item_of_docstring
          (docstring_from_token syntax_token))
        | _ -> None)

let standalone_trivia_item_from_token = fun ~comment_item_of_comment ~docstring_item_of_docstring syntax_token ->
  match Ceibo.Red.SyntaxToken.kind syntax_token with
  | Syntax_kind.COMMENT -> Some (comment_item_of_comment (comment_from_token syntax_token))
  | Syntax_kind.DOCSTRING -> Some (docstring_item_of_docstring (docstring_from_token syntax_token))
  | _ -> None

let rec syntax_node_root = fun node ->
  match Ceibo.Red.SyntaxNode.parent node with
  | Some parent -> syntax_node_root parent
  | None -> node

let source_text_from_syntax_tokens = fun syntax_tokens ->
  let pieces =
    syntax_tokens
    |> List.concat_map
      (fun syntax_token ->
        let trivia_pieces = Ceibo.Red.SyntaxToken.leading_trivia syntax_token
        |> List.map
          (fun syntax_trivia ->
            (Ceibo.Red.SyntaxTrivia.span syntax_trivia, Ceibo.Red.SyntaxTrivia.text syntax_trivia)) in
        let token_piece = (
          Ceibo.Red.SyntaxToken.span syntax_token,
          Ceibo.Red.SyntaxToken.text syntax_token
        ) in
        trivia_pieces @ [ token_piece ])
  in
  let source_length =
    pieces
    |> List.fold_left
      (fun acc ((span: Ceibo.Span.t), _text) ->
        Int.max acc span.end_)
      0
  in
  let buffer = IO.Buffer.create source_length in
  let next_offset = ref 0 in
  pieces |> List.iter
    (fun (({ Ceibo.Span.start; end_ } as _span), piece_text) ->
      let piece_length = Int.min (String.length piece_text) (Int.max 0 (end_ - start)) in
      let gap_length = Int.max 0 (start - !next_offset) in
      if gap_length > 0 then
        IO.Buffer.add_string buffer (String.make gap_length ' ');
      if piece_length > 0 then
        IO.Buffer.add_substring buffer piece_text 0 piece_length;
      next_offset := Int.max !next_offset end_);
  IO.Buffer.contents buffer

let source_text_of_syntax_node_tree = fun node ->
  syntax_node_root node |> Ceibo.Red.SyntaxNode.tokens |> source_text_from_syntax_tokens

let standalone_trivia_item_from_lexed_token = fun ~source ~comment_item_of_comment ~docstring_item_of_docstring (
  lexed_token: Token.t
) ->
  let syntax_kind =
    match lexed_token.kind with
    | Token.Comment _ -> Some Syntax_kind.COMMENT
    | Token.Docstring _ -> Some Syntax_kind.DOCSTRING
    | _ -> None
  in
  match syntax_kind with
  | None -> None
  | Some kind ->
      let { Ceibo.Span.start; end_ } = lexed_token.span in
      let source_len = String.length source in
      let safe_start = Int.max 0 start in
      let safe_len =
        if end_ <= safe_start || safe_start >= source_len then
          0
        else
          Int.min (end_ - safe_start) (source_len - safe_start)
      in
      let text =
        if safe_len = 0 then
          ""
        else
          String.sub source safe_start safe_len
      in
      let syntax_token = synthetic_token ~kind ~text ~start_offset:start ~end_offset:end_
      |> Cst.Token.syntax_token in
      standalone_trivia_item_from_token ~comment_item_of_comment ~docstring_item_of_docstring syntax_token

let same_token_span = fun (left: Token.t) syntax_token ->
  let right = Ceibo.Red.SyntaxToken.span syntax_token in
  left.span.start = right.start && left.span.end_ = right.end_

let original_token_for_syntax_token = fun tokens syntax_token ->
  tokens |> List.find_opt (fun token -> same_token_span token syntax_token)

let leading_trivia_tokens_for_item = fun ~tokens syntax_node ->
  match Ceibo.Red.SyntaxNode.tokens syntax_node with
  | first_token :: _ -> (
      match original_token_for_syntax_token tokens first_token with
      | Some token -> List.map Token.trivia_to_token token.leading_trivia
      | None -> []
    )
  | [] -> []

let leading_trivia_syntax_tokens_for_item = fun syntax_node ->
  match Ceibo.Red.SyntaxNode.tokens syntax_node with
  | first_token :: _ -> Ceibo.Red.SyntaxToken.leading_trivia first_token |> List.map syntax_token_from_trivia
  | [] -> []

let record_field_items_from_leading_trivia = fun leading_trivia ->
  let rec loop = fun saw_newline acc ->
    function
    | [] -> List.rev acc
    | syntax_trivia :: rest ->
        let syntax_token = syntax_token_from_trivia syntax_trivia in
        let kind = Ceibo.Red.SyntaxToken.kind syntax_token in
        if kind = Syntax_kind.WHITESPACE then
          let saw_newline =
            saw_newline || String.contains (Ceibo.Red.SyntaxToken.text syntax_token) "\n" in
          loop saw_newline acc rest
        else
          let item: record_field_item option =
            if saw_newline then
              standalone_trivia_item_from_token
                ~comment_item_of_comment:(fun comment -> (Comment comment: record_field_item))
                ~docstring_item_of_docstring:(fun docstring ->
                  (Docstring docstring: record_field_item))
                syntax_token
            else
              standalone_trivia_item_from_token
                ~comment_item_of_comment:(fun comment -> (TrailingComment comment: record_field_item))
                ~docstring_item_of_docstring:(fun docstring ->
                  (TrailingDocstring docstring: record_field_item))
                syntax_token
          in
          loop false
            (
              match item with
              | Some item -> item :: acc
              | None -> acc
            )
            rest
  in
  loop false [] leading_trivia

let leading_record_field_items_for_field = fun field ->
  match Ceibo.Red.SyntaxNode.tokens (Cst.RecordField.syntax_node field) with
  | first_token :: _ -> record_field_items_from_leading_trivia
    (Ceibo.Red.SyntaxToken.leading_trivia first_token)
  | [] -> []

let eof_leading_trivia_tokens = fun tokens ->
  match List.rev tokens with
  | eof :: _ when eof.Token.kind = Token.EOF -> List.map Token.trivia_to_token eof.Token.leading_trivia
  | _ -> []

let compare_syntax_token_by_span = fun left right ->
  let left_span = Ceibo.Red.SyntaxToken.span left in
  let right_span = Ceibo.Red.SyntaxToken.span right in
  if not (Int.equal left_span.start right_span.start) then
    Int.compare left_span.start right_span.start
  else
    Int.compare left_span.end_ right_span.end_

let source_file_phrase_separator_tokens = fun root ->
  Ceibo.Red.SyntaxNode.direct_tokens root
  |> List.sort compare_syntax_token_by_span
  |> List.filter_map
    (fun syntax_token ->
      if String.equal (Ceibo.Red.SyntaxToken.text syntax_token) ";" then
        Some { Cst.Token.syntax_token = syntax_token }
      else
        None)

let phrase_separator_tokens_between = fun tokens ~start ~end_ ->
  let rec loop = fun acc ->
    function
    | [] -> List.rev acc
    | token :: rest ->
        let span = Cst.Token.span token in
        if span.end_ <= start then
          loop acc rest
        else if span.start >= end_ then
          List.rev acc
        else
          loop (token :: acc) rest
  in
  loop [] tokens

let source_file_items_from_child = fun ~comment_item_of_comment ~docstring_item_of_docstring ~owned_trivia_spans_of_item items_from_node ->
  function
  | Ceibo.Red.Node node when not (is_trivia (Ceibo.Red.SyntaxNode.kind node)) ->
      let items = items_from_node node in
      let owned_trivia_spans = items |> List.concat_map owned_trivia_spans_of_item in
      let after_offset =
        let full_span = Ceibo.Red.SyntaxNode.span node in
        let nontrivia_tokens = Ceibo.Red.SyntaxNode.tokens node
        |> List.filter (fun tok -> not (is_trivia (Ceibo.Red.SyntaxToken.kind tok))) in
        match nontrivia_tokens with
        | [] -> full_span.end_
        | first :: rest ->
            let last =
              List.fold_left (fun _ token -> token) first rest
            in
            (Ceibo.Red.SyntaxToken.span last).end_
      in
      items
      @ standalone_trivia_items_from_node
        ~comment_item_of_comment
        ~docstring_item_of_docstring
        ~after_offset
        ~excluded_spans:owned_trivia_spans
        node
  | Ceibo.Red.Token syntax_token ->
      standalone_trivia_item_from_token ~comment_item_of_comment ~docstring_item_of_docstring syntax_token
      |> Option.to_list
  | _ ->
      []

let substring = fun text start length ->
  let text_length = String.length text in
  if length <= 0 || start >= text_length then
    ""
  else
    let safe_start = Int.max 0 start in
    String.sub text safe_start (Int.min length (text_length - safe_start))

let source_position_indentation_before = fun source position ->
  let safe_position = Int.max 0 (Int.min position (String.length source)) in
  let rec find_line_start index =
    if index <= 0 then
      0
    else if source.[index - 1] = '\n' then
      index
    else
      find_line_start (index - 1)
  in
  let line_start = find_line_start safe_position in
  let rec count index indentation =
    if index >= safe_position || index >= String.length source then
      Some indentation
    else
      match source.[index] with
      | ' '
      | '\t' -> count (index + 1) (indentation + 1)
      | '\r'
      | '\n'
      | _ -> Some indentation
  in
  count line_start 0

let is_ascii_alpha = function
  | 'a' .. 'z'
  | 'A' .. 'Z' -> true
  | _ -> false

let find_char_from = fun text start target ->
  let rec loop index =
    if index >= String.length text then
      None
    else if Char.equal (String.get text index) target then
      Some index
    else
      loop (index + 1)
  in
  loop start

let split_trailing_alpha_suffix = fun text ->
  let rec loop index =
    if index < 0 then
      0
    else if is_ascii_alpha (String.get text index) then
      loop (index - 1)
    else
      index + 1
  in
  let suffix_start = loop (String.length text - 1) in
  (
    substring text 0 suffix_start,
    if suffix_start < String.length text then
      Some (substring text suffix_start (String.length text - suffix_start))
    else
      None
  )

let string_delimiter_and_contents = fun text ->
  let len = String.length text in
  if len > 0 && Char.equal (String.get text 0) '"' then
    let terminated = len > 1 && Char.equal (String.get text (len - 1)) '"' in
    let contents_end =
      if terminated then
        len - 1
      else
        len
    in
    (Cst.DoubleQuote, substring text 1 (contents_end - 1), terminated)
  else if len > 0 && Char.equal (String.get text 0) '{' then
    match find_char_from text 1 '|' with
    | Some pipe_index ->
        let marker = substring text 1 (pipe_index - 1) in
        let closing = "|" ^ marker ^ "}" in
        let closing_len = String.length closing in
        let terminated =
          len >= pipe_index + 1 + closing_len
          && String.equal (substring text (len - closing_len) closing_len) closing in
        let contents_start = pipe_index + 1 in
        let contents_end =
          if terminated then
            len - closing_len
          else
            len
        in
        (
          Cst.Quoted { marker },
          substring text contents_start (contents_end - contents_start),
          terminated
        )
    | None -> (Cst.Quoted { marker = "" }, substring text 1 (len - 1), false)
  else
    (Cst.DoubleQuote, text, false)

let integer_parts = fun text ->
  let base, prefix =
    let starts_with prefix =
      let prefix_len = String.length prefix in
      if String.length text < prefix_len then
        false
      else
        String.equal (substring text 0 prefix_len) prefix
    in
    if starts_with "0x" || starts_with "0X" then
      (Cst.Hexadecimal, Some (substring text 0 2))
    else if starts_with "0o" || starts_with "0O" then
      (Cst.Octal, Some (substring text 0 2))
    else if starts_with "0b" || starts_with "0B" then
      (Cst.Binary, Some (substring text 0 2))
    else
      (Cst.Decimal, None)
  in
  let digit_start =
    match prefix with
    | Some prefix -> String.length prefix
    | None -> 0
  in
  let is_digit =
    match base with
    | Cst.Decimal -> (
        function
        | '0' .. '9'
        | '_' -> true
        | _ -> false
      )
    | Cst.Hexadecimal -> (
        function
        | '0' .. '9'
        | 'a' .. 'f'
        | 'A' .. 'F'
        | '_' -> true
        | _ -> false
      )
    | Cst.Octal -> (
        function
        | '0' .. '7'
        | '_' -> true
        | _ -> false
      )
    | Cst.Binary -> (
        function
        | '0'
        | '1'
        | '_' -> true
        | _ -> false
      )
  in
  let rec find_suffix_start index =
    if index >= String.length text then
      index
    else if is_digit (String.get text index) then
      find_suffix_start (index + 1)
    else
      index
  in
  let suffix_start = find_suffix_start digit_start in
  let digits = substring text digit_start (suffix_start - digit_start) in
  let suffix =
    if suffix_start < String.length text then
      Some (substring text suffix_start (String.length text - suffix_start))
    else
      None
  in
  (base, prefix, digits, suffix)

let float_parts = fun text ->
  let body, suffix = split_trailing_alpha_suffix text in
  let exponent_marker_index =
    match find_char_from body 0 'e' with
    | Some _ as found -> found
    | None -> find_char_from body 0 'E'
  in
  let body_without_exponent, exponent =
    match exponent_marker_index with
    | Some index ->
        let exponent_body = substring body (index + 1) (String.length body - index - 1) in
        let sign, digits =
          if String.length exponent_body = 0 then
            (None, "")
          else
            match String.get exponent_body 0 with
            | '+' -> (Some Cst.Positive, substring exponent_body 1 (String.length exponent_body - 1))
            | '-' -> (Some Cst.Negative, substring exponent_body 1 (String.length exponent_body - 1))
            | _ -> (None, exponent_body)
        in
        (substring body 0 index, Some { Cst.marker = substring body index 1; sign; digits })
    | None -> (body, None)
  in
  let dot_index = find_char_from body_without_exponent 0 '.' in
  let integral_digits, fractional_digits =
    match dot_index with
    | Some index -> (
      substring body_without_exponent 0 index,
      substring body_without_exponent (index + 1) (String.length body_without_exponent - index - 1)
    )
    | None -> (body_without_exponent, "")
  in
  (integral_digits, fractional_digits, exponent, suffix)

let char_contents = fun text ->
  let len = String.length text in
  if len > 0 && Char.equal (String.get text 0) '\'' then
    let contents_end =
      if len > 1 && Char.equal (String.get text (len - 1)) '\'' then
        len - 1
      else
        len
    in
    substring text 1 (contents_end - 1)
  else
    text

let is_constant_syntax_kind = function
  | Syntax_kind.STRING_LITERAL
  | Syntax_kind.INT_LITERAL
  | Syntax_kind.FLOAT_LITERAL
  | Syntax_kind.CHAR_LITERAL
  | Syntax_kind.BOOL_LITERAL
  | Syntax_kind.UNIT_LITERAL -> true
  | _ -> false

let direct_non_trivia_nodes = fun node -> Ceibo.Red.SyntaxNode.direct_nodes node

let direct_non_trivia_tokens = fun node -> Ceibo.Red.SyntaxNode.direct_tokens node

let previous_direct_token_with_text_in_parent = fun ~text node ->
  match Ceibo.Red.SyntaxNode.parent node with
  | Some parent ->
      let node_start = (Ceibo.Red.SyntaxNode.span node).start in
      direct_non_trivia_tokens parent |> List.rev |> List.find_opt
        (fun syntax_token ->
          let span = Ceibo.Red.SyntaxToken.span syntax_token in
          span.end_ <= node_start && String.equal (Ceibo.Red.SyntaxToken.text syntax_token) text)
  | None -> None

let subtree_non_trivia_tokens = fun node -> Ceibo.Red.SyntaxNode.tokens node

let span_of_syntax_node_nontrivia_bounds = fun syntax_node ->
  let full_span = Ceibo.Red.SyntaxNode.span syntax_node in
  match subtree_non_trivia_tokens syntax_node with
  | [] -> full_span
  | first :: rest ->
      let last =
        List.fold_left (fun _ token -> token) first rest
      in
      {
        Ceibo.Span.start = (Ceibo.Red.SyntaxToken.span first).start;
        end_ = (Ceibo.Red.SyntaxToken.span last).end_
      }

let owned_trivia_from_node_excluding_spans = fun _node _excluded_spans -> []

let owned_trivia_from_node_excluding_child_owned_spans = fun _node _child_owned_spans -> []

let owned_trivia_from_node = fun _node -> []

let trivia_span = fun trivia -> Cst.Trivia.token trivia |> Cst.Token.span

let trivia_same_span = fun left right ->
  let left_span = trivia_span left in
  let right_span = trivia_span right in
  left_span.start = right_span.start && left_span.end_ = right_span.end_

let remove_matching_trivia = fun removed trivia ->
  trivia
  |> List.filter
    (fun candidate -> not (List.exists (fun entry -> trivia_same_span entry candidate) removed))

let owned_trivia_without_matching = fun owned removed -> remove_matching_trivia removed owned

let owned_trivia_spans = fun owned -> owned |> List.map trivia_span

let record_field_owned_trivia_spans = fun _field -> []

let syntax_node_of_record_field_item = function
  | RecordField field -> Cst.RecordField.syntax_node field
  | Comment comment -> Cst.Comment.syntax_node comment
  | Docstring docstring -> Cst.Docstring.syntax_node docstring
  | TrailingComment comment -> Cst.Comment.syntax_node comment
  | TrailingDocstring docstring -> Cst.Docstring.syntax_node docstring

let record_field_item_owned_trivia_spans = function
  | RecordField field -> record_field_owned_trivia_spans field
  | Comment _
  | Docstring _
  | TrailingComment _
  | TrailingDocstring _ -> []

let record_field_item_of_comment: Cst.Comment.t -> record_field_item = fun comment -> Comment comment

let trailing_record_field_item_of_comment: Cst.Comment.t -> record_field_item = fun comment ->
  TrailingComment comment

let trailing_record_field_item_of_docstring: Cst.Docstring.t -> record_field_item = fun docstring ->
  TrailingDocstring docstring

let syntax_node_of_object_member_item = function
  | ObjectMember member -> Cst.ObjectMember.syntax_node member
  | Comment comment -> Cst.Comment.syntax_node comment
  | Docstring docstring -> Cst.Docstring.syntax_node docstring

let object_member_owned_trivia_spans = function
  | Cst.ObjectMember.Method _
  | Cst.ObjectMember.Value _
  | Cst.ObjectMember.Inherit _ -> []
  | Cst.ObjectMember.Extension _extension -> []
  | Cst.ObjectMember.Initializer _ -> []

let object_member_item_owned_trivia_spans = function
  | ObjectMember member -> object_member_owned_trivia_spans member
  | Comment _
  | Docstring _ -> []

let object_member_item_of_comment: Cst.Comment.t -> object_member_item = fun comment -> Comment comment

let record_field_item_of_docstring: Cst.Docstring.t -> record_field_item = fun docstring ->
  Docstring docstring

let object_member_item_of_docstring: Cst.Docstring.t -> object_member_item = fun docstring ->
  Docstring docstring

let syntax_node_of_class_field_item = function
  | ClassField field -> Cst.ClassField.syntax_node field
  | Comment comment -> Cst.Comment.syntax_node comment
  | Docstring docstring -> Cst.Docstring.syntax_node docstring

let rec class_field_owned_trivia_spans = function
  | Cst.ClassField.Method _
  | Cst.ClassField.Value _
  | Cst.ClassField.Inherit _
  | Cst.ClassField.Constraint _
  | Cst.ClassField.Initializer _ -> []
  | Cst.ClassField.Attribute { field; _ } -> class_field_owned_trivia_spans field
  | Cst.ClassField.Extension _ -> []

let class_field_item_owned_trivia_spans = function
  | ClassField field -> class_field_owned_trivia_spans field
  | Comment _
  | Docstring _ -> []

let class_field_item_of_comment: Cst.Comment.t -> class_field_item = fun comment -> Comment comment

let class_field_item_of_docstring: Cst.Docstring.t -> class_field_item = fun docstring -> Docstring docstring

let syntax_node_of_class_type_field_item = function
  | ClassTypeField field -> Cst.ClassTypeField.syntax_node field
  | Comment comment -> Cst.Comment.syntax_node comment
  | Docstring docstring -> Cst.Docstring.syntax_node docstring

let rec class_type_field_owned_trivia_spans = function
  | Cst.ClassTypeField.Inherit _
  | Cst.ClassTypeField.Value _
  | Cst.ClassTypeField.Method _
  | Cst.ClassTypeField.Constraint _ -> []
  | Cst.ClassTypeField.Attribute { field; _ } -> class_type_field_owned_trivia_spans field
  | Cst.ClassTypeField.Extension _ -> []

let class_type_field_item_owned_trivia_spans = function
  | ClassTypeField field -> class_type_field_owned_trivia_spans field
  | Comment _
  | Docstring _ -> []

let class_type_field_item_of_comment: Cst.Comment.t -> class_type_field_item = fun comment ->
  Comment comment

let class_type_field_item_of_docstring: Cst.Docstring.t -> class_type_field_item = fun docstring ->
  Docstring docstring

let rec variant_constructor_owned_trivia_spans = fun constructor ->
  let argument_spans =
    match Cst.VariantConstructor.arguments constructor with
    | Some (Cst.ConstructorArguments.Record { fields; _ }) -> fields |> List.concat_map record_field_owned_trivia_spans
    | Some (Cst.ConstructorArguments.Tuple _)
    | None -> []
  in
  argument_spans

and type_definition_owned_trivia_spans = function
  | Cst.TypeDefinition.Record { fields; _ } -> fields |> List.concat_map record_field_owned_trivia_spans
  | Cst.TypeDefinition.Variant { constructors; _ } -> constructors |> List.concat_map variant_constructor_owned_trivia_spans
  | Cst.TypeDefinition.Abstract
  | Cst.TypeDefinition.Alias _
  | Cst.TypeDefinition.Extensible _
  | Cst.TypeDefinition.FirstClassModule _
  | Cst.TypeDefinition.Object _
  | Cst.TypeDefinition.PolyVariant _ -> []

let type_declaration_owned_trivia_spans =
  let rec collect groups_rev decl =
    let groups_rev = type_definition_owned_trivia_spans (Cst.TypeDeclaration.type_definition decl)
    :: groups_rev in
    match Cst.TypeDeclaration.next_and_declaration decl with
    | Some next -> collect groups_rev next
    | None -> List.concat (List.rev groups_rev)
  in
  fun decl -> collect [] decl

let type_definition_owned_trivia_end = fun type_definition ->
  type_definition_owned_trivia_spans type_definition |> List.fold_left
    (fun acc (span: Ceibo.Span.t) ->
      Int.max acc span.end_)
    0

let value_declaration_owned_trivia_spans = fun _decl -> []

let structure_item_owned_trivia_spans = function
  | Cst.StructureItem.TypeDeclaration decl -> type_declaration_owned_trivia_spans decl
  | _ -> []

let signature_item_owned_trivia_spans = function
  | Cst.SignatureItem.TypeDeclaration decl -> type_declaration_owned_trivia_spans decl
  | Cst.SignatureItem.ValueDeclaration decl -> value_declaration_owned_trivia_spans decl
  | _ -> []

let trivia_indentation = fun ~source trivia ->
  source_position_indentation_before source ((trivia_span trivia).start)

let owned_trivia_with_trailing = fun _owned _trailing_trivia -> []

let owned_trivia_with_leading = fun _owned _leading_trivia -> []

let owned_trivia_append_leading = fun _owned _leading_trivia -> []

let owned_trivia_append_trailing = fun _owned _trailing_trivia -> []

let append_record_field_leading_trivia = fun field trivia ->
  let _ = trivia in
  field

let append_variant_constructor_leading_trivia = fun constructor trivia ->
  let _ = trivia in
  constructor

let append_type_declaration_leading_trivia = fun decl trivia ->
  let _ = trivia in
  decl

let remove_type_declaration_leading_trivia = fun decl trivia ->
  let _ = trivia in
  decl

let append_value_declaration_leading_trivia = fun decl trivia ->
  let _ = trivia in
  decl

let append_value_declaration_trailing_comment = fun (decl: Cst.value_declaration) comment ->
  { decl with trailing_comment = Some comment }

let sort_trivia_by_source = fun trivia ->
  List.sort
    (fun left right ->
      Int.compare (trivia_span left).start (trivia_span right).start)
    trivia

let dedup_trivia_by_span = fun trivia ->
  let rec loop = fun seen acc ->
    function
    | [] -> List.rev acc
    | candidate :: rest when List.exists (fun prior -> trivia_same_span prior candidate) seen -> loop
      seen
      acc
      rest
    | candidate :: rest -> loop (candidate :: seen) (candidate :: acc) rest
  in
  loop [] [] trivia

let is_section_docstring_text = fun comment_text ->
  match docstring_kind_from_text comment_text with
  | Cst.Docstring.Section -> true
  | Cst.Docstring.Ordinary -> false

let is_non_section_docstring_trivia = function
  | Cst.Trivia.Docstring docstring -> not (Cst.Docstring.is_section docstring)
  | Cst.Trivia.Comment _ -> false

let is_section_docstring_trivia = function
  | Cst.Trivia.Docstring docstring -> Cst.Docstring.is_section docstring
  | Cst.Trivia.Comment _ -> false

let normalize_value_declaration_owned_trivia = fun decl -> decl

let value_declaration_nontrivia_end = fun (decl: Cst.value_declaration) ->
  match List.rev (Ceibo.Red.SyntaxNode.tokens (Cst.CoreType.syntax_node decl.type_)) with
  | token :: _ -> (Ceibo.Red.SyntaxToken.span token).end_
  | [] -> span_of_syntax_node_nontrivia_bounds (Cst.CoreType.syntax_node decl.type_)
  |> fun span -> span.end_

let split_member_trailing_trivia = fun ~source:_ ~member_indent:_ owned -> (owned, [])

let has_blank_line_between_offsets = fun ~source ~start ~end_ ->
  let source_length = String.length source in
  let start = Int.max 0 (Int.min start source_length) in
  let end_ = Int.max start (Int.min end_ source_length) in
  let rec loop index newline_count =
    if index >= end_ then
      false
    else if Char.equal source.[index] '\n' then
      if newline_count + 1 >= 2 then
        true
      else
        loop (index + 1) (newline_count + 1)
    else
      loop (index + 1) newline_count
  in
  loop start 0

let record_field_nontrivia_end = fun field ->
  span_of_syntax_node_nontrivia_bounds (Cst.RecordField.syntax_node field) |> fun span -> span.end_

let record_field_indent = fun ~source field ->
  source_position_indentation_before
    source
    ((Ceibo.Red.SyntaxNode.span (Cst.RecordField.syntax_node field)).start)
  |> Option.unwrap_or ~default:0

let variant_constructor_nontrivia_end = fun constructor ->
  span_of_syntax_node_nontrivia_bounds (Cst.VariantConstructor.syntax_node constructor)
  |> fun span -> span.end_

let variant_constructor_indent = fun ~source constructor ->
  source_position_indentation_before
    source
    ((Ceibo.Red.SyntaxNode.span (Cst.VariantConstructor.syntax_node constructor)).start)
  |> Option.unwrap_or ~default:0

let trivia_is_comment = function
  | Cst.Trivia.Comment _ -> true
  | Cst.Trivia.Docstring _ -> false

let take_member_postfix_trivia_block = fun ~source ~member_end ~member_indent trivia ->
  let rec loop = fun previous_end acc ->
    function
    | (Cst.Trivia.Comment _ as entry) :: rest when trivia_is_comment entry
    && not
      (has_blank_line_between_offsets
        ~source
        ~start:(Option.unwrap_or ~default:member_end previous_end)
        ~end_:(trivia_span entry).start)
    && (
      match trivia_indentation ~source entry with
      | Some indent -> indent >= member_indent
      | None -> true
    ) -> loop (Some (trivia_span entry).end_) (entry :: acc) rest
    | rest -> (List.rev acc, rest)
  in
  loop None [] (sort_trivia_by_source trivia)

let trivia_between_offsets_from_syntax_node = fun ~node ~after_offset ~before_offset ->
  Ceibo.Red.SyntaxNode.tokens node
  |> List.concat_map
    (fun syntax_token -> Ceibo.Red.SyntaxToken.leading_trivia syntax_token |> List.filter_map trivia_from_syntax_trivia)
  |> List.filter
    (fun trivia ->
      let span = trivia_span trivia in
      span.start >= after_offset && span.end_ <= before_offset)
  |> sort_trivia_by_source

let type_declaration_nontrivia_end = fun decl ->
  span_of_syntax_node_nontrivia_bounds (Cst.TypeDeclaration.syntax_node decl) |> fun span -> span.end_

(* Explicit member-stream normalization is only needed for repeated member
   grammars whose members expose public owned trivia and get rendered
   independently. Today that means variant constructors and record fields.
   Exception declarations stay on the ordinary ordered-item path because they
   are already top-level/body items, while object type fields still lift as
   syntax-only members without owned_trivia and should not grow member-stream
   ownership rules until that public CST surface exists. *)

let normalize_record_field_owned_trivia = fun ~source:_ field -> field

let normalize_record_field_sequence_owned_trivia = fun ~source:_ ~source_node:_ fields -> fields

let update_last_record_field_owned_trivia = fun ~source:_ ~source_node:_ fields -> fields

let normalize_record_fields_owned_trivia = fun ~source fields ->
  let _ = source in
  fields

let normalize_constructor_arguments_owned_trivia = fun ~source ->
  function
  | Cst.ConstructorArguments.Record { opening_token; fields; closing_token } -> Cst.ConstructorArguments.Record {
    opening_token;
    fields = normalize_record_fields_owned_trivia ~source fields;
    closing_token
  }
  | Cst.ConstructorArguments.Tuple _ as arguments -> arguments

let rec normalize_variant_constructor_owned_trivia = fun ~source constructor ->
  let _ = source in
  match Cst.VariantConstructor.arguments constructor with
  | Some arguments -> {
    constructor
    with arguments = Some (normalize_constructor_arguments_owned_trivia ~source arguments)
  }
  | None -> constructor

and normalize_variant_constructor_sequence_owned_trivia = fun ~source ~source_node:_ constructors ->
  constructors |> List.map (normalize_variant_constructor_owned_trivia ~source)

let update_last_variant_constructor_owned_trivia = fun ~source:_ ~following_trivia constructors ->
  (constructors, following_trivia)

let normalize_type_definition_owned_trivia = fun ~source ->
  function
  | Cst.TypeDefinition.Record { syntax_node; opening_token; fields; closing_token } ->
      let normalized_fields = normalize_record_fields_owned_trivia ~source fields in
      Cst.TypeDefinition.Record {
        syntax_node;
        opening_token;
        fields = normalized_fields;
        closing_token
      }
  | Cst.TypeDefinition.Variant { syntax_node; constructors } ->
      let normalized_constructors = constructors
      |> normalize_variant_constructor_sequence_owned_trivia ~source ~source_node:syntax_node in
      Cst.TypeDefinition.Variant { syntax_node; constructors = normalized_constructors }
  | type_definition ->
      type_definition

let normalize_single_type_declaration_owned_trivia = fun ~source decl ->
  let normalized_type_definition = normalize_type_definition_owned_trivia
    ~source
    (Cst.TypeDeclaration.type_definition decl) in
  { decl with type_definition = normalized_type_definition }

let split_type_declaration_trailing_trivia = fun ~source ~has_next_sibling decl ->
  let _ = source in
  let _ = has_next_sibling in
  (decl, [])

let normalize_type_declaration_sequence = fun ~source ~has_next_sibling ?(initial_leading = []) decls ->
  let rec loop = fun carried_leading bubbled_after_group acc ->
    function
    | [] ->
        (List.rev acc, bubbled_after_group)
    | [ decl ] ->
        let decl = normalize_single_type_declaration_owned_trivia ~source decl in
        let decl = append_type_declaration_leading_trivia
          decl
          (sort_trivia_by_source carried_leading) in
        let decl, bubbled_to_next = split_type_declaration_trailing_trivia
          ~source
          ~has_next_sibling
          decl in
        (List.rev (decl :: acc), sort_trivia_by_source (bubbled_after_group @ bubbled_to_next))
    | decl :: rest ->
        let decl = normalize_single_type_declaration_owned_trivia ~source decl in
        let decl = append_type_declaration_leading_trivia
          decl
          (sort_trivia_by_source carried_leading) in
        let decl, bubbled_to_next = split_type_declaration_trailing_trivia
          ~source
          ~has_next_sibling:true
          decl in
        let carry_to_next, bubble_past_group = bubbled_to_next
        |> List.partition (fun trivia -> not (is_section_docstring_trivia trivia)) in
        loop
          carry_to_next
          (sort_trivia_by_source (bubbled_after_group @ bubble_past_group))
          (decl :: acc)
          rest
  in
  loop initial_leading [] [] decls

let rec let_binding_chain_of_list = function
  | [] -> None
  | (binding: Cst.LetBinding.t) :: rest -> Some {
    binding
    with and_binding = let_binding_chain_of_list rest
  }

let let_binding_chain_to_list = fun (binding: Cst.LetBinding.t) ->
  binding :: Cst.LetBinding.and_bindings binding

let rec binding_operator_chain_of_list = function
  | [] -> None
  | (binding: Cst.binding_operator_binding) :: rest -> Some {
    binding
    with and_binding = binding_operator_chain_of_list rest
  }

let rec binding_operator_bindings_of_chain = fun (binding: Cst.binding_operator_binding) ->
  binding :: (
    match binding.and_binding with
    | Some next -> binding_operator_bindings_of_chain next
    | None -> []
  )

let binding_operator_chain_tail = fun (binding: Cst.binding_operator_binding) ->
  match binding.and_binding with
  | Some next -> next :: binding_operator_bindings_of_chain next
  | None -> []

let rec type_declaration_chain_of_list = function
  | [] -> None
  | (decl: Cst.TypeDeclaration.t) :: rest -> Some {
    decl
    with next_and_declaration = type_declaration_chain_of_list rest
  }

let type_declaration_group_members = fun (decl: Cst.TypeDeclaration.t) ->
  let rec loop acc declaration =
    let acc = declaration :: acc in
    match Cst.TypeDeclaration.next_and_declaration declaration with
    | Some next -> loop acc next
    | None -> List.rev acc
  in
  loop [] decl

let has_no_next_type_declaration = fun (decl: Cst.TypeDeclaration.t) ->
  match Cst.TypeDeclaration.next_and_declaration decl with
  | None -> true
  | Some _ -> false

let rec module_signature_chain_of_list = function
  | [] -> None
  | (decl: Cst.ModuleSignature.t) :: rest -> Some {
    decl
    with next_and_declaration = module_signature_chain_of_list rest
  }

let rec module_structure_chain_of_list = function
  | [] -> None
  | (decl: Cst.ModuleStructure.t) :: rest -> Some {
    decl
    with next_and_declaration = module_structure_chain_of_list rest
  }

let normalize_type_declaration_group = fun ~source ~has_next_sibling ?(initial_leading = []) decl ->
  let group_syntax_node = Cst.TypeDeclaration.syntax_node decl in
  let and_member_leading_trivia =
    match Ceibo.Red.SyntaxNode.kind group_syntax_node with
    | Syntax_kind.TYPE_MUTUAL_DECL ->
        direct_non_trivia_tokens group_syntax_node |> List.filter
          (fun syntax_token ->
            String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "and") |> List.map
          (fun syntax_token -> Ceibo.Red.SyntaxToken.leading_trivia syntax_token |> List.filter_map trivia_from_syntax_trivia)
    | _ -> []
  in
  let member_syntax_nodes =
    match Ceibo.Red.SyntaxNode.kind group_syntax_node with
    | Syntax_kind.TYPE_MUTUAL_DECL -> direct_non_trivia_nodes group_syntax_node
    |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_DECL)
    | _ -> type_declaration_group_members decl |> List.map Cst.TypeDeclaration.syntax_node
  in
  let rec restore_member_nodes acc nodes decls =
    match nodes, decls with
    | node :: node_rest, ((decl: Cst.TypeDeclaration.t) :: decl_rest) -> restore_member_nodes
      ({ decl with syntax_node = node; next_and_declaration = None } :: acc)
      node_rest
      decl_rest
    | [], [] -> Some (List.rev acc)
    | _ -> None
  in
  let member_decls =
    match restore_member_nodes
      []
      member_syntax_nodes
      (type_declaration_group_members decl) with
    | Some member_decls -> member_decls
    | None -> type_declaration_group_members decl
    |> List.map (fun (decl: Cst.TypeDeclaration.t) -> { decl with next_and_declaration = None })
  in
  let normalized_decls, bubbled_to_next = normalize_type_declaration_sequence
    ~source
    ~has_next_sibling
    ~initial_leading
    member_decls in
  let normalized_decls =
    match normalized_decls with
    | first :: rest ->
        let rest =
          rest
          |> List.mapi
            (fun index decl ->
              match List.nth_opt and_member_leading_trivia index with
              | Some leading_trivia when not (List.is_empty leading_trivia) -> append_type_declaration_leading_trivia
                decl
                leading_trivia
              | _ -> decl)
        in
        first :: rest
    | [] -> []
  in
  match normalized_decls with
  | [] -> (decl, bubbled_to_next)
  | first :: rest ->
      let first =
        match Ceibo.Red.SyntaxNode.kind group_syntax_node with
        | Syntax_kind.TYPE_MUTUAL_DECL -> { first with syntax_node = group_syntax_node }
        | _ -> first
      in
      ({ first with next_and_declaration = type_declaration_chain_of_list rest }, bubbled_to_next)

let map_last_type_declaration_in_group = fun decl f ->
  let rec loop declaration =
    match Cst.TypeDeclaration.next_and_declaration declaration with
    | None -> f declaration
    | Some next ->
        { declaration with next_and_declaration = Some (loop next) }
  in
  loop decl

let absorb_next_type_declaration_leading_trivia = fun ~source:_ current_decl next_decl ->
  (current_decl, next_decl)

let absorb_following_variant_constructor_trivia = fun ~source:_ decl following_trivia ->
  (decl, following_trivia)

let structure_item_of_trivia = fun trivia ->
  match trivia with
  | Cst.Trivia.Docstring docstring -> Cst.StructureItem.Docstring docstring
  | Cst.Trivia.Comment comment -> Cst.StructureItem.Comment comment

let signature_item_of_trivia = fun trivia ->
  match trivia with
  | Cst.Trivia.Docstring docstring -> Cst.SignatureItem.Docstring docstring
  | Cst.Trivia.Comment comment -> Cst.SignatureItem.Comment comment

let trivia_of_structure_item = function
  | Cst.StructureItem.Docstring docstring -> Some (Cst.Trivia.Docstring docstring)
  | Cst.StructureItem.Comment comment -> Some (Cst.Trivia.Comment comment)
  | _ -> None

let trivia_of_signature_item = function
  | Cst.SignatureItem.Docstring docstring -> Some (Cst.Trivia.Docstring docstring)
  | Cst.SignatureItem.Comment comment -> Some (Cst.Trivia.Comment comment)
  | _ -> None

let rec drop_matching_leading_trivia = fun ~trivia_of_item trivia items ->
  match trivia, items with
  | left :: rest_trivia, item :: rest_items -> (
      match trivia_of_item item with
      | Some right when trivia_same_span left right -> drop_matching_leading_trivia
        ~trivia_of_item
        rest_trivia
        rest_items
      | _ -> items
    )
  | _ -> items

let rec take_leading_trivia_items = fun ~trivia_of_item acc ->
  function
  | item :: rest -> (
      match trivia_of_item item with
      | Some trivia -> take_leading_trivia_items ~trivia_of_item (trivia :: acc) rest
      | None -> (List.rev acc, item :: rest)
    )
  | [] -> (List.rev acc, [])

let drop_owned_trivia_items = fun ~trivia_of_item owned_spans items ->
  items |> List.filter
    (fun item ->
      match trivia_of_item item with
      | Some trivia -> not
        (List.exists (fun owned_span -> span_contains owned_span (trivia_span trivia)) owned_spans)
      | None -> true)

let type_declaration_starts_with_and = fun decl ->
  match direct_non_trivia_tokens (Cst.TypeDeclaration.syntax_node decl) with
  | token :: _ -> String.equal (Ceibo.Red.SyntaxToken.text token) "and"
  | [] -> false

let rec take_adjacent_item_docstrings = fun ~docstring_of_item ->
  function
  | item :: rest -> (
      match docstring_of_item item with
      | Some docstring when not (Cst.Docstring.is_section docstring) ->
          let taken, remaining = take_adjacent_item_docstrings ~docstring_of_item rest in
          (Cst.Trivia.Docstring docstring :: taken, remaining)
      | _ -> ([], item :: rest)
    )
  | [] -> ([], [])

type ('item, 'value_decl) ordered_item_ops = {
  trivia_of_item: 'item -> Cst.trivia option;
  comment_of_item: 'item -> Cst.comment option;
  docstring_of_item: 'item -> Cst.docstring option;
  type_declaration_of_item: 'item -> Cst.TypeDeclaration.t option;
  value_declaration_of_item: 'item -> 'value_decl option;
  item_of_trivia: Cst.trivia -> 'item;
  item_of_type_declaration: Cst.TypeDeclaration.t -> 'item;
  item_of_value_declaration: 'value_decl -> 'item;
  normalize_value_declaration: 'value_decl -> 'value_decl;
  append_value_declaration_leading_trivia: 'value_decl -> Cst.trivia list -> 'value_decl;
  append_value_declaration_trailing_comment: 'value_decl -> Cst.comment -> 'value_decl;
  value_declaration_nontrivia_end: 'value_decl -> int;
}

let has_newline_between_offsets = fun ~source ~start ~end_ ->
  let source_length = String.length source in
  let start = Int.max 0 (Int.min start source_length) in
  let end_ = Int.max start (Int.min end_ source_length) in
  let rec loop index =
    if index >= end_ then
      false
    else if Char.equal source.[index] '\n' then
      true
    else
      loop (index + 1)
  in
  loop start

let rec normalize_ordered_items_owned_trivia = fun ~source ?(at_module_start = true) ops ->
  let finalize_normalized_type_declaration normalized_decl next_trivia rest =
    let normalized_decl, next_trivia = absorb_following_variant_constructor_trivia
      ~source
      normalized_decl
      next_trivia in
    let normalized_decl, rest =
      match rest with
      | next_item :: tail -> (
          match ops.type_declaration_of_item next_item with
          | Some next_decl ->
              let normalized_decl, next_decl = absorb_next_type_declaration_leading_trivia
                ~source
                normalized_decl
                next_decl in
              (normalized_decl, ops.item_of_type_declaration next_decl :: tail)
          | None -> (normalized_decl, rest)
        )
      | [] -> (normalized_decl, rest)
    in
    let leading_trivia_items, tail = take_leading_trivia_items
      ~trivia_of_item:ops.trivia_of_item
      []
      rest in
    let normalized_decl, leading_trivia_items = absorb_following_variant_constructor_trivia
      ~source
      normalized_decl
      leading_trivia_items in
    let rest = List.map ops.item_of_trivia leading_trivia_items @ tail in
    let rest = drop_owned_trivia_items
      ~trivia_of_item:ops.trivia_of_item
      (type_declaration_owned_trivia_spans normalized_decl)
      rest in
    let rest = drop_matching_leading_trivia ~trivia_of_item:ops.trivia_of_item next_trivia rest in
    (normalized_decl, List.map ops.item_of_trivia next_trivia @ rest)
  in
  function
  | [] -> []
  | item :: rest -> (
      match ops.docstring_of_item item with
      | Some docstring when not (Cst.Docstring.is_section docstring) -> (
          let attached_docstrings, remaining = take_adjacent_item_docstrings
            ~docstring_of_item:ops.docstring_of_item
            rest in
          let attached_docstrings = Cst.Trivia.Docstring docstring :: attached_docstrings in
          match remaining with
          | next_item :: tail -> (
              match ops.type_declaration_of_item next_item with
              | Some decl ->
                  let normalized_decl, next_trivia =
                    normalize_type_declaration_group ~source
                      ~has_next_sibling:((
                        match tail with
                        | [] -> false
                        | _ -> true
                      ))
                      ~initial_leading:attached_docstrings
                      decl
                  in
                  let normalized_decl, tail = finalize_normalized_type_declaration
                    normalized_decl
                    next_trivia
                    tail in
                  ops.item_of_type_declaration normalized_decl
                  :: normalize_ordered_items_owned_trivia ~source ~at_module_start:false ops tail
              | None -> item
              :: normalize_ordered_items_owned_trivia ~source ~at_module_start:true ops rest
            )
          | [] -> item :: normalize_ordered_items_owned_trivia ~source ~at_module_start:true ops rest
        )
      | _ -> (
          match ops.trivia_of_item item with
          | Some _ when not at_module_start -> (
              let _, remaining = take_leading_trivia_items
                ~trivia_of_item:ops.trivia_of_item
                []
                (item :: rest) in
              match remaining with
              | next_item :: _ when Option.is_some (ops.value_declaration_of_item next_item) -> normalize_ordered_items_owned_trivia
                ~source
                ~at_module_start:false
                ops
                remaining
              | _ -> item
              :: normalize_ordered_items_owned_trivia ~source ~at_module_start:false ops rest
            )
          | Some _ ->
              item :: normalize_ordered_items_owned_trivia ~source ~at_module_start:true ops rest
          | None -> (
              match ops.type_declaration_of_item item with
              | Some decl ->
                  let normalized_decl, next_trivia =
                    normalize_type_declaration_group ~source
                      ~has_next_sibling:((
                        match rest with
                        | [] -> false
                        | _ -> true
                      ))
                      decl
                  in
                  let normalized_decl, rest = finalize_normalized_type_declaration
                    normalized_decl
                    next_trivia
                    rest in
                  ops.item_of_type_declaration normalized_decl
                  :: normalize_ordered_items_owned_trivia ~source ~at_module_start:false ops rest
              | None -> (
                  match ops.value_declaration_of_item item with
                  | Some decl ->
                      let decl = ops.normalize_value_declaration decl in
                      let decl, rest =
                        match rest with
                        | next_item :: tail -> (
                            match ops.comment_of_item next_item with
                            | Some comment when not
                              (has_newline_between_offsets
                                ~source
                                ~start:(ops.value_declaration_nontrivia_end decl)
                                ~end_:(Ceibo.Red.SyntaxNode.span (Cst.Comment.syntax_node comment)).start) -> (
                              ops.append_value_declaration_trailing_comment decl comment,
                              tail
                            )
                            | _ -> (decl, rest)
                          )
                        | [] -> (decl, rest)
                      in
                      ops.item_of_value_declaration decl
                      :: normalize_ordered_items_owned_trivia ~source ~at_module_start:false ops rest
                  | None -> item
                  :: normalize_ordered_items_owned_trivia ~source ~at_module_start:false ops rest
                )
            )
        )
    )

let structure_item_docstring = function
  | Cst.StructureItem.Docstring docstring -> Some docstring
  | _ -> None

let signature_item_docstring = function
  | Cst.SignatureItem.Docstring docstring -> Some docstring
  | _ -> None

let structure_item_comment = function
  | Cst.StructureItem.Comment comment -> Some comment
  | _ -> None

let signature_item_comment = function
  | Cst.SignatureItem.Comment comment -> Some comment
  | _ -> None

let structure_item_type_declaration = function
  | Cst.StructureItem.TypeDeclaration decl -> Some decl
  | _ -> None

let signature_item_type_declaration = function
  | Cst.SignatureItem.TypeDeclaration decl -> Some decl
  | _ -> None

let structure_item_value_declaration = function
  | _ -> None

let signature_item_value_declaration = function
  | Cst.SignatureItem.ValueDeclaration decl -> Some decl
  | _ -> None

let normalize_structure_items_owned_trivia = fun ~source items ->
  normalize_ordered_items_owned_trivia ~source
    {
      trivia_of_item = trivia_of_structure_item;
      comment_of_item = structure_item_comment;
      docstring_of_item = structure_item_docstring;
      type_declaration_of_item = structure_item_type_declaration;
      value_declaration_of_item = structure_item_value_declaration;
      item_of_trivia = structure_item_of_trivia;
      item_of_type_declaration = (fun decl -> Cst.StructureItem.TypeDeclaration decl);
      item_of_value_declaration = (fun _ -> panic "structure_items cannot contain value declarations");
      normalize_value_declaration = normalize_value_declaration_owned_trivia;
      append_value_declaration_leading_trivia = append_value_declaration_leading_trivia;
      append_value_declaration_trailing_comment = append_value_declaration_trailing_comment;
      value_declaration_nontrivia_end = (fun _ -> 0);
    }
    items

let normalize_signature_items_owned_trivia = fun ~source items ->
  normalize_ordered_items_owned_trivia ~source
    {
      trivia_of_item = trivia_of_signature_item;
      comment_of_item = signature_item_comment;
      docstring_of_item = signature_item_docstring;
      type_declaration_of_item = signature_item_type_declaration;
      value_declaration_of_item = signature_item_value_declaration;
      item_of_trivia = signature_item_of_trivia;
      item_of_type_declaration = (fun decl -> Cst.SignatureItem.TypeDeclaration decl);
      item_of_value_declaration = (fun decl -> Cst.SignatureItem.ValueDeclaration decl);
      normalize_value_declaration = normalize_value_declaration_owned_trivia;
      append_value_declaration_leading_trivia = append_value_declaration_leading_trivia;
      append_value_declaration_trailing_comment = append_value_declaration_trailing_comment;
      value_declaration_nontrivia_end = value_declaration_nontrivia_end;
    }
    items

let rec detach_multiline_signature_value_trailing_comments = fun ~source ->
  function
  | [] ->
      []
  | Cst.SignatureItem.ValueDeclaration decl :: rest -> (
      match decl.trailing_comment with
      | Some comment when has_newline_between_offsets
        ~source
        ~start:(value_declaration_nontrivia_end decl)
        ~end_:(Ceibo.Red.SyntaxNode.span (Cst.Comment.syntax_node comment)).start -> Cst.SignatureItem.ValueDeclaration {
        decl
        with trailing_comment = None
      }
      :: Cst.SignatureItem.Comment comment
      :: detach_multiline_signature_value_trailing_comments ~source rest
      | _ -> Cst.SignatureItem.ValueDeclaration decl
      :: detach_multiline_signature_value_trailing_comments ~source rest
    )
  | item :: rest ->
      item :: detach_multiline_signature_value_trailing_comments ~source rest

let expression_grouping_from_node = fun node ->
  match direct_non_trivia_tokens node with
  | opening :: _ -> (
      match Ceibo.Red.SyntaxToken.text opening with
      | "begin" -> Cst.BeginEnd
      | _ -> Cst.Parens
    )
  | [] -> Cst.Parens

let first_and_last_direct_token = fun node ->
  let rec last = function
    | [] -> None
    | [ token ] -> Some token
    | _ :: rest -> last rest
  in
  match direct_non_trivia_tokens node with
  | [] ->
      None
  | [ token ] ->
      Some (token, token)
  | first :: rest -> (
      match last rest with
      | Some last_token -> Some (first, last_token)
      | None -> Some (first, first)
    )

let rec drop_attribute_shell_tokens = fun tokens ->
  let token_text = Ceibo.Red.SyntaxToken.text in
  let rec loop = fun depth ->
    function
    | [] -> []
    | syntax_token :: rest ->
        let text = token_text syntax_token in
        if String.equal text "[" then
          loop (depth + 1) rest
        else if String.equal text "]" then
          if depth = 1 then
            rest
          else
            loop (depth - 1) rest
        else
          loop depth rest
  in
  loop 1 tokens

let rec drop_leading_declaration_decorator_tokens = fun tokens ->
  let token_text = Ceibo.Red.SyntaxToken.text in
  match tokens with
  | percent_token :: _extension_name :: rest when String.equal (token_text percent_token) "%" -> drop_leading_declaration_decorator_tokens
    rest
  | open_bracket :: rest when String.equal (token_text open_bracket) "[" -> drop_leading_declaration_decorator_tokens
    (drop_attribute_shell_tokens rest)
  | _ -> tokens

let declaration_tokens_after_keywords = fun ~keyword_count tokens ->
  let rec drop count remaining =
    if count <= 0 then
      remaining
    else
      match remaining with
      | _ :: rest -> drop (count - 1) rest
      | [] -> []
  in
  drop keyword_count tokens |> drop_leading_declaration_decorator_tokens

let find_declaration_name_token = fun ~skip_keywords tokens ->
  let token_text = Ceibo.Red.SyntaxToken.text in
  let rec loop = function
    | [] -> None
    | syntax_token :: rest ->
        let text = token_text syntax_token in
        if List.exists (String.equal text) skip_keywords then
          loop rest
        else if String.equal text "%" then
          (
            match rest with
            | _extension_name :: after_extension -> loop after_extension
            | [] -> None
          )
        else if String.equal text "[" then
          loop (drop_attribute_shell_tokens rest)
        else if String.equal text ":" || String.equal text "=" || String.equal text ":=" then
          None
        else
          Some syntax_token
  in
  loop tokens

let find_declaration_name_tokens = fun ~skip_keywords tokens ->
  let token_text = Ceibo.Red.SyntaxToken.text in
  let rec collect_operator = fun acc ->
    function
    | [] -> None
    | syntax_token :: rest ->
        let text = token_text syntax_token in
        if String.equal text ")" then
          (
            match List.rev acc with
            | [] -> None
            | lifted -> Some lifted
          )
        else
          collect_operator (token syntax_token :: acc) rest
  in
  let rec loop = function
    | [] -> None
    | syntax_token :: rest ->
        let text = token_text syntax_token in
        if List.exists (String.equal text) skip_keywords then
          loop rest
        else if String.equal text "%" then
          (
            match rest with
            | _extension_name :: after_extension -> loop after_extension
            | [] -> None
          )
        else if String.equal text "[" then
          loop (drop_attribute_shell_tokens rest)
        else if String.equal text "(" then
          collect_operator [] rest
        else if String.equal text ":" || String.equal text "=" || String.equal text ":=" then
          None
        else
          Some [ token syntax_token ]
  in
  loop tokens

let declaration_name_tokens_from_node = fun ~skip_keywords node ->
  let operator_tokens_from_node node =
    let tokens = direct_non_trivia_tokens node in
    let text = Ceibo.Red.SyntaxToken.text in
    let tokens =
      match tokens with
      | opening_syntax_token :: rest when String.equal (text opening_syntax_token) "(" -> (
          match List.rev rest with
          | closing_syntax_token :: inner_rev when String.equal (text closing_syntax_token) ")" -> List.rev
            inner_rev
          | _ -> tokens
        )
      | _ -> tokens
    in
    match List.map token tokens with
    | [] -> None
    | lifted -> Some lifted
  in
  match
    direct_non_trivia_nodes node |> List.find_map
      (fun child ->
        match Ceibo.Red.SyntaxNode.kind child with
        | Syntax_kind.IDENT_EXPR -> (
            match direct_non_trivia_tokens child |> List.map token with
            | [] -> None
            | lifted -> Some lifted
          )
        | Syntax_kind.OPERATOR_PATTERN ->
            operator_tokens_from_node child
        | _ ->
            None)
  with
  | Some lifted -> Some lifted
  | None -> find_declaration_name_tokens ~skip_keywords (direct_non_trivia_tokens node)

let direct_tokens = fun node -> Ceibo.Red.SyntaxNode.direct_tokens node

let is_literal_token_kind = function
  | Syntax_kind.STRING_LITERAL
  | Syntax_kind.INT_LITERAL
  | Syntax_kind.FLOAT_LITERAL
  | Syntax_kind.CHAR_LITERAL
  | Syntax_kind.BOOL_LITERAL -> true
  | _ -> false

let literal_tokens_from_node = fun ~context node ->
  let direct_nodes = direct_non_trivia_nodes node in
  let direct_tokens = direct_non_trivia_tokens node in
  let scan_direct_tokens syntax_tokens =
    let rec loop sign_token literal_token = function
      | [] -> Some (sign_token, literal_token)
      | syntax_token :: rest ->
          let kind = Ceibo.Red.SyntaxToken.kind syntax_token in
          if is_literal_token_kind kind then
            loop sign_token (Some syntax_token) rest
          else
            let text = Ceibo.Red.SyntaxToken.text syntax_token in
            let sign_token =
              if literal_token = None && (String.equal text "-" || String.equal text "+") then
                Some (token syntax_token)
              else
                sign_token
            in
            loop sign_token literal_token rest
    in
    loop None None syntax_tokens
  in
  let find_literal_token syntax_tokens = syntax_tokens
  |> List.find_opt
    (fun syntax_token -> is_literal_token_kind (Ceibo.Red.SyntaxToken.kind syntax_token)) in
  let from_fallback_tokens syntax_tokens =
    match find_literal_token syntax_tokens with
    | Some literal_syntax_token ->
        let literal_token = token literal_syntax_token in
        let literal_span = Cst.Token.span literal_token in
        let sign_token =
          syntax_tokens
          |> List.find_map
            (fun syntax_token ->
              let token_span = Ceibo.Red.SyntaxToken.span syntax_token in
              let token_text = Ceibo.Red.SyntaxToken.text syntax_token in
              if
                token_span.end_ <= literal_span.start
                && (String.equal token_text "-" || String.equal token_text "+")
              then
                Some (token syntax_token)
              else
                None)
        in
        Some (sign_token, literal_token)
    | None -> None
  in
  match
    if List.is_empty direct_nodes then
      scan_direct_tokens direct_tokens
    else
      None
  with
  | Some (sign_token, Some literal_syntax_token) -> (sign_token, token literal_syntax_token)
  | Some (_, None)
  | None -> (
      match from_fallback_tokens direct_tokens with
      | Some (sign_token, literal_token) -> (sign_token, literal_token)
      | None -> (
          match from_fallback_tokens (subtree_non_trivia_tokens node) with
          | Some (sign_token, literal_token) -> (sign_token, literal_token)
          | None -> bail
            ~message:"expected literal token during Ceibo -> CST lifting"
            ~syntax_node:node
            ~context
        )
    )

let constant_from_parts = fun ~syntax_node ~literal_token ~sign_token ->
  match Ceibo.Red.SyntaxToken.kind (Cst.Token.syntax_token literal_token) with
  | Syntax_kind.STRING_LITERAL ->
      let delimiter, contents, terminated = string_delimiter_and_contents
        (Cst.Token.text literal_token) in
      Cst.Constant.String {
        syntax_node;
        literal_token;
        delimiter;
        contents;
        terminated;
        attributes = [];
      }
  | Syntax_kind.INT_LITERAL ->
      let base, prefix, digits, suffix = integer_parts (Cst.Token.text literal_token) in
      Cst.Constant.Int {
        syntax_node;
        sign_token;
        literal_token;
        base;
        prefix;
        digits;
        suffix;
        attributes = [];
      }
  | Syntax_kind.FLOAT_LITERAL ->
      let integral_digits, fractional_digits, exponent, suffix = float_parts
        (Cst.Token.text literal_token) in
      Cst.Constant.Float {
        syntax_node;
        sign_token;
        literal_token;
        integral_digits;
        fractional_digits;
        exponent;
        suffix;
        attributes = [];
      }
  | Syntax_kind.CHAR_LITERAL ->
      Cst.Constant.Char {
        syntax_node;
        literal_token;
        contents = char_contents (Cst.Token.text literal_token);
        attributes = []
      }
  | Syntax_kind.BOOL_LITERAL ->
      Cst.Constant.Bool {
        syntax_node;
        literal_token;
        value = String.equal (Cst.Token.text literal_token) "true";
        attributes = []
      }
  | Syntax_kind.UNIT_LITERAL ->
      Cst.Constant.Unit { syntax_node; attributes = [] }
  | _ ->
      bail
        ~message:"expected literal token during Ceibo -> CST lifting"
        ~syntax_node
        ~context:[ "constant" ]

let constant_from_syntax_token = fun ~syntax_node syntax_token ->
  let literal_token = token syntax_token in
  let literal_span = Cst.Token.span literal_token in
  let sign_token =
    direct_non_trivia_tokens syntax_node
    |> List.find_map
      (fun syntax_token ->
        let token_span = Ceibo.Red.SyntaxToken.span syntax_token in
        let token_text = Ceibo.Red.SyntaxToken.text syntax_token in
        if
          token_span.end_ <= literal_span.start
          && (String.equal token_text "-" || String.equal token_text "+")
        then
          Some (token syntax_token)
        else
          None)
  in
  constant_from_parts ~syntax_node ~literal_token ~sign_token

let constant_from_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.UNIT_LITERAL -> Cst.Constant.Unit { syntax_node = node; attributes = [] }
  | _ ->
      let sign_token, literal_token = literal_tokens_from_node ~context:[ "constant" ] node in
      constant_from_parts ~syntax_node:node ~literal_token ~sign_token

let module_path_from_tokens = fun ~syntax_node syntax_tokens ->
  let rec skip_to_name = function
    | [] -> None
    | syntax_token :: rest ->
        if String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "." then
          skip_to_name rest
        else
          Some (syntax_token, rest)
  in
  let rec build = fun prefix ->
    function
    | [] ->
        prefix
    | dot_token :: rest when String.equal (Ceibo.Red.SyntaxToken.text dot_token) "." -> (
        match skip_to_name rest with
        | Some (name_syntax_token, tail) -> build
          (Cst.Ident.Qualified {
            syntax_node;
            prefix;
            dot_token = token dot_token;
            name_token = token name_syntax_token
          })
          tail
        | None -> prefix
      )
    | _unexpected :: rest ->
        build prefix rest
  in
  match skip_to_name syntax_tokens with
  | Some (name_syntax_token, rest) -> build
    (Cst.Ident.Ident { syntax_node; name_token = token name_syntax_token })
    rest
  | None -> bail
    ~message:"expected at least one path segment during Ceibo -> CST lifting"
    ~syntax_node
    ~context:[ "module_path" ]

let module_path_from_node = fun node ->
  let parts = direct_non_trivia_tokens node in
  module_path_from_tokens ~syntax_node:node parts

let ident_path_from_node = fun node ->
  match direct_non_trivia_tokens node with
  | first :: rest ->
      let name_token =
        match rest with
        | [] -> token first
        | _ ->
            let tokens = first :: rest in
            let text = tokens |> List.map Ceibo.Red.SyntaxToken.text |> String.concat "" in
            let start_offset = (Ceibo.Red.SyntaxToken.span first).start in
            let last =
              List.fold_left (fun _ token -> token) first rest
            in
            let end_offset = (Ceibo.Red.SyntaxToken.span last).end_ in
            synthetic_token ~kind:Syntax_kind.IDENT_EXPR ~text ~start_offset ~end_offset
      in
      Cst.Ident.Ident { syntax_node = node; name_token }
  | [] -> bail
    ~message:"expected identifier path segment during Ceibo -> CST lifting"
    ~syntax_node:node
    ~context:[ "module_path"; "ident" ]

let rec module_path_like_from_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.MODULE_PATH
  | Syntax_kind.MODULE_TYPE_PATH ->
      module_path_from_node node
  | Syntax_kind.IDENT_EXPR ->
      ident_path_from_node node
  | Syntax_kind.FIELD_ACCESS_EXPR -> (
      match direct_non_trivia_nodes node, List.rev (direct_non_trivia_tokens node) with
      | receiver_node :: _, name_syntax_token :: dot_syntax_token :: _ ->
          let prefix = module_path_like_from_node receiver_node in
          Cst.Ident.Qualified {
            syntax_node = node;
            prefix;
            dot_token = token dot_syntax_token;
            name_token = token name_syntax_token
          }
      | _ -> module_path_from_tokens ~syntax_node:node (direct_non_trivia_tokens node)
    )
  | _ ->
      module_path_from_tokens ~syntax_node:node (direct_non_trivia_tokens node)

let poly_variant_type_path_from_node = fun node ->
  let path_tokens = direct_non_trivia_tokens node
  |> List.filter
    (fun syntax_token -> not (String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "#")) in
  module_path_from_tokens ~syntax_node:node path_tokens

let type_constructor_path_from_node = fun node ->
  let is_identifier_like_text text =
    let is_alpha_or_underscore ch = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch = '_' in
    let len = String.length text in
    if len = 0 then
      false
    else
      let ch = String.get text 0 in
      is_alpha_or_underscore ch || if ch = '#' && len > 1 then
        let next = String.get text 1 in
        is_alpha_or_underscore next
      else if ch = '\\' then
        true
      else
        false
  in
  match
    direct_non_trivia_nodes node |> List.find_opt
      (fun child ->
        let kind = Ceibo.Red.SyntaxNode.kind child in
        kind = Syntax_kind.MODULE_PATH || kind = Syntax_kind.MODULE_TYPE_PATH || kind = Syntax_kind.IDENT_EXPR)
  with
  | Some path_node -> module_path_like_from_node path_node
  | None ->
      let path_tokens =
        direct_non_trivia_tokens node
        |> List.filter
          (fun syntax_token ->
            let text = Ceibo.Red.SyntaxToken.text syntax_token in
            is_identifier_like_text text || String.equal text ".")
      in
      if List.length path_tokens = 0 then
        bail
          ~message:"expected type constructor path during Ceibo -> CST lifting"
          ~syntax_node:node
          ~context:[ "core_type.constr" ]
      else
        module_path_from_tokens ~syntax_node:node path_tokens

let token_starts_with_uppercase = fun token ->
  let text = Cst.Token.text token in
  let len = String.length text in
  len > 0 && let first = String.get text 0 in
  first >= 'A' && first <= 'Z'

let is_constructor_path = fun (path: Cst.Ident.t) ->
  match Cst.Ident.last_segment path with
  | Some segment -> token_starts_with_uppercase segment
  | None -> false

let rec module_like_path_from_expression_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_EXPR ->
      let path = ident_path_from_node node in
      if is_constructor_path path then
        Some path
      else
        None
  | Syntax_kind.MODULE_PATH ->
      let path = module_path_from_node node in
      if is_constructor_path path then
        Some path
      else
        None
  | Syntax_kind.FIELD_ACCESS_EXPR -> (
      match direct_non_trivia_nodes node, List.rev (direct_non_trivia_tokens node) with
      | receiver_node :: _, name_syntax_token :: dot_syntax_token :: _ when token_starts_with_uppercase
        (token name_syntax_token) -> (
          match module_like_path_from_expression_node receiver_node with
          | Some prefix -> Some (Cst.Ident.Qualified {
            syntax_node = node;
            prefix;
            dot_token = token dot_syntax_token;
            name_token = token name_syntax_token
          })
          | None -> None
        )
      | _ -> None
    )
  | _ ->
      None

let constructor_path_from_expression_node = fun node ->
  match module_like_path_from_expression_node node with
  | Some path when is_constructor_path path -> Some path
  | Some _
  | None -> None

let annotation_shell_and_payload = fun ~annotation_kind ~sigils node ->
  let direct_tokens = direct_non_trivia_tokens node in
  let has_sigil tokens =
    tokens
    |> List.exists
      (fun syntax_token ->
        let text = Ceibo.Red.SyntaxToken.text syntax_token in
        List.exists (String.equal text) sigils)
  in
  if has_sigil direct_tokens then
    (node, None)
  else
    let direct_children = direct_non_trivia_nodes node in
    let shell_node_opt = direct_children
    |> List.find_opt
      (fun child ->
        Ceibo.Red.SyntaxNode.kind child = annotation_kind
        && has_sigil (direct_non_trivia_tokens child)) in
    let payload_node_opt = direct_children
    |> List.find_opt (fun child -> Ceibo.Red.SyntaxNode.kind child != annotation_kind) in
    match shell_node_opt with
    | Some shell_node -> (shell_node, payload_node_opt)
    | None -> bail
      ~message:"expected attribute or extension shell during Ceibo -> CST lifting"
      ~syntax_node:node
      ~context:[ "annotation" ]

let annotation_name_from_tokens = fun ~syntax_node ~sigils syntax_tokens ->
  let name_tokens =
    syntax_tokens
    |> List.filter
      (fun syntax_token ->
        let text = Ceibo.Red.SyntaxToken.text syntax_token in
        not
          (String.equal text open_bracket_text
          || String.equal text close_bracket_text
          || List.exists (String.equal text) sigils))
  in
  module_path_from_tokens ~syntax_node name_tokens

type raw_annotation_payload_kind =
  | Unmarked
  | TypePayload
  | PatternPayload

type raw_annotation_payload = {
  kind: raw_annotation_payload_kind;
  text: string;
  start_offset: int;
}

let parse_implementation_fragment = fun source ->
  let tokens = Lexer.tokenize source in
  Parser.parse_implementation ~source tokens

let parse_interface_fragment = fun source ->
  let tokens = Lexer.tokenize source in
  Parser.parse_interface ~source tokens

let make_padded_fragment = fun ~start_offset text -> String.make start_offset ' ' ^ text

let make_wrapped_fragment = fun ~prefix ~suffix ~start_offset text ->
  let padding = Int.max 0 (start_offset - String.length prefix) in
  String.make padding ' ' ^ prefix ^ text ^ suffix

let payload_text_from_tokens = fun all_tokens ~start_offset ~end_offset ->
  all_tokens |> List.filter
    (fun syntax_token ->
      let span = Ceibo.Red.SyntaxToken.span syntax_token in
      span.start >= start_offset && span.end_ <= end_offset) |> List.map Ceibo.Red.SyntaxToken.text |> String.concat
    ""

let attribute_payload_from_shell_impl: (Cst.syntax_node -> Cst.payload option) Cell.t =
  Cell.create (fun _ -> None)

let attribute_payload_from_shell = fun shell_node -> (Cell.get attribute_payload_from_shell_impl) shell_node

let extension_payload_from_shell_impl: (Cst.syntax_node -> Cst.payload option) Cell.t =
  Cell.create (fun _ -> None)

let extension_payload_from_shell = fun shell_node -> (Cell.get extension_payload_from_shell_impl) shell_node

let attribute_from_node node: Cst.attribute =
  let shell_node, _payload_syntax_node = annotation_shell_and_payload
    ~annotation_kind:Syntax_kind.ATTRIBUTE_EXPR
    ~sigils:[ at_text; double_at_text; triple_at_text ]
    node in
  let shell_tokens = direct_non_trivia_tokens shell_node in
  let sigil_syntax_tokens =
    shell_tokens
    |> List.filter
      (fun syntax_token ->
        let text = Ceibo.Red.SyntaxToken.text syntax_token in
        String.equal text at_text || String.equal text double_at_text || String.equal text triple_at_text)
  in
  match sigil_syntax_tokens with
  | _ :: _ -> {
    Cst.syntax_node = node;
    sigil_tokens = List.map token sigil_syntax_tokens;
    name = annotation_name_from_tokens ~syntax_node:shell_node ~sigils:attribute_sigil_texts shell_tokens;
    payload = attribute_payload_from_shell shell_node
  }
  | [] -> bail
    ~message:"expected attribute sigil during Ceibo -> CST lifting"
    ~syntax_node:node
    ~context:[ "attribute" ]

let extension_from_node node: Cst.extension =
  let shell_node, _payload_syntax_node = annotation_shell_and_payload
    ~annotation_kind:Syntax_kind.EXTENSION_EXPR
    ~sigils:[ percent_text; double_percent_text; triple_percent_text ]
    node in
  let shell_tokens = direct_non_trivia_tokens shell_node in
  let sigil_syntax_tokens =
    shell_tokens
    |> List.filter
      (fun syntax_token ->
        let text = Ceibo.Red.SyntaxToken.text syntax_token in
        String.equal text percent_text
        || String.equal text double_percent_text
        || String.equal text triple_percent_text)
  in
  match sigil_syntax_tokens with
  | _ :: _ ->
      {
        Cst.syntax_node = node;
        sigil_tokens = List.map token sigil_syntax_tokens;
        name = annotation_name_from_tokens ~syntax_node:shell_node ~sigils:extension_sigil_texts shell_tokens;
        payload = extension_payload_from_shell shell_node;
        attributes = [];
      }
  | [] -> bail
    ~message:"expected extension sigil during Ceibo -> CST lifting"
    ~syntax_node:node
    ~context:[ "extension" ]

let extension_from_tokens = fun ~syntax_node syntax_tokens : Cst.extension ->
  let sigil_syntax_tokens =
    syntax_tokens
    |> List.filter
      (fun syntax_token ->
        let text = Ceibo.Red.SyntaxToken.text syntax_token in
        String.equal text percent_text
        || String.equal text double_percent_text
        || String.equal text triple_percent_text)
  in
  match sigil_syntax_tokens with
  | _ :: _ ->
      {
        Cst.syntax_node;
        sigil_tokens = List.map token sigil_syntax_tokens;
        name = annotation_name_from_tokens ~syntax_node ~sigils:extension_sigil_texts syntax_tokens;
        payload = None;
        attributes = [];
      }
  | [] -> bail
    ~message:"expected extension sigil during Ceibo -> CST lifting"
    ~syntax_node
    ~context:[ "extension" ]

let attributes_from_node = fun node ->
  direct_non_trivia_nodes node
  |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.ATTRIBUTE_EXPR)
  |> List.map attribute_from_node

let attribute_sigil_text = fun (attribute: Cst.attribute) ->
  direct_non_trivia_tokens attribute.syntax_node |> List.filter_map
    (fun syntax_token ->
      match Ceibo.Red.SyntaxToken.text syntax_token with
      | text when List.exists (String.equal text) attribute_sigil_texts -> Some text
      | _ -> None) |> String.concat ""

let attribute_is_item_like = fun (attribute: Cst.attribute) ->
  match attribute_sigil_text attribute with
  | text when String.equal text double_at_text || String.equal text triple_at_text -> true
  | _ -> false

let attribute_is_floating_item = fun (attribute: Cst.attribute) ->
  String.equal (attribute_sigil_text attribute) triple_at_text

let rec floating_attribute_payload_nodes_from_node = fun node ->
  direct_non_trivia_nodes node |> List.concat_map
    (fun child ->
      let nested = floating_attribute_payload_nodes_from_node child in
      if Ceibo.Red.SyntaxNode.kind child = Syntax_kind.ATTRIBUTE_EXPR then
        let attribute = attribute_from_node child in
        if attribute_is_floating_item attribute then
          child :: nested
        else
          nested
      else
        nested)

let split_payload_item_nodes_from_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.TYPE_DECL -> node :: floating_attribute_payload_nodes_from_node node
  | _ -> [ node ]

let is_initializer_node = fun node ->
  match direct_non_trivia_tokens node with
  | initializer_kw :: _ -> String.equal (Ceibo.Red.SyntaxToken.text initializer_kw) "initializer"
  | [] -> false

let class_field_with_attributes = fun field attributes ->
  List.fold_left
    (fun field attribute ->
      Cst.ClassField.Attribute { syntax_node = Cst.ClassField.syntax_node field; field; attribute })
    field
    attributes

let non_paren_tokens = fun node ->
  direct_non_trivia_tokens node |> List.filter
    (fun syntax_token ->
      let text = Ceibo.Red.SyntaxToken.text syntax_token in
      not (String.equal text "(" || String.equal text ")"))

let is_type_syntax_kind = function
  | Syntax_kind.TYPE_VAR
  | Syntax_kind.TYPE_CONSTR
  | Syntax_kind.TYPE_RECORD
  | Syntax_kind.TYPE_TUPLE
  | Syntax_kind.TYPE_ALIAS
  | Syntax_kind.TYPE_ARROW
  | Syntax_kind.TYPE_PAREN
  | Syntax_kind.TYPE_POLY_VARIANT
  | Syntax_kind.POLY_TYPE
  | Syntax_kind.FIRST_CLASS_MODULE_TYPE
  | Syntax_kind.OBJECT_TYPE
  | Syntax_kind.ATTRIBUTE_EXPR
  | Syntax_kind.EXTENSION_EXPR -> true
  | _ -> false

let rec can_lift_core_type_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.ATTRIBUTE_EXPR -> direct_non_trivia_nodes node |> List.exists can_lift_core_type_node
  | kind -> is_type_syntax_kind kind

let rec peel_outer_type_attributes = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.ATTRIBUTE_EXPR -> (
      match direct_non_trivia_nodes node with
      | first_child :: rest -> (
          match List.find_opt can_lift_core_type_node (first_child :: rest), List.find_opt
            (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.ATTRIBUTE_EXPR)
            rest with
          | Some payload_node, Some attribute_node ->
              let payload_node, attributes = peel_outer_type_attributes payload_node in
              (payload_node, attributes @ [ attribute_from_node attribute_node ])
          | _ -> (node, [])
        )
      | [] -> (node, [])
    )
  | _ -> (node, [])

let rec can_lift_module_type_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.MODULE_TYPE_PATH
  | Syntax_kind.MODULE_TYPE_OF
  | Syntax_kind.MODULE_TYPE_EXPR
  | Syntax_kind.FUNCTOR_TYPE
  | Syntax_kind.EXTENSION_EXPR -> true
  | Syntax_kind.PAREN_EXPR -> direct_non_trivia_nodes node |> List.exists can_lift_module_type_node
  | Syntax_kind.ATTRIBUTE_EXPR -> direct_non_trivia_nodes node |> List.exists can_lift_module_type_node
  | Syntax_kind.SIG_EXPR -> true
  | _ -> false

let rec can_lift_class_type_field_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.OBJECT_INHERIT
  | Syntax_kind.OBJECT_VAL
  | Syntax_kind.OBJECT_METHOD
  | Syntax_kind.TYPE_CONSTRAINT
  | Syntax_kind.EXTENSION_EXPR -> true
  | Syntax_kind.ATTRIBUTE_EXPR -> direct_non_trivia_nodes node |> List.exists can_lift_class_type_field_node
  | _ -> false

let rec can_lift_class_type_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.OBJECT_EXPR
  | Syntax_kind.MODULE_PATH
  | Syntax_kind.TYPE_CONSTR
  | Syntax_kind.TYPE_ARROW
  | Syntax_kind.EXTENSION_EXPR
  | Syntax_kind.ARRAY_INDEX_EXPR ->
      true
  | Syntax_kind.PAREN_EXPR
  | Syntax_kind.ATTRIBUTE_EXPR
  | Syntax_kind.APPLY_EXPR ->
      direct_non_trivia_nodes node |> List.exists can_lift_class_type_node
  | Syntax_kind.IDENT_EXPR -> (
      match direct_non_trivia_tokens node with
      | _ :: _ -> true
      | [] -> false
    )
  | _ ->
      false

let rec can_lift_class_expression_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_EXPR
  | Syntax_kind.MODULE_PATH
  | Syntax_kind.ARRAY_INDEX_EXPR
  | Syntax_kind.OBJECT_EXPR
  | Syntax_kind.FUN_EXPR
  | Syntax_kind.APPLY_EXPR
  | Syntax_kind.LET_EXPR
  | Syntax_kind.LET_REC_EXPR
  | Syntax_kind.TYPED_EXPR
  | Syntax_kind.LOCAL_OPEN_EXPR
  | Syntax_kind.EXTENSION_EXPR -> true
  | Syntax_kind.PAREN_EXPR
  | Syntax_kind.ATTRIBUTE_EXPR -> direct_non_trivia_nodes node |> List.exists can_lift_class_expression_node
  | _ -> false

let rec can_lift_module_expression_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.MODULE_PATH
  | Syntax_kind.STRUCT_EXPR
  | Syntax_kind.MODULE_APPLICATION
  | Syntax_kind.MODULE_UNIT_APPLICATION
  | Syntax_kind.EXTENSION_EXPR
  | Syntax_kind.FIRST_CLASS_MODULE_EXPR ->
      true
  | Syntax_kind.FUNCTOR_TYPE -> (
      match direct_non_trivia_tokens node with
      | first :: _ -> String.equal (Ceibo.Red.SyntaxToken.text first) "functor"
      | [] -> false
    )
  | Syntax_kind.PAREN_EXPR ->
      direct_non_trivia_nodes node |> List.exists can_lift_module_expression_node
  | Syntax_kind.ATTRIBUTE_EXPR ->
      direct_non_trivia_nodes node |> List.exists can_lift_module_expression_node
  | Syntax_kind.IDENT_EXPR -> (
      match direct_non_trivia_tokens node with
      | _ :: _ -> true
      | [] -> false
    )
  | Syntax_kind.SIG_EXPR ->
      false
  | _ ->
      false

let is_pattern_syntax_kind = function
  | Syntax_kind.IDENT_PATTERN
  | Syntax_kind.WILDCARD_PATTERN
  | Syntax_kind.ATTRIBUTE_EXPR
  | Syntax_kind.EXTENSION_EXPR
  | Syntax_kind.LAZY_PATTERN
  | Syntax_kind.EXCEPTION_PATTERN
  | Syntax_kind.RANGE_PATTERN
  | Syntax_kind.OPERATOR_PATTERN
  | Syntax_kind.FIRST_CLASS_MODULE_PATTERN
  | Syntax_kind.STRING_LITERAL
  | Syntax_kind.INT_LITERAL
  | Syntax_kind.FLOAT_LITERAL
  | Syntax_kind.CHAR_LITERAL
  | Syntax_kind.BOOL_LITERAL
  | Syntax_kind.UNIT_LITERAL
  | Syntax_kind.POLY_VARIANT_PATTERN
  | Syntax_kind.POLY_VARIANT_TYPE_PATTERN
  | Syntax_kind.EFFECT_PATTERN
  | Syntax_kind.CONSTRUCTOR_PATTERN
  | Syntax_kind.TUPLE_PATTERN
  | Syntax_kind.LIST_PATTERN
  | Syntax_kind.ARRAY_PATTERN
  | Syntax_kind.RECORD_PATTERN
  | Syntax_kind.CONS_PATTERN
  | Syntax_kind.OR_PATTERN
  | Syntax_kind.AS_PATTERN
  | Syntax_kind.LOCAL_OPEN_PATTERN
  | Syntax_kind.TYPED_PATTERN
  | Syntax_kind.PAREN_PATTERN -> true
  | _ -> false

let is_expression_syntax_kind = function
  | Syntax_kind.IDENT_EXPR
  | Syntax_kind.MODULE_PATH
  | Syntax_kind.OPERATOR_PATTERN
  | Syntax_kind.ATTRIBUTE_EXPR
  | Syntax_kind.EXTENSION_EXPR
  | Syntax_kind.OBJECT_EXPR
  | Syntax_kind.UNIT_LITERAL
  | Syntax_kind.METHOD_CALL_EXPR
  | Syntax_kind.NEW_EXPR
  | Syntax_kind.FIELD_ACCESS_EXPR
  | Syntax_kind.ARRAY_INDEX_EXPR
  | Syntax_kind.STRING_INDEX_EXPR
  | Syntax_kind.ASSIGN_EXPR
  | Syntax_kind.STRING_LITERAL
  | Syntax_kind.INT_LITERAL
  | Syntax_kind.FLOAT_LITERAL
  | Syntax_kind.CHAR_LITERAL
  | Syntax_kind.BOOL_LITERAL
  | Syntax_kind.ASSERT_EXPR
  | Syntax_kind.LAZY_EXPR
  | Syntax_kind.WHILE_EXPR
  | Syntax_kind.FOR_EXPR
  | Syntax_kind.APPLY_EXPR
  | Syntax_kind.POLY_VARIANT_EXPR
  | Syntax_kind.FIRST_CLASS_MODULE_EXPR
  | Syntax_kind.LET_MODULE_EXPR
  | Syntax_kind.LET_EXPR
  | Syntax_kind.LET_REC_EXPR
  | Syntax_kind.TYPED_EXPR
  | Syntax_kind.COERCE_EXPR
  | Syntax_kind.PREFIX_EXPR
  | Syntax_kind.INFIX_EXPR
  | Syntax_kind.SEQUENCE_EXPR
  | Syntax_kind.TUPLE_EXPR
  | Syntax_kind.LIST_EXPR
  | Syntax_kind.ARRAY_EXPR
  | Syntax_kind.RECORD_EXPR
  | Syntax_kind.RECORD_UPDATE_EXPR
  | Syntax_kind.UNREACHABLE_EXPR
  | Syntax_kind.OBJECT_UPDATE_EXPR
  | Syntax_kind.LOCAL_OPEN_EXPR
  | Syntax_kind.FUN_EXPR
  | Syntax_kind.FUNCTION_EXPR
  | Syntax_kind.MATCH_EXPR
  | Syntax_kind.TRY_EXPR
  | Syntax_kind.IF_EXPR
  | Syntax_kind.PAREN_EXPR -> true
  | _ -> false

let rec can_lift_expression_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.ATTRIBUTE_EXPR -> direct_non_trivia_nodes node |> List.exists can_lift_expression_node
  | kind -> is_expression_syntax_kind kind

let is_parameter_like_kind = function
  | Syntax_kind.IDENT_PATTERN
  | Syntax_kind.WILDCARD_PATTERN
  | Syntax_kind.LITERAL_PATTERN
  | Syntax_kind.STRING_LITERAL
  | Syntax_kind.INT_LITERAL
  | Syntax_kind.FLOAT_LITERAL
  | Syntax_kind.CHAR_LITERAL
  | Syntax_kind.BOOL_LITERAL
  | Syntax_kind.UNIT_LITERAL
  | Syntax_kind.CONSTRUCTOR_PATTERN
  | Syntax_kind.TUPLE_PATTERN
  | Syntax_kind.LIST_PATTERN
  | Syntax_kind.ARRAY_PATTERN
  | Syntax_kind.CONS_PATTERN
  | Syntax_kind.RECORD_PATTERN
  | Syntax_kind.OR_PATTERN
  | Syntax_kind.AS_PATTERN
  | Syntax_kind.RANGE_PATTERN
  | Syntax_kind.TYPED_PATTERN
  | Syntax_kind.LAZY_PATTERN
  | Syntax_kind.EXCEPTION_PATTERN
  | Syntax_kind.PAREN_PATTERN
  | Syntax_kind.POLY_VARIANT_PATTERN
  | Syntax_kind.POLY_VARIANT_TYPE_PATTERN
  | Syntax_kind.LOCAL_OPEN_PATTERN
  | Syntax_kind.OPERATOR_PATTERN
  | Syntax_kind.FIRST_CLASS_MODULE_PATTERN
  | Syntax_kind.LABELED_PARAM
  | Syntax_kind.OPTIONAL_PARAM
  | Syntax_kind.OPTIONAL_PARAM_DEFAULT
  | Syntax_kind.LOCALLY_ABSTRACT_TYPE_PARAM -> true
  | _ -> false

let pattern_with_attributes = fun pattern attributes ->
  match attributes with
  | [] -> pattern
  | _ ->
      let append existing = existing @ attributes in
      match pattern with
      | Cst.Pattern.Identifier pattern -> Cst.Pattern.Identifier {
        pattern
        with attributes = append pattern.attributes
      }
      | Cst.Pattern.Wildcard pattern -> Cst.Pattern.Wildcard {
        pattern
        with attributes = append pattern.attributes
      }
      | Cst.Pattern.Extension pattern -> Cst.Pattern.Extension {
        pattern
        with attributes = append pattern.attributes
      }
      | Cst.Pattern.Literal pattern -> Cst.Pattern.Literal {
        pattern
        with attributes = append pattern.attributes
      }
      | Cst.Pattern.Lazy pattern -> Cst.Pattern.Lazy {
        pattern
        with attributes = append pattern.attributes
      }
      | Cst.Pattern.Exception pattern -> Cst.Pattern.Exception {
        pattern
        with attributes = append pattern.attributes
      }
      | Cst.Pattern.Range pattern -> Cst.Pattern.Range {
        pattern
        with attributes = append pattern.attributes
      }
      | Cst.Pattern.Operator pattern -> Cst.Pattern.Operator {
        pattern
        with attributes = append pattern.attributes
      }
      | Cst.Pattern.FirstClassModule pattern -> Cst.Pattern.FirstClassModule {
        pattern
        with attributes = append pattern.attributes
      }
      | Cst.Pattern.PolyVariant pattern -> Cst.Pattern.PolyVariant {
        pattern
        with attributes = append pattern.attributes
      }
      | Cst.Pattern.PolyVariantInherit pattern -> Cst.Pattern.PolyVariantInherit {
        pattern
        with attributes = append pattern.attributes
      }
      | Cst.Pattern.Constructor pattern -> Cst.Pattern.Constructor {
        pattern
        with attributes = append pattern.attributes
      }
      | Cst.Pattern.Tuple pattern -> Cst.Pattern.Tuple {
        pattern
        with attributes = append pattern.attributes
      }
      | Cst.Pattern.List pattern -> Cst.Pattern.List {
        pattern
        with attributes = append pattern.attributes
      }
      | Cst.Pattern.Array pattern -> Cst.Pattern.Array {
        pattern
        with attributes = append pattern.attributes
      }
      | Cst.Pattern.Record pattern -> Cst.Pattern.Record {
        pattern
        with attributes = append pattern.attributes
      }
      | Cst.Pattern.Cons pattern -> Cst.Pattern.Cons {
        pattern
        with attributes = append pattern.attributes
      }
      | Cst.Pattern.Or pattern -> Cst.Pattern.Or {
        pattern
        with attributes = append pattern.attributes
      }
      | Cst.Pattern.Alias pattern -> Cst.Pattern.Alias {
        pattern
        with attributes = append pattern.attributes
      }
      | Cst.Pattern.Typed pattern -> Cst.Pattern.Typed {
        pattern
        with attributes = append pattern.attributes
      }
      | Cst.Pattern.Effect pattern -> Cst.Pattern.Effect {
        pattern
        with attributes = append pattern.attributes
      }
      | Cst.Pattern.LocalOpen pattern -> Cst.Pattern.LocalOpen {
        pattern
        with attributes = append pattern.attributes
      }
      | Cst.Pattern.Parenthesized pattern -> Cst.Pattern.Parenthesized {
        pattern
        with attributes = append pattern.attributes
      }

let name_token_from_ident_pattern = fun node ->
  match direct_non_trivia_tokens node with
  | first :: _ -> Some (token first)
  | [] -> None

let is_identifier_like_text = fun text ->
  let is_alpha_or_underscore ch = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch = '_' in
  let len = String.length text in
  if len = 0 then
    false
  else
    let ch = String.get text 0 in
    is_alpha_or_underscore ch || if ch = '#' && len > 1 then
      let next = String.get text 1 in
      is_alpha_or_underscore next
    else if ch = '\\' then
      if len > 2 && String.get text 1 = '#' then
        let next = String.get text 2 in
        is_alpha_or_underscore next
      else
        false
    else
      false

let rec simple_pattern_name_token = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_PATTERN ->
      name_token_from_ident_pattern node
  | Syntax_kind.ATTRIBUTE_EXPR -> (
      match direct_non_trivia_nodes node with
      | first_child :: _ -> simple_pattern_name_token first_child
      | [] -> None
    )
  | Syntax_kind.TYPED_PATTERN
  | Syntax_kind.PAREN_PATTERN
  | Syntax_kind.LAZY_PATTERN -> (
      match direct_non_trivia_nodes node |> List.find_opt (fun _ -> true) with
      | Some child -> simple_pattern_name_token child
      | None -> None
    )
  | Syntax_kind.LOCAL_OPEN_PATTERN -> (
      match direct_non_trivia_nodes node
      |> List.find_opt (fun child -> is_pattern_syntax_kind (Ceibo.Red.SyntaxNode.kind child)) with
      | Some child -> simple_pattern_name_token child
      | None -> None
    )
  | Syntax_kind.AS_PATTERN -> (
      match direct_non_trivia_nodes node
      |> List.rev
      |> List.find_opt (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.IDENT_PATTERN) with
      | Some child -> name_token_from_ident_pattern child
      | None -> None
    )
  | _ ->
      None

let rec standalone_attribute_node = fun node ->
  Ceibo.Red.SyntaxNode.kind node = Syntax_kind.ATTRIBUTE_EXPR && (
    match direct_non_trivia_nodes node with
    | [] -> true
    | children -> List.for_all standalone_attribute_node children
  )

let is_attribute_node = standalone_attribute_node

let is_extension_node = fun node -> Ceibo.Red.SyntaxNode.kind node = Syntax_kind.EXTENSION_EXPR

let declaration_modifiers_from_nodes = fun nodes ->
  let declaration_extension = nodes |> List.find_opt is_extension_node |> Option.map extension_from_node in
  let declaration_attributes = nodes |> List.filter is_attribute_node |> List.map attribute_from_node in
  (declaration_extension, declaration_attributes)

let declaration_extension_from_gap_tokens = fun ~node ~keywords ~boundary_start ->
  let keyword_end =
    direct_non_trivia_tokens node
    |> List.filter
      (fun syntax_token ->
        let text = Ceibo.Red.SyntaxToken.text syntax_token in
        List.exists (String.equal text) keywords
        && (Ceibo.Red.SyntaxToken.span syntax_token).start < boundary_start)
    |> List.rev
    |> List.find_map
      (fun syntax_token ->
        let text = Ceibo.Red.SyntaxToken.text syntax_token in
        if List.exists (String.equal text) keywords then
          Some ((Ceibo.Red.SyntaxToken.span syntax_token).end_)
        else
          None)
    |> Option.unwrap_or ~default:0
  in
  let syntax_tokens =
    direct_non_trivia_tokens node
    |> List.filter
      (fun syntax_token ->
        let span = Ceibo.Red.SyntaxToken.span syntax_token in
        span.start >= keyword_end && span.end_ <= boundary_start)
  in
  match syntax_tokens with
  | first_token :: _ ->
      let first_text = Ceibo.Red.SyntaxToken.text first_token in
      if List.exists (String.equal first_text) extension_sigil_texts then
        Some (extension_from_tokens ~syntax_node:node syntax_tokens)
      else
        None
  | [] -> None

let is_let_binding_node = fun node ->
  let kind = Ceibo.Red.SyntaxNode.kind node in
  kind = Syntax_kind.LET_BINDING || kind = Syntax_kind.LET_REC_BINDING

let split_at_first_and_binding = fun nodes ->
  let rec loop = fun acc ->
    function
    | child :: rest when is_let_binding_node child -> (List.rev acc, child :: rest)
    | child :: rest -> loop (child :: acc) rest
    | [] -> (List.rev acc, [])
  in
  loop [] nodes

let let_expression_parts = fun ~is_recursive_binding node ->
  let is_recursive_binding =
    is_recursive_binding
    || List.exists
      (fun syntax_token ->
        String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "rec")
      (direct_non_trivia_tokens node)
  in
  let binding_children = direct_non_trivia_nodes node
  |> List.filter (fun child -> not (is_attribute_node child)) in
  match binding_children with
  | exception_decl :: rest when Ceibo.Red.SyntaxNode.kind exception_decl = Syntax_kind.EXCEPTION_DECL -> (
      match List.rev rest with
      | body_node :: _ -> Some (`Exception (exception_decl, body_node))
      | [] -> None
    )
  | binding_pattern_node :: rest -> (
      match List.rev rest with
      | body_node :: rev_prefix ->
          let prefix = List.rev rev_prefix in
          let binding_prefix, and_binding_nodes = split_at_first_and_binding prefix in
          (
            match List.rev binding_prefix with
            | bound_value_node :: rev_param_nodes -> Some (`Value (
              is_recursive_binding,
              binding_pattern_node,
              List.rev rev_param_nodes,
              bound_value_node,
              and_binding_nodes,
              body_node
            ))
            | [] -> None
          )
      | [] -> None
    )
  | [] ->
      None

let is_binding_operator_expression_node = fun node ->
  let non_trivia_children =
    Ceibo.Red.SyntaxNode.children node
    |> List.filter
      (
        function
        | Ceibo.Red.Token syntax_token when is_trivia (Ceibo.Red.SyntaxToken.kind syntax_token) -> false
        | _ -> true
      )
  in
  match Ceibo.Red.SyntaxNode.kind node, non_trivia_children with
  | Syntax_kind.LET_EXPR, Ceibo.Red.Token let_kw :: Ceibo.Red.Token operator_syntax_token :: Ceibo.Red.Node pattern_node :: Ceibo.Red.Token equals_syntax_token :: _ when String.equal
    (Ceibo.Red.SyntaxToken.text let_kw)
    "let"
  && is_pattern_syntax_kind (Ceibo.Red.SyntaxNode.kind pattern_node)
  && String.equal (Ceibo.Red.SyntaxToken.text equals_syntax_token) "="
  && not (String.equal (Ceibo.Red.SyntaxToken.text operator_syntax_token) "rec")
  && not (String.equal (Ceibo.Red.SyntaxToken.text operator_syntax_token) "open")
  && not (String.equal (Ceibo.Red.SyntaxToken.text operator_syntax_token) "module")
  && not (String.equal (Ceibo.Red.SyntaxToken.text operator_syntax_token) "exception") -> true
  | _ -> false

let binding_operator_tokens_from_node = fun node ->
  let rec loop = fun acc ->
    function
    | keyword_syntax_token :: operator_syntax_token :: equals_syntax_token :: rest when (String.equal
      (Ceibo.Red.SyntaxToken.text keyword_syntax_token)
      keyword_let_text
    || String.equal (Ceibo.Red.SyntaxToken.text keyword_syntax_token) keyword_and_text)
    && String.equal (Ceibo.Red.SyntaxToken.text equals_syntax_token) equals_text -> loop
      ((token keyword_syntax_token, token operator_syntax_token, token equals_syntax_token) :: acc)
      rest
    | in_syntax_token :: _ when String.equal (Ceibo.Red.SyntaxToken.text in_syntax_token) keyword_in_text -> (
      List.rev acc,
      token in_syntax_token
    )
    | [] -> bail
      ~message:"expected let-operator in keyword during Ceibo -> CST lifting"
      ~syntax_node:node
      ~context:[ "expression"; "let_operator"; "tokens" ]
    | _ -> bail
      ~message:"expected binding-operator token sequence during Ceibo -> CST lifting"
      ~syntax_node:node
      ~context:[ "expression"; "let_operator"; "tokens" ]
  in
  loop [] (direct_non_trivia_tokens node)

let first_ident_token_in_subtree = fun node ->
  let rec go_node node =
    match direct_non_trivia_tokens node
    |> List.find_opt (fun tok -> is_identifier_like_text (Ceibo.Red.SyntaxToken.text tok)) with
    | Some tok -> Some (token tok)
    | None -> direct_non_trivia_nodes node |> List.find_map go_node
  in
  go_node node

let quoted_type_binder_from_node = fun node ->
  match first_ident_token_in_subtree node with
  | Some name_token -> Cst.TypeBinder.Quoted { syntax_node = node; name_token }
  | None -> bail
    ~message:"expected quantified type binder name during Ceibo -> CST lifting"
    ~syntax_node:node
    ~context:[ "type_binder.quoted" ]

let bare_type_binders_from_tokens = fun syntax_tokens ->
  let rec collect = fun started acc ->
    function
    | [] -> List.rev acc
    | syntax_token :: rest ->
        let text = Ceibo.Red.SyntaxToken.text syntax_token in
        if started then
          if String.equal text "." then
            List.rev acc
          else if is_identifier_like_text text then
            collect true (Cst.TypeBinder.Bare { name_token = token syntax_token } :: acc) rest
          else
            collect true acc rest
        else if String.equal text "type" then
          collect true acc rest
        else
          collect false acc rest
  in
  collect false [] syntax_tokens

let bare_type_binders_from_node = fun ~context node ->
  let binders = bare_type_binders_from_tokens (direct_non_trivia_tokens node) in
  if List.length binders = 0 then
    bail
      ~message:"expected locally abstract type binders during Ceibo -> CST lifting"
      ~syntax_node:node
      ~context
  else
    binders

let locally_abstract_type_parameter_from_node node: Cst.locally_abstract_type_parameter =
  let binders = bare_type_binders_from_node ~context:[ "parameter.locally_abstract" ] node in
  let direct_tokens = direct_non_trivia_tokens node in
  let opening_token, closing_token =
    match direct_tokens with
    | opening_token :: _ -> (token opening_token, token (List.hd (List.rev direct_tokens)))
    | [] -> bail
      ~message:"expected locally abstract type parameter delimiters during Ceibo -> CST lifting"
      ~syntax_node:node
      ~context:[ "parameter.locally_abstract" ]
  in
  let type_keyword_token =
    match
      direct_tokens |> List.find_opt
        (fun syntax_token ->
          String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "type")
    with
    | Some type_keyword_token -> token type_keyword_token
    | None -> bail
      ~message:"expected locally abstract type parameter type keyword during Ceibo -> CST lifting"
      ~syntax_node:node
      ~context:[ "parameter.locally_abstract"; "type_keyword_token" ]
  in
  {
    Cst.syntax_node = node;
    opening_token;
    type_keyword_token;
    binders;
    closing_token;
  }

let constructor_pattern_existentials_from_node node: Cst.constructor_pattern_existentials =
  let binders = bare_type_binders_from_node ~context:[ "pattern.constructor.existentials" ] node in
  let direct_tokens = direct_non_trivia_tokens node in
  let opening_token, closing_token =
    match direct_tokens with
    | opening_token :: _ -> (token opening_token, token (List.hd (List.rev direct_tokens)))
    | [] -> bail
      ~message:"expected constructor pattern existential delimiters during Ceibo -> CST lifting"
      ~syntax_node:node
      ~context:[ "pattern.constructor.existentials" ]
  in
  let type_keyword_token =
    match
      direct_tokens |> List.find_opt
        (fun syntax_token ->
          String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "type")
    with
    | Some type_keyword_token -> token type_keyword_token
    | None -> bail
      ~message:"expected constructor pattern existential type keyword during Ceibo -> CST lifting"
      ~syntax_node:node
      ~context:[ "pattern.constructor.existentials"; "type_keyword_token" ]
  in
  {
    Cst.syntax_node = node;
    opening_token;
    type_keyword_token;
    binders;
    closing_token;
  }

let constructor_pattern_existentials_from_children = fun node ->
  direct_non_trivia_nodes node |> List.find_map
    (fun child ->
      if Ceibo.Red.SyntaxNode.kind child = Syntax_kind.LOCALLY_ABSTRACT_TYPE_PARAM then
        Some (constructor_pattern_existentials_from_node child)
      else
        None)

let constructor_pattern_existentials_from_nodes = fun nodes ->
  nodes |> List.find_map
    (fun child ->
      if Ceibo.Red.SyntaxNode.kind child = Syntax_kind.LOCALLY_ABSTRACT_TYPE_PARAM then
        Some (constructor_pattern_existentials_from_node child)
      else
        None)

let token_with_text = fun node expected ->
  subtree_non_trivia_tokens node |> List.find_opt
    (fun syntax_token ->
      String.equal (Ceibo.Red.SyntaxToken.text syntax_token) expected) |> Option.map token

let direct_token_with_text = fun node expected ->
  direct_non_trivia_tokens node |> List.find_opt
    (fun syntax_token ->
      String.equal (Ceibo.Red.SyntaxToken.text syntax_token) expected) |> Option.map token

let direct_required_token_with_text = fun ~context node expected ->
  match direct_token_with_text node expected with
  | Some token -> token
  | None -> bail
    ~message:(("expected '" ^ expected ^ "' token during Ceibo -> CST lifting"))
    ~syntax_node:node
    ~context

let direct_tokens_between_offsets = fun ~after_offset ~before_offset node ->
  direct_non_trivia_tokens node |> List.filter_map
    (fun syntax_token ->
      let span = Ceibo.Red.SyntaxToken.span syntax_token in
      if span.start >= after_offset && span.end_ <= before_offset then
        Some (token syntax_token)
      else
        None)

let direct_syntax_tokens_between_offsets = fun ~after_offset ~before_offset node ->
  direct_non_trivia_tokens node |> List.filter
    (fun syntax_token ->
      let span = Ceibo.Red.SyntaxToken.span syntax_token in
      span.start >= after_offset && span.end_ <= before_offset)

let direct_required_tokens_with_text_between_offsets = fun ~context ~after_offset ~before_offset ~expected_texts node ->
  let lifted_tokens = direct_syntax_tokens_between_offsets ~after_offset ~before_offset node
  |> List.map token in
  let actual_texts = lifted_tokens |> List.map Cst.Token.text in
  if List.equal String.equal actual_texts expected_texts then
    lifted_tokens
  else
    bail
      ~message:(("expected token sequence [" ^ (expected_texts |> String.concat ", ") ^ "] during Ceibo -> CST lifting"))
      ~syntax_node:node
      ~context

let separator_tokens_between_record_fields = fun ~closing_token (
  fields: Cst.record_expression_field list
) node ->
  let separator_tokens_between_offsets ~after_offset ~before_offset =
    direct_tokens_between_offsets ~after_offset ~before_offset node
    |> List.filter
      (fun token ->
        String.equal (Cst.Token.text token) semicolon_text)
  in
  let closing_span = Cst.Token.span closing_token in
  let rec loop groups_rev = function
    | [] ->
        List.concat (List.rev groups_rev)
    | [ (field: Cst.record_expression_field) ] ->
        let field_span = span_of_syntax_node_nontrivia_bounds field.Cst.syntax_node in
        let trailing = separator_tokens_between_offsets
          ~after_offset:field_span.end_
          ~before_offset:closing_span.start in
        List.concat (List.rev (trailing :: groups_rev))
    | (field: Cst.record_expression_field) :: (((next_field: Cst.record_expression_field) :: _) as rest) ->
        let field_span = span_of_syntax_node_nontrivia_bounds field.Cst.syntax_node in
        let next_span = span_of_syntax_node_nontrivia_bounds next_field.Cst.syntax_node in
        let separators = separator_tokens_between_offsets
          ~after_offset:field_span.end_
          ~before_offset:next_span.start in
        loop (separators :: groups_rev) rest
  in
  loop [] fields

let trailing_module_path_tokens = fun syntax_tokens ->
  let is_ident_text text =
    let len = String.length text in
    let is_alpha_or_underscore ch = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch = '_' in
    if len = 0 then
      false
    else
      let first = String.get text 0 in
      is_alpha_or_underscore first || first = '\\'
  in
  let is_ident_token syntax_token = is_ident_text (Ceibo.Red.SyntaxToken.text syntax_token) in
  let is_dot_token syntax_token = String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "." in
  let rec collect = fun expect_ident acc ->
    function
    | syntax_token :: rest when expect_ident && is_ident_token syntax_token -> collect
      false
      (syntax_token :: acc)
      rest
    | syntax_token :: rest when (not expect_ident) && is_dot_token syntax_token -> collect
      true
      (syntax_token :: acc)
      rest
    | _ when expect_ident -> []
    | _ -> acc
  in
  collect true [] (List.rev syntax_tokens)

let rec take_tokens_until_equals = fun acc ->
  function
  | [] -> List.rev acc
  | syntax_token :: rest ->
      if String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "=" then
        List.rev acc
      else
        take_tokens_until_equals (syntax_token :: acc) rest

let operator_tokens_from_node = fun node ->
  let tokens = direct_non_trivia_tokens node in
  let text = Ceibo.Red.SyntaxToken.text in
  let tokens =
    match tokens with
    | opening_syntax_token :: rest when String.equal (text opening_syntax_token) "(" -> (
        match List.rev rest with
        | closing_syntax_token :: inner_rev when String.equal (text closing_syntax_token) ")" -> List.rev
          inner_rev
        | _ -> tokens
      )
    | _ -> tokens
  in
  tokens |> List.filter
    (fun syntax_token ->
      let text = Ceibo.Red.SyntaxToken.text syntax_token in
      not (String.equal text " ")) |> List.map token

let arrow_label_from_node = fun node ->
  let text syntax_token = Ceibo.Red.SyntaxToken.text syntax_token in
  match direct_non_trivia_tokens node with
  | label_syntax_token :: colon_syntax_token :: _ when String.equal (text colon_syntax_token) ":" -> Some (Cst.ArrowLabel.Named {
    sigil_token = None;
    label_token = token label_syntax_token;
    colon_token = token colon_syntax_token
  })
  | sigil_syntax_token :: label_syntax_token :: colon_syntax_token :: _ when String.equal
    (text sigil_syntax_token)
    "~"
  && String.equal (text colon_syntax_token) ":" -> Some (Cst.ArrowLabel.Named {
    sigil_token = Some (token sigil_syntax_token);
    label_token = token label_syntax_token;
    colon_token = token colon_syntax_token
  })
  | sigil_syntax_token :: label_syntax_token :: colon_syntax_token :: _ when String.equal
    (text sigil_syntax_token)
    "?"
  && String.equal (text colon_syntax_token) ":" -> Some (Cst.ArrowLabel.OptionalNamed {
    sigil_token = token sigil_syntax_token;
    label_token = token label_syntax_token;
    colon_token = token colon_syntax_token
  })
  | _ -> None

let rec module_type_constraint_from_node = fun node ->
  match direct_non_trivia_nodes node |> List.filter can_lift_core_type_node with
  | constrained_type_node :: replacement_type_node :: _ -> (
      match direct_token_with_text node ":=", direct_token_with_text node "=" with
      | Some separator_token, _ -> Cst.ModuleTypeConstraint.{
        syntax_node = node;
        constrained_type = core_type_from_node constrained_type_node;
        replacement_type = core_type_from_node replacement_type_node;
        separator_token
      }
      | None, Some separator_token -> Cst.ModuleTypeConstraint.{
        syntax_node = node;
        constrained_type = core_type_from_node constrained_type_node;
        replacement_type = core_type_from_node replacement_type_node;
        separator_token
      }
      | None, None -> bail
        ~message:"expected = or := token in module type constraint during Ceibo -> CST lifting"
        ~syntax_node:node
        ~context:[ "module_type_constraint" ]
    )
  | _ -> bail
    ~message:"expected constrained and replacement types in module type constraint during Ceibo -> CST lifting"
    ~syntax_node:node
    ~context:[ "module_type.constraint" ]

and functor_parameter_from_node = fun node ->
  let name_token =
    match direct_non_trivia_tokens node with
    | _lparen :: name_token :: _ -> token name_token
    | _ -> bail
      ~message:"expected functor parameter name during Ceibo -> CST lifting"
      ~syntax_node:node
      ~context:[ "module_type.functor.parameter" ]
  in
  let colon_token =
    match direct_token_with_text node ":" with
    | Some colon_token -> colon_token
    | None -> bail
      ~message:"expected functor parameter colon during Ceibo -> CST lifting"
      ~syntax_node:node
      ~context:[ "module_type.functor.parameter"; "colon_token" ]
  in
  let module_type =
    match direct_non_trivia_nodes node |> List.find_opt can_lift_module_type_node with
    | Some module_type_node -> module_type_from_node module_type_node
    | None -> bail
      ~message:"expected functor parameter module type during Ceibo -> CST lifting"
      ~syntax_node:node
      ~context:[ "module_type.functor.parameter" ]
  in
  Cst.FunctorParameter.{ syntax_node = node; name_token; colon_token; module_type }

and module_type_from_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.MODULE_TYPE_PATH ->
      Cst.ModuleType.Path (module_path_from_node node)
  | Syntax_kind.MODULE_TYPE_OF -> (
      match direct_non_trivia_nodes node with
      | module_path_node :: _ -> (
          match direct_token_with_text node "of" with
          | Some of_token -> Cst.ModuleType.TypeOf {
            syntax_node = node;
            of_token;
            module_path = module_path_like_from_node module_path_node
          }
          | None -> bail
            ~message:"expected of token in module type of expression during Ceibo -> CST lifting"
            ~syntax_node:node
            ~context:[ "module_type.type_of" ]
        )
      | [] -> bail
        ~message:"expected module path in module type of expression during Ceibo -> CST lifting"
        ~syntax_node:node
        ~context:[ "module_type.type_of" ]
    )
  | Syntax_kind.MODULE_TYPE_EXPR -> (
      match direct_non_trivia_nodes node with
      | base_node :: constraint_nodes -> Cst.ModuleType.With {
        syntax_node = node;
        base = module_type_from_node base_node;
        constraints = constraint_nodes
        |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_CONSTRAINT)
        |> List.map module_type_constraint_from_node
      }
      | [] -> bail
        ~message:"expected base module type in constrained module type during Ceibo -> CST lifting"
        ~syntax_node:node
        ~context:[ "module_type.with" ]
    )
  | Syntax_kind.FUNCTOR_TYPE -> (
      match List.rev (direct_non_trivia_nodes node) with
      | result_node :: rev_parameter_nodes -> Cst.ModuleType.Functor {
        syntax_node = node;
        parameters = List.rev rev_parameter_nodes
        |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.FUNCTOR_PARAM)
        |> List.map functor_parameter_from_node;
        result = module_type_from_node result_node
      }
      | [] -> bail
        ~message:"expected functor parameters and result module type during Ceibo -> CST lifting"
        ~syntax_node:node
        ~context:[ "module_type.functor" ]
    )
  | Syntax_kind.PAREN_EXPR -> (
      match direct_non_trivia_nodes node |> List.find_opt can_lift_module_type_node, direct_non_trivia_tokens
        node with
      | Some inner_node, (opening_token :: _ :: _ as tokens) ->
          let closing_token = List.hd (List.rev tokens) in
          Cst.ModuleType.Parenthesized {
            syntax_node = node;
            opening_token = token opening_token;
            inner = module_type_from_node inner_node;
            closing_token = token closing_token
          }
      | None, _ ->
          bail
            ~message:"expected inner module type in parenthesized module type during Ceibo -> CST lifting"
            ~syntax_node:node
            ~context:[ "module_type.parenthesized" ]
      | Some _, _ ->
          bail
            ~message:"expected parenthesized module type delimiters during Ceibo -> CST lifting"
            ~syntax_node:node
            ~context:[ "module_type.parenthesized" ]
    )
  | Syntax_kind.ATTRIBUTE_EXPR -> (
      match direct_non_trivia_nodes node with
      | first_child :: rest -> (
          match List.find_opt can_lift_module_type_node (first_child :: rest), List.find_opt
            is_attribute_node
            rest with
          | Some payload_node, Some attribute_node -> Cst.ModuleType.Attribute {
            syntax_node = node;
            module_type = module_type_from_node payload_node;
            attribute = attribute_from_node attribute_node
          }
          | _ -> bail
            ~message:"expected attributed module type payload during Ceibo -> CST lifting"
            ~syntax_node:node
            ~context:[ "module_type.attribute" ]
        )
      | [] -> bail
        ~message:"expected attributed module type contents during Ceibo -> CST lifting"
        ~syntax_node:node
        ~context:[ "module_type.attribute" ]
    )
  | Syntax_kind.EXTENSION_EXPR ->
      Cst.ModuleType.Extension (extension_from_node node)
  | Syntax_kind.SIG_EXPR ->
      Cst.ModuleType.Signature { syntax_node = node; signature_syntax_node = node }
  | _ ->
      bail
        ~message:"unsupported module type shape during Ceibo -> CST lifting"
        ~syntax_node:node
        ~context:[ "module_type" ]

and module_type_from_first_class_module_type_node = fun node ->
  match direct_non_trivia_nodes node with
  | base_node :: constraint_nodes ->
      let { module_type_path; constraints=base_constraints; attribute; _ }: Cst.package_type = package_type_from_module_type_node
        base_node in
      let constraints = constraint_nodes
      |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_CONSTRAINT)
      |> List.map module_type_constraint_from_node in
      {
        Cst.syntax_node = node;
        module_type_path;
        constraints = base_constraints @ constraints;
        attribute
      }
  | [] -> bail
    ~message:"expected module type inside first-class module type during Ceibo -> CST lifting"
    ~syntax_node:node
    ~context:[ "package_type" ]

and package_type_from_module_type_node = fun node ->
  match module_type_from_node node with
  | Cst.ModuleType.Path module_type_path ->
      { Cst.syntax_node = node; module_type_path; constraints = []; attribute = None }
  | Cst.ModuleType.With { syntax_node; base=Cst.ModuleType.Path module_type_path; constraints } ->
      { Cst.syntax_node = syntax_node; module_type_path; constraints; attribute = None }
  | Cst.ModuleType.Attribute { syntax_node; module_type; attribute } ->
      let { module_type_path; constraints; _ }: Cst.package_type = package_type_from_module_type_node
        (Cst.ModuleType.syntax_node module_type) in
      { Cst.syntax_node = syntax_node; module_type_path; constraints; attribute = Some attribute }
  | Cst.ModuleType.With {
    syntax_node;
    base=Cst.ModuleType.Attribute { module_type; attribute; _ };
    constraints
  } -> (
      match ((
        package_type_from_module_type_node (Cst.ModuleType.syntax_node module_type): Cst.package_type
      )) with
      | { module_type_path; constraints=base_constraints; _ } -> {
        Cst.syntax_node = syntax_node;
        module_type_path;
        constraints = base_constraints @ constraints;
        attribute = Some attribute
      }
    )
  | Cst.ModuleType.With { base; _ } ->
      bail
        ~message:"expected package type path base inside constrained package type during Ceibo -> CST lifting"
        ~syntax_node:(Cst.ModuleType.syntax_node base)
        ~context:[ "package_type" ]
  | _ ->
      bail
        ~message:"expected package type path or constrained package type during Ceibo -> CST lifting"
        ~syntax_node:node
        ~context:[ "package_type" ]

and core_type_payload_and_field_attribute = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.ATTRIBUTE_EXPR ->
      let _, payload_syntax_node = annotation_shell_and_payload
        ~annotation_kind:Syntax_kind.ATTRIBUTE_EXPR
        ~sigils:[ at_text; double_at_text; triple_at_text ]
        node in
      let attribute = attribute_from_node node in
      if attribute_is_item_like attribute then
        match payload_syntax_node with
        | Some payload_node when can_lift_core_type_node payload_node -> (
          payload_node,
          Some attribute
        )
        | _ -> (node, None)
      else
        (node, None)
  | _ -> (node, None)

and class_type_field_from_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.OBJECT_INHERIT ->
      let make_field class_type = Cst.ClassTypeField.Inherit { syntax_node = node; class_type } in
      begin
        match direct_non_trivia_nodes node with
        | child :: _ when Ceibo.Red.SyntaxNode.kind child = Syntax_kind.APPLY_EXPR -> (
            match direct_non_trivia_nodes child with
            | payload_node :: rest -> (
                match List.find_opt is_attribute_node rest with
                | Some attribute_node when can_lift_class_type_node payload_node -> Cst.ClassTypeField.Attribute {
                  syntax_node = node;
                  field = make_field (class_type_from_node payload_node);
                  attribute = attribute_from_node attribute_node
                }
                | _ -> unsupported_class_type node
              )
            | [] -> unsupported_class_type node
          )
        | child :: _ ->
            make_field (class_type_from_node child)
        | [] ->
            unsupported_class_type node
      end
  | Syntax_kind.OBJECT_VAL -> (
      match direct_non_trivia_nodes node with
      | name_node :: remainder when Ceibo.Red.SyntaxNode.kind name_node = Syntax_kind.IDENT_EXPR -> (
          match first_ident_token_in_subtree name_node, List.find_opt can_lift_core_type_node remainder with
          | Some name_token, Some type_node ->
              let payload_type_node, field_attribute = core_type_payload_and_field_attribute type_node in
              let modifier_tokens =
                direct_non_trivia_tokens node
                |> List.filter
                  (fun token ->
                    String.equal "mutable" (Ceibo.Red.SyntaxToken.text token))
                |> List.map token
              in
              let field = Cst.ClassTypeField.Value {
                syntax_node = node;
                name_token;
                colon_token =
                  (
                    match direct_token_with_text node ":" with
                    | Some colon_token -> colon_token
                    | None -> bail
                      ~message:"expected class type value colon token during Ceibo -> CST lifting"
                      ~syntax_node:node
                      ~context:[ "class_type_field.value"; "colon_token" ]
                  );
                type_ = core_type_from_node payload_type_node;
                modifier_tokens;
              }
              in
              (
                match field_attribute with
                | Some attribute -> Cst.ClassTypeField.Attribute {
                  syntax_node = node;
                  field;
                  attribute
                }
                | None -> field
              )
          | _ -> unsupported_class_type node
        )
      | _ -> unsupported_class_type node
    )
  | Syntax_kind.OBJECT_METHOD -> (
      match direct_non_trivia_nodes node with
      | name_node :: remainder when Ceibo.Red.SyntaxNode.kind name_node = Syntax_kind.IDENT_EXPR -> (
          match first_ident_token_in_subtree name_node, List.find_opt can_lift_core_type_node remainder with
          | Some name_token, Some type_node ->
              let payload_type_node, field_attribute = core_type_payload_and_field_attribute type_node in
              let modifier_tokens =
                direct_non_trivia_tokens node
                |> List.filter
                  (fun token ->
                    String.equal "private" (Ceibo.Red.SyntaxToken.text token))
                |> List.map token
              in
              let field = Cst.ClassTypeField.Method {
                syntax_node = node;
                name_token;
                colon_token =
                  (
                    match direct_token_with_text node ":" with
                    | Some colon_token -> colon_token
                    | None -> bail
                      ~message:"expected class type method colon token during Ceibo -> CST lifting"
                      ~syntax_node:node
                      ~context:[ "class_type_field.method"; "colon_token" ]
                  );
                type_ = core_type_from_node payload_type_node;
                modifier_tokens;
              }
              in
              (
                match field_attribute with
                | Some attribute -> Cst.ClassTypeField.Attribute {
                  syntax_node = node;
                  field;
                  attribute
                }
                | None -> field
              )
          | _ -> unsupported_class_type node
        )
      | _ -> unsupported_class_type node
    )
  | Syntax_kind.TYPE_CONSTRAINT -> (
      match direct_non_trivia_nodes node |> List.filter can_lift_core_type_node with
      | left_node :: right_node :: _ ->
          let payload_right_node, field_attribute = core_type_payload_and_field_attribute right_node in
          let field = Cst.ClassTypeField.Constraint {
            syntax_node = node;
            left = core_type_from_node left_node;
            equals_token =
              (
                match direct_token_with_text node "=" with
                | Some equals_token -> equals_token
                | None -> bail
                  ~message:"expected class type constraint equals token during Ceibo -> CST lifting"
                  ~syntax_node:node
                  ~context:[ "class_type_field.constraint"; "equals_token" ]
              );
            right = core_type_from_node payload_right_node;
          }
          in
          (
            match field_attribute with
            | Some attribute -> Cst.ClassTypeField.Attribute { syntax_node = node; field; attribute }
            | None -> field
          )
      | _ -> unsupported_class_type node
    )
  | Syntax_kind.ATTRIBUTE_EXPR -> (
      match direct_non_trivia_nodes node with
      | first_child :: rest -> (
          match List.find_opt can_lift_class_type_field_node (first_child :: rest), List.find_opt
            is_attribute_node
            rest with
          | Some payload_node, Some attribute_node -> Cst.ClassTypeField.Attribute {
            syntax_node = node;
            field = class_type_field_from_node payload_node;
            attribute = attribute_from_node attribute_node
          }
          | _ -> unsupported_class_type node
        )
      | [] -> unsupported_class_type node
    )
  | Syntax_kind.EXTENSION_EXPR ->
      Cst.ClassTypeField.Extension (extension_from_node node)
  | _ ->
      unsupported_class_type node

and class_type_from_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_EXPR ->
      Cst.ClassType.Path (ident_path_from_node node)
  | Syntax_kind.MODULE_PATH ->
      Cst.ClassType.Path (module_path_from_node node)
  | Syntax_kind.TYPE_CONSTR ->
      if direct_non_trivia_nodes node |> List.exists
          (fun child ->
            let kind = Ceibo.Red.SyntaxNode.kind child in
            can_lift_core_type_node child
            && not (kind = Syntax_kind.IDENT_EXPR)
            && not (kind = Syntax_kind.MODULE_PATH)
            && not (kind = Syntax_kind.MODULE_TYPE_PATH)) then
        unsupported_class_type node
      else
        Cst.ClassType.Path (type_constructor_path_from_node node)
  | Syntax_kind.TYPE_ARROW -> (
      match direct_non_trivia_nodes node with
      | parameter_node :: result_node :: _ when can_lift_core_type_node parameter_node
      && can_lift_class_type_node result_node -> Cst.ClassType.Arrow {
        syntax_node = node;
        label = arrow_label_from_node node;
        parameter_type = core_type_from_node parameter_node;
        result_type = class_type_from_node result_node
      }
      | _ -> unsupported_class_type node
    )
  | Syntax_kind.OBJECT_EXPR ->
      Cst.ClassType.Signature {
        syntax_node = node;
        fields = direct_non_trivia_nodes node |> List.map class_type_field_from_node
      }
  | Syntax_kind.APPLY_EXPR -> (
      match direct_non_trivia_nodes node with
      | payload_node :: rest -> (
          match List.find_opt is_attribute_node rest with
          | Some attribute_node when can_lift_class_type_node payload_node -> Cst.ClassType.Attribute {
            syntax_node = node;
            class_type = class_type_from_node payload_node;
            attribute = attribute_from_node attribute_node
          }
          | _ -> unsupported_class_type node
        )
      | [] -> unsupported_class_type node
    )
  | Syntax_kind.PAREN_EXPR -> (
      match direct_non_trivia_nodes node |> List.find_opt can_lift_class_type_node, direct_non_trivia_tokens
        node with
      | Some inner_node, (opening_token :: _ :: _ as tokens) ->
          let closing_token = List.hd (List.rev tokens) in
          Cst.ClassType.Parenthesized {
            syntax_node = node;
            opening_token = token opening_token;
            inner = class_type_from_node inner_node;
            closing_token = token closing_token
          }
      | None, _ ->
          unsupported_class_type node
      | Some _, _ ->
          bail
            ~message:"expected parenthesized class type delimiters during Ceibo -> CST lifting"
            ~syntax_node:node
            ~context:[ "class_type.parenthesized" ]
    )
  | Syntax_kind.ATTRIBUTE_EXPR -> (
      match direct_non_trivia_nodes node with
      | first_child :: rest -> (
          match List.find_opt can_lift_class_type_node (first_child :: rest), List.find_opt
            is_attribute_node
            rest with
          | Some payload_node, Some attribute_node -> Cst.ClassType.Attribute {
            syntax_node = node;
            class_type = class_type_from_node payload_node;
            attribute = attribute_from_node attribute_node
          }
          | _ -> unsupported_class_type node
        )
      | [] -> unsupported_class_type node
    )
  | Syntax_kind.EXTENSION_EXPR ->
      Cst.ClassType.Extension (extension_from_node node)
  | _ ->
      unsupported_class_type node

and core_type_from_node = fun node ->
  let child_type_nodes node = direct_non_trivia_nodes node |> List.filter can_lift_core_type_node in
  let type_binders_from_poly_type_node node =
    let quoted_binders = direct_non_trivia_nodes node
    |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_VAR)
    |> List.map quoted_type_binder_from_node in
    if List.length quoted_binders > 0 then
      quoted_binders
    else
      bare_type_binders_from_tokens (direct_non_trivia_tokens node)
  in
  let rec class_type_path_from_node node =
    let rec after_hash = function
      | [] -> None
      | syntax_token :: rest ->
          if String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "#" then
            Some (syntax_token, rest)
          else
            after_hash rest
    in
    match after_hash (direct_non_trivia_tokens node) with
    | Some (hash_syntax_token, class_path_tokens) ->
        if List.length class_path_tokens > 0 then
          (token hash_syntax_token, module_path_from_tokens ~syntax_node:node class_path_tokens)
        else
          bail
            ~message:"expected class type path after # during Ceibo -> CST lifting"
            ~syntax_node:node
            ~context:[ "core_type.class" ]
    | None -> bail
      ~message:"expected class type marker during Ceibo -> CST lifting"
      ~syntax_node:node
      ~context:[ "core_type.class" ]
  and object_type_field_from_node node =
    match first_ident_token_in_subtree node, direct_non_trivia_nodes node |> List.find_opt can_lift_core_type_node with
    | Some field_name, Some field_type_node ->
        ({
            Cst.syntax_node = node;
            field_name;
            colon_token =
              (
                match direct_token_with_text node ":" with
                | Some colon_token -> colon_token
                | None -> bail
                  ~message:"expected object type field colon token during Ceibo -> CST lifting"
                  ~syntax_node:node
                  ~context:[ "core_type.object_field"; "colon_token" ]
              );
            field_type = core_type_from_node field_type_node;
            semicolon_token = direct_token_with_text node semicolon_text;
          }: Cst.object_type_field)
    | _ -> bail
      ~message:"expected object type field name and type during Ceibo -> CST lifting"
      ~syntax_node:node
      ~context:[ "core_type.object_field" ]
  and record_type_field_from_node node =
    let field_name =
      match direct_non_trivia_tokens node with
      | mutable_kw :: name_token :: _ when String.equal (Ceibo.Red.SyntaxToken.text mutable_kw) "mutable" -> Some (token
        name_token)
      | name_token :: _ -> Some (token name_token)
      | [] -> None
    in
    let field_type_node = direct_non_trivia_nodes node |> List.find_opt can_lift_core_type_node in
    match field_name, field_type_node with
    | Some field_name, Some field_type_node ->
        let field_type_node, attributes = peel_outer_type_attributes field_type_node in
        {
          Cst.syntax_node = node;
          field_name;
          mutable_token =
            (
              match direct_non_trivia_tokens node with
              | first :: _ when String.equal (Ceibo.Red.SyntaxToken.text first) "mutable" -> Some (token
                first)
              | _ -> None
            );
          colon_token =
            (
              match direct_token_with_text node ":" with
              | Some colon_token -> colon_token
              | None -> bail
                ~message:"expected record type field colon token during Ceibo -> CST lifting"
                ~syntax_node:node
                ~context:[ "core_type.record_field"; "colon_token" ]
            );
          field_type = core_type_from_node field_type_node;
          semicolon_token = direct_token_with_text node semicolon_text;
          attributes;
        }
    | _ -> bail
      ~message:"expected record type field name and type during Ceibo -> CST lifting"
      ~syntax_node:node
      ~context:[ "core_type.record_field" ]
  and poly_variant_tag_from_node node =
    let direct_children = direct_non_trivia_nodes node in
    let lifted_attributes = direct_children
    |> List.filter is_attribute_node
    |> List.filter (fun attribute_node -> not (can_lift_core_type_node attribute_node))
    |> List.map attribute_from_node in
    match direct_non_trivia_tokens node with
    | _backtick :: tag_name :: _ ->
        {
          Cst.syntax_node = node;
          attributes = lifted_attributes;
          bar_token = previous_direct_token_with_text_in_parent ~text:"|" node |> Option.map token;
          tag_name = token tag_name;
          separator_token = direct_token_with_text node "of";
          payload_type = (direct_children |> List.find_opt can_lift_core_type_node |> Option.map core_type_from_node);
        }
    | tag_name :: _ ->
        {
          Cst.syntax_node = node;
          attributes = lifted_attributes;
          bar_token = previous_direct_token_with_text_in_parent ~text:"|" node |> Option.map token;
          tag_name = token tag_name;
          separator_token = direct_token_with_text node "of";
          payload_type = (direct_children |> List.find_opt can_lift_core_type_node |> Option.map core_type_from_node);
        }
    | [] -> bail
      ~message:"expected poly-variant tag token during Ceibo -> CST lifting"
      ~syntax_node:node
      ~context:[ "core_type.poly_variant_tag" ]
  and poly_variant_bound_from_node node =
    match direct_non_trivia_tokens node with
    | _open_bracket :: marker_token :: _ when String.equal
      (Ceibo.Red.SyntaxToken.text marker_token)
      "<" -> Cst.PolyVariantBound.UpperBound { marker_token = token marker_token }
    | _open_bracket :: marker_token :: _ when String.equal
      (Ceibo.Red.SyntaxToken.text marker_token)
      ">" -> Cst.PolyVariantBound.LowerBound { marker_token = token marker_token }
    | _ -> Cst.PolyVariantBound.Exact
  and row_field_from_node node =
    match Ceibo.Red.SyntaxNode.kind node with
    | Syntax_kind.POLY_VARIANT_TAG ->
        Cst.RowField.Tag (poly_variant_tag_from_node node)
    | _ when can_lift_core_type_node node ->
        let inherited_type =
          match Ceibo.Red.SyntaxNode.kind node with
          | Syntax_kind.TYPE_CONSTR -> Cst.CoreType.Constr {
            syntax_node = node;
            constructor_path = type_constructor_path_from_node node;
            arguments = []
          }
          | _ -> core_type_from_node node
        in
        let bar_token = previous_direct_token_with_text_in_parent ~text:"|" node |> Option.map token in
        Cst.RowField.Inherit { bar_token; syntax_node = node; type_ = inherited_type }
    | _ ->
        bail
          ~message:"expected polymorphic variant row field during Ceibo -> CST lifting"
          ~syntax_node:node
          ~context:[ "core_type.poly_variant.row_field" ]
  and poly_variant_from_node node =
    let direct_tokens = direct_non_trivia_tokens node in
    let opening_token, closing_token =
      match direct_tokens with
      | opening_token :: _ ->
          let closing_token = List.hd (List.rev direct_tokens) in
          (token opening_token, token closing_token)
      | [] -> bail
        ~message:"expected polyvariant delimiters during Ceibo -> CST lifting"
        ~syntax_node:node
        ~context:[ "core_type.poly_variant" ]
    in
    {
      Cst.syntax_node = node;
      opening_token;
      kind = poly_variant_bound_from_node node;
      fields =
        direct_non_trivia_nodes node |> List.filter
          (fun child ->
            let kind = Ceibo.Red.SyntaxNode.kind child in
            kind = Syntax_kind.POLY_VARIANT_TAG || can_lift_core_type_node child) |> List.map row_field_from_node;
      closing_token;
    }
  in
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.TYPE_VAR -> (
      match direct_non_trivia_tokens node with
      | first_token :: _ as syntax_tokens ->
          let syntax_token = List.hd (List.rev syntax_tokens) in
          let lifted = token syntax_token in
          if String.equal (Cst.Token.text lifted) "_" then
            Cst.CoreType.Wildcard { syntax_node = node; wildcard_token = lifted }
          else
            let sigil_token =
              match first_token with
              | first when String.equal (Ceibo.Red.SyntaxToken.text first) "'" -> Some (token first)
              | _ -> None
            in
            Cst.CoreType.Var { syntax_node = node; sigil_token; name_token = lifted }
      | [] -> bail
        ~message:"expected type variable token during Ceibo -> CST lifting"
        ~syntax_node:node
        ~context:[ "core_type.var" ]
    )
  | Syntax_kind.TYPE_CONSTR ->
      let child_types = child_type_nodes node in
      let non_trivia_tokens = direct_non_trivia_tokens node in
      let opens_with_lparen =
        match non_trivia_tokens with
        | first :: _ -> String.equal (Ceibo.Red.SyntaxToken.text first) "("
        | [] -> false
      in
      let closes_with_rparen =
        match List.rev non_trivia_tokens with
        | last :: _ -> String.equal (Ceibo.Red.SyntaxToken.text last) ")"
        | [] -> false
      in
      if opens_with_lparen && closes_with_rparen && List.length child_types = 1 then
        match child_types with
        | [ inner_type ] -> Cst.CoreType.Parenthesized {
          syntax_node = node;
          inner = core_type_from_node inner_type
        }
        | _ -> bail
          ~message:"expected a single inner type inside parenthesized type"
          ~syntax_node:node
          ~context:[ "core_type.parenthesized" ]
      else if non_trivia_tokens |> List.exists
          (fun syntax_token ->
            String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "#") then
        let hash_token, class_path = class_type_path_from_node node in
        Cst.CoreType.Class {
          syntax_node = node;
          hash_token;
          class_path;
          arguments = child_types |> List.map core_type_from_node
        }
      else
        Cst.CoreType.Constr {
          syntax_node = node;
          constructor_path = type_constructor_path_from_node node;
          arguments = child_types |> List.map core_type_from_node
        }
  | Syntax_kind.TYPE_ALIAS -> (
      match child_type_nodes node with
      | type_node :: alias_node :: _ -> (
          match direct_non_trivia_tokens alias_node with
          | [ alias_token ] -> Cst.CoreType.Alias {
            syntax_node = node;
            type_ = core_type_from_node type_node;
            sigil_token = None;
            name_token = token alias_token
          }
          | [sigil_token;alias_token] -> Cst.CoreType.Alias {
            syntax_node = node;
            type_ = core_type_from_node type_node;
            sigil_token = Some (token sigil_token);
            name_token = token alias_token
          }
          | [] -> bail
            ~message:"expected alias name token during Ceibo -> CST lifting"
            ~syntax_node:alias_node
            ~context:[ "core_type.alias" ]
          | _ -> bail
            ~message:"expected alias sigil/name tokens during Ceibo -> CST lifting"
            ~syntax_node:alias_node
            ~context:[ "core_type.alias" ]
        )
      | _ -> bail
        ~message:"expected aliased type and alias variable during Ceibo -> CST lifting"
        ~syntax_node:node
        ~context:[ "core_type.alias" ]
    )
  | Syntax_kind.ATTRIBUTE_EXPR -> (
      match direct_non_trivia_nodes node with
      | first_child :: rest -> (
          match List.find_opt can_lift_core_type_node (first_child :: rest), List.find_opt
            is_attribute_node
            rest with
          | Some payload_node, Some attribute_node -> Cst.CoreType.Attribute {
            syntax_node = node;
            type_ = core_type_from_node payload_node;
            attribute = attribute_from_node attribute_node
          }
          | _ -> bail
            ~message:"expected attribute payload during Ceibo -> CST lifting"
            ~syntax_node:node
            ~context:[ "core_type.attribute" ]
        )
      | [] -> bail
        ~message:"expected attributed type payload during Ceibo -> CST lifting"
        ~syntax_node:node
        ~context:[ "core_type.attribute" ]
    )
  | Syntax_kind.EXTENSION_EXPR ->
      Cst.CoreType.Extension (extension_from_node node)
  | Syntax_kind.POLY_TYPE -> (
      let binders = type_binders_from_poly_type_node node in
      let type_keyword_token =
        match direct_non_trivia_tokens node with
        | first :: _ when String.equal (Ceibo.Red.SyntaxToken.text first) "type" -> Some (token first)
        | _ -> None
      in
      let dot_token =
        match
          direct_non_trivia_tokens node |> List.rev |> List.find_opt
            (fun syntax_token ->
              String.equal (Ceibo.Red.SyntaxToken.text syntax_token) ".")
        with
        | Some dot_token -> token dot_token
        | None -> bail
          ~message:"expected quantified type dot token during Ceibo -> CST lifting"
          ~syntax_node:node
          ~context:[ "core_type.poly"; "dot_token" ]
      in
      let body_node = direct_non_trivia_nodes node
      |> List.rev
      |> List.find_opt
        (fun child ->
          can_lift_core_type_node child && Ceibo.Red.SyntaxNode.kind child != Syntax_kind.TYPE_VAR) in
      match binders, body_node with
      | _ :: _, Some body_node ->
          Cst.CoreType.Poly {
            syntax_node = node;
            type_keyword_token;
            dot_token;
            binders;
            body = core_type_from_node body_node;
          }
      | [], _ -> bail
        ~message:"expected quantified type binders during Ceibo -> CST lifting"
        ~syntax_node:node
        ~context:[ "core_type.poly" ]
      | _, None -> bail
        ~message:"expected quantified type body during Ceibo -> CST lifting"
        ~syntax_node:node
        ~context:[ "core_type.poly" ]
    )
  | Syntax_kind.TYPE_ARROW -> (
      match child_type_nodes node with
      | parameter_node :: result_node :: _ -> Cst.CoreType.Arrow {
        syntax_node = node;
        label = arrow_label_from_node node;
        parameter_type = core_type_from_node parameter_node;
        result_type = core_type_from_node result_node
      }
      | _ -> bail
        ~message:"expected arrow parameter and result types during Ceibo -> CST lifting"
        ~syntax_node:node
        ~context:[ "core_type.arrow" ]
    )
  | Syntax_kind.TYPE_TUPLE ->
      Cst.CoreType.Tuple {
        syntax_node = node;
        elements = child_type_nodes node |> List.map core_type_from_node
      }
  | Syntax_kind.TYPE_PAREN -> (
      match child_type_nodes node with
      | inner_node :: _ -> Cst.CoreType.Parenthesized {
        syntax_node = node;
        inner = core_type_from_node inner_node
      }
      | [] -> bail
        ~message:"expected inner type inside parenthesized type during Ceibo -> CST lifting"
        ~syntax_node:node
        ~context:[ "core_type.parenthesized" ]
    )
  | Syntax_kind.TYPE_POLY_VARIANT ->
      Cst.CoreType.PolyVariant (poly_variant_from_node node)
  | Syntax_kind.TYPE_RECORD ->
      let direct_tokens = direct_non_trivia_tokens node in
      let opening_token, closing_token =
        match direct_tokens with
        | opening_token :: _ ->
            let closing_token = List.hd (List.rev direct_tokens) in
            (token opening_token, token closing_token)
        | [] -> bail
          ~message:"expected record type delimiters during Ceibo -> CST lifting"
          ~syntax_node:node
          ~context:[ "core_type.record" ]
      in
      Cst.CoreType.Record {
        syntax_node = node;
        opening_token;
        fields = direct_non_trivia_nodes node
        |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_RECORD_FIELD)
        |> List.map record_type_field_from_node;
        separator_tokens =
          direct_tokens |> List.filter
            (fun syntax_token ->
              String.equal (Ceibo.Red.SyntaxToken.text syntax_token) semicolon_text) |> List.map token;
        closing_token;
      }
  | Syntax_kind.FIRST_CLASS_MODULE_TYPE ->
      let direct_tokens = direct_non_trivia_tokens node in
      let opening_token, closing_token =
        match direct_tokens with
        | opening_token :: _ ->
            let closing_token = List.hd (List.rev direct_tokens) in
            (token opening_token, token closing_token)
        | [] -> bail
          ~message:"expected first-class module type delimiters during Ceibo -> CST lifting"
          ~syntax_node:node
          ~context:[ "core_type.first_class_module" ]
      in
      (Cst.CoreType.FirstClassModule {
        syntax_node = node;
        opening_token;
        package_type = module_type_from_first_class_module_type_node node;
        closing_token
      })
  | Syntax_kind.OBJECT_TYPE ->
      let direct_tokens = direct_non_trivia_tokens node in
      let opening_token, closing_token =
        match direct_tokens with
        | opening_token :: _ ->
            let closing_token = List.hd (List.rev direct_tokens) in
            (token opening_token, token closing_token)
        | [] -> bail
          ~message:"expected object type delimiters during Ceibo -> CST lifting"
          ~syntax_node:node
          ~context:[ "core_type.object" ]
      in
      Cst.CoreType.Object {
        syntax_node = node;
        opening_token;
        fields = direct_non_trivia_nodes node
        |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.OBJECT_TYPE_FIELD)
        |> List.map object_type_field_from_node;
        closing_token
      }
  | _ ->
      bail
        ~message:"unsupported core type shape during Ceibo -> CST lifting"
        ~syntax_node:node
        ~context:[ "core_type" ]

let rec pattern_from_node = fun node ->
  let pattern_children node = direct_non_trivia_nodes node
  |> List.filter (fun child -> is_pattern_syntax_kind (Ceibo.Red.SyntaxNode.kind child))
  |> List.map pattern_from_node in
  let rec peel_outer_pattern_attributes node =
    match Ceibo.Red.SyntaxNode.kind node with
    | Syntax_kind.ATTRIBUTE_EXPR -> (
        match direct_non_trivia_nodes node with
        | first_child :: rest -> (
            match List.find_opt
              (fun child -> is_pattern_syntax_kind (Ceibo.Red.SyntaxNode.kind child))
              (first_child :: rest), List.find_opt is_attribute_node rest with
            | Some payload_node, Some attribute_node ->
                let payload_node, attributes = peel_outer_pattern_attributes payload_node in
                (payload_node, attributes @ [ attribute_from_node attribute_node ])
            | _ -> (node, [])
          )
        | [] -> (node, [])
      )
    | _ -> (node, [])
  in
  let poly_variant_tag_token node =
    match direct_non_trivia_tokens node with
    | _backtick :: tag_syntax_token :: _ -> Some (token tag_syntax_token)
    | tag_syntax_token :: _ -> Some (token tag_syntax_token)
    | [] -> None
  in
  let rec tuple_pattern_label_token_from_node node =
    match Ceibo.Red.SyntaxNode.kind node with
    | Syntax_kind.IDENT_PATTERN -> (
        match direct_non_trivia_tokens node with
        | label_syntax_token :: _ -> token label_syntax_token
        | [] -> unsupported_pattern node
      )
    | Syntax_kind.TYPED_PATTERN -> (
        match direct_non_trivia_nodes node with
        | label_pattern_node :: _ -> tuple_pattern_label_token_from_node label_pattern_node
        | [] -> unsupported_pattern node
      )
    | _ ->
        unsupported_pattern node
  in
  let tuple_pattern_from_node node =
    let labeled_punning_element label_pattern_node = {
      Cst.label_token = Some (tuple_pattern_label_token_from_node label_pattern_node);
      pattern = pattern_from_node label_pattern_node
    } in
    let labeled_payload_element label_pattern_node payload_pattern_node = {
      Cst.label_token = Some (tuple_pattern_label_token_from_node label_pattern_node);
      pattern = pattern_from_node payload_pattern_node
    } in
    let unlabeled_element pattern_node = {
      Cst.label_token = None;
      pattern = pattern_from_node pattern_node
    } in
    let rec loop children saw_tilde pending_label awaiting_payload acc =
      match children with
      | [] ->
          let acc =
            match pending_label, awaiting_payload with
            | Some label_pattern_node, false -> labeled_punning_element label_pattern_node :: acc
            | _ -> acc
          in
          { Cst.syntax_node = node; elements = List.rev acc; open_tail = None; attributes = [] }
      | Ceibo.Red.Token syntax_token :: rest when is_trivia (Ceibo.Red.SyntaxToken.kind syntax_token) ->
          loop rest saw_tilde pending_label awaiting_payload acc
      | Ceibo.Red.Token syntax_token :: rest ->
          let text = Ceibo.Red.SyntaxToken.text syntax_token in
          if String.equal text "~" then
            loop rest true pending_label awaiting_payload acc
          else if String.equal text ":" then
            loop rest saw_tilde pending_label true acc
          else if String.equal text "," then
            let acc =
              match pending_label, awaiting_payload with
              | Some label_pattern_node, false -> labeled_punning_element label_pattern_node :: acc
              | _ -> acc
            in
            loop rest false None false acc
          else if String.equal text ".." then
            let acc =
              match pending_label, awaiting_payload with
              | Some label_pattern_node, false -> labeled_punning_element label_pattern_node :: acc
              | _ -> acc
            in
            {
              Cst.syntax_node = node;
              elements = List.rev acc;
              open_tail = Some { dotdot_token = token syntax_token };
              attributes = []
            }
          else if String.equal text "(" || String.equal text ")" then
            loop rest saw_tilde pending_label awaiting_payload acc
          else
            loop rest saw_tilde pending_label awaiting_payload acc
      | Ceibo.Red.Node child :: rest ->
          if is_pattern_syntax_kind (Ceibo.Red.SyntaxNode.kind child) then
            if awaiting_payload then
              match pending_label with
              | Some label_pattern_node -> loop
                rest
                false
                None
                false
                (labeled_payload_element label_pattern_node child :: acc)
              | None -> loop rest false None false (unlabeled_element child :: acc)
            else if saw_tilde then
              loop rest false (Some child) false acc
            else
              let acc =
                match pending_label with
                | Some label_pattern_node -> labeled_punning_element label_pattern_node :: acc
                | None -> acc
              in
              loop rest false None false (unlabeled_element child :: acc)
          else
            loop rest saw_tilde pending_label awaiting_payload acc
    in
    Ceibo.Red.SyntaxNode.children node |> fun children -> loop children false None false []
  in
  let node, attributes = peel_outer_pattern_attributes node in
  let pattern =
    match Ceibo.Red.SyntaxNode.kind node with
    | Syntax_kind.IDENT_PATTERN -> (
        match direct_non_trivia_tokens node with
        | first :: _ -> Cst.Pattern.Identifier {
          syntax_node = node;
          name_token = token first;
          attributes = []
        }
        | [] -> unsupported_pattern node
      )
    | Syntax_kind.WILDCARD_PATTERN ->
        Cst.Pattern.Wildcard { syntax_node = node; attributes = [] }
    | Syntax_kind.ATTRIBUTE_EXPR ->
        unsupported_pattern node
    | Syntax_kind.EXTENSION_EXPR ->
        Cst.Pattern.Extension {
          syntax_node = node;
          extension = extension_from_node node;
          attributes = []
        }
    | Syntax_kind.LAZY_PATTERN -> (
        match direct_non_trivia_nodes node with
        | inner_node :: _ -> Cst.Pattern.Lazy {
          syntax_node = node;
          pattern = pattern_from_node inner_node;
          attributes = []
        }
        | _ -> unsupported_pattern node
      )
    | Syntax_kind.EXCEPTION_PATTERN -> (
        match direct_non_trivia_nodes node with
        | inner_node :: _ -> Cst.Pattern.Exception {
          syntax_node = node;
          keyword_token = direct_required_token_with_text ~context:[ "exception_pattern" ] node "exception";
          pattern = pattern_from_node inner_node;
          attributes = []
        }
        | _ -> unsupported_pattern node
      )
    | Syntax_kind.RANGE_PATTERN -> (
        match direct_non_trivia_tokens node with
        | lower_syntax_token :: _range_syntax_token :: upper_syntax_token :: _ -> Cst.Pattern.Range {
          syntax_node = node;
          lower = constant_from_syntax_token ~syntax_node:node lower_syntax_token;
          upper = constant_from_syntax_token ~syntax_node:node upper_syntax_token;
          attributes = []
        }
        | _ -> unsupported_pattern node
      )
    | Syntax_kind.OPERATOR_PATTERN ->
        Cst.Pattern.Operator {
          syntax_node = node;
          operator_tokens = operator_tokens_from_node node;
          attributes = []
        }
    | Syntax_kind.FIRST_CLASS_MODULE_PATTERN -> (
        match direct_non_trivia_tokens node with
        | lparen :: _module_kw :: binding_syntax_token :: _ ->
            let closing_token =
              match List.rev (direct_non_trivia_tokens node) with
              | closing_syntax_token :: _ -> token closing_syntax_token
              | [] -> bail
                ~message:"expected first-class module pattern closing delimiter during Ceibo -> CST lifting"
                ~syntax_node:node
                ~context:[ "pattern.first_class_module"; "closing_token" ]
            in
            Cst.Pattern.FirstClassModule {
              syntax_node = node;
              opening_token = token lparen;
              binding =
                if String.equal (Ceibo.Red.SyntaxToken.text binding_syntax_token) "_" then
                  Cst.Anonymous { wildcard_token = token binding_syntax_token }
                else
                  Cst.Named { name_token = token binding_syntax_token };
              colon_token = token_with_text node ":";
              package_type = (direct_non_trivia_nodes node
              |> List.find_opt can_lift_module_type_node
              |> Option.map package_type_from_module_type_node);
              closing_token;
              attributes = [];
            }
        | _ -> unsupported_pattern node
      )
    | Syntax_kind.STRING_LITERAL
    | Syntax_kind.INT_LITERAL
    | Syntax_kind.FLOAT_LITERAL
    | Syntax_kind.CHAR_LITERAL
    | Syntax_kind.BOOL_LITERAL
    | Syntax_kind.UNIT_LITERAL ->
        Cst.Pattern.Literal {
          syntax_node = node;
          literal = constant_from_node node;
          attributes = []
        }
    | Syntax_kind.POLY_VARIANT_PATTERN -> (
        match poly_variant_tag_token node with
        | Some tag_token -> Cst.Pattern.PolyVariant {
          syntax_node = node;
          tag_token;
          payload = (direct_non_trivia_nodes node
          |> List.find_opt (fun child -> is_pattern_syntax_kind (Ceibo.Red.SyntaxNode.kind child))
          |> Option.map pattern_from_node);
          attributes = []
        }
        | None -> unsupported_pattern node
      )
    | Syntax_kind.POLY_VARIANT_TYPE_PATTERN ->
        Cst.Pattern.PolyVariantInherit {
          syntax_node = node;
          type_path = poly_variant_type_path_from_node node;
          attributes = []
        }
    | Syntax_kind.CONSTRUCTOR_PATTERN ->
        let direct_nodes = direct_non_trivia_nodes node in
        let arguments = direct_nodes
        |> List.filter (fun child -> is_pattern_syntax_kind (Ceibo.Red.SyntaxNode.kind child))
        |> List.map pattern_from_node in
        Cst.Pattern.Constructor {
          syntax_node = node;
          constructor_path = module_path_from_node node;
          existentials = constructor_pattern_existentials_from_nodes direct_nodes;
          arguments;
          attributes = [];
        }
    | Syntax_kind.TUPLE_PATTERN ->
        Cst.Pattern.Tuple (tuple_pattern_from_node node)
    | Syntax_kind.LIST_PATTERN ->
        let direct_tokens = direct_non_trivia_tokens node in
        let opening_token, closing_token =
          match direct_tokens with
          | opening_token :: _ ->
              let closing_token = List.hd (List.rev direct_tokens) in
              (token opening_token, token closing_token)
          | [] -> bail
            ~message:"expected list pattern delimiters during Ceibo -> CST lifting"
            ~syntax_node:node
            ~context:[ "pattern.list" ]
        in
        Cst.Pattern.List {
          syntax_node = node;
          opening_token;
          elements = pattern_children node;
          separator_tokens =
            direct_tokens |> List.filter
              (fun syntax_token ->
                String.equal (Ceibo.Red.SyntaxToken.text syntax_token) semicolon_text) |> List.map token;
          closing_token;
          attributes = [];
        }
    | Syntax_kind.ARRAY_PATTERN ->
        let direct_tokens = direct_non_trivia_tokens node in
        let opening_token, closing_token =
          match direct_tokens with
          | opening_token :: _ ->
              let closing_token = List.hd (List.rev direct_tokens) in
              (token opening_token, token closing_token)
          | [] -> bail
            ~message:"expected array pattern delimiters during Ceibo -> CST lifting"
            ~syntax_node:node
            ~context:[ "pattern.array" ]
        in
        Cst.Pattern.Array {
          syntax_node = node;
          opening_token;
          elements = pattern_children node;
          separator_tokens =
            direct_tokens |> List.filter
              (fun syntax_token ->
                String.equal (Ceibo.Red.SyntaxToken.text syntax_token) semicolon_text) |> List.map token;
          closing_token;
          attributes = [];
        }
    | Syntax_kind.RECORD_PATTERN ->
        let direct_tokens = direct_non_trivia_tokens node in
        let opening_token, closing_token =
          match direct_tokens with
          | opening_token :: _ ->
              let closing_token = List.hd (List.rev direct_tokens) in
              (token opening_token, token closing_token)
          | [] -> bail
            ~message:"expected record pattern delimiters during Ceibo -> CST lifting"
            ~syntax_node:node
            ~context:[ "pattern.record" ]
        in
        let closedness =
          match
            direct_tokens |> List.find_opt
              (fun syntax_token ->
                String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "_")
          with
          | Some wildcard_token -> Cst.Open { wildcard_token = token wildcard_token }
          | None -> Cst.Closed
        in
        Cst.Pattern.Record {
          syntax_node = node;
          opening_token;
          fields = direct_non_trivia_nodes node
          |> List.filter
            (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.RECORD_FIELD_PATTERN)
          |> List.filter_map record_pattern_field_from_node;
          separator_tokens =
            direct_tokens |> List.filter
              (fun syntax_token ->
                String.equal (Ceibo.Red.SyntaxToken.text syntax_token) semicolon_text) |> List.map token;
          closedness;
          closing_token;
          attributes = [];
        }
    | Syntax_kind.CONS_PATTERN -> (
        match direct_non_trivia_nodes node with
        | head_node :: tail_node :: _ -> Cst.Pattern.Cons {
          syntax_node = node;
          head = pattern_from_node head_node;
          tail = pattern_from_node tail_node;
          attributes = []
        }
        | _ -> unsupported_pattern node
      )
    | Syntax_kind.OR_PATTERN ->
        Cst.Pattern.Or {
          syntax_node = node;
          alternatives = pattern_children node;
          separator_tokens =
            direct_non_trivia_tokens node |> List.filter
              (fun syntax_token ->
                String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "|") |> List.map token;
          attributes = [];
        }
    | Syntax_kind.AS_PATTERN -> (
        match direct_non_trivia_nodes node, List.rev (direct_non_trivia_tokens node) with
        | pattern_node :: _, name_syntax_token :: _ -> Cst.Pattern.Alias {
          syntax_node = node;
          pattern = pattern_from_node pattern_node;
          name_token = token name_syntax_token;
          attributes = []
        }
        | _ -> unsupported_pattern node
      )
    | Syntax_kind.TYPED_PATTERN -> (
        match direct_non_trivia_nodes node with
        | pattern_node :: type_node :: _ ->
            Cst.Pattern.Typed {
              syntax_node = node;
              pattern = pattern_from_node pattern_node;
              colon_token =
                (
                  match direct_token_with_text node ":" with
                  | Some colon_token -> colon_token
                  | None -> bail
                    ~message:"expected typed pattern colon token during Ceibo -> CST lifting"
                    ~syntax_node:node
                    ~context:[ "pattern.typed"; "colon_token" ]
                );
              type_ = core_type_from_node type_node;
              attributes = [];
            }
        | _ -> unsupported_pattern node
      )
    | Syntax_kind.EFFECT_PATTERN -> (
        match effect_pattern_from_node node with
        | Some pattern -> Cst.Pattern.Effect pattern
        | None -> unsupported_pattern node
      )
    | Syntax_kind.LOCAL_OPEN_PATTERN -> (
        match local_open_pattern_from_node node with
        | Some pattern -> Cst.Pattern.LocalOpen pattern
        | None -> unsupported_pattern node
      )
    | Syntax_kind.PAREN_PATTERN -> (
        match direct_non_trivia_nodes node with
        | inner_node :: _ -> Cst.Pattern.Parenthesized {
          syntax_node = node;
          inner = pattern_from_node inner_node;
          attributes = []
        }
        | [] -> unsupported_pattern node
      )
    | _ ->
        unsupported_pattern node
  in
  pattern_with_attributes pattern attributes

and record_pattern_field_from_node = fun node ->
  let lifted_field_path =
    let tokens = direct_non_trivia_tokens node |> take_tokens_until_equals [] in
    module_path_from_tokens ~syntax_node:node tokens
  in
  match Cst.Ident.segments lifted_field_path with
  | [] -> None
  | _ -> Some {
    syntax_node = node;
    field_path = lifted_field_path;
    equals_token = direct_token_with_text node "=";
    pattern = (direct_non_trivia_nodes node
    |> List.find_opt (fun child -> is_pattern_syntax_kind (Ceibo.Red.SyntaxNode.kind child))
    |> Option.map pattern_from_node)
  }

and local_open_pattern_from_node = fun node ->
  match direct_non_trivia_nodes node with
  | module_path_node :: pattern_node :: _ ->
      let dot_token = direct_required_token_with_text ~context:[ "pattern"; "local_open" ] node "." in
      let module_path_span = span_of_syntax_node_nontrivia_bounds module_path_node in
      let pattern_span = span_of_syntax_node_nontrivia_bounds pattern_node in
      let boundary_tokens = direct_tokens_between_offsets
        ~after_offset:module_path_span.end_
        ~before_offset:pattern_span.start
        node in
      let opening_token =
        boundary_tokens
        |> List.find_opt
          (fun token ->
            let text = Cst.Token.text token in
            String.equal text "("
            || String.equal text "["
            || String.equal text "[|"
            || String.equal text "{")
      in
      let trailing_tokens = direct_tokens_between_offsets
        ~after_offset:pattern_span.end_
        ~before_offset:(span_of_syntax_node_nontrivia_bounds node).end_
        node in
      let closing_token =
        trailing_tokens
        |> List.find_opt
          (fun token ->
            let text = Cst.Token.text token in
            String.equal text ")"
            || String.equal text "]"
            || String.equal text "|]"
            || String.equal text "}")
      in
      Some {
        syntax_node = node;
        module_path = module_path_like_from_node module_path_node;
        dot_token;
        opening_token;
        pattern = pattern_from_node pattern_node;
        closing_token;
        attributes = [];
      }
  | _ -> None

and effect_pattern_from_node = fun node ->
  match direct_non_trivia_nodes node with
  | effect_node :: continuation_node :: _ -> Some {
    syntax_node = node;
    effect_pattern = pattern_from_node effect_node;
    continuation = pattern_from_node continuation_node;
    attributes = []
  }
  | _ -> None

let rec parameter_from_node = fun node ->
  let rec binding_name_token_from_pattern = function
    | Cst.Pattern.Identifier { name_token; _ } -> Some name_token
    | Cst.Pattern.Typed { pattern; _ }
    | Cst.Pattern.Lazy { pattern; _ }
    | Cst.Pattern.LocalOpen { pattern; _ } -> binding_name_token_from_pattern pattern
    | Cst.Pattern.Parenthesized { inner; _ } -> binding_name_token_from_pattern inner
    | Cst.Pattern.Alias { name_token; _ } -> Some name_token
    | _ -> None
  in
  let binding_name_matches_label ~label_name_token binding_name_token =
    match binding_name_token with
    | Some binding_name_token -> String.equal
      (Cst.Token.text binding_name_token)
      (Cst.Token.text label_name_token)
    | None -> false
  in
  let binding_pattern_from_direct_nodes ~label_name_token direct_nodes =
    match direct_nodes with
    | binding_pattern_node :: type_node :: _ when is_pattern_syntax_kind
      (Ceibo.Red.SyntaxNode.kind binding_pattern_node)
    && is_type_syntax_kind (Ceibo.Red.SyntaxNode.kind type_node) ->
        Some (
          Cst.Pattern.Typed {
            syntax_node = node;
            pattern = pattern_from_node binding_pattern_node;
            colon_token =
              (
                match direct_token_with_text node ":" with
                | Some colon_token -> colon_token
                | None -> bail
                  ~message:"expected typed parameter colon token during Ceibo -> CST lifting"
                  ~syntax_node:node
                  ~context:[ "parameter.typed_pattern"; "colon_token" ]
              );
            type_ = core_type_from_node type_node;
            attributes = [];
          }
        )
    | binding_pattern_node :: _ when is_pattern_syntax_kind
      (Ceibo.Red.SyntaxNode.kind binding_pattern_node) ->
        Some (pattern_from_node binding_pattern_node)
    | type_node :: _ when is_type_syntax_kind (Ceibo.Red.SyntaxNode.kind type_node) ->
        let identifier_pattern = Cst.Pattern.Identifier {
          syntax_node = node;
          name_token = label_name_token;
          attributes = []
        } in
        Some (
          Cst.Pattern.Typed {
            syntax_node = node;
            pattern = identifier_pattern;
            colon_token =
              (
                match direct_token_with_text node ":" with
                | Some colon_token -> colon_token
                | None -> bail
                  ~message:"expected typed parameter colon token during Ceibo -> CST lifting"
                  ~syntax_node:node
                  ~context:[ "parameter.typed_pattern"; "colon_token" ]
              );
            type_ = core_type_from_node type_node;
            attributes = [];
          }
        )
    | _ ->
        None
  in
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.LABELED_PARAM -> (
      let direct_nodes = direct_non_trivia_nodes node in
      match token_with_text node "~", first_ident_token_in_subtree node with
      | Some sigil_token, Some label_name_token ->
          let binding_pattern = binding_pattern_from_direct_nodes ~label_name_token direct_nodes in
          let binding_name_token =
            match binding_pattern with
            | Some pattern -> binding_name_token_from_pattern pattern
            | None -> None
          in
          Cst.Parameter.Labeled {
            syntax_node = node;
            sigil_token = sigil_token;
            label_token = label_name_token;
            colon_token = token_with_text node ":";
            binding_name_token;
            binding_name_matches_label = binding_name_matches_label ~label_name_token binding_name_token;
            binding_pattern;
          }
      | _ -> unsupported_parameter node
    )
  | Syntax_kind.OPTIONAL_PARAM -> (
      let direct_nodes = direct_non_trivia_nodes node in
      match token_with_text node "?", first_ident_token_in_subtree node with
      | Some sigil_token, Some label_name_token ->
          let binding_pattern = binding_pattern_from_direct_nodes ~label_name_token direct_nodes in
          let default_value =
            if Option.is_some (token_with_text node "=") then
              direct_nodes |> List.rev |> List.find_opt can_lift_expression_node |> Option.map expression_from_node
            else
              None
          in
          let binding_name_token =
            match binding_pattern with
            | Some pattern -> binding_name_token_from_pattern pattern
            | None -> None
          in
          Cst.Parameter.Optional {
            syntax_node = node;
            sigil_token = sigil_token;
            label_token = label_name_token;
            colon_token = token_with_text node ":";
            equals_token = token_with_text node "=";
            binding_name_token;
            binding_name_matches_label = binding_name_matches_label ~label_name_token binding_name_token;
            default_value;
            binding_pattern;
          }
      | _ -> unsupported_parameter node
    )
  | Syntax_kind.OPTIONAL_PARAM_DEFAULT -> (
      let direct_nodes = direct_non_trivia_nodes node in
      match token_with_text node "?", direct_nodes with
      | Some sigil_token, _ -> (
          let label_token =
            match first_ident_token_in_subtree node with
            | Some token -> token
            | None -> bail
              ~message:"expected optional parameter label during Ceibo -> CST lifting"
              ~syntax_node:node
              ~context:[ "parameter.optional_default" ]
          in
          let binding_pattern = binding_pattern_from_direct_nodes ~label_name_token:label_token direct_nodes in
          let default_value = direct_nodes
          |> List.rev
          |> List.find_opt can_lift_expression_node
          |> Option.map expression_from_node in
          match binding_pattern, default_value with
          | Some binding_pattern, Some default_value ->
              let binding_name_token = binding_name_token_from_pattern binding_pattern in
              Cst.Parameter.Optional {
                syntax_node = node;
                sigil_token = sigil_token;
                label_token;
                colon_token = token_with_text node ":";
                equals_token = token_with_text node "=";
                binding_name_token;
                binding_name_matches_label = binding_name_matches_label
                  ~label_name_token:label_token
                  binding_name_token;
                default_value = Some default_value;
                binding_pattern = Some binding_pattern;
              }
          | _ -> unsupported_parameter node
        )
      | _ -> unsupported_parameter node
    )
  | Syntax_kind.LOCALLY_ABSTRACT_TYPE_PARAM ->
      Cst.Parameter.LocallyAbstract (locally_abstract_type_parameter_from_node node)
  | _ ->
      Cst.Parameter.Positional {
        syntax_node = node;
        pattern = pattern_from_node node;
        name_token = simple_pattern_name_token node
      }

and parameter_with_attributes = fun parameter attributes ->
  match attributes, parameter with
  | [], _ ->
      parameter
  | _, Cst.Parameter.Positional parameter ->
      Cst.Parameter.Positional {
        parameter
        with pattern = pattern_with_attributes parameter.pattern attributes
      }
  | _, Cst.Parameter.Labeled parameter -> (
      match parameter.binding_pattern with
      | Some pattern -> Cst.Parameter.Labeled {
        parameter
        with binding_pattern = Some (pattern_with_attributes pattern attributes)
      }
      | None -> Cst.Parameter.Labeled parameter
    )
  | _, Cst.Parameter.Optional parameter -> (
      match parameter.binding_pattern with
      | Some pattern -> Cst.Parameter.Optional {
        parameter
        with binding_pattern = Some (pattern_with_attributes pattern attributes)
      }
      | None -> Cst.Parameter.Optional parameter
    )
  | _, Cst.Parameter.LocallyAbstract _ ->
      parameter

and parameter_candidate_node = fun node ->
  let kind = Ceibo.Red.SyntaxNode.kind node in
  is_parameter_like_kind kind
  || (kind = Syntax_kind.ATTRIBUTE_EXPR && not (standalone_attribute_node node))

and parameters_from_nodes = fun nodes ->
  let rec loop = fun pending_attributes acc ->
    function
    | [] ->
        List.rev acc
    | node :: rest when standalone_attribute_node node ->
        loop (pending_attributes @ [ attribute_from_node node ]) acc rest
    | node :: rest when parameter_candidate_node node ->
        let parameter = parameter_with_attributes (parameter_from_node node) pending_attributes in
        loop [] (parameter :: acc) rest
    | _ :: rest ->
        loop pending_attributes acc rest
  in
  loop [] [] nodes

and apply_argument_from_node = fun node ->
  let first_nontrivia_expression_child node = direct_non_trivia_nodes node
  |> List.find_opt can_lift_expression_node
  |> Option.map expression_from_node in
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.LABELED_ARG -> (
      match direct_non_trivia_tokens node with
      | sigil_syntax_token :: label_syntax_token :: _ -> Cst.Labeled {
        syntax_node = node;
        sigil_token = token sigil_syntax_token;
        label_token = token label_syntax_token;
        value = first_nontrivia_expression_child node
      }
      | _ -> unsupported_expression node
    )
  | Syntax_kind.OPTIONAL_ARG -> (
      match direct_non_trivia_tokens node with
      | sigil_syntax_token :: label_syntax_token :: _ -> Cst.Optional {
        syntax_node = node;
        sigil_token = token sigil_syntax_token;
        label_token = token label_syntax_token;
        value = first_nontrivia_expression_child node
      }
      | _ -> unsupported_expression node
    )
  | _ ->
      Cst.Positional (expression_from_node node)

and collect_apply_arguments = fun acc ->
  function
  | Cst.Expression.Apply { callee; argument; _ } -> collect_apply_arguments (argument :: acc) callee
  | expression -> (expression, acc)

and rebuild_apply_chain = fun ~syntax_node callee ->
  function
  | [] -> callee
  | arguments -> List.fold_left
    (fun callee argument -> Cst.Expression.Apply { syntax_node; callee; argument; attributes = [] })
    callee
    arguments

and split_greedy_argument_value = fun value ->
  match value with
  | Cst.Expression.Infix { left; operator_tokens; right; _ } ->
      let head, extra_arguments, tail = split_greedy_argument_value left in
      let tail =
        match tail with
        | None -> Some (operator_tokens, right)
        | Some (tail_operator_tokens, tail_right) ->
            Some (
              tail_operator_tokens,
              Cst.Expression.Infix {
                syntax_node = Cst.Expression.syntax_node value;
                left = tail_right;
                operator_tokens;
                right;
                attributes = [];
              }
            )
      in
      (head, extra_arguments, tail)
  | Cst.Expression.PolyVariant ({ payload=Some payload; _ } as poly_variant) ->
      let head = Cst.Expression.PolyVariant { poly_variant with payload = None } in
      (head, [ Cst.Positional payload ], None)
  | _ ->
      let head, extra_arguments = collect_apply_arguments [] value in
      (head, extra_arguments, None)

and normalize_greedy_tuple_argument_value = fun ~syntax_node ~callee make_argument ->
  function
  | Cst.Expression.Tuple { elements=first :: second :: rest; _ } -> (
      let trailing = second :: rest in
      let payload, extra_arguments, infix_tail = split_greedy_argument_value first in
      match extra_arguments, infix_tail with
      | _ :: _, None ->
          let left = rebuild_apply_chain
            ~syntax_node
            (Cst.Expression.Apply {
              syntax_node;
              callee;
              argument = make_argument payload;
              attributes = []
            })
            extra_arguments in
          Some (Cst.Expression.Tuple { syntax_node; elements = left :: trailing; attributes = [] })
      | _ -> None
    )
  | _ -> None

and normalize_greedy_labeled_argument = fun ~syntax_node ~callee argument ->
  let rebuild value extra_arguments make_argument =
    let callee = Cst.Expression.Apply {
      syntax_node;
      callee;
      argument = make_argument value;
      attributes = []
    } in
    rebuild_apply_chain ~syntax_node callee extra_arguments
  in
  let wrap_infix_tail = fun left ->
    function
    | Some (operator_tokens, right) ->
        Cst.Expression.Infix {
          syntax_node;
          left;
          operator_tokens;
          right;
          attributes = [];
        }
    | None -> left
  in
  match argument with
  | Cst.Labeled ({ value=Some value; _ } as labeled_argument) -> (
      match normalize_greedy_tuple_argument_value
        ~syntax_node
        ~callee
        (fun value -> Cst.Labeled { labeled_argument with value = Some value })
        value with
      | Some expression -> expression
      | None ->
          let head, extra_arguments, infix_tail = split_greedy_argument_value value in
          let left =
            match extra_arguments with
            | _ :: _ -> rebuild
              head
              extra_arguments
              (fun value -> Cst.Labeled { labeled_argument with value = Some value })
            | [] -> Cst.Expression.Apply {
              syntax_node;
              callee;
              argument = Cst.Labeled { labeled_argument with value = Some head };
              attributes = []
            }
          in
          wrap_infix_tail left infix_tail
    )
  | Cst.Optional ({ value=Some value; _ } as optional_argument) -> (
      match normalize_greedy_tuple_argument_value
        ~syntax_node
        ~callee
        (fun value -> Cst.Optional { optional_argument with value = Some value })
        value with
      | Some expression -> expression
      | None ->
          let head, extra_arguments, infix_tail = split_greedy_argument_value value in
          let left =
            match extra_arguments with
            | _ :: _ -> rebuild
              head
              extra_arguments
              (fun value -> Cst.Optional { optional_argument with value = Some value })
            | [] -> Cst.Expression.Apply {
              syntax_node;
              callee;
              argument = Cst.Optional { optional_argument with value = Some head };
              attributes = []
            }
          in
          wrap_infix_tail left infix_tail
    )
  | _ ->
      Cst.Expression.Apply { syntax_node; callee; argument; attributes = [] }

and expression_with_type_annotation = fun ~syntax_node ~expression type_node ->
  let type_ = core_type_from_node type_node in
  let colon_token =
    match direct_token_with_text syntax_node ":" with
    | Some colon_token -> colon_token
    | None -> bail
      ~message:"expected expression type ascription colon token during Ceibo -> CST lifting"
      ~syntax_node
      ~context:[ "expression.type_ascription"; "colon_token" ]
  in
  match type_ with
  | Cst.CoreType.Poly { binders; _ } when List.exists Cst.TypeBinder.is_quoted binders ->
      Cst.Expression.Polymorphic {
        syntax_node;
        expression;
        colon_token;
        type_;
        attributes = [];
      }
  | _ -> Cst.Expression.TypeAscription {
    syntax_node;
    expression;
    kind = Cst.Type { colon_token; type_ };
    attributes = []
  }

and binding_type_annotation_node = fun prefix_nodes -> prefix_nodes |> List.find_opt can_lift_core_type_node

and binding_parameter_nodes = fun prefix_nodes ->
  prefix_nodes |> List.filter (fun child -> is_parameter_like_kind (Ceibo.Red.SyntaxNode.kind child))

and binding_parameters_from_prefix = fun prefix_nodes -> binding_parameter_nodes prefix_nodes |> parameters_from_nodes

and fun_return_type_from_prefix = fun prefix_nodes ->
  binding_type_annotation_node prefix_nodes |> Option.map core_type_from_node

and binding_value_from_prefix = fun ~binding_syntax_node ~prefix_nodes ~value_node ->
  let value = expression_from_node value_node in
  match binding_type_annotation_node prefix_nodes with
  | Some type_node -> expression_with_type_annotation
    ~syntax_node:binding_syntax_node
    ~expression:value
    type_node
  | None -> value

and constrain_module_expression = fun ~syntax_node ~module_expression module_type ->
  Cst.ModuleExpression.Constraint {
    syntax_node;
    module_expression;
    colon_token =
      (
        match direct_token_with_text syntax_node ":" with
        | Some colon_token -> colon_token
        | None -> bail
          ~message:"expected module expression constraint colon token during Ceibo -> CST lifting"
          ~syntax_node
          ~context:[ "module_expression.constraint"; "colon_token" ]
      );
    module_type;
  }

and module_expression_from_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_EXPR ->
      Cst.ModuleExpression.Path (ident_path_from_node node)
  | Syntax_kind.MODULE_PATH ->
      Cst.ModuleExpression.Path (module_path_from_node node)
  | Syntax_kind.STRUCT_EXPR ->
      Cst.ModuleExpression.Structure {
        syntax_node = node;
        item_syntax_nodes = direct_non_trivia_nodes node |> List.concat_map split_payload_item_nodes_from_node
      }
  | Syntax_kind.FUNCTOR_TYPE -> (
      match direct_non_trivia_tokens node, List.rev (direct_non_trivia_nodes node) with
      | functor_kw :: _, body_node :: rev_parameter_nodes when String.equal
        (Ceibo.Red.SyntaxToken.text functor_kw)
        "functor" -> Cst.ModuleExpression.Functor {
        syntax_node = node;
        parameters = List.rev rev_parameter_nodes
        |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.FUNCTOR_PARAM)
        |> List.map functor_parameter_from_node;
        body = module_expression_from_node body_node
      }
      | _ -> unsupported_module_expression node
    )
  | Syntax_kind.MODULE_APPLICATION -> (
      match direct_non_trivia_nodes node with
      | callee_node :: argument_node :: _ -> Cst.ModuleExpression.Apply {
        syntax_node = node;
        callee = module_expression_from_node callee_node;
        argument = module_expression_from_node argument_node
      }
      | _ -> unsupported_module_expression node
    )
  | Syntax_kind.MODULE_UNIT_APPLICATION -> (
      match direct_non_trivia_nodes node with
      | callee_node :: _ -> Cst.ModuleExpression.ApplyUnit {
        syntax_node = node;
        callee = module_expression_from_node callee_node
      }
      | _ -> unsupported_module_expression node
    )
  | Syntax_kind.FIRST_CLASS_MODULE_EXPR -> (
      match non_paren_tokens node, direct_non_trivia_nodes node with
      | val_kw :: _, expression_node :: _ when String.equal (Ceibo.Red.SyntaxToken.text val_kw) "val" ->
          let direct_tokens = direct_non_trivia_tokens node in
          let opening_token, closing_token =
            match direct_tokens with
            | opening_token :: _ ->
                let closing_token = List.hd (List.rev direct_tokens) in
                (token opening_token, token closing_token)
            | [] -> bail
              ~message:"expected first-class module unpack delimiters during Ceibo -> CST lifting"
              ~syntax_node:node
              ~context:[ "module_expression.module_unpack" ]
          in
          let package_type = direct_non_trivia_nodes node
          |> List.find_opt can_lift_module_type_node
          |> Option.map package_type_from_module_type_node in
          Cst.ModuleExpression.ModuleUnpack {
            syntax_node = node;
            opening_token;
            expression = expression_from_node expression_node;
            colon_token = token_with_text node ":";
            package_type;
            closing_token;
          }
      | _ -> unsupported_module_expression node
    )
  | Syntax_kind.PAREN_EXPR -> (
      match direct_non_trivia_nodes node |> List.find_opt can_lift_module_expression_node, direct_non_trivia_tokens
        node with
      | Some inner_node, (opening_token :: _ :: _ as tokens) ->
          let closing_token = List.hd (List.rev tokens) in
          Cst.ModuleExpression.Parenthesized {
            syntax_node = node;
            opening_token = token opening_token;
            inner = module_expression_from_node inner_node;
            closing_token = token closing_token
          }
      | None, _ ->
          unsupported_module_expression node
      | Some _, _ ->
          bail
            ~message:"expected parenthesized module expression delimiters during Ceibo -> CST lifting"
            ~syntax_node:node
            ~context:[ "module_expression.parenthesized" ]
    )
  | Syntax_kind.ATTRIBUTE_EXPR -> (
      match direct_non_trivia_nodes node with
      | first_child :: rest -> (
          match List.find_opt can_lift_module_expression_node (first_child :: rest), List.find_opt
            is_attribute_node
            rest with
          | Some payload_node, Some attribute_node -> Cst.ModuleExpression.Attribute {
            syntax_node = node;
            module_expression = module_expression_from_node payload_node;
            attribute = attribute_from_node attribute_node
          }
          | _ -> unsupported_module_expression node
        )
      | [] -> unsupported_module_expression node
    )
  | Syntax_kind.EXTENSION_EXPR ->
      Cst.ModuleExpression.Extension (extension_from_node node)
  | _ ->
      unsupported_module_expression node

and expression_from_node = fun node ->
  let known_expression_children node = direct_non_trivia_nodes node
  |> List.filter can_lift_expression_node
  |> List.map expression_from_node in
  let rec peel_outer_expression_attributes node =
    match Ceibo.Red.SyntaxNode.kind node with
    | Syntax_kind.ATTRIBUTE_EXPR -> (
        match direct_non_trivia_nodes node with
        | first_child :: rest -> (
            match List.find_opt can_lift_expression_node (first_child :: rest), List.find_opt
              is_attribute_node
              rest with
            | Some payload_node, Some attribute_node ->
                let payload_node, attributes = peel_outer_expression_attributes payload_node in
                (payload_node, attributes @ [ attribute_from_node attribute_node ])
            | _ -> (node, [])
          )
        | [] -> (node, [])
      )
    | _ -> (node, [])
  in
  let constant_with_attributes constant attributes =
    match attributes with
    | [] -> constant
    | _ ->
        let append existing = existing @ attributes in
        match constant with
        | Cst.Constant.String constant -> Cst.Constant.String {
          constant
          with attributes = append constant.attributes
        }
        | Cst.Constant.Int constant -> Cst.Constant.Int {
          constant
          with attributes = append constant.attributes
        }
        | Cst.Constant.Float constant -> Cst.Constant.Float {
          constant
          with attributes = append constant.attributes
        }
        | Cst.Constant.Char constant -> Cst.Constant.Char {
          constant
          with attributes = append constant.attributes
        }
        | Cst.Constant.Bool constant -> Cst.Constant.Bool {
          constant
          with attributes = append constant.attributes
        }
        | Cst.Constant.Unit constant -> Cst.Constant.Unit {
          constant
          with attributes = append constant.attributes
        }
  in
  let expression_with_attributes expression attributes =
    match attributes with
    | [] -> expression
    | _ ->
        let append existing = existing @ attributes in
        match expression with
        | Cst.Expression.Path expr -> Cst.Expression.Path {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.Constructor expr -> Cst.Expression.Constructor {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.Operator expr -> Cst.Expression.Operator {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.Literal literal -> Cst.Expression.Literal (constant_with_attributes
          literal
          attributes)
        | Cst.Expression.Unreachable expr -> Cst.Expression.Unreachable {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.Extension ext -> Cst.Expression.Extension {
          ext
          with attributes = append ext.attributes
        }
        | Cst.Expression.Object expr -> Cst.Expression.Object {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.PolyVariant expr -> Cst.Expression.PolyVariant {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.ModulePack expr -> Cst.Expression.ModulePack {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.LetModule expr -> Cst.Expression.LetModule {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.LetException expr -> Cst.Expression.LetException {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.Assert expr -> Cst.Expression.Assert {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.Lazy expr -> Cst.Expression.Lazy {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.While expr -> Cst.Expression.While {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.For expr -> Cst.Expression.For {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.Apply expr -> Cst.Expression.Apply {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.MethodCall expr -> Cst.Expression.MethodCall {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.New expr -> Cst.Expression.New {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.Prefix expr -> Cst.Expression.Prefix {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.FieldAccess expr -> Cst.Expression.FieldAccess {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.Index expr -> Cst.Expression.Index {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.ObjectOverride expr -> Cst.Expression.ObjectOverride {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.InstanceVariableAssign expr -> Cst.Expression.InstanceVariableAssign {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.FieldAssign expr -> Cst.Expression.FieldAssign {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.Assign expr -> Cst.Expression.Assign {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.Infix expr -> Cst.Expression.Infix {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.TypeAscription expr -> Cst.Expression.TypeAscription {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.Polymorphic expr -> Cst.Expression.Polymorphic {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.Sequence expr -> Cst.Expression.Sequence {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.Tuple expr -> Cst.Expression.Tuple {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.List expr -> Cst.Expression.List {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.Array expr -> Cst.Expression.Array {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.Record (Cst.RecordExpression.Literal expr) -> Cst.Expression.Record (Cst.RecordExpression.Literal {
          expr
          with attributes = append expr.attributes
        })
        | Cst.Expression.Record (Cst.RecordExpression.Update expr) -> Cst.Expression.Record (Cst.RecordExpression.Update {
          expr
          with attributes = append expr.attributes
        })
        | Cst.Expression.LocalOpen (Cst.LetOpen expr) -> Cst.Expression.LocalOpen (Cst.LetOpen {
          expr
          with attributes = append expr.attributes
        })
        | Cst.Expression.LocalOpen (Cst.Delimited expr) -> Cst.Expression.LocalOpen (Cst.Delimited {
          expr
          with attributes = append expr.attributes
        })
        | Cst.Expression.Fun expr -> Cst.Expression.Fun {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.Function expr -> Cst.Expression.Function {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.LetOperator expr -> Cst.Expression.LetOperator {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.Let expr -> Cst.Expression.Let {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.Match expr -> Cst.Expression.Match {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.Try expr -> Cst.Expression.Try {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.If expr -> Cst.Expression.If {
          expr
          with attributes = append expr.attributes
        }
        | Cst.Expression.Parenthesized expr -> Cst.Expression.Parenthesized {
          expr
          with attributes = append expr.attributes
        }
  in
  let poly_variant_tag_token node =
    match direct_non_trivia_tokens node with
    | _backtick :: tag_syntax_token :: _ -> Some (token tag_syntax_token)
    | tag_syntax_token :: _ -> Some (token tag_syntax_token)
    | [] -> None
  in
  let node, attributes = peel_outer_expression_attributes node in
  let expression =
    match Ceibo.Red.SyntaxNode.kind node with
    | Syntax_kind.IDENT_EXPR -> (
        let path = ident_path_from_node node in
        if is_constructor_path path then
          Cst.Expression.Constructor {
            syntax_node = node;
            constructor_path = path;
            payload = None;
            attributes = []
          }
        else
          Cst.Expression.Path { syntax_node = node; path; attributes = [] }
      )
    | Syntax_kind.MODULE_PATH -> (
        let path = module_path_from_node node in
        if is_constructor_path path then
          Cst.Expression.Constructor {
            syntax_node = node;
            constructor_path = path;
            payload = None;
            attributes = []
          }
        else
          Cst.Expression.Path { syntax_node = node; path; attributes = [] }
      )
    | Syntax_kind.OPERATOR_PATTERN ->
        Cst.Expression.Operator {
          syntax_node = node;
          operator_tokens = operator_tokens_from_node node;
          attributes = []
        }
    | Syntax_kind.UNREACHABLE_EXPR -> (
        match direct_non_trivia_tokens node with
        | dot_syntax_token :: _ -> Cst.Expression.Unreachable {
          syntax_node = node;
          dot_token = token dot_syntax_token;
          attributes = []
        }
        | [] -> unsupported_expression node
      )
    | Syntax_kind.EXTENSION_EXPR ->
        Cst.Expression.Extension (extension_from_node node)
    | Syntax_kind.OBJECT_EXPR -> (
        match object_expression_from_node node with
        | Some expr -> Cst.Expression.Object expr
        | None -> unsupported_expression node
      )
    | Syntax_kind.UNIT_LITERAL ->
        Cst.Expression.Literal (constant_from_node node)
    | Syntax_kind.METHOD_CALL_EXPR -> (
        match method_call_expression_from_node node with
        | Some expr -> Cst.Expression.MethodCall expr
        | None -> unsupported_expression node
      )
    | Syntax_kind.NEW_EXPR -> (
        match new_expression_from_node node with
        | Some expr -> Cst.Expression.New expr
        | None -> unsupported_expression node
      )
    | Syntax_kind.FIELD_ACCESS_EXPR -> (
        match constructor_path_from_expression_node node with
        | Some constructor_path -> Cst.Expression.Constructor {
          syntax_node = node;
          constructor_path;
          payload = None;
          attributes = []
        }
        | None -> (
            match field_access_expression_from_node node with
            | Some expr -> expr
            | None -> unsupported_expression node
          )
      )
    | Syntax_kind.ARRAY_INDEX_EXPR -> (
        match index_expression_from_node node with
        | Some expr -> Cst.Expression.Index expr
        | None -> unsupported_expression node
      )
    | Syntax_kind.STRING_INDEX_EXPR -> (
        match index_expression_from_node node with
        | Some expr -> Cst.Expression.Index expr
        | None -> unsupported_expression node
      )
    | Syntax_kind.ASSIGN_EXPR -> (
        match assign_expression_from_node node with
        | Some expr -> expr
        | None -> unsupported_expression node
      )
    | Syntax_kind.STRING_LITERAL
    | Syntax_kind.INT_LITERAL
    | Syntax_kind.FLOAT_LITERAL
    | Syntax_kind.CHAR_LITERAL
    | Syntax_kind.BOOL_LITERAL ->
        Cst.Expression.Literal (constant_from_node node)
    | Syntax_kind.ASSERT_EXPR -> (
        match direct_non_trivia_nodes node |> List.find_opt can_lift_expression_node |> Option.map expression_from_node with
        | Some asserted -> Cst.Expression.Assert { syntax_node = node; asserted; attributes = [] }
        | None -> unsupported_expression node
      )
    | Syntax_kind.LAZY_EXPR -> (
        match direct_non_trivia_nodes node |> List.find_opt can_lift_expression_node |> Option.map expression_from_node with
        | Some body -> Cst.Expression.Lazy { syntax_node = node; body; attributes = [] }
        | None -> unsupported_expression node
      )
    | Syntax_kind.WHILE_EXPR -> (
        match direct_non_trivia_nodes node
        |> List.filter (fun child -> not (is_attribute_node child)) with
        | condition_node :: body_node :: _ -> Cst.Expression.While {
          syntax_node = node;
          condition = expression_from_node condition_node;
          body = expression_from_node body_node;
          attributes = []
        }
        | _ -> unsupported_expression node
      )
    | Syntax_kind.FOR_EXPR -> (
        let non_trivia_tokens = direct_non_trivia_tokens node in
        let direction =
          non_trivia_tokens
          |> List.find_opt
            (fun syntax_token ->
              let text = Ceibo.Red.SyntaxToken.text syntax_token in
              String.equal text "to" || String.equal text "downto")
          |> Option.map
            (fun direction_syntax_token ->
              let direction_token = token direction_syntax_token in
              match Ceibo.Red.SyntaxToken.text direction_syntax_token with
              | "to" -> Cst.To { direction_token }
              | "downto" -> Cst.Downto { direction_token }
              | _ -> bail
                ~message:"expected for-loop direction token during Ceibo -> CST lifting"
                ~syntax_node:node
                ~context:[ "expression.for"; "direction" ])
        in
        match (direct_non_trivia_nodes node
        |> List.filter (fun child -> not (is_attribute_node child))), non_trivia_tokens, direction with
        | start_node :: end_node :: body_node :: _, _for_kw :: iterator_syntax_token :: _, Some direction ->
            Cst.Expression.For {
              syntax_node = node;
              iterator_token = token iterator_syntax_token;
              equals_token =
                (
                  match direct_token_with_text node "=" with
                  | Some equals_token -> equals_token
                  | None -> bail
                    ~message:"expected for-loop equals token during Ceibo -> CST lifting"
                    ~syntax_node:node
                    ~context:[ "expression.for"; "equals_token" ]
                );
              start_expr = expression_from_node start_node;
              direction;
              end_expr = expression_from_node end_node;
              body = expression_from_node body_node;
              attributes = [];
            }
        | _ -> unsupported_expression node
      )
    | Syntax_kind.APPLY_EXPR -> (
        match direct_non_trivia_nodes node with
        | callee_node :: [ attribute_node ] when Ceibo.Red.SyntaxNode.kind attribute_node
        = Syntax_kind.ATTRIBUTE_EXPR ->
            let attribute_payload_node = direct_non_trivia_nodes attribute_node |> List.find_opt can_lift_expression_node in
            (
              match attribute_payload_node with
              | Some payload_node when Ceibo.Red.SyntaxNode.kind payload_node
              = Ceibo.Red.SyntaxNode.kind callee_node
              && Ceibo.Red.SyntaxNode.span payload_node = Ceibo.Red.SyntaxNode.span callee_node -> expression_with_attributes
                (expression_from_node callee_node)
                [ attribute_from_node attribute_node ]
              | _ -> (
                  match constructor_path_from_expression_node callee_node, apply_argument_from_node attribute_node with
                  | Some constructor_path, Cst.Positional payload -> Cst.Expression.Constructor {
                    syntax_node = node;
                    constructor_path;
                    payload = Some payload;
                    attributes = []
                  }
                  | _ -> normalize_greedy_labeled_argument
                    ~syntax_node:node
                    ~callee:(expression_from_node callee_node)
                    (apply_argument_from_node attribute_node)
                )
            )
        | callee_node :: argument_node :: _ -> (
            match constructor_path_from_expression_node callee_node, apply_argument_from_node argument_node with
            | Some constructor_path, Cst.Positional payload -> Cst.Expression.Constructor {
              syntax_node = node;
              constructor_path;
              payload = Some payload;
              attributes = []
            }
            | _ -> normalize_greedy_labeled_argument
              ~syntax_node:node
              ~callee:(expression_from_node callee_node)
              (apply_argument_from_node argument_node)
          )
        | _ ->
            unsupported_expression node
      )
    | Syntax_kind.POLY_VARIANT_EXPR -> (
        match poly_variant_tag_token node with
        | Some tag_token -> Cst.Expression.PolyVariant {
          syntax_node = node;
          tag_token;
          payload = (direct_non_trivia_nodes node
          |> List.find_opt can_lift_expression_node
          |> Option.map expression_from_node);
          attributes = []
        }
        | None -> unsupported_expression node
      )
    | Syntax_kind.FIRST_CLASS_MODULE_EXPR -> (
        match non_paren_tokens node, direct_non_trivia_nodes node with
        | module_kw :: _, module_expression_node :: _ when String.equal
          (Ceibo.Red.SyntaxToken.text module_kw)
          "module" ->
            let direct_tokens = direct_non_trivia_tokens node in
            let opening_token, closing_token =
              match direct_tokens with
              | opening_token :: _ ->
                  let closing_token = List.hd (List.rev direct_tokens) in
                  (token opening_token, token closing_token)
              | [] -> bail
                ~message:"expected first-class module delimiters during Ceibo -> CST lifting"
                ~syntax_node:node
                ~context:[ "expression.module_pack" ]
            in
            let package_type = direct_non_trivia_nodes node
            |> List.find_opt can_lift_module_type_node
            |> Option.map package_type_from_module_type_node in
            Cst.Expression.ModulePack {
              syntax_node = node;
              opening_token;
              module_expression = module_expression_from_node module_expression_node;
              colon_token = token_with_text node ":";
              package_type;
              closing_token;
              attributes = [];
            }
        | _ -> unsupported_expression node
      )
    | Syntax_kind.LET_MODULE_EXPR -> (
        match let_module_expression_from_node node with
        | Some expr -> Cst.Expression.LetModule expr
        | None -> unsupported_expression node
      )
    | Syntax_kind.LET_EXPR -> (
        if is_binding_operator_expression_node node then
          match let_operator_expression_from_node node with
          | Some expr -> Cst.Expression.LetOperator expr
          | None -> unsupported_expression node
        else
          match let_expression_parts ~is_recursive_binding:false node with
          | Some (`Exception (exception_decl_node, body_node)) -> (
              match direct_token_with_text exception_decl_node "exception", find_declaration_name_token
                ~skip_keywords:[ "exception" ]
                (direct_non_trivia_tokens exception_decl_node) with
              | Some keyword_token, Some name_syntax_token ->
                  Cst.Expression.LetException {
                    syntax_node = node;
                    exception_declaration =
                      {
                        syntax_node = exception_decl_node;
                        keyword_token;
                        name_token = token name_syntax_token;
                        rhs =
                          (
                            match
                              direct_non_trivia_nodes exception_decl_node |> List.find_opt
                                (fun child ->
                                  can_lift_core_type_node child
                                  || match Ceibo.Red.SyntaxNode.kind child with
                                  | Syntax_kind.MODULE_PATH
                                  | Syntax_kind.MODULE_TYPE_PATH
                                  | Syntax_kind.IDENT_EXPR
                                  | Syntax_kind.FIELD_ACCESS_EXPR -> true
                                  | _ -> false)
                            with
                            | Some child when can_lift_core_type_node child -> (
                                match direct_token_with_text exception_decl_node "of" with
                                | Some of_token -> Some Cst.(Payload {
                                  of_token;
                                  payload_type = core_type_from_node child
                                })
                                | None -> None
                              )
                            | Some child -> (
                                match direct_token_with_text exception_decl_node "=" with
                                | Some equals_token -> Some Cst.(Alias {
                                  equals_token;
                                  alias = module_path_like_from_node child
                                })
                                | None -> None
                              )
                            | _ ->
                                None
                          );
                      };
                    body = expression_from_node body_node;
                    attributes = [];
                  }
              | _ -> unsupported_expression node
            )
          | _ -> (
              match let_expression_from_node ~is_recursive_binding:false node with
              | Some expr -> Cst.Expression.Let expr
              | None -> unsupported_expression node
            )
      )
    | Syntax_kind.LET_REC_EXPR -> (
        match let_expression_from_node ~is_recursive_binding:true node with
        | Some expr -> Cst.Expression.Let expr
        | None -> unsupported_expression node
      )
    | Syntax_kind.TYPED_EXPR -> (
        match direct_non_trivia_nodes node with
        | expr_node :: type_node :: _ -> expression_with_type_annotation
          ~syntax_node:node
          ~expression:(expression_from_node expr_node)
          type_node
        | _ -> unsupported_expression node
      )
    | Syntax_kind.COERCE_EXPR -> (
        match direct_non_trivia_nodes node with
        | expr_node :: to_type_node :: [] -> Cst.Expression.TypeAscription {
          syntax_node = node;
          expression = expression_from_node expr_node;
          kind = Cst.Coerce {
            coercion_tokens = direct_required_tokens_with_text_between_offsets
              ~context:[ "expression.type_ascription"; "coercion_tokens" ]
              ~after_offset:(Ceibo.Red.SyntaxNode.span expr_node).end_
              ~before_offset:(Ceibo.Red.SyntaxNode.span to_type_node).start
              ~expected_texts:[ ":"; ">" ]
              node;
            type_ = core_type_from_node to_type_node
          };
          attributes = []
        }
        | expr_node :: from_type_node :: to_type_node :: _ ->
            Cst.Expression.TypeAscription {
              syntax_node = node;
              expression = expression_from_node expr_node;
              kind =
                Cst.ConstraintCoerce {
                  colon_token =
                    (
                      match direct_token_with_text node ":" with
                      | Some colon_token -> colon_token
                      | None -> bail
                        ~message:"expected expression constraint-coercion colon token during Ceibo -> CST lifting"
                        ~syntax_node:node
                        ~context:[ "expression.type_ascription"; "colon_token" ]
                    );
                  from_type = core_type_from_node from_type_node;
                  coercion_tokens = (direct_required_tokens_with_text_between_offsets
                    ~context:[ "expression.type_ascription"; "coercion_tokens" ]
                    ~after_offset:(Ceibo.Red.SyntaxNode.span from_type_node).end_
                    ~before_offset:(Ceibo.Red.SyntaxNode.span to_type_node).start
                    ~expected_texts:[ ":"; ">" ]
                    node);
                  to_type = core_type_from_node to_type_node;
                };
              attributes = [];
            }
        | _ -> unsupported_expression node
      )
    | Syntax_kind.PREFIX_EXPR -> (
        match prefix_expression_from_node node with
        | Some expr -> Cst.Expression.Prefix expr
        | None -> unsupported_expression node
      )
    | Syntax_kind.INFIX_EXPR -> (
        match direct_non_trivia_nodes node, direct_non_trivia_tokens node with
        | left_node :: right_node :: _, operator_syntax_tokens ->
            Cst.Expression.Infix {
              syntax_node = node;
              left = expression_from_node left_node;
              operator_tokens = List.map token operator_syntax_tokens;
              right = expression_from_node right_node;
              attributes = [];
            }
        | _ -> unsupported_expression node
      )
    | Syntax_kind.SEQUENCE_EXPR -> (
        match sequence_expression_from_node node with
        | Some expr -> Cst.Expression.Sequence expr
        | None -> unsupported_expression node
      )
    | Syntax_kind.TUPLE_EXPR ->
        Cst.Expression.Tuple {
          syntax_node = node;
          elements = known_expression_children node;
          attributes = []
        }
    | Syntax_kind.LIST_EXPR ->
        let direct_tokens = direct_non_trivia_tokens node in
        let opening_token, closing_token =
          match direct_tokens with
          | opening_token :: _ ->
              let closing_token = List.hd (List.rev direct_tokens) in
              (token opening_token, token closing_token)
          | [] -> bail
            ~message:"expected list expression delimiters during Ceibo -> CST lifting"
            ~syntax_node:node
            ~context:[ "expression.list" ]
        in
        Cst.Expression.List {
          syntax_node = node;
          opening_token;
          elements = known_expression_children node;
          separator_tokens =
            direct_tokens |> List.filter
              (fun syntax_token ->
                String.equal (Ceibo.Red.SyntaxToken.text syntax_token) semicolon_text) |> List.map token;
          closing_token;
          attributes = [];
        }
    | Syntax_kind.ARRAY_EXPR ->
        let direct_tokens = direct_non_trivia_tokens node in
        let opening_token, closing_token =
          match direct_tokens with
          | opening_token :: _ ->
              let closing_token = List.hd (List.rev direct_tokens) in
              (token opening_token, token closing_token)
          | [] -> bail
            ~message:"expected array expression delimiters during Ceibo -> CST lifting"
            ~syntax_node:node
            ~context:[ "expression.array" ]
        in
        Cst.Expression.Array {
          syntax_node = node;
          opening_token;
          elements = known_expression_children node;
          separator_tokens =
            direct_tokens |> List.filter
              (fun syntax_token ->
                String.equal (Ceibo.Red.SyntaxToken.text syntax_token) semicolon_text) |> List.map token;
          closing_token;
          attributes = [];
        }
    | Syntax_kind.RECORD_EXPR -> (
        match record_literal_expression_from_node node with
        | Some expr -> Cst.Expression.Record (Cst.RecordExpression.Literal expr)
        | None -> unsupported_expression node
      )
    | Syntax_kind.RECORD_UPDATE_EXPR -> (
        match record_update_expression_from_node node with
        | Some expr -> Cst.Expression.Record (Cst.RecordExpression.Update expr)
        | None -> unsupported_expression node
      )
    | Syntax_kind.OBJECT_UPDATE_EXPR -> (
        match object_override_expression_from_node node with
        | Some expr -> Cst.Expression.ObjectOverride expr
        | None -> unsupported_expression node
      )
    | Syntax_kind.LOCAL_OPEN_EXPR -> (
        match poly_variant_expression_from_local_open_node node with
        | Some expr -> expr
        | None -> (
            match local_open_expression_from_node node with
            | Some expr -> Cst.Expression.LocalOpen expr
            | None -> unsupported_expression node
          )
      )
    | Syntax_kind.FUN_EXPR -> (
        match fun_expression_from_node node with
        | Some expr -> Cst.Expression.Fun expr
        | None -> unsupported_expression node
      )
    | Syntax_kind.FUNCTION_EXPR -> (
        match function_expression_from_node node with
        | Some expr -> Cst.Expression.Function expr
        | None -> unsupported_expression node
      )
    | Syntax_kind.MATCH_EXPR -> (
        match match_expression_from_node node with
        | Some expr -> Cst.Expression.Match expr
        | None -> unsupported_expression node
      )
    | Syntax_kind.TRY_EXPR -> (
        match try_expression_from_node node with
        | Some expr -> Cst.Expression.Try expr
        | None -> unsupported_expression node
      )
    | Syntax_kind.IF_EXPR -> (
        let expression_children = direct_non_trivia_nodes node |> List.filter can_lift_expression_node in
        match expression_children with
        | condition_node :: then_node :: else_nodes ->
            let then_token = direct_required_token_with_text ~context:[ "if_expression" ] node "then" in
            let else_token = direct_token_with_text node "else" in
            let then_branch = expression_from_node then_node in
            let else_branch =
              match else_nodes with
              | else_node :: _ -> Some (expression_from_node else_node)
              | [] -> None
            in
            Cst.Expression.If {
              syntax_node = node;
              keyword_token = direct_required_token_with_text ~context:[ "if_expression" ] node "if";
              then_token;
              else_token;
              condition = expression_from_node condition_node;
              then_branch;
              else_branch;
              attributes = [];
            }
        | _ -> unsupported_expression node
      )
    | Syntax_kind.PAREN_EXPR -> (
        match direct_non_trivia_nodes node
        |> List.filter (fun child -> not (is_attribute_node child)) with
        | inner_node :: _ ->
            let opening_token, closing_token =
              match first_and_last_direct_token node with
              | Some (opening_token, closing_token) -> (token opening_token, token closing_token)
              | None -> bail
                ~message:"expected grouping delimiter tokens during Ceibo -> CST lifting"
                ~syntax_node:node
                ~context:[ "parenthesized_expression" ]
            in
            Cst.Expression.Parenthesized {
              syntax_node = node;
              opening_token;
              closing_token;
              grouping = expression_grouping_from_node node;
              inner = expression_from_node inner_node;
              attributes = [];
            }
        | [] -> unsupported_expression node
      )
    | _ ->
        unsupported_expression node
  in
  expression_with_attributes expression attributes

and object_method_from_node = fun node ->
  let children_without_attributes = direct_non_trivia_nodes node
  |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child != Syntax_kind.ATTRIBUTE_EXPR) in
  match children_without_attributes with
  | name_node :: remainder when Ceibo.Red.SyntaxNode.kind name_node = Syntax_kind.IDENT_EXPR -> (
      match first_ident_token_in_subtree name_node with
      | Some name_token ->
          let modifier_tokens =
            direct_non_trivia_tokens node
            |> List.filter
              (fun token ->
                let text = Ceibo.Red.SyntaxToken.text token in
                String.equal "!" text || String.equal "private" text)
            |> List.map token
          in
          let body = remainder |> List.rev |> List.find_opt can_lift_expression_node |> Option.map expression_from_node in
          let type_ = List.find_opt can_lift_core_type_node remainder |> Option.map core_type_from_node in
          Some {
            Cst.syntax_node = node;
            attributes = attributes_from_node node;
            name_token;
            body =
              (
                match body with
                | Some body -> body
                | None -> bail
                  ~message:"expected body expression for object method during Ceibo -> CST lifting"
                  ~syntax_node:node
                  ~context:[ "object_member"; "method" ]
              );
            equals_token =
              (
                match direct_token_with_text node "=" with
                | Some equals_token -> equals_token
                | None -> bail
                  ~message:"expected object method equals token during Ceibo -> CST lifting"
                  ~syntax_node:node
                  ~context:[ "object_member"; "method"; "equals_token" ]
              );
            type_;
            colon_token = direct_token_with_text node ":";
            modifier_tokens;
          }
      | None -> None
    )
  | _ -> None

and object_value_from_node = fun node ->
  let children_without_attributes = direct_non_trivia_nodes node
  |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child != Syntax_kind.ATTRIBUTE_EXPR) in
  match children_without_attributes with
  | name_node :: remainder when Ceibo.Red.SyntaxNode.kind name_node = Syntax_kind.IDENT_EXPR -> (
      match first_ident_token_in_subtree name_node with
      | Some name_token ->
          let modifier_tokens =
            direct_non_trivia_tokens node
            |> List.filter
              (fun token ->
                let text = Ceibo.Red.SyntaxToken.text token in
                String.equal "!" text || String.equal "mutable" text)
            |> List.map token
          in
          let value = remainder |> List.rev |> List.find_opt can_lift_expression_node |> Option.map expression_from_node in
          let type_ = List.find_opt can_lift_core_type_node remainder |> Option.map core_type_from_node in
          Some {
            Cst.syntax_node = node;
            attributes = attributes_from_node node;
            name_token;
            value =
              (
                match value with
                | Some value -> value
                | None -> bail
                  ~message:"expected bound expression for object value during Ceibo -> CST lifting"
                  ~syntax_node:node
                  ~context:[ "object_member"; "value" ]
              );
            equals_token =
              (
                match direct_token_with_text node "=" with
                | Some equals_token -> equals_token
                | None -> bail
                  ~message:"expected object value equals token during Ceibo -> CST lifting"
                  ~syntax_node:node
                  ~context:[ "object_member"; "value"; "equals_token" ]
              );
            type_;
            colon_token = direct_token_with_text node ":";
            modifier_tokens;
          }
      | None -> None
    )
  | _ -> None

and object_inherit_from_node = fun node ->
  match
    direct_non_trivia_nodes node
    |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child != Syntax_kind.ATTRIBUTE_EXPR)
    |> List.find_map
      (fun child ->
        if can_lift_expression_node child then
          Some (expression_from_node child)
        else
          None)
  with
  | Some expression -> Some {
    Cst.syntax_node = node;
    attributes = attributes_from_node node;
    expression
  }
  | None -> None

and object_initializer_from_node = fun node ->
  match direct_non_trivia_tokens node, direct_non_trivia_nodes node with
  | initializer_kw :: _, children when String.equal (Ceibo.Red.SyntaxToken.text initializer_kw) "initializer" ->
      let body =
        children
        |> List.filter (fun child -> not (is_attribute_node child))
        |> List.find_map
          (fun child ->
            if can_lift_expression_node child then
              Some (expression_from_node child)
            else
              None)
      in
      Some (
        ({
            Cst.syntax_node = node;
            body =
              (
                match body with
                | Some body -> body
                | None -> bail
                  ~message:"expected body expression for object initializer during Ceibo -> CST lifting"
                  ~syntax_node:node
                  ~context:[ "object_member"; "initializer" ]
              );
          }: Cst.object_initializer)
      )
  | _ -> None

and object_expression_from_node = fun node ->
  let non_trivia_children = direct_non_trivia_nodes node in
  let self_pattern, member_children =
    match non_trivia_children with
    | self_node :: rest when Ceibo.Red.SyntaxNode.kind self_node = Syntax_kind.OBJECT_SELF -> (
        match direct_non_trivia_nodes self_node with
        | pattern_node :: _ -> (Some (pattern_from_node pattern_node), rest)
        | [] -> (None, rest)
      )
    | _ -> (None, non_trivia_children)
  in
  let rec lift_members = fun acc ->
    function
    | [] -> Some (List.rev acc)
    | child :: rest -> (
        match Ceibo.Red.SyntaxNode.kind child with
        | Syntax_kind.OBJECT_METHOD -> (
            match object_method_from_node child with
            | Some member -> lift_members ((Cst.ObjectMember.Method member) :: acc) rest
            | None -> None
          )
        | Syntax_kind.OBJECT_VAL -> (
            match object_value_from_node child with
            | Some member -> lift_members ((Cst.ObjectMember.Value member) :: acc) rest
            | None -> None
          )
        | Syntax_kind.OBJECT_INHERIT -> (
            match object_inherit_from_node child with
            | Some member -> lift_members ((Cst.ObjectMember.Inherit member) :: acc) rest
            | None -> None
          )
        | Syntax_kind.EXTENSION_EXPR ->
            lift_members ((Cst.ObjectMember.Extension (extension_from_node child)) :: acc) rest
        | Syntax_kind.IDENT_EXPR -> (
            match object_initializer_from_node child with
            | Some member -> lift_members ((Cst.ObjectMember.Initializer member) :: acc) rest
            | None -> None
          )
        | Syntax_kind.ATTRIBUTE_EXPR ->
            lift_members acc rest
        | _ ->
            None
      )
  in
  match lift_members [] member_children with
  | Some members -> Some ((
    { syntax_node = node; self_pattern; members; attributes = [] }: Cst.object_expression
  ))
  | None -> None

and method_call_expression_from_node = fun node ->
  match direct_non_trivia_nodes node, List.rev (direct_non_trivia_tokens node) with
  | receiver_node :: _, method_name_tok :: _ ->
      let direct_tokens = direct_non_trivia_tokens node in
      let hash_token =
        match
          direct_tokens |> List.find_opt
            (fun syntax_token ->
              String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "#")
        with
        | Some syntax_token -> token syntax_token
        | None -> bail
          ~message:"expected method call hash token during Ceibo -> CST lifting"
          ~syntax_node:node
          ~context:[ "expression"; "method_call"; "hash_token" ]
      in
      Some {
        Cst.syntax_node = node;
        receiver = expression_from_node receiver_node;
        hash_token;
        method_name = token method_name_tok;
        attributes = [];
      }
  | _ -> None

and new_expression_from_node = fun node ->
  match direct_non_trivia_nodes node |> List.filter (fun child -> not (is_attribute_node child)) with
  | class_path_node :: _ -> Some {
    Cst.syntax_node = node;
    class_path = module_path_like_from_node class_path_node;
    attributes = []
  }
  | [] -> None

and object_override_field_from_node = fun node ->
  let lifted_field_path = record_field_path_from_node node in
  match Cst.Ident.segments lifted_field_path with
  | [ field_name ] -> Some ((
    {
      Cst.syntax_node = node;
      field_name;
      equals_token = direct_token_with_text node "=";
      value = record_field_value_from_node node
    }:
      Cst.object_override_field
  ))
  | _ -> None

and object_override_expression_from_node = fun node ->
  let direct_tokens = direct_non_trivia_tokens node in
  let opening_token, closing_token =
    match direct_tokens with
    | opening_token :: _ ->
        let closing_token = List.hd (List.rev direct_tokens) in
        (token opening_token, token closing_token)
    | [] -> bail
      ~message:"expected object override delimiters during Ceibo -> CST lifting"
      ~syntax_node:node
      ~context:[ "expression.object_override" ]
  in
  let children = direct_non_trivia_nodes node in
  if
    List.for_all (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.RECORD_FIELD) children
  then
    Some {
      Cst.syntax_node = node;
      opening_token;
      fields = List.filter_map object_override_field_from_node children;
      separator_tokens =
        direct_tokens |> List.filter
          (fun syntax_token ->
            String.equal (Ceibo.Red.SyntaxToken.text syntax_token) semicolon_text) |> List.map token;
      closing_token;
      attributes = [];
    }
  else
    None

and field_access_expression_from_node = fun node ->
  match direct_non_trivia_nodes node, List.rev (direct_non_trivia_tokens node) with
  | receiver_node :: _, field_token :: _ ->
      let receiver =
        match module_like_path_from_expression_node receiver_node with
        | Some path -> Cst.Expression.Path { syntax_node = receiver_node; path; attributes = [] }
        | None -> expression_from_node receiver_node
      in
      Some (Cst.Expression.FieldAccess {
        syntax_node = node;
        receiver;
        field_name = token field_token;
        attributes = []
      })
  | _ -> None

and index_expression_from_node = fun node ->
  match direct_non_trivia_nodes node, direct_non_trivia_tokens node with
  | collection_node :: index_node :: _, punctuation_tokens -> (
      match List.rev punctuation_tokens with
      | closing_syntax_token :: reversed_opening_tokens ->
          Some {
            syntax_node = node;
            collection = expression_from_node collection_node;
            opening_tokens = List.rev reversed_opening_tokens |> List.map token;
            index = expression_from_node index_node;
            closing_token = token closing_syntax_token;
            attributes = [];
          }
      | [] -> bail
        ~message:"expected index expression punctuation during Ceibo -> CST lifting"
        ~syntax_node:node
        ~context:[ "expression.index" ]
    )
  | _, _ -> None

and assign_expression_from_node = fun node ->
  match direct_non_trivia_nodes node, direct_non_trivia_tokens node with
  | target_node :: value_node :: _, operator_syntax_token :: _ ->
      let operator_token = token operator_syntax_token in
      Some (
        match Ceibo.Red.SyntaxNode.kind target_node, direct_non_trivia_tokens target_node with
        | Syntax_kind.IDENT_EXPR, name_syntax_token :: _ when String.equal
          (Cst.Token.text operator_token)
          "<-" ->
            Cst.Expression.InstanceVariableAssign {
              syntax_node = node;
              name_token = token name_syntax_token;
              operator_token;
              value = expression_from_node value_node;
              attributes = [];
            }
        | Syntax_kind.FIELD_ACCESS_EXPR, _ when String.equal (Cst.Token.text operator_token) "<-" -> (
            match field_access_expression_from_node target_node with
            | Some (Cst.Expression.FieldAccess target) ->
                Cst.Expression.FieldAssign {
                  syntax_node = node;
                  target;
                  operator_token;
                  value = expression_from_node value_node;
                  attributes = [];
                }
            | _ ->
                Cst.Expression.Assign {
                  syntax_node = node;
                  target = expression_from_node target_node;
                  operator_token;
                  value = expression_from_node value_node;
                  attributes = [];
                }
          )
        | _ ->
            Cst.Expression.Assign {
              syntax_node = node;
              target = expression_from_node target_node;
              operator_token;
              value = expression_from_node value_node;
              attributes = [];
            }
      )
  | _ -> None

and prefix_expression_from_node = fun node ->
  match direct_non_trivia_nodes node, direct_non_trivia_tokens node with
  | operand_node :: _, operator_syntax_token :: _ -> Some {
    syntax_node = node;
    operator_token = token operator_syntax_token;
    operand = expression_from_node operand_node;
    attributes = []
  }
  | _ -> None

and sequence_expression_from_node = fun node ->
  match direct_non_trivia_nodes node with
  | first :: rest ->
      let expressions = List.map expression_from_node (first :: rest) in
      let separator_tokens =
        direct_non_trivia_tokens node
        |> List.filter
          (fun syntax_token ->
            String.equal (Ceibo.Red.SyntaxToken.text syntax_token) semicolon_text)
        |> List.map token
      in
      let separator_token =
        match separator_tokens with
        | separator_token :: _ -> separator_token
        | [] -> bail
          ~message:"expected sequence separator during Ceibo -> CST lifting"
          ~syntax_node:node
          ~context:[ "sequence_expression" ]
      in
      Some {
        syntax_node = node;
        separator_token;
        separator_tokens;
        expressions;
        attributes = [];
      }
  | _ -> None

and record_field_path_from_node = fun node ->
  let tokens = direct_non_trivia_tokens node |> take_tokens_until_equals [] in
  module_path_from_tokens ~syntax_node:node tokens

and record_field_value_from_node = fun node ->
  direct_non_trivia_nodes node |> List.find_map
    (fun child ->
      if can_lift_expression_node child then
        Some (expression_from_node child)
      else
        None)

and record_expression_field_from_node = fun node ->
  let lifted_field_path = record_field_path_from_node node in
  match Cst.Ident.last_segment lifted_field_path with
  | None -> None
  | Some field_name ->
      let value, source =
        match record_field_value_from_node node with
        | Some value -> (value, Cst.Explicit)
        | None -> (
          Cst.Expression.Path { syntax_node = node; path = lifted_field_path; attributes = [] },
          Cst.Punned
        )
      in
      Some (
        ({
            Cst.syntax_node = node;
            field_path = lifted_field_path;
            field_name;
            equals_token = direct_token_with_text node "=";
            value;
            source;
          }: Cst.record_expression_field)
      )

and record_literal_expression_from_node = fun node ->
  let direct_tokens = direct_non_trivia_tokens node in
  let opening_token, closing_token =
    match direct_tokens with
    | opening_token :: _ ->
        let closing_token = List.hd (List.rev direct_tokens) in
        (token opening_token, token closing_token)
    | [] -> bail
      ~message:"expected record literal delimiters during Ceibo -> CST lifting"
      ~syntax_node:node
      ~context:[ "expression.record" ]
  in
  let fields = direct_non_trivia_nodes node
  |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.RECORD_FIELD)
  |> List.filter_map record_expression_field_from_node in
  Some (
    ({
        syntax_node = node;
        opening_token;
        fields;
        separator_tokens = separator_tokens_between_record_fields ~closing_token fields node;
        closing_token;
        attributes = [];
      }: Cst.record_literal_expression)
  )

and record_update_expression_from_node = fun node ->
  let direct_tokens = direct_non_trivia_tokens node in
  let opening_token, closing_token =
    match direct_tokens with
    | opening_token :: _ ->
        let closing_token = List.hd (List.rev direct_tokens) in
        (token opening_token, token closing_token)
    | [] -> bail
      ~message:"expected record update delimiters during Ceibo -> CST lifting"
      ~syntax_node:node
      ~context:[ "expression.record_update" ]
  in
  match direct_non_trivia_nodes node with
  | base_node :: rest -> (
      let fields = rest
      |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.RECORD_FIELD)
      |> List.filter_map record_expression_field_from_node in
      let lifted_base =
        match Ceibo.Red.SyntaxNode.kind base_node with
        | Syntax_kind.RECORD_FIELD -> (
            match record_expression_field_from_node base_node with
            | Some { field_path; source=Cst.Punned; _ } -> Some (Cst.Expression.Path {
              syntax_node = base_node;
              path = field_path;
              attributes = []
            })
            | _ -> None
          )
        | _ ->
            if can_lift_expression_node base_node then
              Some (expression_from_node base_node)
            else
              None
      in
      match lifted_base with
      | Some base ->
          Some (
            ({
                syntax_node = node;
                opening_token;
                base;
                with_token = direct_required_token_with_text
                  ~context:[ "expression"; "record_update" ]
                  node
                  "with";
                fields;
                separator_tokens = separator_tokens_between_record_fields ~closing_token fields node;
                closing_token;
                attributes = [];
              }: Cst.record_update_expression)
          )
      | None -> None
    )
  | [] -> None

and poly_variant_expression_from_local_open_node = fun node ->
  let poly_variant_tag_token poly_variant_node =
    match direct_non_trivia_tokens poly_variant_node with
    | _backtick :: tag_syntax_token :: _ -> Some (token tag_syntax_token)
    | tag_syntax_token :: _ -> Some (token tag_syntax_token)
    | [] -> None
  in
  let non_trivia_children = direct_non_trivia_nodes node in
  match non_trivia_children with
  | module_path_node :: body_node :: _ when can_lift_expression_node body_node -> (
      match Ceibo.Red.SyntaxNode.kind module_path_node, direct_non_trivia_nodes module_path_node with
      | Syntax_kind.FIELD_ACCESS_EXPR, receiver_node :: _ -> (
          let receiver_payload_node = direct_non_trivia_nodes receiver_node |> List.find_opt can_lift_expression_node in
          match Ceibo.Red.SyntaxNode.kind receiver_node, poly_variant_tag_token receiver_node, receiver_payload_node with
          | Syntax_kind.POLY_VARIANT_EXPR, Some tag_token, Some prefix_node -> (
              match List.rev (direct_non_trivia_tokens module_path_node) with
              | name_syntax_token :: dot_syntax_token :: _ ->
                  Some (
                    Cst.Expression.PolyVariant {
                      syntax_node = node;
                      tag_token;
                      payload =
                        Some (
                          Cst.Expression.LocalOpen (
                            Cst.Delimited {
                              syntax_node = node;
                              module_path = Cst.Ident.Qualified {
                                syntax_node = module_path_node;
                                prefix = module_path_like_from_node prefix_node;
                                dot_token = token dot_syntax_token;
                                name_token = token name_syntax_token
                              };
                              dot_token = token dot_syntax_token;
                              opening_token = None;
                              body = expression_from_node body_node;
                              closing_token = None;
                              attributes = [];
                            }
                          )
                        );
                      attributes = [];
                    }
                  )
              | _ -> None
            )
          | _ -> None
        )
      | _ -> None
    )
  | _ -> None

and local_open_expression_from_node = fun node ->
  let non_trivia_children = direct_non_trivia_nodes node in
  let non_trivia_tokens = direct_non_trivia_tokens node in
  match non_trivia_tokens, non_trivia_children with
  | first_token :: _, _ when String.equal (Ceibo.Red.SyntaxToken.text first_token) "let" ->
      let let_token = direct_required_token_with_text
        ~context:[ "expression"; "local_open"; "let_open" ]
        node
        "let" in
      let open_token = direct_required_token_with_text
        ~context:[ "expression"; "local_open"; "let_open" ]
        node
        "open" in
      let in_token = direct_required_token_with_text
        ~context:[ "expression"; "local_open"; "let_open" ]
        node
        "in" in
      let module_path =
        match direct_syntax_tokens_between_offsets
          ~after_offset:(Cst.Token.span open_token).end_
          ~before_offset:(Cst.Token.span in_token).start
          node
        |> trailing_module_path_tokens with
        | [] -> None
        | syntax_tokens -> Some (module_path_from_tokens ~syntax_node:node syntax_tokens)
      in
      let body_expr =
        List.rev non_trivia_children
        |> List.find_map
          (fun child ->
            if can_lift_expression_node child then
              Some (expression_from_node child)
            else
              None)
      in
      (
        match module_path, body_expr with
        | Some module_path, Some body ->
            Some (
              Cst.LetOpen {
                syntax_node = node;
                let_token;
                open_token;
                module_path;
                in_token;
                body;
                attributes = [];
              }
            )
        | _ -> None
      )
  | _, module_path_node :: body_node :: _ when can_lift_expression_node body_node ->
      let module_path = module_path_like_from_node module_path_node in
      let dot_token = direct_required_token_with_text
        ~context:[ "expression"; "local_open"; "delimited" ]
        node
        "." in
      let module_path_span = span_of_syntax_node_nontrivia_bounds module_path_node in
      let body_span = span_of_syntax_node_nontrivia_bounds body_node in
      let boundary_tokens = direct_tokens_between_offsets
        ~after_offset:module_path_span.end_
        ~before_offset:body_span.start
        node in
      let opening_token =
        boundary_tokens
        |> List.find_opt
          (fun token ->
            let text = Cst.Token.text token in
            String.equal text "("
            || String.equal text "["
            || String.equal text "[|"
            || String.equal text "{")
      in
      let trailing_tokens = direct_tokens_between_offsets
        ~after_offset:body_span.end_
        ~before_offset:(span_of_syntax_node_nontrivia_bounds node).end_
        node in
      let closing_token =
        trailing_tokens
        |> List.find_opt
          (fun token ->
            let text = Cst.Token.text token in
            String.equal text ")"
            || String.equal text "]"
            || String.equal text "|]"
            || String.equal text "}")
      in
      Some (
        Cst.Delimited {
          syntax_node = node;
          module_path;
          dot_token;
          opening_token;
          body = expression_from_node body_node;
          closing_token;
          attributes = [];
        }
      )
  | _ ->
      None

and let_module_expression_from_node = fun node ->
  let direct_children = direct_non_trivia_nodes node in
  let payload_children = direct_children |> List.filter (fun child -> not (is_attribute_node child)) in
  match direct_non_trivia_tokens node, payload_children with
  | _let_kw :: _module_kw :: module_name_token :: _, module_expression_node :: body_node :: _ ->
      Some {
        syntax_node = node;
        module_name_token = token module_name_token;
        equals_token =
          (
            match direct_token_with_text node "=" with
            | Some equals_token -> equals_token
            | None -> bail
              ~message:"expected let module equals token during Ceibo -> CST lifting"
              ~syntax_node:node
              ~context:[ "expression.let_module"; "equals_token" ]
          );
        module_expression = module_expression_from_node module_expression_node;
        body = expression_from_node body_node;
        attributes = [];
      }
  | _ -> None

and fun_expression_from_node = fun node ->
  match List.rev (direct_non_trivia_nodes node) with
  | body_node :: rev_prefix_nodes ->
      let prefix_nodes = List.rev rev_prefix_nodes in
      let arrow_token = direct_required_token_with_text ~context:[ "fun_expression" ] node "->" in
      let body = fun_body_from_node body_node in
      let return_type = fun_return_type_from_prefix prefix_nodes in
      Some {
        syntax_node = node;
        keyword_token = direct_required_token_with_text ~context:[ "fun_expression" ] node "fun";
        colon_token =
          (
            match return_type with
            | Some _ -> direct_token_with_text node ":"
            | None -> None
          );
        arrow_token;
        parameters = parameters_from_nodes prefix_nodes;
        return_type;
        body;
        attributes = [];
      }
  | [] -> None

and function_case_body_from_node = fun node ->
  let cases = direct_non_trivia_nodes node
  |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.MATCH_CASE)
  |> List.filter_map match_case_from_node in
  let cases =
    match direct_token_with_text node "|", cases with
    | Some leading_bar_token, ({ Cst.bar_token=None; _ } as first_case) :: rest -> {
      first_case
      with bar_token = Some leading_bar_token
    }
    :: rest
    | _ -> cases
  in
  ({ Cst.syntax_node = node; cases }: Cst.function_case_body)

and fun_body_from_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.FUNCTION_EXPR -> Cst.Cases (function_case_body_from_node node)
  | _ -> Cst.Expression (expression_from_node node)

and function_expression_from_node = fun node ->
  Some {
    Cst.syntax_node = node;
    keyword_token = direct_required_token_with_text ~context:[ "function_expression" ] node "function";
    cases = (function_case_body_from_node node).cases;
    attributes = []
  }

and let_operator_expression_from_node = fun node ->
  let operator_tokens, in_token = binding_operator_tokens_from_node node in
  let lifted_children = direct_non_trivia_nodes node
  |> List.filter (fun child -> not (is_attribute_node child)) in
  match List.rev lifted_children with
  | body_node :: rev_binding_nodes ->
      let binding_nodes = List.rev rev_binding_nodes in
      let expected_binding_nodes = 2 * List.length operator_tokens in
      if
        List.length operator_tokens = 0
        || not (Int.equal (List.length binding_nodes) expected_binding_nodes)
      then
        None
      else
        let rec lift_bindings acc token_triplets nodes =
          match token_triplets, nodes with
          | [], [] ->
              List.rev acc
          | (keyword_token, operator_token, equals_token) :: rest_pairs, binding_pattern_node :: bound_value_node :: rest_nodes ->
              let binding: Cst.binding_operator_binding = {
                keyword_token = keyword_token;
                operator_token = operator_token;
                equals_token = equals_token;
                binding_pattern = pattern_from_node binding_pattern_node;
                bound_value = expression_from_node bound_value_node;
                and_binding = None;
              }
              in
              lift_bindings (binding :: acc) rest_pairs rest_nodes
          | _ ->
              bail
                ~message:"expected alternating binding-operator pattern/value nodes during Ceibo -> CST lifting"
                ~syntax_node:node
                ~context:[ "expression"; "let_operator"; "bindings" ]
        in
        (
          match lift_bindings [] operator_tokens binding_nodes with
          | binding :: and_bindings ->
              Some {
                Cst.syntax_node = node;
                binding = { binding with and_binding = binding_operator_chain_of_list and_bindings };
                in_token;
                body = expression_from_node body_node;
                attributes = [];
              }
          | [] -> None
        )
  | [] -> None

and let_expression_from_node = fun ~is_recursive_binding node ->
  if is_binding_operator_expression_node node then
    None
  else
    match let_expression_parts ~is_recursive_binding node with
    | Some (`Value (is_recursive_binding, binding_pattern_node, prefix_nodes, bound_value_node, and_binding_nodes, body_node)) ->
        let and_keyword_tokens =
          direct_non_trivia_tokens node
          |> List.filter
            (fun syntax_token ->
              String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "and")
          |> List.map token
        in
        let lift_and_binding and_keyword_token node =
          let direct_children = direct_non_trivia_nodes node in
          let binding_attributes = direct_children |> List.filter is_attribute_node |> List.map attribute_from_node in
          let binding_children = direct_children
          |> List.filter (fun child -> not (is_attribute_node child)) in
          match binding_children with
          | nested_binding_pattern_node :: rest -> (
              match List.rev rest with
              | value_node :: rev_param_nodes ->
                  let prefix_nodes = List.rev rev_param_nodes in
                  let binding_equals_token =
                    match direct_token_with_text node "=" with
                    | Some equals_token -> equals_token
                    | None -> bail
                      ~message:"expected and-binding equals during Ceibo -> CST lifting"
                      ~syntax_node:node
                      ~context:[ "let_expression.and" ]
                  in
                  Some Cst.LetBinding.{
                    syntax_node = node;
                    keyword_token = and_keyword_token;
                    rec_token = direct_token_with_text node "rec";
                    equals_token = binding_equals_token;
                    attributes = binding_attributes;
                    binding_pattern = pattern_from_node nested_binding_pattern_node;
                    parameters = binding_parameters_from_prefix prefix_nodes;
                    value = binding_value_from_prefix
                      ~binding_syntax_node:node
                      ~prefix_nodes:prefix_nodes
                      ~value_node:value_node;
                    and_binding = None;
                  }
              | [] -> None
            )
          | [] -> None
        in
        let equals_token =
          match direct_token_with_text node "=" with
          | Some equals_token -> equals_token
          | None -> bail
            ~message:"expected let equals during Ceibo -> CST lifting"
            ~syntax_node:node
            ~context:[ "let_expression" ]
        in
        let in_token =
          match direct_token_with_text node "in" with
          | Some in_token -> in_token
          | None -> bail
            ~message:"expected let-in keyword during Ceibo -> CST lifting"
            ~syntax_node:node
            ~context:[ "let_expression" ]
        in
        Some {
          syntax_node = node;
          keyword_token =
            (
              match direct_token_with_text node "let" with
              | Some keyword_token -> keyword_token
              | None -> bail
                ~message:"expected let keyword during Ceibo -> CST lifting"
                ~syntax_node:node
                ~context:[ "let_expression" ]
            );
          rec_token = direct_token_with_text node "rec";
          equals_token;
          in_token;
          binding_pattern = pattern_from_node binding_pattern_node;
          parameters = binding_parameters_from_prefix prefix_nodes;
          bound_value = binding_value_from_prefix
            ~binding_syntax_node:node
            ~prefix_nodes:prefix_nodes
            ~value_node:bound_value_node;
          and_binding =
            and_binding_nodes |> List.mapi
              (fun index and_binding_node ->
                match List.nth_opt and_keyword_tokens index with
                | Some and_keyword_token -> lift_and_binding and_keyword_token and_binding_node
                | None -> bail
                  ~message:"expected matching and keyword for let-expression binding during Ceibo -> CST lifting"
                  ~syntax_node:and_binding_node
                  ~context:[ "let_expression"; "and_bindings" ]) |> List.filter_map
              (fun binding -> binding) |> let_binding_chain_of_list;
          body = expression_from_node body_node;
          attributes = [];
        }
    | _ -> None

and apply_payload_and_item_attribute = fun ~can_lift_payload node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.APPLY_EXPR -> (
      match direct_non_trivia_nodes node with
      | payload_node :: [ attribute_node ] when is_attribute_node attribute_node ->
          let attribute = attribute_from_node attribute_node in
          if attribute_is_item_like attribute && can_lift_payload payload_node then
            (payload_node, Some attribute)
          else
            (node, None)
      | _ -> (node, None)
    )
  | _ -> (node, None)

and expression_payload_and_field_attribute = fun node ->
  apply_payload_and_item_attribute ~can_lift_payload:can_lift_expression_node node

and class_expression_payload_and_field_attribute = fun node ->
  apply_payload_and_item_attribute ~can_lift_payload:can_lift_class_expression_node node

and class_method_from_node = fun node ->
  let children_without_attributes = direct_non_trivia_nodes node
  |> List.filter (fun child -> not (is_attribute_node child)) in
  match children_without_attributes with
  | name_node :: remainder when Ceibo.Red.SyntaxNode.kind name_node = Syntax_kind.IDENT_EXPR -> (
      match first_ident_token_in_subtree name_node with
      | Some name_token ->
          let modifier_tokens =
            direct_non_trivia_tokens node
            |> List.filter
              (fun token ->
                let text = Ceibo.Red.SyntaxToken.text token in
                String.equal "!" text || String.equal "private" text || String.equal "virtual" text)
            |> List.map token
          in
          let body, field_attributes =
            match remainder |> List.rev |> List.find_opt can_lift_expression_node with
            | Some body_node ->
                let payload_node, field_attribute = expression_payload_and_field_attribute body_node in
                (Some (expression_from_node payload_node), Option.to_list field_attribute)
            | None -> (None, [])
          in
          let type_, type_attributes =
            match List.find_opt can_lift_core_type_node remainder with
            | Some type_node ->
                let payload_type_node, field_attribute = core_type_payload_and_field_attribute type_node in
                (Some (core_type_from_node payload_type_node), Option.to_list field_attribute)
            | None -> (None, [])
          in
          let is_virtual =
            List.exists
              (fun tok ->
                String.equal (Ceibo.Red.SyntaxToken.text tok) "virtual")
              (direct_non_trivia_tokens node)
          in
          let colon_token = direct_token_with_text node ":" in
          let definition: Cst.method_definition =
            if is_virtual then
              match type_ with
              | Some type_ -> (
                  match
                    List.find_opt
                      (fun token ->
                        String.equal (Cst.Token.text token) "virtual")
                      modifier_tokens
                  with
                  | Some virtual_token -> Cst.VirtualMethod { virtual_token; type_ }
                  | None -> bail
                    ~message:"expected virtual token for virtual class method during Ceibo -> CST lifting"
                    ~syntax_node:node
                    ~context:[ "class_field"; "method"; "virtual" ]
                )
              | None -> bail
                ~message:"expected type annotation for virtual class method during Ceibo -> CST lifting"
                ~syntax_node:node
                ~context:[ "class_field"; "method"; "virtual" ]
            else
              match body with
              | Some body ->
                  Cst.ConcreteMethod {
                    body;
                    type_ =
                      Option.map
                        (fun type_ ->
                          (
                            (
                              match colon_token with
                              | Some colon_token -> colon_token
                              | None -> bail
                                ~message:"expected class method colon token during Ceibo -> CST lifting"
                                ~syntax_node:node
                                ~context:[ "class_field"; "method"; "colon_token" ]
                            ),
                            type_
                          ))
                        type_;
                  }
              | None -> bail
                ~message:"expected body expression for concrete class method during Ceibo -> CST lifting"
                ~syntax_node:node
                ~context:[ "class_field"; "method"; "concrete" ]
          in
          let field: Cst.class_method = {
            Cst.syntax_node = node;
            name_token;
            concrete_equals_token = direct_token_with_text node "=";
            virtual_colon_token = colon_token;
            definition;
            modifier_tokens;
          }
          in
          Some (field, type_attributes @ field_attributes)
      | None -> None
    )
  | _ -> None

and class_value_from_node = fun node ->
  let children_without_attributes = direct_non_trivia_nodes node
  |> List.filter (fun child -> not (is_attribute_node child)) in
  match children_without_attributes with
  | name_node :: remainder when Ceibo.Red.SyntaxNode.kind name_node = Syntax_kind.IDENT_EXPR -> (
      match first_ident_token_in_subtree name_node with
      | Some name_token ->
          let modifier_tokens =
            direct_non_trivia_tokens node
            |> List.filter
              (fun token ->
                let text = Ceibo.Red.SyntaxToken.text token in
                String.equal "!" text || String.equal "mutable" text || String.equal "virtual" text)
            |> List.map token
          in
          let value, field_attributes =
            match remainder |> List.rev |> List.find_opt can_lift_expression_node with
            | Some value_node ->
                let payload_node, field_attribute = expression_payload_and_field_attribute value_node in
                (Some (expression_from_node payload_node), Option.to_list field_attribute)
            | None -> (None, [])
          in
          let type_, type_attributes =
            match List.find_opt can_lift_core_type_node remainder with
            | Some type_node ->
                let payload_type_node, field_attribute = core_type_payload_and_field_attribute type_node in
                (Some (core_type_from_node payload_type_node), Option.to_list field_attribute)
            | None -> (None, [])
          in
          let is_virtual =
            List.exists
              (fun tok ->
                String.equal (Ceibo.Red.SyntaxToken.text tok) "virtual")
              (direct_non_trivia_tokens node)
          in
          let colon_token = direct_token_with_text node ":" in
          let definition: Cst.value_definition =
            if is_virtual then
              match type_ with
              | Some type_ -> (
                  match
                    List.find_opt
                      (fun token ->
                        String.equal (Cst.Token.text token) "virtual")
                      modifier_tokens
                  with
                  | Some virtual_token -> Cst.VirtualValue { virtual_token; type_ }
                  | None -> bail
                    ~message:"expected virtual token for virtual class value during Ceibo -> CST lifting"
                    ~syntax_node:node
                    ~context:[ "class_field"; "value"; "virtual" ]
                )
              | None -> bail
                ~message:"expected type annotation for virtual class value during Ceibo -> CST lifting"
                ~syntax_node:node
                ~context:[ "class_field"; "value"; "virtual" ]
            else
              match value with
              | Some value ->
                  Cst.ConcreteValue {
                    value;
                    type_ =
                      Option.map
                        (fun type_ ->
                          (
                            (
                              match colon_token with
                              | Some colon_token -> colon_token
                              | None -> bail
                                ~message:"expected class value colon token during Ceibo -> CST lifting"
                                ~syntax_node:node
                                ~context:[ "class_field"; "value"; "colon_token" ]
                            ),
                            type_
                          ))
                        type_;
                  }
              | None -> bail
                ~message:"expected bound expression for concrete class value during Ceibo -> CST lifting"
                ~syntax_node:node
                ~context:[ "class_field"; "value"; "concrete" ]
          in
          let field: Cst.class_value = {
            Cst.syntax_node = node;
            name_token;
            concrete_equals_token = direct_token_with_text node "=";
            virtual_colon_token = colon_token;
            definition;
            modifier_tokens;
          }
          in
          Some (field, type_attributes @ field_attributes)
      | None -> None
    )
  | _ -> None

and class_inherit_from_node = fun node ->
  match
    direct_non_trivia_nodes node
    |> List.filter (fun child -> not (is_attribute_node child))
    |> List.find_map
      (fun child ->
        if can_lift_class_expression_node child then
          let payload_node, field_attribute = class_expression_payload_and_field_attribute child in
          Some (class_expression_from_node payload_node, Option.to_list field_attribute)
        else
          None)
  with
  | Some (class_expression, field_attributes) -> Some (
    ({ syntax_node = node; class_expression }: Cst.class_inherit),
    field_attributes
  )
  | None -> None

and class_constraint_from_node = fun node ->
  match direct_non_trivia_nodes node |> List.filter can_lift_core_type_node with
  | left_node :: right_node :: _ ->
      let payload_right_node, _field_attribute = core_type_payload_and_field_attribute right_node in
      Some (
        ({
            syntax_node = node;
            left = core_type_from_node left_node;
            equals_token =
              (
                match direct_token_with_text node "=" with
                | Some equals_token -> equals_token
                | None -> bail
                  ~message:"expected class constraint equals token during Ceibo -> CST lifting"
                  ~syntax_node:node
                  ~context:[ "class_constraint"; "equals_token" ]
              );
            right = core_type_from_node payload_right_node;
          }: Cst.class_constraint),
        []
      )
  | _ -> None

and class_initializer_from_node = fun node ->
  match direct_non_trivia_tokens node, direct_non_trivia_nodes node with
  | initializer_kw :: _, children when String.equal (Ceibo.Red.SyntaxToken.text initializer_kw) "initializer" ->
      let body, field_attributes =
        match
          children |> List.filter (fun child -> not (is_attribute_node child)) |> List.find_map
            (fun child ->
              if can_lift_expression_node child then
                let payload_node, field_attribute = expression_payload_and_field_attribute child in
                Some (Some (expression_from_node payload_node), Option.to_list field_attribute)
              else
                None)
        with
        | Some body_and_attributes -> body_and_attributes
        | None -> (None, [])
      in
      Some (
        ({
            syntax_node = node;
            body =
              (
                match body with
                | Some body -> body
                | None -> bail
                  ~message:"expected body expression for class initializer during Ceibo -> CST lifting"
                  ~syntax_node:node
                  ~context:[ "class_field"; "initializer" ]
              );
          }: Cst.class_initializer),
        field_attributes
      )
  | _ -> None

and class_field_from_node = fun node ->
  let direct_attributes = attributes_from_node node in
  let field, lifted_attributes =
    match Ceibo.Red.SyntaxNode.kind node with
    | Syntax_kind.OBJECT_METHOD -> (
        match class_method_from_node node with
        | Some (field, field_attributes) -> (Cst.ClassField.Method field, field_attributes)
        | None -> unsupported_class_expression node
      )
    | Syntax_kind.OBJECT_VAL -> (
        match class_value_from_node node with
        | Some (field, field_attributes) -> (Cst.ClassField.Value field, field_attributes)
        | None -> unsupported_class_expression node
      )
    | Syntax_kind.OBJECT_INHERIT -> (
        match class_inherit_from_node node with
        | Some (field, field_attributes) -> (Cst.ClassField.Inherit field, field_attributes)
        | None -> unsupported_class_expression node
      )
    | Syntax_kind.TYPE_CONSTRAINT -> (
        match class_constraint_from_node node with
        | Some (field, field_attributes) -> (Cst.ClassField.Constraint field, field_attributes)
        | None -> unsupported_class_expression node
      )
    | Syntax_kind.EXTENSION_EXPR ->
        (Cst.ClassField.Extension (extension_from_node node), [])
    | Syntax_kind.IDENT_EXPR when is_initializer_node node -> (
        match class_initializer_from_node node with
        | Some (field, field_attributes) -> (Cst.ClassField.Initializer field, field_attributes)
        | None -> unsupported_class_expression node
      )
    | _ ->
        unsupported_class_expression node
  in
  class_field_with_attributes field (direct_attributes @ lifted_attributes)

and class_structure_from_node = fun node ->
  let non_trivia_children = direct_non_trivia_nodes node in
  let self_pattern, field_children =
    match non_trivia_children with
    | self_node :: rest when Ceibo.Red.SyntaxNode.kind self_node = Syntax_kind.OBJECT_SELF -> (
        match direct_non_trivia_nodes self_node with
        | pattern_node :: _ -> (Some (pattern_from_node pattern_node), rest)
        | [] -> (None, rest)
      )
    | _ -> (None, non_trivia_children)
  in
  let rec lift_fields = fun acc ->
    function
    | [] -> Some (List.rev acc)
    | child :: rest -> (
        match Ceibo.Red.SyntaxNode.kind child with
        | Syntax_kind.OBJECT_METHOD
        | Syntax_kind.OBJECT_VAL
        | Syntax_kind.OBJECT_INHERIT
        | Syntax_kind.TYPE_CONSTRAINT
        | Syntax_kind.EXTENSION_EXPR -> lift_fields (class_field_from_node child :: acc) rest
        | Syntax_kind.IDENT_EXPR when is_initializer_node child -> lift_fields
          (class_field_from_node child :: acc)
          rest
        | _ -> None
      )
  in
  match lift_fields [] field_children with
  | Some fields -> Some (({ syntax_node = node; self_pattern; fields }: Cst.class_structure))
  | None -> None

and class_let_expression_from_node = fun ~is_recursive_binding node ->
  if is_binding_operator_expression_node node then
    None
  else
    match let_expression_parts ~is_recursive_binding node with
    | Some (`Value (is_recursive_binding, binding_pattern_node, prefix_nodes, bound_value_node, and_binding_nodes, body_node)) when can_lift_class_expression_node
      body_node ->
        let and_keyword_tokens =
          direct_non_trivia_tokens node
          |> List.filter
            (fun syntax_token ->
              String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "and")
          |> List.map token
        in
        let lift_and_binding and_keyword_token node =
          let direct_children = direct_non_trivia_nodes node in
          let binding_attributes = direct_children |> List.filter is_attribute_node |> List.map attribute_from_node in
          let binding_children = direct_children
          |> List.filter (fun child -> not (is_attribute_node child)) in
          match binding_children with
          | nested_binding_pattern_node :: rest -> (
              match List.rev rest with
              | value_node :: rev_param_nodes ->
                  let prefix_nodes = List.rev rev_param_nodes in
                  Some Cst.LetBinding.{
                    syntax_node = node;
                    keyword_token = and_keyword_token;
                    rec_token = direct_token_with_text node "rec";
                    equals_token =
                      (
                        match direct_token_with_text node "=" with
                        | Some equals_token -> equals_token
                        | None -> bail
                          ~message:"expected class let and-binding equals during Ceibo -> CST lifting"
                          ~syntax_node:node
                          ~context:[ "class_let_expression.and" ]
                      );
                    attributes = binding_attributes;
                    binding_pattern = pattern_from_node nested_binding_pattern_node;
                    parameters = binding_parameters_from_prefix prefix_nodes;
                    value = binding_value_from_prefix
                      ~binding_syntax_node:node
                      ~prefix_nodes:prefix_nodes
                      ~value_node:value_node;
                    and_binding = None;
                  }
              | [] -> None
            )
          | [] -> None
        in
        Some (
          ({
              syntax_node = node;
              keyword_token =
                (
                  match direct_token_with_text node "let" with
                  | Some keyword_token -> keyword_token
                  | None -> bail
                    ~message:"expected class let keyword during Ceibo -> CST lifting"
                    ~syntax_node:node
                    ~context:[ "class_let_expression" ]
                );
              rec_token = direct_token_with_text node "rec";
              equals_token =
                (
                  match direct_token_with_text node "=" with
                  | Some equals_token -> equals_token
                  | None -> bail
                    ~message:"expected class let equals during Ceibo -> CST lifting"
                    ~syntax_node:node
                    ~context:[ "class_let_expression" ]
                );
              in_token =
                (
                  match direct_token_with_text node "in" with
                  | Some in_token -> in_token
                  | None -> bail
                    ~message:"expected class let-in keyword during Ceibo -> CST lifting"
                    ~syntax_node:node
                    ~context:[ "class_let_expression" ]
                );
              binding_pattern = pattern_from_node binding_pattern_node;
              parameters = binding_parameters_from_prefix prefix_nodes;
              bound_value = binding_value_from_prefix
                ~binding_syntax_node:node
                ~prefix_nodes:prefix_nodes
                ~value_node:bound_value_node;
              and_binding =
                and_binding_nodes |> List.mapi
                  (fun index and_binding_node ->
                    match List.nth_opt and_keyword_tokens index with
                    | Some and_keyword_token -> lift_and_binding and_keyword_token and_binding_node
                    | None -> bail
                      ~message:"expected matching and keyword for class let-expression binding during Ceibo -> CST lifting"
                      ~syntax_node:and_binding_node
                      ~context:[ "class_let_expression"; "and_bindings" ]) |> List.filter_map
                  (fun binding -> binding) |> let_binding_chain_of_list;
              body = class_expression_from_node body_node;
            }: Cst.class_let_expression)
        )
    | _ -> None

and local_open_class_expression_from_node = fun node ->
  let non_trivia_children = direct_non_trivia_nodes node in
  let non_trivia_tokens = direct_non_trivia_tokens node in
  match non_trivia_tokens, non_trivia_children with
  | first_token :: _, _ when String.equal (Ceibo.Red.SyntaxToken.text first_token) "let" ->
      let let_token = direct_required_token_with_text
        ~context:[ "class_expression"; "local_open"; "let_open" ]
        node
        "let" in
      let open_token = direct_required_token_with_text
        ~context:[ "class_expression"; "local_open"; "let_open" ]
        node
        "open" in
      let in_token = direct_required_token_with_text
        ~context:[ "class_expression"; "local_open"; "let_open" ]
        node
        "in" in
      let module_path =
        match direct_syntax_tokens_between_offsets
          ~after_offset:(Cst.Token.span open_token).end_
          ~before_offset:(Cst.Token.span in_token).start
          node
        |> trailing_module_path_tokens with
        | [] -> None
        | syntax_tokens -> Some (module_path_from_tokens ~syntax_node:node syntax_tokens)
      in
      let body_expr =
        List.rev non_trivia_children
        |> List.find_map
          (fun child ->
            if can_lift_class_expression_node child then
              Some (class_expression_from_node child)
            else
              None)
      in
      (
        match module_path, body_expr with
        | Some module_path, Some body ->
            Some (
              Cst.LetOpen {
                syntax_node = node;
                let_token;
                open_token;
                module_path;
                in_token;
                body;
              }
            )
        | _ -> None
      )
  | _, module_path_node :: body_node :: _ when can_lift_class_expression_node body_node ->
      let module_path = module_path_like_from_node module_path_node in
      let dot_token = direct_required_token_with_text
        ~context:[ "class_expression"; "local_open"; "delimited" ]
        node
        "." in
      let module_path_span = span_of_syntax_node_nontrivia_bounds module_path_node in
      let body_span = span_of_syntax_node_nontrivia_bounds body_node in
      let boundary_tokens = direct_tokens_between_offsets
        ~after_offset:module_path_span.end_
        ~before_offset:body_span.start
        node in
      let opening_token =
        boundary_tokens
        |> List.find_opt
          (fun token ->
            let text = Cst.Token.text token in
            String.equal text "("
            || String.equal text "["
            || String.equal text "[|"
            || String.equal text "{")
      in
      let trailing_tokens = direct_tokens_between_offsets
        ~after_offset:body_span.end_
        ~before_offset:(span_of_syntax_node_nontrivia_bounds node).end_
        node in
      let closing_token =
        trailing_tokens
        |> List.find_opt
          (fun token ->
            let text = Cst.Token.text token in
            String.equal text ")"
            || String.equal text "]"
            || String.equal text "|]"
            || String.equal text "}")
      in
      Some (
        Cst.Delimited {
          syntax_node = node;
          module_path;
          dot_token;
          opening_token;
          body = class_expression_from_node body_node;
          closing_token;
        }
      )
  | _ ->
      None

and class_expression_from_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_EXPR ->
      Cst.ClassExpression.Path (ident_path_from_node node)
  | Syntax_kind.MODULE_PATH ->
      Cst.ClassExpression.Path (module_path_from_node node)
  | Syntax_kind.ARRAY_INDEX_EXPR -> (
      match direct_non_trivia_nodes node with
      | module_path_node :: class_expression_node :: _ ->
          Cst.ClassExpression.LocalOpen (
            Cst.Delimited {
              syntax_node = node;
              module_path = module_path_like_from_node module_path_node;
              dot_token = direct_required_token_with_text
                ~context:[ "class_expression"; "local_open"; "delimited" ]
                node
                ".";
              opening_token = None;
              body = class_expression_from_node class_expression_node;
              closing_token = None;
            }
          )
      | _ -> unsupported_class_expression node
    )
  | Syntax_kind.OBJECT_EXPR -> (
      match class_structure_from_node node with
      | Some structure -> Cst.ClassExpression.Structure structure
      | None -> unsupported_class_expression node
    )
  | Syntax_kind.FUN_EXPR -> (
      match List.rev (direct_non_trivia_nodes node) with
      | body_node :: rev_param_nodes when can_lift_class_expression_node body_node -> Cst.ClassExpression.Fun {
        syntax_node = node;
        parameters = rev_param_nodes |> List.rev |> parameters_from_nodes;
        body = class_expression_from_node body_node
      }
      | _ -> unsupported_class_expression node
    )
  | Syntax_kind.APPLY_EXPR -> (
      match direct_non_trivia_nodes node with
      | callee_node :: [ attribute_node ] when can_lift_class_expression_node callee_node
      && Ceibo.Red.SyntaxNode.kind attribute_node = Syntax_kind.ATTRIBUTE_EXPR -> Cst.ClassExpression.Attribute {
        syntax_node = node;
        class_expression = class_expression_from_node callee_node;
        attribute = attribute_from_node attribute_node
      }
      | callee_node :: argument_node :: _ when can_lift_class_expression_node callee_node -> Cst.ClassExpression.Apply {
        syntax_node = node;
        callee = class_expression_from_node callee_node;
        argument = apply_argument_from_node argument_node
      }
      | _ -> unsupported_class_expression node
    )
  | Syntax_kind.LET_EXPR -> (
      match class_let_expression_from_node ~is_recursive_binding:false node with
      | Some expr -> Cst.ClassExpression.Let expr
      | None -> unsupported_class_expression node
    )
  | Syntax_kind.LET_REC_EXPR -> (
      match class_let_expression_from_node ~is_recursive_binding:true node with
      | Some expr -> Cst.ClassExpression.Let expr
      | None -> unsupported_class_expression node
    )
  | Syntax_kind.TYPED_EXPR -> (
      match direct_non_trivia_nodes node with
      | expression_node :: type_node :: _ when can_lift_class_expression_node expression_node
      && can_lift_class_type_node type_node ->
          Cst.ClassExpression.Constraint {
            syntax_node = node;
            class_expression = class_expression_from_node expression_node;
            colon_token =
              (
                match direct_token_with_text node ":" with
                | Some colon_token -> colon_token
                | None -> bail
                  ~message:"expected class expression constraint colon token during Ceibo -> CST lifting"
                  ~syntax_node:node
                  ~context:[ "class_expression.constraint"; "colon_token" ]
              );
            class_type = class_type_from_node type_node;
          }
      | _ -> unsupported_class_expression node
    )
  | Syntax_kind.LOCAL_OPEN_EXPR -> (
      match local_open_class_expression_from_node node with
      | Some expr -> Cst.ClassExpression.LocalOpen expr
      | None -> unsupported_class_expression node
    )
  | Syntax_kind.PAREN_EXPR -> (
      match direct_non_trivia_nodes node |> List.find_opt can_lift_class_expression_node, direct_non_trivia_tokens
        node with
      | Some inner_node, (opening_token :: _ :: _ as tokens) ->
          let closing_token = List.hd (List.rev tokens) in
          Cst.ClassExpression.Parenthesized {
            syntax_node = node;
            opening_token = token opening_token;
            inner = class_expression_from_node inner_node;
            closing_token = token closing_token
          }
      | None, _ ->
          unsupported_class_expression node
      | Some _, _ ->
          bail
            ~message:"expected parenthesized class expression delimiters during Ceibo -> CST lifting"
            ~syntax_node:node
            ~context:[ "class_expression.parenthesized" ]
    )
  | Syntax_kind.ATTRIBUTE_EXPR -> (
      match direct_non_trivia_nodes node with
      | first_child :: rest -> (
          match List.find_opt can_lift_class_expression_node (first_child :: rest), List.find_opt
            is_attribute_node
            rest with
          | Some payload_node, Some attribute_node -> Cst.ClassExpression.Attribute {
            syntax_node = node;
            class_expression = class_expression_from_node payload_node;
            attribute = attribute_from_node attribute_node
          }
          | _ -> unsupported_class_expression node
        )
      | [] -> unsupported_class_expression node
    )
  | Syntax_kind.EXTENSION_EXPR ->
      Cst.ClassExpression.Extension (extension_from_node node)
  | _ ->
      unsupported_class_expression node

and let_binding_from_binding_operator_binding = fun
  ~binding_syntax_node ({
      keyword_token=binding_keyword_token;
      equals_token=binding_equals_token;
      binding_pattern=clause_pattern;
      bound_value=clause_value;
      _
    }: Cst.binding_operator_binding) ->
  Cst.LetBinding.{
    syntax_node = binding_syntax_node;
    keyword_token = binding_keyword_token;
    rec_token = None;
    equals_token = binding_equals_token;
    attributes = [];
    binding_pattern = clause_pattern;
    parameters = [];
    value = clause_value;
    and_binding = None;
  }

and match_case_from_node = fun node ->
  let non_trivia_children = direct_non_trivia_nodes node in
  let direct_tokens = direct_non_trivia_tokens node in
  let direct_token_with_text_in_tokens expected =
    direct_tokens
    |> List.find_opt
      (fun syntax_token ->
        String.equal (Ceibo.Red.SyntaxToken.text syntax_token) expected)
    |> Option.map token
  in
  let direct_required_token_with_text_in_tokens ~context expected =
    match direct_token_with_text_in_tokens expected with
    | Some token -> token
    | None -> bail
      ~message:(("expected '" ^ expected ^ "' token during Ceibo -> CST lifting"))
      ~syntax_node:node
      ~context
  in
  let has_guard =
    direct_tokens
    |> List.exists
      (fun syntax_token ->
        String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "when")
  in
  match non_trivia_children with
  | pattern_node :: rest -> (
      let expression_children = rest |> List.filter can_lift_expression_node |> List.map expression_from_node in
      match expression_children, has_guard with
      | [], _ ->
          None
      | body_exprs, false -> (
          match List.rev body_exprs with
          | body_expr :: _ ->
              let arrow_token = direct_required_token_with_text_in_tokens ~context:[ "match_case" ] "->" in
              Some {
                syntax_node = node;
                bar_token = direct_token_with_text_in_tokens "|";
                when_token = None;
                arrow_token;
                pattern = pattern_from_node pattern_node;
                guard = None;
                body = body_expr;
              }
          | [] -> None
        )
      | guard_expr :: body_expr :: _, true ->
          let arrow_token = direct_required_token_with_text_in_tokens ~context:[ "match_case" ] "->" in
          Some {
            syntax_node = node;
            bar_token = direct_token_with_text_in_tokens "|";
            when_token = direct_token_with_text_in_tokens "when";
            arrow_token;
            pattern = pattern_from_node pattern_node;
            guard = Some guard_expr;
            body = body_expr;
          }
      | body_expr :: _, true ->
          let arrow_token = direct_required_token_with_text_in_tokens ~context:[ "match_case" ] "->" in
          Some {
            syntax_node = node;
            bar_token = direct_token_with_text_in_tokens "|";
            when_token = direct_token_with_text_in_tokens "when";
            arrow_token;
            pattern = pattern_from_node pattern_node;
            guard = None;
            body = body_expr;
          }
    )
  | [] -> None

and match_expression_from_node = fun node ->
  let non_attribute_children = direct_non_trivia_nodes node
  |> List.filter (fun child -> not (is_attribute_node child)) in
  match non_attribute_children with
  | scrutinee_node :: rest ->
      let match_cases = rest
      |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.MATCH_CASE)
      |> List.filter_map match_case_from_node in
      Some {
        syntax_node = node;
        keyword_token = direct_required_token_with_text ~context:[ "match_expression" ] node "match";
        with_token = direct_required_token_with_text ~context:[ "match_expression" ] node "with";
        scrutinee = expression_from_node scrutinee_node;
        cases = match_cases;
        attributes = [];
      }
  | [] -> None

and try_expression_from_node = fun node ->
  let non_attribute_children = direct_non_trivia_nodes node
  |> List.filter (fun child -> not (is_attribute_node child)) in
  match non_attribute_children with
  | body_node :: rest ->
      let match_cases = rest
      |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.MATCH_CASE)
      |> List.filter_map match_case_from_node in
      Some {
        syntax_node = node;
        keyword_token = direct_required_token_with_text ~context:[ "try_expression" ] node "try";
        with_token = direct_required_token_with_text ~context:[ "try_expression" ] node "with";
        body = expression_from_node body_node;
        cases = match_cases;
        attributes = [];
      }
  | [] -> None

and type_variable_from_node = fun node ->
  match List.rev (direct_non_trivia_tokens node) with
  | name_tok :: _ -> Some Cst.TypeVariable.{ syntax_node = node; name_token = token name_tok }
  | [] -> None

and type_parameter_from_node = fun node ->
  let lifted_type_variable =
    direct_non_trivia_nodes node
    |> List.find_opt (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_VAR)
    |> function
    | Some child -> type_variable_from_node child
    | None -> None
  in
  let parameter_variance =
    direct_non_trivia_tokens node
    |> List.find_map
      (fun syntax_token ->
        match Ceibo.Red.SyntaxToken.text syntax_token with
        | "+" -> Some (Cst.TypeParameterVariance.Covariant { marker_token = token syntax_token })
        | "-" -> Some (Cst.TypeParameterVariance.Contravariant { marker_token = token syntax_token })
        | _ -> None)
  in
  let parameter_injectivity_token = direct_token_with_text node "!" in
  Cst.TypeParameter.{
    syntax_node = node;
    variance = parameter_variance;
    injectivity_token = parameter_injectivity_token;
    type_variable = lifted_type_variable
  }

and type_parameters_from_node = fun node ->
  direct_non_trivia_nodes node
  |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_PARAM)
  |> List.map type_parameter_from_node

let record_field_name_token = fun node ->
  match direct_non_trivia_tokens node with
  | mutable_kw :: field_name :: _ when String.equal (Ceibo.Red.SyntaxToken.text mutable_kw) "mutable" -> Some (token
    field_name)
  | field_name :: _ -> Some (token field_name)
  | [] -> None

let type_constraint_from_node = fun node ->
  match direct_non_trivia_nodes node |> List.filter can_lift_core_type_node with
  | left_node :: right_node :: _ ->
      Some Cst.TypeConstraint.{
        syntax_node = node;
        left = core_type_from_node left_node;
        equals_token =
          (
            match direct_token_with_text node "=" with
            | Some equals_token -> equals_token
            | None -> bail
              ~message:"expected type constraint equals token during Ceibo -> CST lifting"
              ~syntax_node:node
              ~context:[ "type_constraint"; "equals_token" ]
          );
        right = core_type_from_node right_node;
      }
  | _ -> None

let private_flag_from_type_declaration_node = fun node ->
  direct_non_trivia_tokens node |> List.find_opt
    (fun syntax_token ->
      String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "private") |> function
  | Some private_token -> Cst.PrivateFlag.Private { private_token = token private_token }
  | None -> Cst.PrivateFlag.Public

let record_field_from_node = fun node ->
  record_field_name_token node |> Option.map
    (fun field_name ->
      let field_type_node = direct_non_trivia_nodes node |> List.find_opt can_lift_core_type_node in
      let lifted_field_type, lifted_attributes =
        match field_type_node with
        | Some field_type_node ->
            let field_type_node, attributes = peel_outer_type_attributes field_type_node in
            (core_type_from_node field_type_node, attributes)
        | None -> bail
          ~message:"expected record field type during Ceibo -> CST lifting"
          ~syntax_node:node
          ~context:[ "type_definition.record_field" ]
      in
      Cst.RecordField.{
        syntax_node = node;
        field_name;
        mutable_token =
          (
            match direct_non_trivia_tokens node with
            | first :: _ when String.equal (Ceibo.Red.SyntaxToken.text first) "mutable" -> Some (token
              first)
            | _ -> None
          );
        colon_token =
          (
            match direct_token_with_text node ":" with
            | Some colon_token -> colon_token
            | None -> bail
              ~message:"expected record field colon token during Ceibo -> CST lifting"
              ~syntax_node:node
              ~context:[ "type_definition.record_field"; "colon_token" ]
          );
        field_type = lifted_field_type;
        semicolon_token = direct_token_with_text node semicolon_text;
        attributes = lifted_attributes;
      })

let variant_constructor_from_node = fun node ->
  let constructor_payload_signature payload_type =
    let body =
      match payload_type with
      | Cst.CoreType.Poly { body; _ } -> body
      | _ -> payload_type
    in
    let rec collect_parameters = fun acc ->
      function
      | Cst.CoreType.Arrow { parameter_type; result_type; _ } -> collect_parameters
        (parameter_type :: acc)
        result_type
      | result_type -> (List.rev acc, result_type)
    in
    collect_parameters [] body
  in
  let direct_children = direct_non_trivia_nodes node in
  let lifted_attributes = direct_children
  |> List.filter is_attribute_node
  |> List.filter (fun attribute_node -> not (can_lift_core_type_node attribute_node))
  |> List.map attribute_from_node in
  let constructor_children = direct_children
  |> List.filter (fun child -> not (is_attribute_node child && not (can_lift_core_type_node child))) in
  match constructor_children with
  | first_child :: _ -> (
      match direct_non_trivia_tokens first_child with
      | constructor_name :: _ ->
          let is_gadt_constructor =
            direct_non_trivia_tokens node
            |> List.exists
              (fun syntax_token ->
                String.equal (Ceibo.Red.SyntaxToken.text syntax_token) ":")
          in
          let lifted_payload_type =
            constructor_children
            |> List.find_opt
              (fun child ->
                let kind = Ceibo.Red.SyntaxNode.kind child in
                can_lift_core_type_node child && kind != Syntax_kind.IDENT_EXPR)
            |> Option.map core_type_from_node
          in
          let lifted_arguments =
            if direct_non_trivia_tokens node |> List.exists
                (fun syntax_token ->
                  String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "of") then
              let type_nodes = constructor_children |> List.filter can_lift_core_type_node in
              match type_nodes with
              | [] ->
                  None
              | [ record_node ] when Ceibo.Red.SyntaxNode.kind record_node = Syntax_kind.TYPE_RECORD ->
                  let direct_tokens = direct_non_trivia_tokens record_node in
                  let opening_token, closing_token =
                    match direct_tokens with
                    | opening_token :: _ ->
                        let closing_token = List.hd (List.rev direct_tokens) in
                        (token opening_token, token closing_token)
                    | [] -> bail
                      ~message:"expected constructor record argument delimiters during Ceibo -> CST lifting"
                      ~syntax_node:record_node
                      ~context:[ "variant_constructor"; "record_arguments" ]
                  in
                  Some (Cst.ConstructorArguments.Record {
                    opening_token;
                    fields = direct_non_trivia_nodes record_node
                    |> List.filter
                      (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_RECORD_FIELD)
                    |> List.filter_map record_field_from_node;
                    closing_token
                  })
              | [ tuple_node ] when Ceibo.Red.SyntaxNode.kind tuple_node = Syntax_kind.TYPE_TUPLE ->
                  Some (Cst.ConstructorArguments.Tuple (direct_non_trivia_nodes tuple_node
                  |> List.filter can_lift_core_type_node
                  |> List.map core_type_from_node))
              | _ ->
                  Some (Cst.ConstructorArguments.Tuple (List.map core_type_from_node type_nodes))
            else if is_gadt_constructor then
              (
                match lifted_payload_type with
                | Some payload_type ->
                    let parameter_types, _result_type = constructor_payload_signature payload_type in
                    if List.length parameter_types = 0 then
                      None
                    else
                      Some (Cst.ConstructorArguments.Tuple parameter_types)
                | None -> None
              )
            else
              None
          in
          let lifted_result_type =
            if is_gadt_constructor then
              lifted_payload_type |> Option.map
                (fun payload_type ->
                  let _parameter_types, result_type = constructor_payload_signature payload_type in
                  result_type)
            else
              None
          in
          Some Cst.VariantConstructor.{
            syntax_node = node;
            attributes = lifted_attributes;
            bar_token = previous_direct_token_with_text_in_parent ~text:"|" node |> Option.map token;
            constructor_name = token constructor_name;
            separator_token =
              (
                match direct_token_with_text node "of" with
                | Some separator_token -> Some separator_token
                | None -> direct_token_with_text node ":"
              );
            arguments = lifted_arguments;
            payload_type = lifted_payload_type;
            arrow_token =
              subtree_non_trivia_tokens node |> List.find_opt
                (fun syntax_token ->
                  String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "->") |> Option.map token;
            result_type = lifted_result_type;
          }
      | [] -> None
    )
  | [] -> None

let poly_variant_tag_from_node = fun node ->
  let direct_children = direct_non_trivia_nodes node in
  let lifted_attributes = direct_children
  |> List.filter is_attribute_node
  |> List.filter (fun attribute_node -> not (can_lift_core_type_node attribute_node))
  |> List.map attribute_from_node in
  match direct_non_trivia_tokens node with
  | _backtick :: tag_name :: _ ->
      Cst.PolyVariantTag.{
        syntax_node = node;
        attributes = lifted_attributes;
        bar_token = previous_direct_token_with_text_in_parent ~text:"|" node |> Option.map token;
        tag_name = token tag_name;
        separator_token = direct_token_with_text node "of";
        payload_type = (direct_children |> List.find_opt can_lift_core_type_node |> Option.map core_type_from_node);
      }
  | tag_name :: _ ->
      Cst.PolyVariantTag.{
        syntax_node = node;
        attributes = lifted_attributes;
        bar_token = previous_direct_token_with_text_in_parent ~text:"|" node |> Option.map token;
        tag_name = token tag_name;
        separator_token = direct_token_with_text node "of";
        payload_type = (direct_children |> List.find_opt can_lift_core_type_node |> Option.map core_type_from_node);
      }
  | [] -> bail
    ~message:"expected poly-variant tag token during Ceibo -> CST lifting"
    ~syntax_node:node
    ~context:[ "type_definition.poly_variant_tag" ]

let poly_variant_bound_from_node = fun node ->
  match direct_non_trivia_tokens node with
  | _open_bracket :: marker_token :: _ when String.equal (Ceibo.Red.SyntaxToken.text marker_token) "<" -> Cst.PolyVariantBound.UpperBound {
    marker_token = token marker_token
  }
  | _open_bracket :: marker_token :: _ when String.equal (Ceibo.Red.SyntaxToken.text marker_token) ">" -> Cst.PolyVariantBound.LowerBound {
    marker_token = token marker_token
  }
  | _ -> Cst.PolyVariantBound.Exact

let row_field_from_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.POLY_VARIANT_TAG ->
      Cst.RowField.Tag (poly_variant_tag_from_node node)
  | _ when can_lift_core_type_node node ->
      let inherited_type =
        match Ceibo.Red.SyntaxNode.kind node with
        | Syntax_kind.TYPE_CONSTR -> Cst.CoreType.Constr {
          syntax_node = node;
          constructor_path = type_constructor_path_from_node node;
          arguments = []
        }
        | _ -> core_type_from_node node
      in
      let bar_token = previous_direct_token_with_text_in_parent ~text:"|" node |> Option.map token in
      Cst.RowField.Inherit { bar_token; syntax_node = node; type_ = inherited_type }
  | _ ->
      bail
        ~message:"expected polymorphic variant row field during Ceibo -> CST lifting"
        ~syntax_node:node
        ~context:[ "type_definition.poly_variant.row_field" ]

let poly_variant_from_node = fun node ->
  let direct_tokens = direct_non_trivia_tokens node in
  let opening_token, closing_token =
    match direct_tokens with
    | opening_token :: _ ->
        let closing_token = List.hd (List.rev direct_tokens) in
        (token opening_token, token closing_token)
    | [] -> bail
      ~message:"expected type definition polyvariant delimiters during Ceibo -> CST lifting"
      ~syntax_node:node
      ~context:[ "type_definition.poly_variant" ]
  in
  {
    Cst.syntax_node = node;
    opening_token;
    kind = poly_variant_bound_from_node node;
    fields =
      direct_non_trivia_nodes node |> List.filter
        (fun child ->
          let kind = Ceibo.Red.SyntaxNode.kind child in
          kind = Syntax_kind.POLY_VARIANT_TAG || can_lift_core_type_node child) |> List.map row_field_from_node;
    closing_token;
  }

let type_declaration_name_path = fun node ->
  let is_name_node child =
    let kind = Ceibo.Red.SyntaxNode.kind child in
    kind = Syntax_kind.IDENT_EXPR || kind = Syntax_kind.MODULE_PATH
  in
  direct_non_trivia_nodes node |> List.find_opt is_name_node |> Option.map
    (fun child ->
      match Ceibo.Red.SyntaxNode.kind child with
      | Syntax_kind.MODULE_PATH -> module_path_from_node child
      | Syntax_kind.IDENT_EXPR -> ident_path_from_node child
      | _ -> bail
        ~message:"expected type declaration name path during Ceibo -> CST lifting"
        ~syntax_node:child
        ~context:[ "type_declaration"; "name" ])

let is_type_extension_node = fun node ->
  Ceibo.Red.SyntaxNode.kind node = Syntax_kind.TYPE_DECL && (
    direct_non_trivia_tokens node |> List.exists
      (fun syntax_token ->
        String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "+")
  )

let is_type_definition_body_kind = function
  | Syntax_kind.TYPE_VARIANT_CONSTR
  | Syntax_kind.TYPE_RECORD
  | Syntax_kind.TYPE_POLY_VARIANT
  | Syntax_kind.TYPE_EXTENSIBLE
  | Syntax_kind.OBJECT_TYPE
  | Syntax_kind.FIRST_CLASS_MODULE_TYPE -> true
  | _ -> false

let type_manifest_alias_from_node = fun node ->
  let direct_children = direct_non_trivia_nodes node in
  if
    not
      (List.exists (fun child -> is_type_definition_body_kind (Ceibo.Red.SyntaxNode.kind child)) direct_children)
  then
    None
  else
    let rec loop = function
      | [] -> None
      | child :: _ when is_type_definition_body_kind (Ceibo.Red.SyntaxNode.kind child) -> None
      | child :: rest ->
          if can_lift_core_type_node child then
            Some (core_type_from_node child)
          else
            loop rest
    in
    loop direct_children

let type_declaration_has_nonrec = fun node ->
  direct_non_trivia_tokens node |> List.exists
    (fun syntax_token ->
      String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "nonrec")

let type_definition_from_node = fun node ->
  if Ceibo.Red.SyntaxNode.kind node = Syntax_kind.TYPE_EXTENSIBLE then
    Cst.TypeDefinition.Extensible { syntax_node = node }
  else
    let direct_children = direct_non_trivia_nodes node in
    let variant_constructors = direct_children
    |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_VARIANT_CONSTR)
    |> List.filter_map variant_constructor_from_node in
    if List.length variant_constructors > 0 then
      Cst.TypeDefinition.Variant { syntax_node = node; constructors = variant_constructors }
    else
      match direct_children
      |> List.find_opt (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_RECORD) with
      | Some record_node ->
          let direct_tokens = direct_non_trivia_tokens record_node in
          let opening_token, closing_token =
            match direct_tokens with
            | opening_token :: _ ->
                let closing_token = List.hd (List.rev direct_tokens) in
                (token opening_token, token closing_token)
            | [] -> bail
              ~message:"expected type definition record delimiters during Ceibo -> CST lifting"
              ~syntax_node:record_node
              ~context:[ "type_definition.record" ]
          in
          let fields = direct_non_trivia_nodes record_node
          |> List.filter
            (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_RECORD_FIELD)
          |> List.filter_map record_field_from_node in
          Cst.TypeDefinition.Record {
            syntax_node = record_node;
            opening_token;
            fields;
            closing_token
          }
      | None -> (
          match direct_children
          |> List.find_opt
            (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_POLY_VARIANT) with
          | Some poly_variant_node -> Cst.TypeDefinition.PolyVariant (poly_variant_from_node poly_variant_node)
          | None -> (
              match
                direct_children |> List.find_opt
                  (fun child ->
                    let kind = Ceibo.Red.SyntaxNode.kind child in
                    kind = Syntax_kind.TYPE_EXTENSIBLE
                    || kind = Syntax_kind.OBJECT_TYPE
                    || kind = Syntax_kind.FIRST_CLASS_MODULE_TYPE)
              with
              | Some first ->
                  let kind = Ceibo.Red.SyntaxNode.kind first in
                  if kind = Syntax_kind.TYPE_EXTENSIBLE then
                    Cst.TypeDefinition.Extensible { syntax_node = first }
                  else if kind = Syntax_kind.OBJECT_TYPE then
                    let direct_tokens = direct_non_trivia_tokens first in
                    let opening_token, closing_token =
                      match direct_tokens with
                      | opening_token :: _ ->
                          let closing_token = List.hd (List.rev direct_tokens) in
                          (token opening_token, token closing_token)
                      | [] -> bail
                        ~message:"expected type definition object delimiters during Ceibo -> CST lifting"
                        ~syntax_node:first
                        ~context:[ "type_definition.object" ]
                    in
                    Cst.TypeDefinition.Object {
                      syntax_node = first;
                      opening_token;
                      fields =
                        direct_non_trivia_nodes first
                        |> List.filter
                          (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.OBJECT_TYPE_FIELD)
                        |> List.map
                          (fun field_node ->
                            match first_ident_token_in_subtree field_node, direct_non_trivia_nodes field_node
                            |> List.find_opt can_lift_core_type_node with
                            | Some field_name, Some field_type_node ->
                                ({
                                    Cst.syntax_node = field_node;
                                    field_name;
                                    colon_token =
                                      (
                                        match direct_token_with_text field_node ":" with
                                        | Some colon_token -> colon_token
                                        | None -> bail
                                          ~message:"expected object type field colon token during Ceibo -> CST lifting"
                                          ~syntax_node:field_node
                                          ~context:[ "type_definition.object_field"; "colon_token" ]
                                      );
                                    field_type = core_type_from_node field_type_node;
                                    semicolon_token = direct_token_with_text field_node semicolon_text;
                                  }: Cst.object_type_field)
                            | _ -> bail
                              ~message:"expected object type field name and type during Ceibo -> CST lifting"
                              ~syntax_node:field_node
                              ~context:[ "type_definition.object_field" ]);
                      closing_token;
                    }
                  else if kind = Syntax_kind.FIRST_CLASS_MODULE_TYPE then
                    let direct_tokens = direct_non_trivia_tokens first in
                    let opening_token, closing_token =
                      match direct_tokens with
                      | opening_token :: _ ->
                          let closing_token = List.hd (List.rev direct_tokens) in
                          (token opening_token, token closing_token)
                      | [] -> bail
                        ~message:"expected type definition first-class module delimiters during Ceibo -> CST lifting"
                        ~syntax_node:first
                        ~context:[ "type_definition.first_class_module" ]
                    in
                    Cst.TypeDefinition.FirstClassModule {
                      syntax_node = first;
                      opening_token;
                      package_type = module_type_from_first_class_module_type_node first;
                      closing_token
                    }
                  else
                    bail
                      ~message:"unsupported type definition body during Ceibo -> CST lifting"
                      ~syntax_node:first
                      ~context:[ "type_definition.body" ]
              | None ->
                  let remaining_nodes =
                    direct_children
                    |> List.filter
                      (fun child ->
                        let kind = Ceibo.Red.SyntaxNode.kind child in
                        kind != Syntax_kind.TYPE_PARAM
                        && kind != Syntax_kind.IDENT_EXPR
                        && kind != Syntax_kind.MODULE_PATH
                        && not
                          (kind = Syntax_kind.ATTRIBUTE_EXPR && not (can_lift_core_type_node child)))
                  in
                  match remaining_nodes with
                  | [] -> Cst.TypeDefinition.Abstract
                  | first :: _ ->
                      let kind = Ceibo.Red.SyntaxNode.kind first in
                      if kind = Syntax_kind.TYPE_EXTENSIBLE then
                        Cst.TypeDefinition.Extensible { syntax_node = first }
                      else if can_lift_core_type_node first then
                        Cst.TypeDefinition.Alias {
                          syntax_node = first;
                          manifest = core_type_from_node first
                        }
                      else
                        bail
                          ~message:"unsupported type definition shape during Ceibo -> CST lifting"
                          ~syntax_node:first
                          ~context:[ "type_definition" ]
            )
        )

let type_declaration_from_node = fun ?keyword_token node ->
  let lifted_keyword_token =
    match keyword_token with
    | Some keyword_token -> keyword_token
    | None ->
        match
          direct_non_trivia_tokens node |> List.find_opt
            (fun syntax_token ->
              let text = Ceibo.Red.SyntaxToken.text syntax_token in
              String.equal text "type" || String.equal text "and")
        with
        | Some syntax_token -> token syntax_token
        | None -> bail
          ~message:"expected type declaration keyword token during Ceibo -> CST lifting"
          ~syntax_node:node
          ~context:[ "type_declaration"; "keyword_token" ]
  in
  let lifted_type_params = direct_non_trivia_nodes node
  |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_PARAM)
  |> List.map type_parameter_from_node in
  let lifted_constraints = direct_non_trivia_nodes node
  |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_CONSTRAINT)
  |> List.filter_map type_constraint_from_node in
  let manifest_alias_opt = type_manifest_alias_from_node node in
  let definition = type_definition_from_node node in
  let nonrec_token_opt = direct_token_with_text node "nonrec" in
  let destructive_substitution_token_opt = direct_token_with_text node ":=" in
  let equals_tokens =
    direct_non_trivia_tokens node
    |> List.filter
      (fun syntax_token ->
        String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "=")
    |> List.map token
  in
  let manifest_equals_token_opt, definition_equals_token_opt =
    match manifest_alias_opt, definition, equals_tokens with
    | Some _, _, manifest_equals_token :: definition_equals_token :: _ -> (
      Some manifest_equals_token,
      Some definition_equals_token
    )
    | Some _, _, [ manifest_equals_token ] -> (Some manifest_equals_token, None)
    | None, Cst.TypeDefinition.Abstract, _ -> (None, None)
    | None, _, definition_equals_token :: _ -> (None, Some definition_equals_token)
    | None, _, [] -> (None, None)
    | Some _, _, [] -> (None, None)
  in
  match type_declaration_name_path node with
  | Some lifted_type_name -> (
      match Cst.Ident.last_segment lifted_type_name with
      | Some _ ->
          Some Cst.TypeDeclaration.{
            syntax_node = node;
            keyword_token = lifted_keyword_token;
            nonrec_token = nonrec_token_opt;
            type_name = lifted_type_name;
            type_params = lifted_type_params;
            type_definition = definition;
            manifest_equals_token = manifest_equals_token_opt;
            manifest_alias = manifest_alias_opt;
            definition_equals_token = definition_equals_token_opt;
            destructive_substitution_token = destructive_substitution_token_opt;
            private_flag = private_flag_from_type_declaration_node node;
            constraints = lifted_constraints;
            attributes = [];
            next_and_declaration = None;
          }
      | None -> None
    )
  | None -> None

let rec flatten_type_declaration_group = fun (decl: Cst.TypeDeclaration.t) ->
  let direct_type_decl_nodes = direct_non_trivia_nodes (Cst.TypeDeclaration.syntax_node decl)
  |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_DECL) in
  match grouped_type_declaration_from_nodes
    ~group_syntax_node:(Cst.TypeDeclaration.syntax_node decl)
    direct_type_decl_nodes with
  | Some grouped_decl -> type_declaration_group_members grouped_decl
  | None ->
      let next_and_declaration = Cst.TypeDeclaration.next_and_declaration decl in
      let decl = { decl with next_and_declaration = None } in
      decl
      :: (
        match next_and_declaration with
        | Some next -> flatten_type_declaration_group next
        | None -> []
      )

and grouped_type_declaration_from_nodes = fun ~group_syntax_node nodes ->
  let and_keyword_tokens =
    direct_non_trivia_tokens group_syntax_node
    |> List.filter
      (fun syntax_token ->
        String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "and")
    |> List.map token
  in
  let decls =
    nodes
    |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_DECL)
    |> List.mapi
      (fun index node ->
        let keyword_token =
          if index = 0 then
            direct_non_trivia_tokens node |> List.find_opt
              (fun syntax_token ->
                String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "type") |> Option.map token
          else
            List.nth_opt and_keyword_tokens (index - 1)
        in
        match keyword_token with
        | Some keyword_token -> type_declaration_from_node ~keyword_token node
        | None -> bail
          ~message:"expected type/and keyword for grouped type declaration during Ceibo -> CST lifting"
          ~syntax_node:node
          ~context:[ "type_declaration"; "keyword_token" ])
    |> List.filter_map (fun decl -> decl)
  in
  match decls with
  | [] -> None
  | first :: rest -> Some {
    first
    with syntax_node = group_syntax_node;
    next_and_declaration = type_declaration_chain_of_list rest
  }

let merge_type_declaration_groups = fun (first: Cst.TypeDeclaration.t) (next: Cst.TypeDeclaration.t) ->
  match flatten_type_declaration_group first @ flatten_type_declaration_group next with
  | [] -> first
  | first :: rest -> { first with next_and_declaration = type_declaration_chain_of_list rest }

let rec coalesce_structure_type_declaration_groups = function
  | Cst.StructureItem.TypeDeclaration first :: Cst.StructureItem.TypeDeclaration next :: rest when type_declaration_starts_with_and
    next
  && has_no_next_type_declaration next -> coalesce_structure_type_declaration_groups
    (Cst.StructureItem.TypeDeclaration (merge_type_declaration_groups first next) :: rest)
  | item :: rest -> item :: coalesce_structure_type_declaration_groups rest
  | [] -> []

let rec coalesce_signature_type_declaration_groups = function
  | Cst.SignatureItem.TypeDeclaration first :: Cst.SignatureItem.TypeDeclaration next :: rest when type_declaration_starts_with_and
    next
  && has_no_next_type_declaration next -> coalesce_signature_type_declaration_groups
    (Cst.SignatureItem.TypeDeclaration (merge_type_declaration_groups first next) :: rest)
  | item :: rest -> item :: coalesce_signature_type_declaration_groups rest
  | [] -> []

let type_extension_from_node = fun node ->
  let lifted_extension_operator_tokens =
    let rec find_plus_equals = function
      | plus_token :: equals_token :: _ when String.equal (Ceibo.Red.SyntaxToken.text plus_token) "+"
      && String.equal (Ceibo.Red.SyntaxToken.text equals_token) "=" -> Some [
        token plus_token;
        token equals_token
      ]
      | _ :: rest -> find_plus_equals rest
      | [] -> None
    in
    match find_plus_equals (direct_non_trivia_tokens node) with
    | Some extension_operator_tokens -> extension_operator_tokens
    | None -> bail
      ~message:"expected type extension operator tokens during Ceibo -> CST lifting"
      ~syntax_node:node
      ~context:[ "type_extension"; "extension_operator_tokens" ]
  in
  let extension_type_params = direct_non_trivia_nodes node
  |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_PARAM)
  |> List.map type_parameter_from_node in
  let extension_constructors = direct_non_trivia_nodes node
  |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_VARIANT_CONSTR)
  |> List.filter_map variant_constructor_from_node in
  match type_declaration_name_path node with
  | Some extension_type_name when List.length extension_constructors > 0 -> (
      match Cst.Ident.last_segment extension_type_name with
      | Some _ ->
          Some Cst.TypeExtension.{
            syntax_node = node;
            type_name = extension_type_name;
            type_params = extension_type_params;
            extension_operator_tokens = lifted_extension_operator_tokens;
            constructors = extension_constructors;
          }
      | None -> None
    )
  | _ -> None

let let_binding_from_node_with_keyword = fun ~keyword_token ~is_recursive_binding node ->
  let binding_keyword_token = keyword_token in
  let binding_rec_token = direct_token_with_text node "rec" in
  let binding_equals_token =
    match direct_token_with_text node "=" with
    | Some equals_token -> equals_token
    | None -> bail
      ~message:"expected let-binding equals during Ceibo -> CST lifting"
      ~syntax_node:node
      ~context:[ "let_binding" ]
  in
  let direct_children = direct_non_trivia_nodes node in
  let binding_attributes = direct_children |> List.filter is_attribute_node |> List.map attribute_from_node in
  let binding_children = direct_children |> List.filter (fun child -> not (is_attribute_node child)) in
  match binding_children with
  | binding_pattern_node :: rest -> (
      match List.rev rest with
      | value_node :: rev_param_nodes ->
          let prefix_nodes = List.rev rev_param_nodes in
          Some Cst.LetBinding.{
            syntax_node = node;
            keyword_token = binding_keyword_token;
            rec_token = binding_rec_token;
            equals_token = binding_equals_token;
            attributes = binding_attributes;
            binding_pattern = pattern_from_node binding_pattern_node;
            parameters = binding_parameters_from_prefix prefix_nodes;
            value = binding_value_from_prefix
              ~binding_syntax_node:node
              ~prefix_nodes:prefix_nodes
              ~value_node:value_node;
            and_binding = None;
          }
      | [] -> None
    )
  | [] -> None

let let_binding_from_node = fun ~is_recursive_binding node ->
  let binding_keyword_token =
    match direct_token_with_text node "let" with
    | Some keyword_token -> keyword_token
    | None -> (
        match direct_token_with_text node "and" with
        | Some keyword_token -> keyword_token
        | None -> bail
          ~message:"expected let-binding keyword during Ceibo -> CST lifting"
          ~syntax_node:node
          ~context:[ "let_binding" ]
      )
  in
  let_binding_from_node_with_keyword ~keyword_token:binding_keyword_token ~is_recursive_binding node

let let_expression_binding_from_node = fun ~is_recursive_binding node ->
  if (not is_recursive_binding) && is_binding_operator_expression_node node then
    match let_operator_expression_from_node node with
    | Some expr -> Some (let_binding_from_binding_operator_binding ~binding_syntax_node:node expr.binding)
    | None -> None
  else
    match let_expression_parts ~is_recursive_binding node with
    | Some (`Value (is_recursive_binding, binding_pattern_node, prefix_nodes, bound_value_node, _and_binding_nodes, _body_node)) ->
        let binding_equals_token =
          match direct_token_with_text node "=" with
          | Some equals_token -> equals_token
          | None -> bail
            ~message:"expected let-expression binding equals during Ceibo -> CST lifting"
            ~syntax_node:node
            ~context:[ "let_expression.binding" ]
        in
        Some Cst.LetBinding.{
          syntax_node = node;
          keyword_token =
            (
              match direct_token_with_text node "let" with
              | Some keyword_token -> keyword_token
              | None -> bail
                ~message:"expected let-expression binding keyword during Ceibo -> CST lifting"
                ~syntax_node:node
                ~context:[ "let_expression.binding" ]
            );
          rec_token = direct_token_with_text node "rec";
          equals_token = binding_equals_token;
          attributes = [];
          binding_pattern = pattern_from_node binding_pattern_node;
          parameters = binding_parameters_from_prefix prefix_nodes;
          value = binding_value_from_prefix
            ~binding_syntax_node:node
            ~prefix_nodes:prefix_nodes
            ~value_node:bound_value_node;
          and_binding = None;
        }
    | _ -> None

let module_declaration_parts_from_node = fun node ->
  let direct_tokens = direct_non_trivia_tokens node in
  let keyword_token =
    direct_tokens
    |> List.find_opt
      (fun syntax_token ->
        let text = Ceibo.Red.SyntaxToken.text syntax_token in
        String.equal text "module" || String.equal text "and")
    |> Option.map token
  in
  let rec_token = direct_token_with_text node "rec" in
  match find_declaration_name_token ~skip_keywords:[ "module"; "rec"; "and" ] direct_tokens with
  | Some module_name -> (
      let direct_children = direct_non_trivia_nodes node in
      let has_equals =
        direct_non_trivia_tokens node
        |> List.exists
          (fun syntax_token ->
            String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "=")
      in
      let equals_token = direct_token_with_text node "=" in
      let colon_token = direct_token_with_text node ":" in
      let lifted_module_expression =
        if has_equals then
          direct_children
          |> List.rev
          |> List.find_opt can_lift_module_expression_node
          |> Option.map module_expression_from_node
        else
          None
      in
      let module_type_search_children =
        if has_equals then
          match direct_children |> List.rev |> List.find_opt can_lift_module_expression_node with
          | Some module_expression_node -> direct_children
          |> List.filter (fun child -> child != module_expression_node)
          | None -> direct_children
        else
          direct_children
      in
      let lifted_module_type = module_type_search_children
      |> List.find_opt can_lift_module_type_node
      |> Option.map module_type_from_node in
      let lifted_module_expression =
        match lifted_module_expression, lifted_module_type with
        | Some module_expression, Some module_type when has_equals -> Some (constrain_module_expression
          ~syntax_node:node
          ~module_expression
          module_type)
        | _ -> lifted_module_expression
      in
      let module_name = token module_name in
      let functor_parameters = direct_children
      |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.FUNCTOR_PARAM)
      |> List.map functor_parameter_from_node in
      let owned_trivia = owned_trivia_from_node node in
      Some (
        keyword_token,
        rec_token,
        module_name,
        functor_parameters,
        colon_token,
        equals_token,
        lifted_module_type,
        lifted_module_expression,
        owned_trivia
      )
    )
  | None -> None

let rec module_signature_group_from_nodes = fun ~group_syntax_node ~is_recursive_group module_decl_nodes ->
  let and_keyword_tokens =
    direct_non_trivia_tokens group_syntax_node
    |> List.filter
      (fun syntax_token ->
        String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "and")
    |> List.map token
  in
  let declarations =
    module_decl_nodes
    |> List.mapi
      (fun index module_decl_node ->
        let keyword_token =
          if index = 0 then
            direct_non_trivia_tokens module_decl_node |> List.find_opt
              (fun syntax_token ->
                String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "module") |> Option.map token
          else
            List.nth_opt and_keyword_tokens (index - 1)
        in
        match module_declaration_parts_from_node module_decl_node with
        | Some (_parts_keyword_token, rec_token, module_name, functor_parameters, colon_token, equals_token, Some module_type, None, _owned_trivia) -> (
            match keyword_token with
            | Some keyword_token ->
                {
                  Cst.ModuleSignature.syntax_node = module_decl_node;
                  keyword_token;
                  rec_token;
                  module_name;
                  functor_parameters;
                  colon_token;
                  equals_token;
                  definition = Cst.ModuleSignature.Signature module_type;
                  next_and_declaration = None;
                }
            | None -> bail
              ~message:"expected module/and keyword for signature module declaration during Ceibo -> CST lifting"
              ~syntax_node:module_decl_node
              ~context:[ "item"; "module_signature" ]
          )
        | Some (_parts_keyword_token, rec_token, module_name, functor_parameters, colon_token, equals_token, None, Some module_expression, _owned_trivia) -> (
            match keyword_token with
            | Some keyword_token ->
                {
                  Cst.ModuleSignature.syntax_node = module_decl_node;
                  keyword_token;
                  rec_token;
                  module_name;
                  functor_parameters;
                  colon_token;
                  equals_token;
                  definition = Cst.ModuleSignature.Alias module_expression;
                  next_and_declaration = None;
                }
            | None -> bail
              ~message:"expected module/and keyword for signature module declaration during Ceibo -> CST lifting"
              ~syntax_node:module_decl_node
              ~context:[ "item"; "module_signature" ]
          )
        | Some _ ->
            bail
              ~message:"expected signature module declaration during Ceibo -> CST lifting"
              ~syntax_node:module_decl_node
              ~context:[ "item"; "module_signature" ]
        | None ->
            bail
              ~message:"expected signature module declaration during Ceibo -> CST lifting"
              ~syntax_node:module_decl_node
              ~context:[ "item"; "module_signature" ])
  in
  match declarations with
  | [] -> None
  | first :: rest -> Some {
    first
    with syntax_node = group_syntax_node;
    next_and_declaration = module_signature_chain_of_list rest
  }

let rec module_structure_group_from_nodes = fun ~group_syntax_node ~is_recursive_group module_decl_nodes ->
  let and_keyword_tokens =
    direct_non_trivia_tokens group_syntax_node
    |> List.filter
      (fun syntax_token ->
        String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "and")
    |> List.map token
  in
  let declarations =
    module_decl_nodes
    |> List.mapi
      (fun index module_decl_node ->
        let keyword_token =
          if index = 0 then
            direct_non_trivia_tokens module_decl_node |> List.find_opt
              (fun syntax_token ->
                String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "module") |> Option.map token
          else
            List.nth_opt and_keyword_tokens (index - 1)
        in
        match module_declaration_parts_from_node module_decl_node with
        | Some (_parts_keyword_token, rec_token, module_name, functor_parameters, colon_token, equals_token, module_type, Some module_expression, _owned_trivia) -> (
            match keyword_token with
            | Some keyword_token ->
                {
                  Cst.ModuleStructure.syntax_node = module_decl_node;
                  keyword_token;
                  rec_token;
                  module_name;
                  functor_parameters;
                  colon_token;
                  equals_token =
                    (
                      match equals_token with
                      | Some equals_token -> equals_token
                      | None -> bail
                        ~message:"expected module structure equals token during Ceibo -> CST lifting"
                        ~syntax_node:module_decl_node
                        ~context:[ "item"; "module_structure"; "equals_token" ]
                    );
                  module_type;
                  module_expression;
                  next_and_declaration = None;
                }
            | None -> bail
              ~message:"expected module/and keyword for structure module declaration during Ceibo -> CST lifting"
              ~syntax_node:module_decl_node
              ~context:[ "item"; "module_structure" ]
          )
        | Some _ ->
            bail
              ~message:"expected structure module declaration during Ceibo -> CST lifting"
              ~syntax_node:module_decl_node
              ~context:[ "item"; "module_structure" ]
        | None ->
            bail
              ~message:"expected structure module declaration during Ceibo -> CST lifting"
              ~syntax_node:module_decl_node
              ~context:[ "item"; "module_structure" ])
  in
  match declarations with
  | [] -> None
  | first :: rest -> Some {
    first
    with syntax_node = group_syntax_node;
    next_and_declaration = module_structure_chain_of_list rest
  }

let module_type_declaration_from_node = fun node ->
  let module_keyword_token_opt = direct_token_with_text node "module" in
  let type_keyword_token_opt = direct_token_with_text node "type" in
  match find_declaration_name_token
    ~skip_keywords:[ "module"; "type" ]
    (direct_non_trivia_tokens node) with
  | Some module_type_name -> (
      match module_keyword_token_opt, type_keyword_token_opt with
      | Some lifted_module_keyword_token, Some lifted_type_keyword_token ->
          Some Cst.ModuleTypeDeclaration.{
            syntax_node = node;
            module_keyword_token = lifted_module_keyword_token;
            type_keyword_token = lifted_type_keyword_token;
            module_type_name = token module_type_name;
            equals_token = direct_token_with_text node "=";
            module_type = (direct_non_trivia_nodes node
            |> List.rev
            |> List.find_opt can_lift_module_type_node
            |> Option.map module_type_from_node);
          }
      | _ -> None
    )
  | None -> None

let class_declaration_from_node = fun node ->
  let has_equals_body =
    direct_non_trivia_tokens node
    |> List.exists
      (fun syntax_token ->
        String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "=")
  in
  let has_colon_body =
    direct_non_trivia_tokens node
    |> List.exists
      (fun syntax_token ->
        String.equal (Ceibo.Red.SyntaxToken.text syntax_token) ":")
  in
  let class_type_and_body_from_child child =
    match Ceibo.Red.SyntaxNode.kind child with
    | Syntax_kind.INFIX_EXPR -> (
        match direct_non_trivia_nodes child with
        | class_type_node :: class_body_node :: _ when can_lift_class_type_node class_type_node
        && can_lift_class_expression_node class_body_node -> (
          Some (class_type_from_node class_type_node),
          Some (class_expression_from_node class_body_node)
        )
        | _ -> (None, None)
      )
    | _ -> (
        (
          if has_colon_body && can_lift_class_type_node child then
            Some (class_type_from_node child)
          else
            None
        ),
        if has_equals_body && can_lift_class_expression_node child then
          Some (class_expression_from_node child)
        else
          None
      )
  in
  let children = direct_non_trivia_nodes node
  |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child != Syntax_kind.TYPE_PARAM) in
  let rec split_at_name = fun acc ->
    function
    | child :: rest when Ceibo.Red.SyntaxNode.kind child = Syntax_kind.IDENT_EXPR -> Some (
      child,
      List.rev acc,
      rest
    )
    | child :: rest -> split_at_name (child :: acc) rest
    | [] -> None
  in
  match split_at_name [] children with
  | Some (name_node, prefix, remainder) -> (
      match first_ident_token_in_subtree name_node, List.rev remainder with
      | Some class_name, class_body_node :: rev_prefix ->
          let class_declaration_extension, class_declaration_attributes =
            let declaration_extension, declaration_attributes = declaration_modifiers_from_nodes prefix in
            let boundary_start =
              match prefix with
              | first_prefix_node :: _ -> (Ceibo.Red.SyntaxNode.span first_prefix_node).start
              | [] -> (Cst.Token.span class_name).start
            in
            let declaration_extension =
              match declaration_extension with
              | Some _ -> declaration_extension
              | None -> declaration_extension_from_gap_tokens ~node ~keywords:[ "class" ] ~boundary_start
            in
            (declaration_extension, declaration_attributes)
          in
          let suffix_class_type, suffix_class_body = class_type_and_body_from_child class_body_node in
          let prefix_class_type =
            if Option.is_some suffix_class_type then
              None
            else
              match List.rev rev_prefix with
              | class_type_node :: _ when can_lift_class_type_node class_type_node -> Some (class_type_from_node
                class_type_node)
              | _ -> None
          in
          let declaration_class_type =
            match suffix_class_type with
            | Some _ -> suffix_class_type
            | None -> prefix_class_type
          in
          (
            match suffix_class_body, declaration_class_type with
            | Some declaration_class_body, declaration_class_type ->
                Some (`Definition Cst.ClassDefinition.{
                  syntax_node = node;
                  keyword_token =
                    (
                      match direct_token_with_text node "class" with
                      | Some lifted_keyword_token -> lifted_keyword_token
                      | None -> bail
                        ~message:"expected class definition keyword token during Ceibo -> CST lifting"
                        ~syntax_node:node
                        ~context:[ "item"; "class_definition"; "keyword_token" ]
                    );
                  type_params = type_parameters_from_node node;
                  declaration_extension = class_declaration_extension;
                  declaration_attributes = class_declaration_attributes;
                  class_name = class_name;
                  colon_token = direct_token_with_text node ":";
                  class_type = declaration_class_type;
                  equals_token =
                    (
                      match direct_token_with_text node "=" with
                      | Some equals_token -> equals_token
                      | None -> (
                          match token_with_text class_body_node "=" with
                          | Some equals_token -> equals_token
                          | None -> bail
                            ~message:"expected class definition equals token during Ceibo -> CST lifting"
                            ~syntax_node:node
                            ~context:[ "item"; "class_definition"; "equals_token" ]
                        )
                    );
                  class_body = declaration_class_body;
                })
            | None, Some declaration_class_type ->
                Some (`Declaration Cst.ClassDeclaration.{
                  syntax_node = node;
                  keyword_token =
                    (
                      match direct_token_with_text node "class" with
                      | Some lifted_keyword_token -> lifted_keyword_token
                      | None -> bail
                        ~message:"expected class declaration keyword token during Ceibo -> CST lifting"
                        ~syntax_node:node
                        ~context:[ "item"; "class_declaration"; "keyword_token" ]
                    );
                  type_params = type_parameters_from_node node;
                  declaration_extension = class_declaration_extension;
                  declaration_attributes = class_declaration_attributes;
                  class_name = class_name;
                  colon_token =
                    (
                      match direct_token_with_text node ":" with
                      | Some equals_token -> equals_token
                      | None -> bail
                        ~message:"expected class declaration colon token during Ceibo -> CST lifting"
                        ~syntax_node:node
                        ~context:[ "item"; "class_declaration"; "colon_token" ]
                    );
                  class_type = declaration_class_type;
                })
            | None, None -> None
          )
      | _ -> None
    )
  | None -> None

let class_type_declaration_from_node = fun node ->
  let children = direct_non_trivia_nodes node
  |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child != Syntax_kind.TYPE_PARAM) in
  let rec split_at_name = fun acc ->
    function
    | child :: rest when Ceibo.Red.SyntaxNode.kind child = Syntax_kind.IDENT_EXPR -> Some (
      child,
      List.rev acc,
      rest
    )
    | child :: rest -> split_at_name (child :: acc) rest
    | [] -> None
  in
  match split_at_name [] children with
  | Some (name_node, prefix, body_node :: _) -> (
      match first_ident_token_in_subtree name_node with
      | Some class_type_name ->
          if can_lift_class_type_node body_node then
            let declaration_extension, declaration_attributes =
              let declaration_extension, declaration_attributes = declaration_modifiers_from_nodes prefix in
              let boundary_start =
                match prefix with
                | first_prefix_node :: _ -> (Ceibo.Red.SyntaxNode.span first_prefix_node).start
                | [] -> (Cst.Token.span class_type_name).start
              in
              let declaration_extension =
                match declaration_extension with
                | Some _ -> declaration_extension
                | None -> declaration_extension_from_gap_tokens
                  ~node
                  ~keywords:[ "class"; "type" ]
                  ~boundary_start
              in
              (declaration_extension, declaration_attributes)
            in
            Some {
              Cst.syntax_node = node;
              class_keyword_token =
                (
                  match direct_token_with_text node "class" with
                  | Some lifted_class_keyword_token -> lifted_class_keyword_token
                  | None -> bail
                    ~message:"expected class type declaration class keyword token during Ceibo -> CST lifting"
                    ~syntax_node:node
                    ~context:[ "class_type_declaration"; "class_keyword_token" ]
                );
              type_keyword_token =
                (
                  match direct_token_with_text node "type" with
                  | Some lifted_type_keyword_token -> lifted_type_keyword_token
                  | None -> bail
                    ~message:"expected class type declaration type keyword token during Ceibo -> CST lifting"
                    ~syntax_node:node
                    ~context:[ "class_type_declaration"; "type_keyword_token" ]
                );
              type_params = type_parameters_from_node node;
              declaration_extension;
              declaration_attributes;
              class_type_name;
              equals_token =
                (
                  match direct_token_with_text node "=" with
                  | Some equals_token -> equals_token
                  | None -> bail
                    ~message:"expected class type declaration equals token during Ceibo -> CST lifting"
                    ~syntax_node:node
                    ~context:[ "class_type_declaration"; "equals_token" ]
                );
              class_type_body = class_type_from_node body_node;
            }
          else
            None
      | None -> None
    )
  | _ -> None

let open_statement_from_node = fun node ->
  let tokens = direct_non_trivia_tokens node in
  let open_keyword_token_opt = direct_token_with_text node "open" in
  let bang_token_opt =
    tokens
    |> List.find_opt
      (fun syntax_token ->
        String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "!")
    |> Option.map token
  in
  let lifted_target = direct_non_trivia_nodes node
  |> List.find_opt can_lift_module_expression_node
  |> Option.map
    (fun target_node -> Cst.OpenStatement.ModuleExpression (module_expression_from_node target_node)) in
  match open_keyword_token_opt, lifted_target with
  | Some open_keyword_token, Some lifted_target ->
      Some Cst.OpenStatement.{
        syntax_node = node;
        keyword_token = open_keyword_token;
        target = lifted_target;
        bang_token = bang_token_opt
      }
  | Some open_keyword_token, None ->
      let module_tokens =
        tokens
        |> List.filter
          (fun syntax_token ->
            let text = Ceibo.Red.SyntaxToken.text syntax_token in
            not (String.equal text "open" || String.equal text "!"))
      in
      (
        match module_tokens with
        | [] -> None
        | _ -> Some Cst.OpenStatement.{
          syntax_node = node;
          keyword_token = open_keyword_token;
          target = Cst.OpenStatement.Path (module_path_from_tokens ~syntax_node:node module_tokens);
          bang_token = bang_token_opt
        }
      )
  | None, _ ->
      None

let value_declaration_from_node = fun node ->
  let direct_children = direct_non_trivia_nodes node in
  let lifted_keyword_token = direct_token_with_text node "val" in
  let lifted_name_tokens = declaration_name_tokens_from_node ~skip_keywords:[ "val" ] node in
  let lifted_colon_token = direct_token_with_text node ":" in
  let lifted_type_node = List.rev direct_children |> List.find_opt can_lift_core_type_node in
  match lifted_keyword_token, lifted_name_tokens, lifted_colon_token, lifted_type_node with
  | Some lifted_keyword_token, Some lifted_name_tokens, Some lifted_colon_token, Some lifted_type_node ->
      Some (
        ({
            syntax_node = node;
            keyword_token = lifted_keyword_token;
            name_tokens = lifted_name_tokens;
            colon_token = lifted_colon_token;
            type_ = core_type_from_node lifted_type_node;
            trailing_comment = None;
          }: Cst.value_declaration)
      )
  | (None, _, _, _)
  | (_, None, _, _) -> None
  | (_, _, None, _)
  | (_, _, _, None) -> None

let external_declaration_from_node = fun node ->
  let direct_children = direct_non_trivia_nodes node in
  let lifted_keyword_token = direct_token_with_text node "external" in
  let lifted_primitive_name_tokens = direct_non_trivia_tokens node
  |> List.filter
    (fun syntax_token -> Ceibo.Red.SyntaxToken.kind syntax_token = Syntax_kind.STRING_LITERAL)
  |> List.map token in
  let external_name_tokens = declaration_name_tokens_from_node ~skip_keywords:[ "external" ] node in
  let lifted_colon_token = direct_token_with_text node ":" in
  let lifted_equals_token = direct_token_with_text node "=" in
  match lifted_keyword_token, external_name_tokens, lifted_colon_token, lifted_equals_token with
  | Some lifted_keyword_token, Some lifted_name_tokens, Some lifted_colon_token, Some lifted_equals_token -> (
      match direct_children |> List.find_opt can_lift_core_type_node with
      | Some lifted_type_node ->
          Some (
            ({
                syntax_node = node;
                keyword_token = lifted_keyword_token;
                name_tokens = lifted_name_tokens;
                colon_token = lifted_colon_token;
                type_ = core_type_from_node lifted_type_node;
                equals_token = lifted_equals_token;
                primitive_name_tokens = lifted_primitive_name_tokens;
                attributes = attributes_from_node node;
              }: Cst.external_declaration)
          )
      | None -> None
    )
  | _ -> None

let include_statement_from_node = fun node ->
  let include_keyword_token_opt = direct_token_with_text node "include" in
  match direct_non_trivia_nodes node
  |> List.find_opt
    (fun child -> can_lift_module_expression_node child || can_lift_module_type_node child) with
  | Some included_node -> (
      match include_keyword_token_opt with
      | Some include_keyword_token ->
          Some (
            ({
                syntax_node = node;
                keyword_token = include_keyword_token;
                target =
                  if can_lift_module_expression_node included_node then
                    Cst.ModuleExpression (module_expression_from_node included_node)
                  else if can_lift_module_type_node included_node then
                    Cst.ModuleType (module_type_from_node included_node)
                  else
                    bail
                      ~message:"expected include target during Ceibo -> CST lifting"
                      ~syntax_node:included_node
                      ~context:[ "include_statement" ];
              }: Cst.include_statement)
          )
      | None -> None
    )
  | None -> None

let exception_declaration_from_node = fun node ->
  let keyword_token_opt = direct_token_with_text node "exception" in
  let rhs =
    match
      direct_non_trivia_nodes node |> List.find_opt
        (fun child ->
          can_lift_core_type_node child || match Ceibo.Red.SyntaxNode.kind child with
          | Syntax_kind.MODULE_PATH
          | Syntax_kind.MODULE_TYPE_PATH
          | Syntax_kind.IDENT_EXPR
          | Syntax_kind.FIELD_ACCESS_EXPR -> true
          | _ -> false)
    with
    | Some child when can_lift_core_type_node child -> (
        match direct_token_with_text node "of" with
        | Some of_token -> Some Cst.(Payload { of_token; payload_type = core_type_from_node child })
        | None -> None
      )
    | Some child -> (
        match direct_token_with_text node "=" with
        | Some equals_token -> Some Cst.(Alias {
          equals_token;
          alias = module_path_like_from_node child
        })
        | None -> None
      )
    | _ ->
        None
  in
  match keyword_token_opt, find_declaration_name_token
    ~skip_keywords:[ "exception" ]
    (direct_non_trivia_tokens node) with
  | Some keyword_token, Some name_syntax_token -> Some ((
    { syntax_node = node; keyword_token; name_token = token name_syntax_token; rhs }: Cst.exception_declaration
  ))
  | _ -> None

let rec structure_items_from_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.TYPE_DECL -> (
      if is_type_extension_node node then
        match type_extension_from_node node with
        | Some decl -> [ Cst.StructureItem.TypeExtension decl ]
        | None -> unsupported_item node
      else
        match type_declaration_from_node node with
        | Some decl -> [ Cst.StructureItem.TypeDeclaration decl ]
        | None -> unsupported_item node
    )
  | Syntax_kind.TYPE_MUTUAL_DECL ->
      let child_nodes = direct_non_trivia_nodes node in
      if
        child_nodes != []
        && List.for_all (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.MODULE_DECL) child_nodes
      then
        match module_structure_group_from_nodes ~group_syntax_node:node ~is_recursive_group:true child_nodes with
        | Some decl -> [ Cst.StructureItem.ModuleDeclaration decl ]
        | None -> unsupported_item node
      else if
        child_nodes != []
        && List.for_all (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_DECL) child_nodes
      then
        match grouped_type_declaration_from_nodes ~group_syntax_node:node child_nodes with
        | Some decl -> [ Cst.StructureItem.TypeDeclaration decl ]
        | None -> unsupported_item node
      else
        child_nodes |> List.concat_map structure_items_from_node
  | Syntax_kind.LET_BINDING -> (
      match let_binding_from_node ~is_recursive_binding:false node with
      | Some binding -> [ Cst.StructureItem.LetBinding binding ]
      | None -> unsupported_item node
    )
  | Syntax_kind.LET_REC_BINDING -> (
      match let_binding_from_node ~is_recursive_binding:true node with
      | Some binding -> [ Cst.StructureItem.LetBinding binding ]
      | None -> unsupported_item node
    )
  | Syntax_kind.LET_MUTUAL_DECL ->
      let binding_nodes =
        direct_non_trivia_nodes node
        |> List.filter
          (fun child ->
            let kind = Ceibo.Red.SyntaxNode.kind child in
            kind = Syntax_kind.LET_BINDING || kind = Syntax_kind.LET_REC_BINDING)
      in
      (
        match binding_nodes with
        | first_node :: rest_nodes ->
            let first_binding_tokens = direct_non_trivia_tokens first_node in
            let is_recursive_group =
              first_binding_tokens
              |> List.exists
                (fun token ->
                  String.equal (Ceibo.Red.SyntaxToken.text token) "rec")
            in
            let let_keyword_token =
              match
                List.find_opt
                  (fun token ->
                    String.equal (Ceibo.Red.SyntaxToken.text token) "let")
                  first_binding_tokens
              with
              | Some token -> token
              | None -> bail
                ~message:"expected let keyword for let-mutual declaration during Ceibo -> CST lifting"
                ~syntax_node:first_node
                ~context:[ "item"; "let_mutual_declaration" ]
            in
            let group_tokens = direct_non_trivia_tokens node in
            let and_keyword_tokens =
              group_tokens
              |> List.filter
                (fun token ->
                  String.equal (Ceibo.Red.SyntaxToken.text token) "and")
            in
            (
              match let_binding_from_node_with_keyword
                ~keyword_token:(token let_keyword_token)
                ~is_recursive_binding:is_recursive_group
                first_node with
              | Some first_binding ->
                  let and_bindings =
                    rest_nodes
                    |> List.mapi
                      (fun index binding_node ->
                        match List.nth_opt and_keyword_tokens index with
                        | Some and_keyword_token -> let_binding_from_node_with_keyword
                          ~keyword_token:(token and_keyword_token)
                          ~is_recursive_binding:is_recursive_group
                          binding_node
                        | None -> bail
                          ~message:"expected matching and keyword for let-mutual declaration during Ceibo -> CST lifting"
                          ~syntax_node:binding_node
                          ~context:[ "item"; "let_mutual_declaration"; "and_bindings" ])
                    |> List.filter_map (fun value -> value)
                  in
                  let grouped_binding = {
                    first_binding
                    with syntax_node = node;
                    and_binding = let_binding_chain_of_list and_bindings
                  } in
                  [ Cst.StructureItem.LetBinding grouped_binding ]
              | None -> unsupported_item node
            )
        | [] -> unsupported_item node
      )
  | Syntax_kind.CLASS_DECL -> (
      match class_declaration_from_node node with
      | Some (`Definition decl) -> [ Cst.StructureItem.ClassDeclaration decl ]
      | Some (`Declaration _) -> unsupported_item node
      | None -> unsupported_item node
    )
  | Syntax_kind.CLASS_TYPE_DECL -> (
      match class_type_declaration_from_node node with
      | Some decl -> [ Cst.StructureItem.ClassTypeDeclaration decl ]
      | None -> unsupported_item node
    )
  | Syntax_kind.MODULE_DECL -> (
      match module_structure_group_from_nodes
        ~group_syntax_node:node
        ~is_recursive_group:false
        [ node ] with
      | Some decl -> [ Cst.StructureItem.ModuleDeclaration decl ]
      | None -> unsupported_item node
    )
  | Syntax_kind.MODULE_TYPE_DECL -> (
      match module_type_declaration_from_node node with
      | Some decl -> [ Cst.StructureItem.ModuleTypeDeclaration decl ]
      | None -> unsupported_item node
    )
  | Syntax_kind.OPEN_STMT -> (
      match open_statement_from_node node with
      | Some stmt -> [ Cst.StructureItem.OpenStatement stmt ]
      | None -> unsupported_item node
    )
  | Syntax_kind.EXTERNAL_DECL -> (
      match external_declaration_from_node node with
      | Some decl -> [ Cst.StructureItem.ExternalDeclaration decl ]
      | None -> unsupported_item node
    )
  | Syntax_kind.INCLUDE_STMT -> (
      match include_statement_from_node node with
      | Some stmt -> [ Cst.StructureItem.IncludeStatement stmt ]
      | None -> unsupported_item node
    )
  | Syntax_kind.EXCEPTION_DECL -> (
      match exception_declaration_from_node node with
      | Some decl -> [ Cst.StructureItem.ExceptionDeclaration decl ]
      | None -> unsupported_item node
    )
  | Syntax_kind.ATTRIBUTE_EXPR ->
      [ Cst.StructureItem.Attribute (attribute_from_node node) ]
  | Syntax_kind.APPLY_EXPR when List.for_all is_attribute_node (direct_non_trivia_nodes node) ->
      direct_non_trivia_nodes node
      |> List.map (fun child -> Cst.StructureItem.Attribute (attribute_from_node child))
  | Syntax_kind.EXTENSION_EXPR ->
      [ Cst.StructureItem.Extension (extension_from_node node) ]
  | Syntax_kind.SEQUENCE_EXPR ->
      [ Cst.StructureItem.Expression (expression_from_node node) ]
  | _ ->
      [ Cst.StructureItem.Expression (expression_from_node node) ]

let rec signature_items_from_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.TYPE_DECL -> (
      if is_type_extension_node node then
        match type_extension_from_node node with
        | Some decl -> [ Cst.SignatureItem.TypeExtension decl ]
        | None -> unsupported_item node
      else
        match type_declaration_from_node node with
        | Some decl -> [ Cst.SignatureItem.TypeDeclaration decl ]
        | None -> unsupported_item node
    )
  | Syntax_kind.TYPE_MUTUAL_DECL ->
      let child_nodes = direct_non_trivia_nodes node in
      if
        child_nodes != []
        && List.for_all (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.MODULE_DECL) child_nodes
      then
        match module_signature_group_from_nodes ~group_syntax_node:node ~is_recursive_group:true child_nodes with
        | Some decl -> [ Cst.SignatureItem.ModuleDeclaration decl ]
        | None -> unsupported_item node
      else if
        child_nodes != []
        && List.for_all (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_DECL) child_nodes
      then
        match grouped_type_declaration_from_nodes ~group_syntax_node:node child_nodes with
        | Some decl -> [ Cst.SignatureItem.TypeDeclaration decl ]
        | None -> unsupported_item node
      else
        child_nodes |> List.concat_map signature_items_from_node
  | Syntax_kind.CLASS_DECL -> (
      match class_declaration_from_node node with
      | Some (`Declaration decl) -> [ Cst.SignatureItem.ClassDeclaration decl ]
      | Some (`Definition _) -> unsupported_item node
      | None -> unsupported_item node
    )
  | Syntax_kind.CLASS_TYPE_DECL -> (
      match class_type_declaration_from_node node with
      | Some decl -> [ Cst.SignatureItem.ClassTypeDeclaration decl ]
      | None -> unsupported_item node
    )
  | Syntax_kind.MODULE_DECL -> (
      match module_signature_group_from_nodes
        ~group_syntax_node:node
        ~is_recursive_group:false
        [ node ] with
      | Some decl -> [ Cst.SignatureItem.ModuleDeclaration decl ]
      | None -> unsupported_item node
    )
  | Syntax_kind.MODULE_TYPE_DECL -> (
      match module_type_declaration_from_node node with
      | Some decl -> [ Cst.SignatureItem.ModuleTypeDeclaration decl ]
      | None -> unsupported_item node
    )
  | Syntax_kind.OPEN_STMT -> (
      match open_statement_from_node node with
      | Some stmt -> [ Cst.SignatureItem.OpenStatement stmt ]
      | None -> unsupported_item node
    )
  | Syntax_kind.VAL_DECL -> (
      match value_declaration_from_node node with
      | Some decl -> [ Cst.SignatureItem.ValueDeclaration decl ]
      | None -> unsupported_item node
    )
  | Syntax_kind.EXTERNAL_DECL -> (
      match external_declaration_from_node node with
      | Some decl -> [ Cst.SignatureItem.ExternalDeclaration decl ]
      | None -> unsupported_item node
    )
  | Syntax_kind.INCLUDE_STMT -> (
      match include_statement_from_node node with
      | Some stmt -> [ Cst.SignatureItem.IncludeStatement stmt ]
      | None -> unsupported_item node
    )
  | Syntax_kind.EXCEPTION_DECL -> (
      match exception_declaration_from_node node with
      | Some decl -> [ Cst.SignatureItem.ExceptionDeclaration decl ]
      | None -> unsupported_item node
    )
  | Syntax_kind.ATTRIBUTE_EXPR ->
      [ Cst.SignatureItem.Attribute (attribute_from_node node) ]
  | Syntax_kind.APPLY_EXPR when List.for_all is_attribute_node (direct_non_trivia_nodes node) ->
      direct_non_trivia_nodes node
      |> List.map (fun child -> Cst.SignatureItem.Attribute (attribute_from_node child))
  | Syntax_kind.EXTENSION_EXPR ->
      [ Cst.SignatureItem.Extension (extension_from_node node) ]
  | _ ->
      unsupported_item node

let raw_annotation_payload_from_shell = fun shell_node ->
  let all_tokens = direct_tokens shell_node in
  let shell_tokens = direct_non_trivia_tokens shell_node in
  let shell_close_offset =
    match List.rev all_tokens with
    | close_token :: _ -> (Ceibo.Red.SyntaxToken.span close_token).start
    | [] -> Ceibo.Red.SyntaxNode.span shell_node |> fun span -> span.end_
  in
  let payload_non_trivia_tokens =
    let rec skip_qualified_name_tail = function
      | dot_token :: _name_token :: rest when String.equal (Ceibo.Red.SyntaxToken.text dot_token) dot_text -> skip_qualified_name_tail
        rest
      | rest -> rest
    in
    let rec skip_sigils = function
      | syntax_token :: rest when is_annotation_sigil_text (Ceibo.Red.SyntaxToken.text syntax_token) -> skip_sigils
        rest
      | rest -> rest
    in
    let skip_name = function
      | _name_token :: rest -> skip_qualified_name_tail rest
      | [] -> []
    in
    match shell_tokens with
    | open_token :: rest when String.equal (Ceibo.Red.SyntaxToken.text open_token) open_bracket_text -> (
        let rest = rest |> skip_sigils |> skip_name in
        match List.rev rest with
        | close_token :: payload_rev when String.equal (Ceibo.Red.SyntaxToken.text close_token) close_bracket_text
        || String.equal (Ceibo.Red.SyntaxToken.text close_token) close_brace_text -> List.rev payload_rev
        | _ -> rest
      )
    | _ -> []
  in
  match payload_non_trivia_tokens with
  | [] ->
      None
  | marker_token :: rest when String.equal (Ceibo.Red.SyntaxToken.text marker_token) colon_text -> (
      match rest with
      | first_content_token :: _ -> Some {
        kind = TypePayload;
        text = payload_text_from_tokens
          all_tokens
          ~start_offset:(Ceibo.Red.SyntaxToken.span first_content_token).start
          ~end_offset:shell_close_offset;
        start_offset = (Ceibo.Red.SyntaxToken.span first_content_token).start
      }
      | [] -> None
    )
  | marker_token :: rest when String.equal (Ceibo.Red.SyntaxToken.text marker_token) question_mark_text -> (
      match rest with
      | first_content_token :: _ -> Some {
        kind = PatternPayload;
        text = payload_text_from_tokens
          all_tokens
          ~start_offset:(Ceibo.Red.SyntaxToken.span first_content_token).start
          ~end_offset:shell_close_offset;
        start_offset = (Ceibo.Red.SyntaxToken.span first_content_token).start
      }
      | [] -> None
    )
  | first_payload_token :: _ ->
      Some {
        kind = Unmarked;
        text = payload_text_from_tokens
          all_tokens
          ~start_offset:(Ceibo.Red.SyntaxToken.span first_payload_token).start
          ~end_offset:shell_close_offset;
        start_offset = (Ceibo.Red.SyntaxToken.span first_payload_token).start
      }

let annotation_payload_from_shell_default = fun shell_node ->
  let all_tokens = direct_tokens shell_node in
  let shell_tokens = direct_non_trivia_tokens shell_node in
  let rec skip_sigils = function
    | syntax_token :: rest when is_annotation_sigil_text (Ceibo.Red.SyntaxToken.text syntax_token) -> skip_sigils
      rest
    | rest -> rest
  in
  let rec skip_qualified_name_tail = fun last_name_token ->
    function
    | dot_token :: name_token :: rest when String.equal (Ceibo.Red.SyntaxToken.text dot_token) dot_text -> skip_qualified_name_tail
      name_token
      rest
    | rest -> (last_name_token, rest)
  in
  match shell_tokens with
  | open_token :: rest when String.equal (Ceibo.Red.SyntaxToken.text open_token) open_bracket_text -> (
      match skip_sigils rest with
      | name_token :: rest ->
          let last_name_token, rest = skip_qualified_name_tail name_token rest in
          let start_offset = (Ceibo.Red.SyntaxToken.span last_name_token).end_ in
          let end_offset =
            match List.rev rest with
            | close_token :: _ when String.equal (Ceibo.Red.SyntaxToken.text close_token) close_bracket_text
            || String.equal (Ceibo.Red.SyntaxToken.text close_token) close_brace_text -> (Ceibo.Red.SyntaxToken.span
              close_token).start
            | _ -> Ceibo.Red.SyntaxNode.span shell_node |> fun span -> span.end_
          in
          let tokens =
            all_tokens
            |> List.filter
              (fun syntax_token ->
                let span = Ceibo.Red.SyntaxToken.span syntax_token in
                span.start >= start_offset && span.end_ <= end_offset)
            |> List.map token
          in
          if tokens = [] then
            None
          else
            Some (Cst.Payload.Opaque { tokens })
      | [] -> None
    )
  | _ -> None

let structure_payload_item_syntax_nodes_from_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.VAL_DECL -> (
      match value_declaration_from_node node with
      | Some decl -> [ decl.syntax_node ]
      | None -> unsupported_item node
    )
  | _ -> structure_items_from_node node |> List.map Cst.StructureItem.syntax_node

let signature_payload_item_syntax_nodes_from_node = fun node ->
  signature_items_from_node node |> List.map Cst.SignatureItem.syntax_node

let item_syntax_nodes_from_parse_result = fun ~kind result ->
  if List.length result.Parser.diagnostics > 0 then
    None
  else
    let root = Ceibo.Red.new_root result.tree in
    try
      Some (
        match kind with
        | `Implementation -> direct_non_trivia_nodes root |> List.concat_map structure_payload_item_syntax_nodes_from_node
        | `Interface -> direct_non_trivia_nodes root |> List.concat_map signature_payload_item_syntax_nodes_from_node
      )
    with
    | Bail _ -> None

let () =
  Cell.set attribute_payload_from_shell_impl annotation_payload_from_shell_default;
  Cell.set extension_payload_from_shell_impl annotation_payload_from_shell_default

let build_source_file_body = fun ~source ~tokens ~comment_item_of_comment ~docstring_item_of_docstring ~syntax_node_of_item ~owned_trivia_spans_of_item tree items_from_node ->
  let root = Ceibo.Red.new_root tree in
  let phrase_separator_tokens = source_file_phrase_separator_tokens root in
  let item_nodes = Ceibo.Red.SyntaxNode.direct_nodes root
  |> List.filter (fun node -> not (is_trivia (Ceibo.Red.SyntaxNode.kind node))) in
  let next_index =
    let cell = Cell.create 0 in
    fun () ->
      let index = Cell.get cell in
      Cell.set cell (index + 1);
      index
  in
  let item_entries =
    item_nodes
    |> List.concat_map
      (fun node ->
        items_from_node node |> List.map
          (fun item ->
            let span = span_of_syntax_node_nontrivia_bounds (syntax_node_of_item item) in
            (next_index (), span, item)))
  in
  let owned_trivia_spans = item_entries
  |> List.concat_map (fun ((_, _, item)) -> owned_trivia_spans_of_item item) in
  let trivia_entries =
    ((item_entries
    |> List.concat_map
      (fun ((_, _, item)) -> leading_trivia_tokens_for_item ~tokens (syntax_node_of_item item)))
    @ eof_leading_trivia_tokens tokens)
    |> List.filter
      (fun ({ Token.span; _ }: Token.t) ->
        not (List.exists (fun owned_span -> span_contains owned_span span) owned_trivia_spans))
    |> List.filter_map
      (fun lexed_token ->
        standalone_trivia_item_from_lexed_token
          ~source
          ~comment_item_of_comment
          ~docstring_item_of_docstring
          lexed_token
        |> Option.map
          (fun item ->
            let syntax_node = syntax_node_of_item item in
            (next_index (), Ceibo.Red.SyntaxNode.span syntax_node, item)))
  in
  let ordered_entries =
    List.sort
      (fun ((left_index, (left_span: Ceibo.Span.t), _)) ((right_index, (right_span: Ceibo.Span.t), _)) ->
        let order =
          if not (Int.equal left_span.start right_span.start) then
            Int.compare left_span.start right_span.start
          else if not (Int.equal left_span.end_ right_span.end_) then
            Int.compare left_span.end_ right_span.end_
          else
            0
        in
        if Int.equal order 0 then
          Int.compare left_index right_index
        else
          order)
      (item_entries @ trivia_entries)
  in
  let file_items = ordered_entries |> List.map (fun ((_, _, item)) -> item) in
  let trailing_phrase_separator_tokens =
    ordered_entries
    |> List.mapi
      (fun index ((_, (span: Ceibo.Span.t), _)) ->
        let next_boundary =
          match List.nth_opt ordered_entries (index + 1) with
          | Some (_, (next_span: Ceibo.Span.t), _) -> next_span.start
          | None -> (Ceibo.Red.SyntaxNode.span root).end_
        in
        phrase_separator_tokens_between phrase_separator_tokens ~start:span.end_ ~end_:next_boundary)
  in
  (root, file_items, phrase_separator_tokens, trailing_phrase_separator_tokens)

let build_items_from_payload_nodes = fun ~comment_item_of_comment ~docstring_item_of_docstring ~syntax_node_of_item ~owned_trivia_spans_of_item ?container_node payload_nodes items_from_node ->
  let next_index =
    let cell = Cell.create 0 in
    fun () ->
      let index = Cell.get cell in
      Cell.set cell (index + 1);
      index
  in
  let item_entries =
    payload_nodes
    |> List.filter (fun node -> not (is_trivia (Ceibo.Red.SyntaxNode.kind node)))
    |> List.concat_map
      (fun node ->
        items_from_node node |> List.map
          (fun item ->
            let span = span_of_syntax_node_nontrivia_bounds (syntax_node_of_item item) in
            (next_index (), span, item)))
  in
  let owned_trivia_spans = item_entries
  |> List.concat_map (fun ((_, _, item)) -> owned_trivia_spans_of_item item) in
  let trivia_entries =
    let terminal_tokens =
      match container_node with
      | Some node -> (
          match List.rev (direct_non_trivia_tokens node) with
          | closing_token :: _ -> Ceibo.Red.SyntaxToken.leading_trivia closing_token |> List.map syntax_token_from_trivia
          | [] -> []
        )
      | None ->
          payload_nodes |> List.concat_map
            (fun payload_node ->
              match List.rev (direct_non_trivia_tokens payload_node) with
              | closing_token :: _ -> Ceibo.Red.SyntaxToken.leading_trivia closing_token
              |> List.map syntax_token_from_trivia
              | [] -> [])
    in
    ((item_entries
    |> List.concat_map
      (fun ((_, _, item)) -> leading_trivia_syntax_tokens_for_item (syntax_node_of_item item)))
    @ terminal_tokens)
    |> List.filter
      (fun syntax_token ->
        let token_span = Ceibo.Red.SyntaxToken.span syntax_token in
        not (List.exists (fun owned_span -> span_contains owned_span token_span) owned_trivia_spans))
    |> List.filter_map
      (fun syntax_token ->
        standalone_trivia_item_from_token ~comment_item_of_comment ~docstring_item_of_docstring syntax_token
        |> Option.map
          (fun item ->
            let syntax_node = syntax_node_of_item item in
            (next_index (), Ceibo.Red.SyntaxNode.span syntax_node, item)))
  in
  List.sort
    (fun ((left_index, (left_span: Ceibo.Span.t), _)) ((right_index, (right_span: Ceibo.Span.t), _)) ->
      let order =
        if not (Int.equal left_span.start right_span.start) then
          Int.compare left_span.start right_span.start
        else if not (Int.equal left_span.end_ right_span.end_) then
          Int.compare left_span.end_ right_span.end_
        else
          0
      in
      if Int.equal order 0 then
        Int.compare left_index right_index
      else
        order)
    (item_entries @ trivia_entries) |> List.map (fun ((_, _, item)) -> item)

let record_field_items_of_fields = fun fields ->
  let next_index =
    let cell = Cell.create 0 in
    fun () ->
      let index = Cell.get cell in
      Cell.set cell (index + 1);
      index
  in
  let item_entries: (int * Ceibo.Span.t * record_field_item) list =
    fields
    |> List.map
      (fun field ->
        let item = RecordField field in
        let span = span_of_syntax_node_nontrivia_bounds (Cst.RecordField.syntax_node field) in
        (next_index (), span, item))
  in
  let owned_trivia_spans = item_entries
  |> List.concat_map (fun ((_, _, item)) -> record_field_item_owned_trivia_spans item) in
  let terminal_items =
    match fields with
    | field :: _ -> (
        match Ceibo.Red.SyntaxNode.parent (Cst.RecordField.syntax_node field) with
        | Some source_node -> (
            match List.rev (direct_non_trivia_tokens source_node) with
            | closing_token :: _ -> record_field_items_from_leading_trivia
              (Ceibo.Red.SyntaxToken.leading_trivia closing_token)
            | [] -> []
          )
        | None -> []
      )
    | [] -> []
  in
  let trivia_entries: (int * Ceibo.Span.t * record_field_item) list =
    (
      (
        item_entries |> List.concat_map
          (fun ((_, _, item)) ->
            match item with
            | RecordField field -> leading_record_field_items_for_field field
            | Comment _
            | Docstring _
            | TrailingComment _
            | TrailingDocstring _ -> [])
      ) @ terminal_items
    )
    |> List.filter
      (fun item ->
        let token_span = Ceibo.Red.SyntaxNode.span (syntax_node_of_record_field_item item) in
        not (List.exists (fun owned_span -> span_contains owned_span token_span) owned_trivia_spans))
    |> List.map
      (fun (item: record_field_item) ->
        let syntax_node = syntax_node_of_record_field_item item in
        (next_index (), Ceibo.Red.SyntaxNode.span syntax_node, item))
  in
  List.sort
    (fun ((left_index, (left_span: Ceibo.Span.t), _)) ((right_index, (right_span: Ceibo.Span.t), _)) ->
      let order =
        if not (Int.equal left_span.start right_span.start) then
          Int.compare left_span.start right_span.start
        else if not (Int.equal left_span.end_ right_span.end_) then
          Int.compare left_span.end_ right_span.end_
        else
          0
      in
      if Int.equal order 0 then
        Int.compare left_index right_index
      else
        order)
    (item_entries @ trivia_entries) |> List.map (fun ((_, _, item)) -> item)

let object_member_items_of_members = fun ?source_node members ->
  let next_index =
    let cell = Cell.create 0 in
    fun () ->
      let index = Cell.get cell in
      Cell.set cell (index + 1);
      index
  in
  let item_entries =
    members
    |> List.map
      (fun member ->
        let item = ObjectMember member in
        let span = span_of_syntax_node_nontrivia_bounds (Cst.ObjectMember.syntax_node member) in
        (next_index (), span, item))
  in
  let owned_trivia_spans = item_entries
  |> List.concat_map (fun ((_, _, item)) -> object_member_item_owned_trivia_spans item) in
  let terminal_tokens =
    match members, source_node with
    | _ :: _, Some source_node -> (
        match List.rev (direct_non_trivia_tokens source_node) with
        | closing_token :: _ when String.equal (Ceibo.Red.SyntaxToken.text closing_token) "end" -> Ceibo.Red.SyntaxToken.leading_trivia
          closing_token
        |> List.map syntax_token_from_trivia
        | _ -> []
      )
    | member :: _, None -> (
        match Ceibo.Red.SyntaxNode.parent (Cst.ObjectMember.syntax_node member) with
        | Some source_node -> (
            match List.rev (direct_non_trivia_tokens source_node) with
            | closing_token :: _ when String.equal (Ceibo.Red.SyntaxToken.text closing_token) "end" -> Ceibo.Red.SyntaxToken.leading_trivia
              closing_token
            |> List.map syntax_token_from_trivia
            | [] -> []
            | _ -> []
          )
        | None -> []
      )
    | [], Some source_node -> (
        match List.rev (direct_non_trivia_tokens source_node) with
        | closing_token :: _ when String.equal (Ceibo.Red.SyntaxToken.text closing_token) "end" -> Ceibo.Red.SyntaxToken.leading_trivia
          closing_token
        |> List.map syntax_token_from_trivia
        | _ -> []
      )
    | [], None ->
        []
  in
  let trivia_entries =
    ((item_entries
    |> List.concat_map
      (fun ((_, _, item)) ->
        leading_trivia_syntax_tokens_for_item (syntax_node_of_object_member_item item)))
    @ terminal_tokens)
    |> List.filter
      (fun syntax_token ->
        let token_span = Ceibo.Red.SyntaxToken.span syntax_token in
        not (List.exists (fun owned_span -> span_contains owned_span token_span) owned_trivia_spans))
    |> List.filter_map
      (fun syntax_token ->
        standalone_trivia_item_from_token
          ~comment_item_of_comment:object_member_item_of_comment
          ~docstring_item_of_docstring:object_member_item_of_docstring
          syntax_token
        |> Option.map
          (fun item ->
            let item: object_member_item = item in
            let syntax_node = syntax_node_of_object_member_item item in
            (next_index (), Ceibo.Red.SyntaxNode.span syntax_node, item)))
  in
  List.sort
    (fun ((left_index, (left_span: Ceibo.Span.t), _)) ((right_index, (right_span: Ceibo.Span.t), _)) ->
      let order =
        if not (Int.equal left_span.start right_span.start) then
          Int.compare left_span.start right_span.start
        else if not (Int.equal left_span.end_ right_span.end_) then
          Int.compare left_span.end_ right_span.end_
        else
          0
      in
      if Int.equal order 0 then
        Int.compare left_index right_index
      else
        order)
    (item_entries @ trivia_entries) |> List.map (fun ((_, _, item)) -> item)

let class_field_items_of_fields = fun ?source_node fields ->
  let next_index =
    let cell = Cell.create 0 in
    fun () ->
      let index = Cell.get cell in
      Cell.set cell (index + 1);
      index
  in
  let item_entries =
    fields
    |> List.map
      (fun field ->
        let item = ClassField field in
        let span = span_of_syntax_node_nontrivia_bounds (Cst.ClassField.syntax_node field) in
        (next_index (), span, item))
  in
  let owned_trivia_spans = item_entries
  |> List.concat_map (fun ((_, _, item)) -> class_field_item_owned_trivia_spans item) in
  let terminal_tokens =
    match fields, source_node with
    | _ :: _, Some source_node -> (
        match List.rev (direct_non_trivia_tokens source_node) with
        | closing_token :: _ when String.equal (Ceibo.Red.SyntaxToken.text closing_token) "end" -> Ceibo.Red.SyntaxToken.leading_trivia
          closing_token
        |> List.map syntax_token_from_trivia
        | _ -> []
      )
    | field :: _, None -> (
        match Ceibo.Red.SyntaxNode.parent (Cst.ClassField.syntax_node field) with
        | Some source_node -> (
            match List.rev (direct_non_trivia_tokens source_node) with
            | closing_token :: _ when String.equal (Ceibo.Red.SyntaxToken.text closing_token) "end" -> Ceibo.Red.SyntaxToken.leading_trivia
              closing_token
            |> List.map syntax_token_from_trivia
            | _ -> []
          )
        | None -> []
      )
    | [], Some source_node -> (
        match List.rev (direct_non_trivia_tokens source_node) with
        | closing_token :: _ when String.equal (Ceibo.Red.SyntaxToken.text closing_token) "end" -> Ceibo.Red.SyntaxToken.leading_trivia
          closing_token
        |> List.map syntax_token_from_trivia
        | _ -> []
      )
    | [], None ->
        []
  in
  let trivia_entries =
    ((item_entries
    |> List.concat_map
      (fun ((_, _, item)) ->
        leading_trivia_syntax_tokens_for_item (syntax_node_of_class_field_item item)))
    @ terminal_tokens)
    |> List.filter
      (fun syntax_token ->
        let token_span = Ceibo.Red.SyntaxToken.span syntax_token in
        not (List.exists (fun owned_span -> span_contains owned_span token_span) owned_trivia_spans))
    |> List.filter_map
      (fun syntax_token ->
        standalone_trivia_item_from_token
          ~comment_item_of_comment:class_field_item_of_comment
          ~docstring_item_of_docstring:class_field_item_of_docstring
          syntax_token
        |> Option.map
          (fun item ->
            let item: class_field_item = item in
            let syntax_node = syntax_node_of_class_field_item item in
            (next_index (), Ceibo.Red.SyntaxNode.span syntax_node, item)))
  in
  List.sort
    (fun ((left_index, (left_span: Ceibo.Span.t), _)) ((right_index, (right_span: Ceibo.Span.t), _)) ->
      let order =
        if not (Int.equal left_span.start right_span.start) then
          Int.compare left_span.start right_span.start
        else if not (Int.equal left_span.end_ right_span.end_) then
          Int.compare left_span.end_ right_span.end_
        else
          0
      in
      if Int.equal order 0 then
        Int.compare left_index right_index
      else
        order)
    (item_entries @ trivia_entries) |> List.map (fun ((_, _, item)) -> item)

let class_type_field_items_of_fields = fun ?source_node fields ->
  let next_index =
    let cell = Cell.create 0 in
    fun () ->
      let index = Cell.get cell in
      Cell.set cell (index + 1);
      index
  in
  let item_entries =
    fields
    |> List.map
      (fun field ->
        let item = ClassTypeField field in
        let span = span_of_syntax_node_nontrivia_bounds (Cst.ClassTypeField.syntax_node field) in
        (next_index (), span, item))
  in
  let owned_trivia_spans = item_entries
  |> List.concat_map (fun ((_, _, item)) -> class_type_field_item_owned_trivia_spans item) in
  let terminal_tokens =
    match fields, source_node with
    | _ :: _, Some source_node -> (
        match List.rev (direct_non_trivia_tokens source_node) with
        | closing_token :: _ when String.equal (Ceibo.Red.SyntaxToken.text closing_token) "end" -> Ceibo.Red.SyntaxToken.leading_trivia
          closing_token
        |> List.map syntax_token_from_trivia
        | _ -> []
      )
    | field :: _, None -> (
        match Ceibo.Red.SyntaxNode.parent (Cst.ClassTypeField.syntax_node field) with
        | Some source_node -> (
            match List.rev (direct_non_trivia_tokens source_node) with
            | closing_token :: _ when String.equal (Ceibo.Red.SyntaxToken.text closing_token) "end" -> Ceibo.Red.SyntaxToken.leading_trivia
              closing_token
            |> List.map syntax_token_from_trivia
            | _ -> []
          )
        | None -> []
      )
    | [], Some source_node -> (
        match List.rev (direct_non_trivia_tokens source_node) with
        | closing_token :: _ when String.equal (Ceibo.Red.SyntaxToken.text closing_token) "end" -> Ceibo.Red.SyntaxToken.leading_trivia
          closing_token
        |> List.map syntax_token_from_trivia
        | _ -> []
      )
    | [], None ->
        []
  in
  let trivia_entries =
    ((item_entries
    |> List.concat_map
      (fun ((_, _, item)) ->
        leading_trivia_syntax_tokens_for_item (syntax_node_of_class_type_field_item item)))
    @ terminal_tokens)
    |> List.filter
      (fun syntax_token ->
        let token_span = Ceibo.Red.SyntaxToken.span syntax_token in
        not (List.exists (fun owned_span -> span_contains owned_span token_span) owned_trivia_spans))
    |> List.filter_map
      (fun syntax_token ->
        standalone_trivia_item_from_token
          ~comment_item_of_comment:class_type_field_item_of_comment
          ~docstring_item_of_docstring:class_type_field_item_of_docstring
          syntax_token
        |> Option.map
          (fun item ->
            let item: class_type_field_item = item in
            let syntax_node = syntax_node_of_class_type_field_item item in
            (next_index (), Ceibo.Red.SyntaxNode.span syntax_node, item)))
  in
  List.sort
    (fun ((left_index, (left_span: Ceibo.Span.t), _)) ((right_index, (right_span: Ceibo.Span.t), _)) ->
      let order =
        if not (Int.equal left_span.start right_span.start) then
          Int.compare left_span.start right_span.start
        else if not (Int.equal left_span.end_ right_span.end_) then
          Int.compare left_span.end_ right_span.end_
        else
          0
      in
      if Int.equal order 0 then
        Int.compare left_index right_index
      else
        order)
    (item_entries @ trivia_entries) |> List.map (fun ((_, _, item)) -> item)

let rec validate_pattern = fun ~context ->
  function
  | Cst.Pattern.Identifier _
  | Cst.Pattern.Wildcard _
  | Cst.Pattern.Literal _
  | Cst.Pattern.Extension _ ->
      ()
  | Cst.Pattern.Lazy { pattern; _ } ->
      validate_pattern ~context:(("pattern.lazy" :: context)) pattern
  | Cst.Pattern.Exception { pattern; _ } ->
      validate_pattern ~context:(("pattern.exception" :: context)) pattern
  | Cst.Pattern.Range _
  | Cst.Pattern.Operator _
  | Cst.Pattern.PolyVariantInherit _ ->
      ()
  | Cst.Pattern.FirstClassModule { package_type; _ } ->
      Option.iter
        (fun ({ constraints; _ }: Cst.package_type) ->
          List.iteri
            (fun index ({ constrained_type; replacement_type; _ }: Cst.module_type_constraint) ->
              validate_core_type
                ~context:((("pattern.first_class_module.type.constraint[" ^ Int.to_string index ^ "].target")
                :: context))
                constrained_type;
              validate_core_type
                ~context:((("pattern.first_class_module.type.constraint[" ^ Int.to_string index ^ "].replacement")
                :: context))
                replacement_type)
            constraints)
        package_type
  | Cst.Pattern.PolyVariant { payload; _ } ->
      Option.iter (validate_pattern ~context:(("pattern.poly_variant.payload" :: context))) payload
  | Cst.Pattern.Constructor { arguments; _ } ->
      List.iteri
        (fun index argument ->
          validate_pattern
            ~context:((("pattern.constructor.argument[" ^ Int.to_string index ^ "]") :: context))
            argument)
        arguments
  | Cst.Pattern.Tuple { elements; _ } ->
      List.iteri
        (fun index ({ Cst.pattern; _ }: Cst.tuple_pattern_element) ->
          validate_pattern
            ~context:((("pattern.tuple.element[" ^ Int.to_string index ^ "]") :: context))
            pattern)
        elements
  | Cst.Pattern.List { elements; _ }
  | Cst.Pattern.Array { elements; _ }
  | Cst.Pattern.Or { alternatives=elements; _ } ->
      List.iteri
        (fun index pattern ->
          validate_pattern ~context:((("pattern.element[" ^ Int.to_string index ^ "]") :: context)) pattern)
        elements
  | Cst.Pattern.Record { fields; _ } ->
      List.iteri
        (fun index (field: Cst.record_pattern_field) ->
          Option.iter
            (validate_pattern
              ~context:((("pattern.record.field[" ^ Int.to_string index ^ "].pattern") :: context)))
            field.pattern)
        fields
  | Cst.Pattern.Cons { head; tail; _ } ->
      validate_pattern ~context:(("pattern.cons.head" :: context)) head;
      validate_pattern ~context:(("pattern.cons.tail" :: context)) tail
  | Cst.Pattern.Alias { pattern; _ } ->
      validate_pattern ~context:(("pattern.alias.pattern" :: context)) pattern
  | Cst.Pattern.Typed { pattern; type_; _ } ->
      validate_pattern ~context:(("pattern.typed.pattern" :: context)) pattern;
      validate_core_type ~context:(("pattern.typed.type" :: context)) type_
  | Cst.Pattern.Effect { effect_pattern; continuation; _ } ->
      validate_pattern ~context:(("pattern.effect.effect" :: context)) effect_pattern;
      validate_pattern ~context:(("pattern.effect.continuation" :: context)) continuation
  | Cst.Pattern.LocalOpen { pattern; _ } ->
      validate_pattern ~context:(("pattern.local_open.pattern" :: context)) pattern
  | Cst.Pattern.Parenthesized { inner; _ } ->
      validate_pattern ~context:(("pattern.parenthesized" :: context)) inner

and validate_parameter = fun ~context ->
  function
  | Cst.Parameter.Positional _
  | Cst.Parameter.Labeled _
  | Cst.Parameter.Optional _
  | Cst.Parameter.LocallyAbstract _ -> ()

and validate_module_type = fun ~context ->
  function
  | Cst.ModuleType.Path _
  | Cst.ModuleType.TypeOf _
  | Cst.ModuleType.Signature _
  | Cst.ModuleType.Extension _ ->
      ()
  | Cst.ModuleType.Parenthesized { inner; _ } ->
      validate_module_type ~context:(("module_type.parenthesized" :: context)) inner
  | Cst.ModuleType.Attribute { module_type; _ } ->
      validate_module_type ~context:(("module_type.attribute" :: context)) module_type
  | Cst.ModuleType.With { base; constraints; _ } ->
      validate_module_type ~context:(("module_type.with.base" :: context)) base;
      List.iteri
        (fun index ({ constrained_type; replacement_type; _ }: Cst.module_type_constraint) ->
          validate_core_type
            ~context:((("module_type.with.constraint[" ^ Int.to_string index ^ "].target") :: context))
            constrained_type;
          validate_core_type
            ~context:((("module_type.with.constraint[" ^ Int.to_string index ^ "].replacement") :: context))
            replacement_type)
        constraints
  | Cst.ModuleType.Functor { parameters; result; _ } ->
      List.iteri
        (fun index ({ module_type; _ }: Cst.functor_parameter) ->
          validate_module_type
            ~context:((("module_type.functor.parameter[" ^ Int.to_string index ^ "]") :: context))
            module_type)
        parameters;
      validate_module_type ~context:(("module_type.functor.result" :: context)) result

and validate_core_type = fun ~context ->
  function
  | Cst.CoreType.Wildcard _
  | Cst.CoreType.Var _
  | Cst.CoreType.Extension _ ->
      ()
  | Cst.CoreType.Poly { body; _ } ->
      validate_core_type ~context:(("core_type.poly.body" :: context)) body
  | Cst.CoreType.FirstClassModule { package_type; _ } ->
      List.iteri
        (fun index ({ constrained_type; replacement_type; _ }: Cst.module_type_constraint) ->
          validate_core_type
            ~context:((("core_type.first_class_module.constraint[" ^ Int.to_string index ^ "].target")
            :: context))
            constrained_type;
          validate_core_type
            ~context:((("core_type.first_class_module.constraint[" ^ Int.to_string index ^ "].replacement")
            :: context))
            replacement_type)
        package_type.constraints
  | Cst.CoreType.Constr { arguments; _ } ->
      List.iteri
        (fun index type_ ->
          validate_core_type
            ~context:((("core_type.constr.arg[" ^ Int.to_string index ^ "]") :: context))
            type_)
        arguments
  | Cst.CoreType.Class { arguments; _ } ->
      List.iteri
        (fun index type_ ->
          validate_core_type
            ~context:((("core_type.class.arg[" ^ Int.to_string index ^ "]") :: context))
            type_)
        arguments
  | Cst.CoreType.Alias { type_; _ } ->
      validate_core_type ~context:(("core_type.alias.type" :: context)) type_
  | Cst.CoreType.Attribute { type_; _ } ->
      validate_core_type ~context:(("core_type.attribute.type" :: context)) type_
  | Cst.CoreType.Arrow { parameter_type; result_type; _ } ->
      validate_core_type ~context:(("core_type.arrow.parameter" :: context)) parameter_type;
      validate_core_type ~context:(("core_type.arrow.result" :: context)) result_type
  | Cst.CoreType.Tuple { elements; _ } ->
      List.iteri
        (fun index type_ ->
          validate_core_type
            ~context:((("core_type.tuple.element[" ^ Int.to_string index ^ "]") :: context))
            type_)
        elements
  | Cst.CoreType.Parenthesized { inner; _ } ->
      validate_core_type ~context:(("core_type.parenthesized" :: context)) inner
  | Cst.CoreType.PolyVariant poly_variant ->
      validate_poly_variant ~context:(("core_type.poly_variant" :: context)) poly_variant
  | Cst.CoreType.Record { fields; _ } ->
      List.iteri
        (fun index ({ field_type; _ }: Cst.record_type_field) ->
          validate_core_type
            ~context:((("core_type.record.field[" ^ Int.to_string index ^ "].type") :: context))
            field_type)
        fields
  | Cst.CoreType.Object { fields; _ } ->
      List.iteri
        (fun index ({ field_type; _ }: Cst.object_type_field) ->
          validate_core_type
            ~context:((("core_type.object.field[" ^ Int.to_string index ^ "].type") :: context))
            field_type)
        fields

and validate_row_field = fun ~context index ->
  function
  | Cst.RowField.Tag tag -> Option.iter
    (validate_core_type
      ~context:((("row_field[" ^ Int.to_string index ^ "].tag.payload") :: context)))
    (Cst.PolyVariantTag.payload_type tag)
  | Cst.RowField.Inherit { type_; _ } -> validate_core_type
    ~context:((("row_field[" ^ Int.to_string index ^ "].inherit") :: context))
    type_

and validate_poly_variant = fun ~context poly_variant ->
  Cst.PolyVariant.fields poly_variant |> List.iteri (validate_row_field ~context)

and validate_class_type_field = fun ~context ->
  function
  | Cst.ClassTypeField.Inherit { class_type; _ } ->
      validate_class_type ~context:(("class_type_field.inherit" :: context)) class_type
  | Cst.ClassTypeField.Value { type_; _ } ->
      validate_core_type ~context:(("class_type_field.value" :: context)) type_
  | Cst.ClassTypeField.Method { type_; _ } ->
      validate_core_type ~context:(("class_type_field.method" :: context)) type_
  | Cst.ClassTypeField.Constraint { left; right; _ } ->
      validate_core_type ~context:(("class_type_field.constraint.left" :: context)) left;
      validate_core_type ~context:(("class_type_field.constraint.right" :: context)) right
  | Cst.ClassTypeField.Attribute { field; _ } ->
      validate_class_type_field ~context:(("class_type_field.attribute" :: context)) field
  | Cst.ClassTypeField.Extension _ ->
      ()

and validate_class_type = fun ~context ->
  function
  | Cst.ClassType.Path _
  | Cst.ClassType.Extension _ ->
      ()
  | Cst.ClassType.Signature { fields; _ } ->
      List.iteri
        (fun index field ->
          validate_class_type_field
            ~context:((("class_type.signature.field[" ^ Int.to_string index ^ "]") :: context))
            field)
        fields
  | Cst.ClassType.Arrow { parameter_type; result_type; _ } ->
      validate_core_type ~context:(("class_type.arrow.parameter" :: context)) parameter_type;
      validate_class_type ~context:(("class_type.arrow.result" :: context)) result_type
  | Cst.ClassType.Parenthesized { inner; _ } ->
      validate_class_type ~context:(("class_type.parenthesized" :: context)) inner
  | Cst.ClassType.Attribute { class_type; _ } ->
      validate_class_type ~context:(("class_type.attribute" :: context)) class_type

and validate_class_field = fun ~context ->
  function
  | Cst.ClassField.Method { definition; _ } -> (
      match definition with
      | Cst.ConcreteMethod { body; type_ } ->
          validate_expression ~context:(("class_field.method.body" :: context)) body;
          Option.iter
            (fun ((_, type_)) ->
              validate_core_type ~context:(("class_field.method.type" :: context)) type_)
            type_
      | Cst.VirtualMethod { type_; _ } -> validate_core_type
        ~context:(("class_field.method.type" :: context))
        type_
    )
  | Cst.ClassField.Value { definition; _ } -> (
      match definition with
      | Cst.ConcreteValue { value; type_ } ->
          validate_expression ~context:(("class_field.value.value" :: context)) value;
          Option.iter
            (fun ((_, type_)) ->
              validate_core_type ~context:(("class_field.value.type" :: context)) type_)
            type_
      | Cst.VirtualValue { type_; _ } -> validate_core_type
        ~context:(("class_field.value.type" :: context))
        type_
    )
  | Cst.ClassField.Inherit { class_expression; _ } ->
      validate_class_expression ~context:(("class_field.inherit.class_expression" :: context)) class_expression
  | Cst.ClassField.Constraint { left; right; _ } ->
      validate_core_type ~context:(("class_field.constraint.left" :: context)) left;
      validate_core_type ~context:(("class_field.constraint.right" :: context)) right
  | Cst.ClassField.Initializer { body; _ } ->
      validate_expression ~context:(("class_field.initializer.body" :: context)) body
  | Cst.ClassField.Attribute { field; _ } ->
      validate_class_field ~context:(("class_field.attribute" :: context)) field
  | Cst.ClassField.Extension _ ->
      ()

and validate_class_expression = fun ~context ->
  function
  | Cst.ClassExpression.Path _
  | Cst.ClassExpression.Extension _ ->
      ()
  | Cst.ClassExpression.Structure { self_pattern; fields; _ } ->
      Option.iter
        (validate_pattern ~context:(("class_expression.structure.self_pattern" :: context)))
        self_pattern;
      List.iteri
        (fun index field ->
          validate_class_field
            ~context:((("class_expression.structure.field[" ^ Int.to_string index ^ "]") :: context))
            field)
        fields
  | Cst.ClassExpression.Fun { body; _ } ->
      validate_class_expression ~context:(("class_expression.fun.body" :: context)) body
  | Cst.ClassExpression.Apply { callee; argument; _ } ->
      validate_class_expression ~context:(("class_expression.apply.callee" :: context)) callee;
      validate_apply_argument ~context:(("class_expression.apply.argument" :: context)) argument
  | Cst.ClassExpression.Let {
    parameters;
    bound_value;
    and_binding;
    body;
    _
  } ->
      List.iteri
        (fun index parameter ->
          validate_parameter
            ~context:((("class_expression.let.parameters[" ^ Int.to_string index ^ "]") :: context))
            parameter)
        parameters;
      validate_expression ~context:(("class_expression.let.bound_value" :: context)) bound_value;
      List.iteri (fun index binding ->
        validate_expression
          ~context:((("class_expression.let.and_binding[" ^ Int.to_string index ^ "]") :: context))
          (Cst.LetBinding.value binding))
        (Option.to_list and_binding |> List.concat_map let_binding_chain_to_list);
      validate_class_expression ~context:(("class_expression.let.body" :: context)) body
  | Cst.ClassExpression.Constraint { class_expression; class_type; _ } ->
      validate_class_expression ~context:(("class_expression.constraint.expression" :: context)) class_expression;
      validate_class_type ~context:(("class_expression.constraint.class_type" :: context)) class_type
  | Cst.ClassExpression.LocalOpen (Cst.LetOpen { body; _ })
  | Cst.ClassExpression.LocalOpen (Cst.Delimited { body; _ }) ->
      validate_class_expression ~context:(("class_expression.local_open" :: context)) body
  | Cst.ClassExpression.Parenthesized { inner; _ } ->
      validate_class_expression ~context:(("class_expression.parenthesized" :: context)) inner
  | Cst.ClassExpression.Attribute { class_expression; _ } ->
      validate_class_expression ~context:(("class_expression.attribute" :: context)) class_expression

and validate_apply_argument = fun ~context ->
  function
  | Cst.Positional expr -> validate_expression ~context:(("apply_argument.positional" :: context)) expr
  | Cst.Labeled { value; _ } -> Option.iter
    (validate_expression ~context:(("apply_argument.labeled.value" :: context)))
    value
  | Cst.Optional { value; _ } -> Option.iter
    (validate_expression ~context:(("apply_argument.optional.value" :: context)))
    value

and validate_module_expression = fun ~context ->
  function
  | Cst.ModuleExpression.Path _
  | Cst.ModuleExpression.Structure _
  | Cst.ModuleExpression.Extension _ ->
      ()
  | Cst.ModuleExpression.Functor { parameters; body; _ } ->
      List.iteri
        (fun index ({ module_type; _ }: Cst.functor_parameter) ->
          validate_module_type
            ~context:((("module_expression.functor.parameter[" ^ Int.to_string index ^ "]") :: context))
            module_type)
        parameters;
      validate_module_expression ~context:(("module_expression.functor.body" :: context)) body
  | Cst.ModuleExpression.Apply { callee; argument; _ } ->
      validate_module_expression ~context:(("module_expression.apply.callee" :: context)) callee;
      validate_module_expression ~context:(("module_expression.apply.argument" :: context)) argument
  | Cst.ModuleExpression.ApplyUnit { callee; _ } ->
      validate_module_expression ~context:(("module_expression.apply_unit.callee" :: context)) callee
  | Cst.ModuleExpression.Constraint { module_expression; module_type; _ } ->
      validate_module_expression ~context:(("module_expression.constraint.expression" :: context)) module_expression;
      validate_module_type ~context:(("module_expression.constraint.type" :: context)) module_type
  | Cst.ModuleExpression.ModuleUnpack { expression; package_type; _ } ->
      validate_expression ~context:(("module_expression.unpack.expression" :: context)) expression;
      Option.iter
        (fun ({ constraints; _ }: Cst.package_type) ->
          List.iteri
            (fun index ({ constrained_type; replacement_type; _ }: Cst.module_type_constraint) ->
              validate_core_type
                ~context:((("module_expression.unpack.type.constraint[" ^ Int.to_string index ^ "].target")
                :: context))
                constrained_type;
              validate_core_type
                ~context:((("module_expression.unpack.type.constraint[" ^ Int.to_string index ^ "].replacement")
                :: context))
                replacement_type)
            constraints)
        package_type
  | Cst.ModuleExpression.Parenthesized { inner; _ } ->
      validate_module_expression ~context:(("module_expression.parenthesized" :: context)) inner
  | Cst.ModuleExpression.Attribute { module_expression; _ } ->
      validate_module_expression ~context:(("module_expression.attribute" :: context)) module_expression

and validate_object_member = fun ~context ->
  function
  | Cst.ObjectMember.Method { body; type_; _ } ->
      validate_expression ~context:(("object_member.method.body" :: context)) body;
      Option.iter (validate_core_type ~context:(("object_member.method.type" :: context))) type_
  | Cst.ObjectMember.Value { value; type_; _ } ->
      validate_expression ~context:(("object_member.value.value" :: context)) value;
      Option.iter (validate_core_type ~context:(("object_member.value.type" :: context))) type_
  | Cst.ObjectMember.Inherit { expression; _ } ->
      validate_expression ~context:(("object_member.inherit.expression" :: context)) expression
  | Cst.ObjectMember.Extension _ ->
      ()
  | Cst.ObjectMember.Initializer { body; _ } ->
      validate_expression ~context:(("object_member.initializer.body" :: context)) body

and validate_fun_body = fun ~context ->
  function
  | Cst.Expression expression -> validate_expression ~context:(("fun_body.expression" :: context)) expression
  | Cst.Cases { cases; _ } -> List.iteri
    (fun index case ->
      validate_match_case ~context:((("fun_body.case[" ^ Int.to_string index ^ "]") :: context)) case)
    cases

and validate_expression = fun ~context ->
  function
  | Cst.Expression.Path _
  | Cst.Expression.Operator _
  | Cst.Expression.Literal _
  | Cst.Expression.Unreachable _
  | Cst.Expression.Extension _ ->
      ()
  | Cst.Expression.Constructor { payload; _ } ->
      Option.iter (validate_expression ~context:(("expression.constructor.payload" :: context))) payload
  | Cst.Expression.ModulePack { module_expression; package_type; _ } ->
      validate_module_expression ~context:(("expression.first_class_module.expression" :: context)) module_expression;
      Option.iter
        (fun ({ constraints; _ }: Cst.package_type) ->
          List.iteri
            (fun index ({ constrained_type; replacement_type; _ }: Cst.module_type_constraint) ->
              validate_core_type
                ~context:((("expression.first_class_module.type.constraint["
                ^ Int.to_string index
                ^ "].target")
                :: context))
                constrained_type;
              validate_core_type
                ~context:((("expression.first_class_module.type.constraint["
                ^ Int.to_string index
                ^ "].replacement")
                :: context))
                replacement_type)
            constraints)
        package_type
  | Cst.Expression.Object { self_pattern; members; _ } ->
      Option.iter (validate_pattern ~context:(("expression.object.self_pattern" :: context))) self_pattern;
      List.iteri
        (fun index member ->
          validate_object_member
            ~context:((("expression.object.member[" ^ Int.to_string index ^ "]") :: context))
            member)
        members
  | Cst.Expression.LetModule { body; _ } ->
      validate_expression ~context:(("expression.let_module.body" :: context)) body
  | Cst.Expression.LetException { body; _ } ->
      validate_expression ~context:(("expression.let_exception.body" :: context)) body
  | Cst.Expression.PolyVariant { payload; _ } ->
      Option.iter (validate_expression ~context:(("expression.poly_variant.payload" :: context))) payload
  | Cst.Expression.Assert { asserted; _ } ->
      validate_expression ~context:(("expression.assert.asserted" :: context)) asserted
  | Cst.Expression.Lazy { body; _ } ->
      validate_expression ~context:(("expression.lazy.body" :: context)) body
  | Cst.Expression.While { condition; body; _ } ->
      validate_expression ~context:(("expression.while.condition" :: context)) condition;
      validate_expression ~context:(("expression.while.body" :: context)) body
  | Cst.Expression.For { start_expr; end_expr; body; _ } ->
      validate_expression ~context:(("expression.for.start" :: context)) start_expr;
      validate_expression ~context:(("expression.for.end" :: context)) end_expr;
      validate_expression ~context:(("expression.for.body" :: context)) body
  | Cst.Expression.Apply { callee; argument; _ } ->
      validate_expression ~context:(("expression.apply.callee" :: context)) callee;
      validate_apply_argument ~context:(("expression.apply.argument" :: context)) argument
  | Cst.Expression.MethodCall { receiver; _ } ->
      validate_expression ~context:(("expression.method_call.receiver" :: context)) receiver
  | Cst.Expression.New _ ->
      ()
  | Cst.Expression.Prefix { operand; _ } ->
      validate_expression ~context:(("expression.prefix.operand" :: context)) operand
  | Cst.Expression.FieldAccess { receiver; _ } ->
      validate_expression ~context:(("expression.field_access.receiver" :: context)) receiver
  | Cst.Expression.Index { collection; index; _ } ->
      validate_expression ~context:(("expression.index.collection" :: context)) collection;
      validate_expression ~context:(("expression.index.index" :: context)) index
  | Cst.Expression.ObjectOverride { fields; _ } ->
      List.iteri
        (fun index (field: Cst.object_override_field) ->
          Option.iter
            (validate_expression
              ~context:((("expression.object_override.field[" ^ Int.to_string index ^ "].value") :: context)))
            field.value)
        fields
  | Cst.Expression.InstanceVariableAssign { value; _ } ->
      validate_expression ~context:(("expression.instance_variable_assign.value" :: context)) value
  | Cst.Expression.FieldAssign { target; value; _ } ->
      validate_expression ~context:(("expression.field_assign.receiver" :: context)) target.receiver;
      validate_expression ~context:(("expression.field_assign.value" :: context)) value
  | Cst.Expression.Assign { target; value; _ } ->
      validate_expression ~context:(("expression.assign.target" :: context)) target;
      validate_expression ~context:(("expression.assign.value" :: context)) value
  | Cst.Expression.Infix { left; right; _ } ->
      validate_expression ~context:(("expression.infix.left" :: context)) left;
      validate_expression ~context:(("expression.infix.right" :: context)) right
  | Cst.Expression.TypeAscription { expression; kind; _ } ->
      validate_expression ~context:(("expression.type_ascription.expression" :: context)) expression;
      (
        match kind with
        | Cst.Type { type_; _ }
        | Cst.Coerce { type_; _ } -> validate_core_type
          ~context:(("expression.type_ascription.type" :: context))
          type_
        | Cst.ConstraintCoerce { from_type; to_type; _ } ->
            validate_core_type ~context:(("expression.type_ascription.from_type" :: context)) from_type;
            validate_core_type ~context:(("expression.type_ascription.to_type" :: context)) to_type
      )
  | Cst.Expression.Polymorphic { expression; type_; _ } ->
      validate_expression ~context:(("expression.polymorphic.expression" :: context)) expression;
      validate_core_type ~context:(("expression.polymorphic.type" :: context)) type_
  | Cst.Expression.Sequence { expressions; _ } ->
      List.iter (validate_expression ~context:(("expression.sequence.expressions" :: context))) expressions
  | Cst.Expression.Tuple { elements; _ }
  | Cst.Expression.List { elements; _ }
  | Cst.Expression.Array { elements; _ } ->
      List.iteri
        (fun index expr ->
          validate_expression
            ~context:((("expression.element[" ^ Int.to_string index ^ "]") :: context))
            expr)
        elements
  | Cst.Expression.Record (Cst.RecordExpression.Literal { fields; _ }) ->
      List.iteri
        (fun index (field: Cst.record_expression_field) ->
          validate_expression
            ~context:((("expression.record.field[" ^ Int.to_string index ^ "].value") :: context))
            field.value)
        fields
  | Cst.Expression.Record (Cst.RecordExpression.Update { base; fields; _ }) ->
      validate_expression ~context:(("expression.record.base" :: context)) base;
      List.iteri
        (fun index (field: Cst.record_expression_field) ->
          validate_expression
            ~context:((("expression.record.field[" ^ Int.to_string index ^ "].value") :: context))
            field.value)
        fields
  | Cst.Expression.LocalOpen (Cst.LetOpen { body; _ })
  | Cst.Expression.LocalOpen (Cst.Delimited { body; _ }) ->
      validate_expression ~context:(("expression.local_open.body" :: context)) body
  | Cst.Expression.Fun { parameters; return_type; body; _ } ->
      List.iteri
        (fun index parameter ->
          validate_parameter
            ~context:((("expression.fun.parameter[" ^ Int.to_string index ^ "]") :: context))
            parameter)
        parameters;
      Option.iter
        (fun return_type -> validate_core_type ~context:(("expression.fun.return_type" :: context)) return_type)
        return_type;
      validate_fun_body ~context:(("expression.fun.body" :: context)) body
  | Cst.Expression.Function { cases; _ } ->
      List.iteri
        (fun index case ->
          validate_match_case
            ~context:((("expression.function.case[" ^ Int.to_string index ^ "]") :: context))
            case)
        cases
  | Cst.Expression.LetOperator { binding; body; _ } ->
      validate_pattern ~context:(("expression.let_operator.binding.pattern" :: context)) binding.binding_pattern;
      validate_expression ~context:(("expression.let_operator.binding.value" :: context)) binding.bound_value;
      List.iteri
        (fun index ({ binding_pattern; bound_value; _ }: Cst.binding_operator_binding) ->
          validate_pattern
            ~context:((("expression.let_operator.and_bindings[" ^ Int.to_string index ^ "].pattern")
            :: context))
            binding_pattern;
          validate_expression
            ~context:((("expression.let_operator.and_bindings[" ^ Int.to_string index ^ "].value")
            :: context))
            bound_value)
        (binding_operator_chain_tail binding);
      validate_expression ~context:(("expression.let_operator.body" :: context)) body
  | Cst.Expression.Let {
    binding_pattern;
    parameters;
    bound_value;
    and_binding;
    body;
    _
  } ->
      validate_pattern ~context:(("expression.let.pattern" :: context)) binding_pattern;
      List.iteri
        (fun index parameter ->
          validate_parameter
            ~context:((("expression.let.parameters[" ^ Int.to_string index ^ "]") :: context))
            parameter)
        parameters;
      validate_expression ~context:(("expression.let.bound_value" :: context)) bound_value;
      List.iteri
        (fun index binding ->
          validate_pattern
            ~context:((("expression.let.and_bindings[" ^ Int.to_string index ^ "].pattern") :: context))
            (Cst.LetBinding.binding_pattern binding);
          validate_expression
            ~context:((("expression.let.and_bindings[" ^ Int.to_string index ^ "].value") :: context))
            (Cst.LetBinding.value binding))
        (Option.to_list and_binding |> List.concat_map let_binding_chain_to_list);
      validate_expression ~context:(("expression.let.body" :: context)) body
  | Cst.Expression.Match { scrutinee; cases; _ } ->
      validate_expression ~context:(("expression.match.scrutinee" :: context)) scrutinee;
      List.iteri
        (fun index case ->
          validate_match_case
            ~context:((("expression.match.case[" ^ Int.to_string index ^ "]") :: context))
            case)
        cases
  | Cst.Expression.Try { body; cases; _ } ->
      validate_expression ~context:(("expression.try.body" :: context)) body;
      List.iteri
        (fun index case ->
          validate_match_case
            ~context:((("expression.try.case[" ^ Int.to_string index ^ "]") :: context))
            case)
        cases
  | Cst.Expression.If { condition; then_branch; else_branch; _ } ->
      validate_expression ~context:(("expression.if.condition" :: context)) condition;
      validate_expression ~context:(("expression.if.then_branch" :: context)) then_branch;
      Option.iter (validate_expression ~context:(("expression.if.else_branch" :: context))) else_branch
  | Cst.Expression.Parenthesized { inner; _ } ->
      validate_expression ~context:(("expression.parenthesized" :: context)) inner

and validate_match_case = fun ~context ({ pattern; guard; body; _ }: Cst.match_case) ->
  validate_pattern ~context:(("match_case.pattern" :: context)) pattern;
  Option.iter (validate_expression ~context:(("match_case.guard" :: context))) guard;
  validate_expression ~context:(("match_case.body" :: context)) body

let validate_constructor_arguments = fun ~context ->
  function
  | Cst.ConstructorArguments.Tuple elements -> List.iteri
    (fun index element ->
      validate_core_type
        ~context:((("constructor_arguments.tuple[" ^ Int.to_string index ^ "]") :: context))
        element)
    elements
  | Cst.ConstructorArguments.Record { fields; _ } -> List.iteri
    (fun index field ->
      validate_core_type
        ~context:((("constructor_arguments.record[" ^ Int.to_string index ^ "].type") :: context))
        (Cst.RecordField.field_type field))
    fields

let validate_type_definition = fun ~context ->
  function
  | Cst.TypeDefinition.Abstract -> ()
  | Cst.TypeDefinition.Alias { manifest; _ } -> validate_core_type
    ~context:(("type_definition.alias" :: context))
    manifest
  | Cst.TypeDefinition.Extensible _ -> ()
  | Cst.TypeDefinition.FirstClassModule { package_type; _ } ->
      List.iteri
        (fun index ({ constrained_type; replacement_type; _ }: Cst.module_type_constraint) ->
          validate_core_type
            ~context:((("type_definition.first_class_module.constraint[" ^ Int.to_string index ^ "].target")
            :: context))
            constrained_type;
          validate_core_type
            ~context:((("type_definition.first_class_module.constraint[" ^ Int.to_string index ^ "].replacement")
            :: context))
            replacement_type)
        package_type.constraints
  | Cst.TypeDefinition.Object { fields; _ } -> List.iteri
    (fun index ({ field_type; _ }: Cst.object_type_field) ->
      validate_core_type
        ~context:((("type_definition.object.field[" ^ Int.to_string index ^ "].type") :: context))
        field_type)
    fields
  | Cst.TypeDefinition.Record { fields; _ } -> List.iteri
    (fun index field ->
      validate_core_type
        ~context:((("type_definition.record.field[" ^ Int.to_string index ^ "].type") :: context))
        (Cst.RecordField.field_type field))
    fields
  | Cst.TypeDefinition.Variant { constructors; _ } ->
      List.iteri
        (fun index constructor ->
          Option.iter
            (validate_constructor_arguments
              ~context:((("type_definition.variant.constructor[" ^ Int.to_string index ^ "].arguments")
              :: context)))
            (Cst.VariantConstructor.arguments constructor);
          Option.iter
            (validate_core_type
              ~context:((("type_definition.variant.constructor[" ^ Int.to_string index ^ "].payload")
              :: context)))
            (Cst.VariantConstructor.payload_type constructor);
          Option.iter
            (validate_core_type
              ~context:((("type_definition.variant.constructor[" ^ Int.to_string index ^ "].result")
              :: context)))
            (Cst.VariantConstructor.result_type constructor))
        constructors
  | Cst.TypeDefinition.PolyVariant poly_variant -> validate_poly_variant
    ~context:(("type_definition.poly_variant" :: context))
    poly_variant

let validate_type_constraint = fun ~context ({ left; right; _ }: Cst.type_constraint) ->
  validate_core_type ~context:(("type_constraint.left" :: context)) left;
  validate_core_type ~context:(("type_constraint.right" :: context)) right

let rec validate_type_declaration = fun ~context (decl: Cst.TypeDeclaration.t) ->
  let type_definition = Cst.TypeDeclaration.type_definition decl in
  let manifest_alias = Cst.TypeDeclaration.manifest_alias decl in
  let constraints = Cst.TypeDeclaration.constraints decl in
  Option.iter (validate_core_type ~context:(("item.type_declaration.manifest_alias" :: context))) manifest_alias;
  validate_type_definition ~context:(("item.type_declaration" :: context)) type_definition;
  List.iteri
    (fun index constraint_ ->
      validate_type_constraint
        ~context:((("item.type_declaration.constraint[" ^ Int.to_string index ^ "]") :: context))
        constraint_)
    constraints;
  let rec validate_tail = fun index ->
    function
    | Some declaration ->
        validate_type_declaration
          ~context:((("item.type_declaration.next_and_declaration[" ^ Int.to_string index ^ "]") :: context))
          declaration;
        validate_tail (index + 1) (Cst.TypeDeclaration.next_and_declaration declaration)
    | None -> ()
  in
  validate_tail 0 (Cst.TypeDeclaration.next_and_declaration decl)

let validate_type_extension = fun ~context ({ constructors; _ }: Cst.TypeExtension.t) ->
  List.iteri
    (fun index constructor ->
      Option.iter
        (validate_constructor_arguments
          ~context:((("item.type_extension.constructor[" ^ Int.to_string index ^ "].arguments") :: context)))
        (Cst.VariantConstructor.arguments constructor);
      Option.iter
        (validate_core_type
          ~context:((("item.type_extension.constructor[" ^ Int.to_string index ^ "].payload") :: context)))
        (Cst.VariantConstructor.payload_type constructor);
      Option.iter
        (validate_core_type
          ~context:((("item.type_extension.constructor[" ^ Int.to_string index ^ "].result") :: context)))
        (Cst.VariantConstructor.result_type constructor))
    constructors

let validate_class_declaration = fun ~context (decl: Cst.ClassDeclaration.t) ->
  validate_class_type
    ~context:(("item.class_declaration.type" :: context))
    (Cst.ClassDeclaration.class_type decl)

let validate_class_definition = fun ~context (decl: Cst.ClassDefinition.t) ->
  Option.iter
    (validate_class_type ~context:(("item.class_definition.type" :: context)))
    (Cst.ClassDefinition.class_type decl);
  validate_class_expression
    ~context:(("item.class_definition.body" :: context))
    (Cst.ClassDefinition.class_body decl)

let validate_class_type_declaration = fun ~context (
  { class_type_body; _ }: Cst.class_type_declaration
) ->
  validate_class_type ~context:(("item.class_type_declaration.body" :: context)) class_type_body

let validate_module_type_declaration = fun ~context ({ module_type; _ }: Cst.ModuleTypeDeclaration.t) ->
  Option.iter (validate_module_type ~context:(("item.module_type_declaration" :: context))) module_type

let validate_open_statement = fun ~context stmt ->
  match Cst.OpenStatement.target stmt with
  | Cst.OpenStatement.Path _ -> ()
  | Cst.OpenStatement.ModuleExpression expr -> validate_module_expression
    ~context:(("item.open_statement.target" :: context))
    expr

let validate_structure_item = fun ~context ->
  function
  | Cst.StructureItem.TypeDeclaration decl ->
      validate_type_declaration ~context decl
  | Cst.StructureItem.TypeExtension decl ->
      validate_type_extension ~context decl
  | Cst.StructureItem.LetBinding { binding_pattern; value; and_binding; _ } ->
      validate_pattern ~context:(("item.let_binding.pattern" :: context)) binding_pattern;
      validate_expression ~context:(("item.let_binding.value" :: context)) value;
      let rec validate_and_bindings = fun index ->
        function
        | Some binding ->
            validate_pattern
              ~context:((("item.let_binding.and_bindings[" ^ Int.to_string index ^ "].pattern") :: context))
              (Cst.LetBinding.binding_pattern binding);
            validate_expression
              ~context:((("item.let_binding.and_bindings[" ^ Int.to_string index ^ "].value") :: context))
              (Cst.LetBinding.value binding);
            validate_and_bindings (index + 1) (Cst.LetBinding.and_binding binding)
        | None -> ()
      in
      validate_and_bindings 0 and_binding
  | Cst.StructureItem.Expression expr ->
      validate_expression ~context:(("item.expression" :: context)) expr
  | Cst.StructureItem.ClassDeclaration decl ->
      validate_class_definition ~context decl
  | Cst.StructureItem.ClassTypeDeclaration decl ->
      validate_class_type_declaration ~context decl
  | Cst.StructureItem.Attribute _
  | Cst.StructureItem.Extension _ ->
      ()
  | Cst.StructureItem.Docstring _ ->
      ()
  | Cst.StructureItem.Comment _ ->
      ()
  | Cst.StructureItem.ExternalDeclaration { type_; _ } ->
      validate_core_type ~context:(("item.external_declaration.type" :: context)) type_
  | Cst.StructureItem.ModuleTypeDeclaration decl ->
      validate_module_type_declaration ~context decl
  | Cst.StructureItem.OpenStatement stmt ->
      validate_open_statement ~context stmt
  | Cst.StructureItem.ModuleDeclaration _
  | Cst.StructureItem.IncludeStatement _
  | Cst.StructureItem.ExceptionDeclaration _ ->
      ()

let validate_signature_item = fun ~context ->
  function
  | Cst.SignatureItem.TypeDeclaration decl -> validate_type_declaration ~context decl
  | Cst.SignatureItem.TypeExtension decl -> validate_type_extension ~context decl
  | Cst.SignatureItem.Attribute _
  | Cst.SignatureItem.Extension _ -> ()
  | Cst.SignatureItem.Docstring _ -> ()
  | Cst.SignatureItem.Comment _ -> ()
  | Cst.SignatureItem.ClassDeclaration decl -> validate_class_declaration ~context decl
  | Cst.SignatureItem.ClassTypeDeclaration decl -> validate_class_type_declaration ~context decl
  | Cst.SignatureItem.ModuleTypeDeclaration decl -> validate_module_type_declaration ~context decl
  | Cst.SignatureItem.OpenStatement stmt -> validate_open_statement ~context stmt
  | Cst.SignatureItem.ValueDeclaration { type_; _ } -> validate_core_type
    ~context:(("item.value_declaration.type" :: context))
    type_
  | Cst.SignatureItem.ExternalDeclaration { type_; _ } -> validate_core_type
    ~context:(("item.external_declaration.type" :: context))
    type_
  | Cst.SignatureItem.ModuleDeclaration _
  | Cst.SignatureItem.IncludeStatement _
  | Cst.SignatureItem.ExceptionDeclaration _ -> ()

let validate_source_file = fun source_file ->
  (
    match source_file with
    | Cst.Implementation { items; _ } -> List.iteri
      (fun index item ->
        validate_structure_item ~context:[ "source_file.items[" ^ Int.to_string index ^ "]" ] item)
      items
    | Cst.Interface { items; _ } -> List.iteri
      (fun index item ->
        validate_signature_item ~context:[ "source_file.items[" ^ Int.to_string index ^ "]" ] item)
      items
  )

let attribute_is_declaration_attribute = fun (attribute: Cst.attribute) ->
  String.equal (attribute_sigil_text attribute) double_at_text

let rec take_structure_type_declaration_attributes = fun acc ->
  function
  | Cst.StructureItem.Attribute attribute :: rest when attribute_is_declaration_attribute attribute -> take_structure_type_declaration_attributes
    (attribute :: acc)
    rest
  | items -> (List.rev acc, items)

let rec attach_structure_type_declaration_attributes = function
  | Cst.StructureItem.TypeDeclaration decl :: rest ->
      let attributes, rest = take_structure_type_declaration_attributes [] rest in
      let decl =
        if attributes = [] then
          decl
        else
          { decl with attributes = decl.attributes @ attributes }
      in
      Cst.StructureItem.TypeDeclaration decl :: attach_structure_type_declaration_attributes rest
  | item :: rest ->
      item :: attach_structure_type_declaration_attributes rest
  | [] ->
      []

let rec take_signature_type_declaration_attributes = fun acc ->
  function
  | Cst.SignatureItem.Attribute attribute :: rest when attribute_is_declaration_attribute attribute -> take_signature_type_declaration_attributes
    (attribute :: acc)
    rest
  | items -> (List.rev acc, items)

let rec attach_signature_type_declaration_attributes = function
  | Cst.SignatureItem.TypeDeclaration decl :: rest ->
      let attributes, rest = take_signature_type_declaration_attributes [] rest in
      let decl =
        if attributes = [] then
          decl
        else
          { decl with attributes = decl.attributes @ attributes }
      in
      Cst.SignatureItem.TypeDeclaration decl :: attach_signature_type_declaration_attributes rest
  | item :: rest ->
      item :: attach_signature_type_declaration_attributes rest
  | [] ->
      []

let normalize_structure_items = fun ~source items ->
  items
  |> coalesce_structure_type_declaration_groups
  |> attach_structure_type_declaration_attributes
  |> normalize_structure_items_owned_trivia ~source

let normalize_signature_items = fun ~source items ->
  items
  |> coalesce_signature_type_declaration_groups
  |> attach_signature_type_declaration_attributes
  |> normalize_signature_items_owned_trivia ~source
  |> detach_multiline_signature_value_trailing_comments ~source

let lift = fun ~kind ~source ~tokens tree ->
  let cst =
    match kind with
    | `Implementation ->
        let syntax_node, items, phrase_separator_tokens, trailing_phrase_separator_tokens =
          build_source_file_body
            ~source
            ~tokens
            ~comment_item_of_comment:(fun comment -> Cst.StructureItem.Comment comment)
            ~docstring_item_of_docstring:(fun doc -> Cst.StructureItem.Docstring doc)
            ~syntax_node_of_item:Cst.StructureItem.syntax_node
            ~owned_trivia_spans_of_item:structure_item_owned_trivia_spans
            tree
            structure_items_from_node
        in
        let items = normalize_structure_items ~source items in
        Cst.Implementation {
          syntax_node;
          phrase_separator_tokens;
          trailing_phrase_separator_tokens;
          items
        }
    | `Interface ->
        let syntax_node, items, phrase_separator_tokens, trailing_phrase_separator_tokens =
          build_source_file_body
            ~source
            ~tokens
            ~comment_item_of_comment:(fun comment -> Cst.SignatureItem.Comment comment)
            ~docstring_item_of_docstring:(fun doc -> Cst.SignatureItem.Docstring doc)
            ~syntax_node_of_item:Cst.SignatureItem.syntax_node
            ~owned_trivia_spans_of_item:signature_item_owned_trivia_spans
            tree
            signature_items_from_node
        in
        let items = normalize_signature_items ~source items in
        Cst.Interface {
          syntax_node;
          phrase_separator_tokens;
          trailing_phrase_separator_tokens;
          items
        }
  in
  cst

let create_from_ceibo = fun ~kind ~source ~tokens tree ->
  match lift ~kind ~source ~tokens tree with
  | cst -> Ok cst
  | exception Bail error -> Error error

let structure_item_payload_nodes_from_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.STRUCT_EXPR -> direct_non_trivia_nodes node |> List.concat_map split_payload_item_nodes_from_node
  | _ -> split_payload_item_nodes_from_node node

let signature_item_payload_nodes_from_node = fun node ->
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.SIGNATURE -> direct_non_trivia_nodes node |> List.concat_map split_payload_item_nodes_from_node
  | Syntax_kind.SIG_EXPR -> direct_non_trivia_nodes node |> List.concat_map split_payload_item_nodes_from_node
  | _ -> split_payload_item_nodes_from_node node

let raw_structure_items_from_syntax_node = fun node ->
  let items =
    match Ceibo.Red.SyntaxNode.kind node with
    | Syntax_kind.STRUCT_EXPR -> build_items_from_payload_nodes
      ~comment_item_of_comment:(fun comment -> Cst.StructureItem.Comment comment)
      ~docstring_item_of_docstring:(fun doc -> Cst.StructureItem.Docstring doc)
      ~syntax_node_of_item:Cst.StructureItem.syntax_node
      ~owned_trivia_spans_of_item:structure_item_owned_trivia_spans
      ~container_node:node
      (structure_item_payload_nodes_from_node node)
      structure_items_from_node
    | _ -> structure_items_from_node node
  in
  match items with
  | items -> Ok items
  | exception Bail error -> Error error

let structure_items_from_syntax_node = fun node ->
  raw_structure_items_from_syntax_node node
  |> Result.map (normalize_structure_items ~source:(source_text_of_syntax_node_tree node))

let structure_items_from_syntax_node_with_source = fun ~source node ->
  raw_structure_items_from_syntax_node node |> Result.map (normalize_structure_items ~source)

let structure_items_from_syntax_nodes = fun nodes ->
  match build_items_from_payload_nodes
    ~comment_item_of_comment:(fun comment -> Cst.StructureItem.Comment comment)
    ~docstring_item_of_docstring:(fun doc -> Cst.StructureItem.Docstring doc)
    ~syntax_node_of_item:Cst.StructureItem.syntax_node
    ~owned_trivia_spans_of_item:structure_item_owned_trivia_spans
    nodes
    structure_items_from_node with
  | items -> (
      match nodes with
      | first :: _ -> Ok (normalize_structure_items ~source:(source_text_of_syntax_node_tree first) items)
      | [] -> Ok items
    )
  | exception Bail error -> Error error

let structure_items_of_module_expression = function
  | Cst.ModuleExpression.Structure { syntax_node; _ } -> structure_items_from_syntax_node syntax_node
  | module_expression -> Error {
    message = "module expression does not have a structural item stream";
    syntax_kind = Cst.syntax_kind (Cst.ModuleExpression.syntax_node module_expression);
    span = Ceibo.Red.SyntaxNode.span (Cst.ModuleExpression.syntax_node module_expression);
    context = [ "module_expression" ]
  }

let raw_signature_items_from_syntax_node = fun node ->
  let items =
    match Ceibo.Red.SyntaxNode.kind node with
    | Syntax_kind.SIGNATURE -> build_items_from_payload_nodes
      ~comment_item_of_comment:(fun comment -> Cst.SignatureItem.Comment comment)
      ~docstring_item_of_docstring:(fun doc -> Cst.SignatureItem.Docstring doc)
      ~syntax_node_of_item:Cst.SignatureItem.syntax_node
      ~owned_trivia_spans_of_item:signature_item_owned_trivia_spans
      ~container_node:node
      (signature_item_payload_nodes_from_node node)
      signature_items_from_node
    | Syntax_kind.SIG_EXPR -> build_items_from_payload_nodes
      ~comment_item_of_comment:(fun comment -> Cst.SignatureItem.Comment comment)
      ~docstring_item_of_docstring:(fun doc -> Cst.SignatureItem.Docstring doc)
      ~syntax_node_of_item:Cst.SignatureItem.syntax_node
      ~owned_trivia_spans_of_item:signature_item_owned_trivia_spans
      ~container_node:node
      (signature_item_payload_nodes_from_node node)
      signature_items_from_node
    | _ -> signature_items_from_node node
  in
  match items with
  | items -> Ok items
  | exception Bail error -> Error error

let signature_items_from_syntax_node = fun node ->
  raw_signature_items_from_syntax_node node
  |> Result.map (normalize_signature_items ~source:(source_text_of_syntax_node_tree node))

let signature_items_from_syntax_node_with_source = fun ~source node ->
  raw_signature_items_from_syntax_node node |> Result.map (normalize_signature_items ~source)

let signature_items_from_syntax_nodes = fun nodes ->
  match build_items_from_payload_nodes
    ~comment_item_of_comment:(fun comment -> Cst.SignatureItem.Comment comment)
    ~docstring_item_of_docstring:(fun doc -> Cst.SignatureItem.Docstring doc)
    ~syntax_node_of_item:Cst.SignatureItem.syntax_node
    ~owned_trivia_spans_of_item:signature_item_owned_trivia_spans
    nodes
    signature_items_from_node with
  | items -> (
      match nodes with
      | first :: _ -> Ok (normalize_signature_items ~source:(source_text_of_syntax_node_tree first) items)
      | [] -> Ok items
    )
  | exception Bail error -> Error error

let signature_items_of_module_type = function
  | Cst.ModuleType.Signature { signature_syntax_node; _ } -> signature_items_from_syntax_node signature_syntax_node
  | module_type -> Error {
    message = "module type does not have a structural item stream";
    syntax_kind = Cst.syntax_kind (Cst.ModuleType.syntax_node module_type);
    span = Ceibo.Red.SyntaxNode.span (Cst.ModuleType.syntax_node module_type);
    context = [ "module_type" ]
  }

let pattern_of_syntax_node = fun node ->
  match pattern_from_node node with
  | pattern -> Ok pattern
  | exception Bail error -> Error error

let expression_of_syntax_node = fun node ->
  match expression_from_node node with
  | expression -> Ok expression
  | exception Bail error -> Error error

open Std
open Std.Collections
module Doc = Doc

let blank_line = Doc.concat [ Doc.line; Doc.line ]

let arrow = Doc.concat [ Doc.space; Doc.arrow; Doc.space ]

let multiline_list_threshold = 10

let star = Doc.text "*"

let at = Doc.text "@"

let let_binding_group_items = fun (binding: Syn.Cst.let_binding) ->
  binding :: Syn.Cst.LetBinding.and_bindings binding

let rec binding_operator_group_items = fun (binding: Syn.Cst.binding_operator_binding) ->
  binding :: (
    match binding.and_binding with
    | Some next -> binding_operator_group_items next
    | None -> []
  )

type error_context_entry =
  | Context_label of string
  | Context_syntax_kind of Syn.SyntaxKind.t

type error = {
  message: string;
  context: error_context_entry list;
}

exception Unsupported of error

let error_to_string = fun err ->
  let context =
    err.context
    |> List.map
      (
        function
        | Context_label label -> label
        | Context_syntax_kind kind -> Syn.SyntaxKind.to_string kind
      )
  in
  match context with
  | [] -> err.message
  | context -> err.message ^ " [" ^ String.concat " > " context ^ "]"

let unsupported_with_context_entries = fun ?(context = []) message ->
  raise (Unsupported { message; context })

let unsupported = fun ?(context = []) message ->
  unsupported_with_context_entries ~context:(List.map (fun label -> Context_label label) context) message

let unsupported_syntax = fun ?(context = []) ~syntax_node message ->
  let kind = Syn.Cst.syntax_kind syntax_node in
  unsupported_with_context_entries
    ~context:((List.map (fun label -> Context_label label) context @ [ Context_syntax_kind kind ]))
    message

let extension_payload_context = [ "extension"; "payload" ]

type pending_trivia_entry =
  | TriviaComment of int * Doc.t
  | TriviaDocstring of int * bool * Doc.t
  | TriviaBreak of int * int

let doc_of_token = fun token -> Doc.text (Syn.Cst.Token.text token)

let doc_of_ident = fun ident ->
  Syn.Cst.Ident.segments ident |> List.map doc_of_token |> Doc.join (Doc.text ".")

let token_requires_parenthesized_value_name = fun (token: Syn.Cst.Token.t) ->
  Syn.Cst.Token.is_operator_like_name token

let render_declaration_name = fun name_tokens ->
  match name_tokens with
  | [] ->
      Doc.empty
  | [ name_token ] ->
      if token_requires_parenthesized_value_name name_token then
        Doc.concat [ Doc.lparen; Doc.space; doc_of_token name_token; Doc.space; Doc.rparen ]
      else
        doc_of_token name_token
  | operator_tokens ->
      let operator = operator_tokens |> List.map doc_of_token |> Doc.concat in
      Doc.concat [ Doc.lparen; Doc.space; operator; Doc.space; Doc.rparen ]

let render_value_declaration_name = fun (decl: Syn.Cst.value_declaration) ->
  render_declaration_name decl.name_tokens

let binding_has_explicit_fun_rhs = fun (binding: Syn.Cst.let_binding) ->
  List.is_empty binding.parameters && match binding.value with
  | Syn.Cst.Expression.Fun _ -> true
  | _ -> false

let phrase_separator_doc_of_tokens = fun tokens ->
  match tokens with
  | [] -> None
  | tokens -> Some (tokens |> List.map doc_of_token |> Doc.concat)

let is_section_docstring_text = fun comment_text ->
  let len = String.length comment_text in
  if len < 5 then
    false
  else
    let body = String.sub comment_text 3 (len - 5) |> String.trim in
    String.length body > 0 && (Char.equal body.[0] '{' || Char.equal body.[0] '#')

let pending_trivia_position = function
  | TriviaComment (position, _)
  | TriviaDocstring (position, _, _) -> position
  | TriviaBreak (position, _) -> position

let compare_pending_trivia_by_position = fun left right ->
  Int.compare (pending_trivia_position left) (pending_trivia_position right)

let render_pending_trivia = fun ?(strip_trailing_breaks = true) pending ->
  let break_doc break_count = List.init break_count (fun _ -> Doc.line) |> Doc.concat in
  let rec strip_trailing_blanks = function
    | [] ->
        []
    | [ TriviaBreak _ ] ->
        []
    | entry :: rest ->
        let rest = strip_trailing_blanks rest in
        (
          match entry, rest with
          | TriviaBreak _, [] -> []
          | _ -> entry :: rest
        )
  in
  let rec loop = fun acc separator ->
    function
    | [] ->
        acc
    | TriviaBreak (_, break_count) :: rest ->
        let separator = break_doc break_count in
        loop acc separator rest
    | (TriviaComment (_, doc) | TriviaDocstring (_, _, doc)) :: rest ->
        let acc =
          match acc with
          | None -> Some doc
          | Some current -> Some (Doc.concat [ current; separator; doc ])
        in
        loop acc Doc.line rest
  in
  let pending = List.sort compare_pending_trivia_by_position pending in
  let pending =
    if strip_trailing_breaks then
      strip_trailing_blanks pending
    else
      pending
  in
  let trailing_break =
    if strip_trailing_breaks then
      None
    else
      match List.rev pending with
      | TriviaBreak (_, break_count) :: _ -> Some (break_doc break_count)
      | _ -> None
  in
  match loop None Doc.line pending, trailing_break with
  | Some doc, Some trailing_break -> Some (Doc.concat [ doc; trailing_break ])
  | doc, None -> doc
  | None, Some _ -> None

let pending_entry_of_trivia = fun trivia ->
  let span = Syn.Cst.Token.span (Syn.Cst.Trivia.token trivia) in
  match trivia with
  | Syn.Cst.Trivia.Comment comment -> Some (TriviaComment (
    span.start,
    Doc.text (Syn.Cst.Comment.text comment)
  ))
  | Syn.Cst.Trivia.Docstring docstring -> Some (TriviaDocstring (
    span.start,
    Syn.Cst.Docstring.is_section docstring,
    Doc.text (Syn.Cst.Docstring.text docstring)
  ))

let pending_doc_of_trivia = fun trivia -> trivia |> List.filter_map pending_entry_of_trivia |> render_pending_trivia

let pending_doc_of_token_leading_trivia = fun token ->
  Syn.Cst.Token.leading_trivia token |> List.filter_map Syn.Cst.trivia_of_syntax_trivia |> pending_doc_of_trivia

let pending_doc_of_token_leading_trivia_after_fresh_line = fun ~after token ->
  let raw_leading_trivia = Ceibo.Red.SyntaxToken.leading_trivia token.Syn.Cst.Token.syntax_token in
  let rec collect = fun seen_newline acc ->
    function
    | [] -> List.rev acc
    | syntax_trivia :: rest ->
        let span = Ceibo.Red.SyntaxTrivia.span syntax_trivia in
        if span.start < after then
          collect seen_newline acc rest
        else
          match Syn.Cst.trivia_of_syntax_trivia syntax_trivia with
          | Some trivia when seen_newline || Int.equal after 0 ->
              collect false (trivia :: acc) rest
          | Some _ ->
              collect false acc rest
          | None ->
              let seen_newline =
                seen_newline
                || String.exists
                  (fun ch ->
                    Char.equal ch '\n')
                  (Ceibo.Red.SyntaxTrivia.text syntax_trivia)
              in
              collect seen_newline acc rest
  in
  raw_leading_trivia |> collect false [] |> pending_doc_of_trivia

let pending_doc_of_trivia_before_node = fun ~after syntax_node ->
  Syn.Cst.leading_trivia_before_node ~after syntax_node |> pending_doc_of_trivia

let doc_with_leading_trivia = fun trivia doc ->
  match trivia with
  | None -> doc
  | Some trivia -> Doc.concat [ trivia; Doc.line; doc ]

let doc_of_token_with_leading_trivia = fun token ->
  doc_of_token token |> doc_with_leading_trivia (pending_doc_of_token_leading_trivia token)

let doc_of_token_with_filtered_leading_trivia = fun ~after token ->
  doc_of_token token |> doc_with_leading_trivia
    (Syn.Cst.leading_trivia_after ~after token |> pending_doc_of_trivia)

let doc_of_declaration_token_with_filtered_leading_trivia = fun ~after token ->
  doc_of_token token
  |> doc_with_leading_trivia (pending_doc_of_token_leading_trivia_after_fresh_line ~after token)

let token_has_renderable_leading_trivia = fun token ->
  match pending_doc_of_token_leading_trivia token with
  | Some _ -> true
  | None -> false

type boundary_trivia_layout =
  | Inline_trivia of Doc.t
  | Block_trivia of Doc.t

let boundary_trivia_before_node = fun ~after syntax_node ->
  match Ceibo.Red.SyntaxNode.first_token syntax_node with
  | None -> None
  | Some first_token ->
      let raw_leading_trivia = Ceibo.Red.SyntaxToken.leading_trivia first_token
      |> List.filter
        (fun syntax_trivia -> (Ceibo.Red.SyntaxTrivia.span syntax_trivia).start >= after) in
      let trivia_doc = raw_leading_trivia |> List.filter_map Syn.Cst.trivia_of_syntax_trivia |> pending_doc_of_trivia in
      let has_newline =
        raw_leading_trivia
        |> List.exists
          (fun syntax_trivia ->
            String.exists
              (fun ch ->
                Char.equal ch '\n')
              (Ceibo.Red.SyntaxTrivia.text syntax_trivia))
      in
      match trivia_doc with
      | None -> None
      | Some doc when has_newline -> Some (Block_trivia doc)
      | Some doc -> Some (Inline_trivia doc)

let doc_with_boundary_trivia = fun trivia doc ->
  match trivia with
  | None -> doc
  | Some (Inline_trivia trivia) -> Doc.concat [ trivia; Doc.space; doc ]
  | Some (Block_trivia trivia) -> Doc.concat [ trivia; Doc.line; doc ]

let doc_with_trailing_trivia = fun doc trivia ->
  match trivia with
  | None -> doc
  | Some trivia -> Doc.concat [ doc; Doc.line; trivia ]

let separator_before_first_owned_trivia = fun ?(after_rendered_body = false) trivia ->
  match after_rendered_body with
  | false -> Doc.empty
  | true -> (
      match trivia with
      | Syn.Cst.Trivia.Comment _ -> Doc.space
      | Syn.Cst.Trivia.Docstring _ -> Doc.line
    )

let separator_between_owned_trivia = fun _previous _current -> Doc.line

let doc_of_owned_trivia = fun ?(after_rendered_body = false) trivia ->
  let trivia =
    trivia
    |> List.sort
      (fun left right ->
        Int.compare
          ((Syn.Cst.Token.span (Syn.Cst.Trivia.token left)).start)
          ((Syn.Cst.Token.span (Syn.Cst.Trivia.token right)).start))
  in
  let trivia_doc = function
    | Syn.Cst.Trivia.Comment comment -> Doc.text (Syn.Cst.Comment.text comment)
    | Syn.Cst.Trivia.Docstring docstring -> Doc.text (Syn.Cst.Docstring.text docstring)
  in
  let rec loop = fun acc previous ->
    function
    | [] -> acc
    | trivia :: rest ->
        let separator =
          match previous, acc with
          | _, None -> separator_before_first_owned_trivia ~after_rendered_body trivia
          | Some previous, Some _ -> separator_between_owned_trivia previous trivia
          | None, Some _ -> separator_between_owned_trivia trivia trivia
        in
        let piece = Doc.concat [ separator; trivia_doc trivia ] in
        let acc =
          match acc with
          | None -> Some piece
          | Some current -> Some (Doc.concat [ current; piece ])
        in
        loop acc (Some trivia) rest
  in
  loop None None trivia

let kw_module = Doc.text "module"

let kw_let = Doc.text "let"

let kw_rec = Doc.text "rec"

let kw_and = Doc.text "and"

let kw_in = Doc.text "in"

let kw_else = Doc.text "else"

let kw_with = Doc.text "with"

let kw_when = Doc.text "when"

let kw_function = Doc.text "function"

let kw_fun = Doc.text "fun"

let kw_open = Doc.text "open"

let kw_val = Doc.text "val"

let kw_type = Doc.text "type"

let kw_class = Doc.text "class"

let kw_external = Doc.text "external"

let kw_constraint = Doc.text "constraint"

let kw_mutable = Doc.text "mutable"

let kw_private = Doc.text "private"

let kw_assert = Doc.text "assert"

let kw_lazy = Doc.text "lazy"

let kw_while = Doc.text "while"

let kw_for = Doc.text "for"

let kw_do = Doc.text "do"

let kw_done = Doc.text "done"

let kw_new = Doc.text "new"

let kw_object = Doc.text "object"

let kw_method = Doc.text "method"

let kw_inherit = Doc.text "inherit"

let kw_initializer = Doc.text "initializer"

let kw_virtual = Doc.text "virtual"

let kw_end = Doc.text "end"

let hash = Doc.text "#"

let coercion_arrow = Doc.text ":>"

let object_override_open = Doc.text "{<"

let object_override_close = Doc.text ">}"

let join_map = fun separator f ->
  function
  | [] -> Doc.empty
  | first :: rest -> Doc.concat
    (f first :: List.map (fun item -> Doc.concat [ separator; f item ]) rest)

let group_digits_from_left = fun ~group_size digits ->
  let digits = String.split_on_char '_' digits |> String.concat "" in
  let length = String.length digits in
  if length <= group_size then
    digits
  else
    let buffer = IO.Buffer.create (length + length / group_size) in
    let rec loop index =
      if index >= length then
        IO.Buffer.contents buffer
      else
        (
          if index > 0 then
            IO.Buffer.add_char buffer '_';
          let chunk_size = Int.min group_size (length - index) in
          IO.Buffer.add_string buffer (String.sub digits index chunk_size);
          loop (index + chunk_size)
        )
    in
    loop 0

let group_digits_from_right = fun ~group_size digits ->
  let digits = String.split_on_char '_' digits |> String.concat "" in
  let length = String.length digits in
  if length <= group_size then
    digits
  else
    let first_group_size =
      match length mod group_size with
      | 0 -> group_size
      | remainder -> remainder
    in
    let buffer = IO.Buffer.create (length + length / group_size) in
    IO.Buffer.add_string buffer (String.sub digits 0 first_group_size);
    let rec loop index =
      if index >= length then
        IO.Buffer.contents buffer
      else (
        IO.Buffer.add_char buffer '_';
        IO.Buffer.add_string buffer (String.sub digits index group_size);
        loop (index + group_size)
      )
    in
    loop first_group_size

let render_integer_constant = fun (literal: Syn.Cst.integer_constant) ->
  let prefix =
    match literal.base with
    | Syn.Cst.Decimal -> Option.unwrap_or literal.prefix ~default:""
    | Syn.Cst.Hexadecimal -> "0x"
    | Syn.Cst.Octal -> "0o"
    | Syn.Cst.Binary -> "0b"
  in
  let digits =
    match literal.base with
    | Syn.Cst.Decimal
    | Syn.Cst.Octal -> group_digits_from_right ~group_size:3 literal.digits
    | Syn.Cst.Binary -> group_digits_from_right ~group_size:4 literal.digits
    | Syn.Cst.Hexadecimal -> literal.digits
    |> String.lowercase_ascii
    |> group_digits_from_right ~group_size:4
  in
  let suffix = Option.unwrap_or literal.suffix ~default:"" in
  prefix ^ digits ^ suffix

let render_float_constant = fun (literal: Syn.Cst.float_constant) ->
  let exponent =
    match literal.exponent with
    | None -> ""
    | Some exponent ->
        let sign =
          match exponent.sign with
          | None -> ""
          | Some Syn.Cst.Positive -> "+"
          | Some Syn.Cst.Negative -> "-"
        in
        exponent.marker ^ sign ^ exponent.digits
  in
  let suffix = Option.unwrap_or literal.suffix ~default:"" in
  let normalized_integral_digits = String.split_on_char '_' literal.integral_digits
  |> String.concat "" in
  let integral_digits =
    if String.length normalized_integral_digits >= 8 then
      group_digits_from_right ~group_size:3 normalized_integral_digits
    else
      normalized_integral_digits
  in
  let fractional_digits = group_digits_from_left ~group_size:3 literal.fractional_digits in
  integral_digits ^ "." ^ fractional_digits ^ exponent ^ suffix

let render_literal = function
  | Syn.Cst.Literal.Int literal -> Doc.concat
    [
      Option.unwrap_or (Option.map doc_of_token literal.sign_token) ~default:Doc.empty;
      Doc.text (render_integer_constant literal)
    ]
  | Syn.Cst.Literal.Float literal -> Doc.concat
    [
      Option.unwrap_or (Option.map doc_of_token literal.sign_token) ~default:Doc.empty;
      Doc.text (render_float_constant literal)
    ]
  | Syn.Cst.Literal.String literal -> doc_of_token literal.literal_token
  | Syn.Cst.Literal.Char literal -> doc_of_token literal.literal_token
  | Syn.Cst.Literal.Bool literal ->
      Doc.text
        (
          if literal.value then
            "true"
          else
            "false"
        )
  | Syn.Cst.Literal.Unit _ -> Doc.text "()"

let render_type_binder = function
  | Syn.Cst.TypeBinder.Quoted binder -> Doc.text
    (Syn.Cst.TypeBinder.text (Syn.Cst.TypeBinder.Quoted binder))
  | Syn.Cst.TypeBinder.Bare binder -> Doc.text
    (Syn.Cst.TypeBinder.text (Syn.Cst.TypeBinder.Bare binder))

let render_arrow_label = function
  | None -> Doc.empty
  | Some (Syn.Cst.ArrowLabel.Named { sigil_token; label_token; colon_token }) -> Doc.concat
    [
      Option.unwrap_or (Option.map doc_of_token sigil_token) ~default:Doc.empty;
      doc_of_token label_token;
      doc_of_token colon_token
    ]
  | Some (Syn.Cst.ArrowLabel.OptionalNamed { sigil_token; label_token; colon_token }) -> Doc.concat
    [ doc_of_token sigil_token; doc_of_token label_token; doc_of_token colon_token ]

let rec core_type_needs_parens_in_application = function
  | Syn.Cst.CoreType.Arrow _
  | Syn.Cst.CoreType.Tuple _
  | Syn.Cst.CoreType.PolyVariant _
  | Syn.Cst.CoreType.Record _
  | Syn.Cst.CoreType.Object _
  | Syn.Cst.CoreType.Alias _ -> true
  | Syn.Cst.CoreType.Attribute { type_; _ } -> core_type_needs_parens_in_application type_
  | Syn.Cst.CoreType.Parenthesized _ -> false
  | _ -> false

let render_type_parameter = fun parameter ->
  let variance =
    match Syn.Cst.TypeParameter.variance parameter with
    | None -> Doc.empty
    | Some (Syn.Cst.TypeParameterVariance.Covariant { marker_token }) -> doc_of_token marker_token
    | Some (Syn.Cst.TypeParameterVariance.Contravariant { marker_token }) -> doc_of_token marker_token
  in
  let injective =
    match Syn.Cst.TypeParameter.injectivity_token parameter with
    | Some injectivity_token -> doc_of_token injectivity_token
    | None -> Doc.empty
  in
  let variable =
    match Syn.Cst.TypeParameter.type_variable parameter with
    | Some type_variable -> Doc.text (Syn.Cst.TypeVariable.text type_variable)
    | None -> Doc.text "_"
  in
  Doc.concat [ variance; injective; variable ]

let render_type_parameters = fun parameters ->
  match parameters with
  | [] -> Doc.empty
  | [ parameter ] -> render_type_parameter parameter
  | parameters when List.length parameters > 6 -> Doc.concat
    [
      Doc.lparen;
      Doc.line;
      Doc.indent 2 (join_map (Doc.concat [ Doc.comma; Doc.line ]) render_type_parameter parameters);
      Doc.line;
      Doc.rparen
    ]
  | parameters -> Doc.concat
    [
      Doc.lparen;
      join_map (Doc.concat [ Doc.comma; Doc.space ]) render_type_parameter parameters;
      Doc.rparen
    ]

let rec core_type_arrow_arity = function
  | Syn.Cst.CoreType.Arrow { result_type; _ } -> 1 + core_type_arrow_arity result_type
  | Syn.Cst.CoreType.Parenthesized { inner; _ } -> core_type_arrow_arity inner
  | _ -> 0

let rec core_type_has_labeled_arrow = function
  | Syn.Cst.CoreType.Arrow { label=Some _; _ } -> true
  | Syn.Cst.CoreType.Arrow { result_type; _ } -> core_type_has_labeled_arrow result_type
  | Syn.Cst.CoreType.Parenthesized { inner; _ } -> core_type_has_labeled_arrow inner
  | _ -> false

let rec core_type_prefers_multiline = function
  | Syn.Cst.CoreType.Arrow arrow -> core_type_arrow_arity (Syn.Cst.CoreType.Arrow arrow) >= 5
  || core_type_prefers_multiline arrow.parameter_type
  || core_type_prefers_multiline arrow.result_type
  | Syn.Cst.CoreType.Tuple { elements; _ } -> List.length elements > 3
  || List.exists core_type_prefers_multiline elements
  | Syn.Cst.CoreType.PolyVariant _
  | Syn.Cst.CoreType.Record _
  | Syn.Cst.CoreType.Object _ -> true
  | Syn.Cst.CoreType.Parenthesized { inner; _ } -> core_type_prefers_multiline inner
  | Syn.Cst.CoreType.Alias { type_; _ } -> core_type_prefers_multiline type_
  | Syn.Cst.CoreType.Attribute { type_; _ } -> core_type_prefers_multiline type_
  | Syn.Cst.CoreType.Constr { arguments; _ } -> List.exists core_type_prefers_multiline arguments
  | _ -> false

let rec core_type_is_atomic = function
  | Syn.Cst.CoreType.Wildcard _
  | Syn.Cst.CoreType.Var _ -> true
  | Syn.Cst.CoreType.Constr { arguments=[]; _ } -> true
  | Syn.Cst.CoreType.Constr { arguments=[ Syn.Cst.CoreType.Tuple { elements; _ } ]; _ } -> List.for_all
    core_type_is_atomic
    elements
  | Syn.Cst.CoreType.Constr { arguments=[ argument ]; _ } -> core_type_is_atomic argument
  | Syn.Cst.CoreType.Parenthesized { inner; _ } -> core_type_is_atomic inner
  | Syn.Cst.CoreType.Attribute { type_; _ } -> core_type_is_atomic type_
  | _ -> false

let rec render_first_class_module_type_constraint = fun ~keyword (
  constraint_: Syn.Cst.module_type_constraint
) ->
  let separator = Doc.concat [ Doc.space; doc_of_token constraint_.separator_token; Doc.space ] in
  Doc.concat
    [
      keyword;
      Doc.space;
      kw_type;
      Doc.space;
      render_core_type constraint_.constrained_type;
      separator;
      render_core_type constraint_.replacement_type;
    ]

and render_first_class_functor_parameter = fun (
  { name_token; colon_token; module_type; _ }: Syn.Cst.functor_parameter
) ->
  Doc.concat
    [
      Doc.lparen;
      doc_of_token name_token;
      Doc.space;
      doc_of_token colon_token;
      Doc.space;
      render_first_class_module_type_doc module_type;
      Doc.rparen;
    ]

and render_package_type_doc = fun (
  { module_type_path; constraints; attribute; _ }: Syn.Cst.package_type
) ->
  let first, rest =
    match constraints with
    | [] -> (Doc.empty, [])
    | first :: rest -> (render_first_class_module_type_constraint ~keyword:kw_with first, rest)
  in
  let base =
    if List.is_empty constraints then
      doc_of_ident module_type_path
    else
      Doc.concat
        (doc_of_ident module_type_path
        :: Doc.space
        :: first
        :: List.map
          (fun constraint_ ->
            Doc.concat
              [ Doc.space; render_first_class_module_type_constraint ~keyword:kw_and constraint_; ])
          rest)
  in
  match attribute with
  | Some attribute -> Doc.concat [ base; Doc.space; render_attribute attribute ]
  | None -> base

and render_core_type_extension_doc = fun (extension: Syn.Cst.extension) ->
  let payload_doc =
    match extension.payload with
    | None -> Doc.empty
    | Some (Syn.Cst.Payload.Opaque { tokens }) -> Doc.concat (List.map doc_of_token tokens)
  in
  Doc.concat
    [
      Doc.lbracket;
      extension.sigil_tokens |> List.map doc_of_token |> Doc.concat;
      doc_of_ident extension.name;
      payload_doc;
      Doc.rbracket;
    ]

and render_first_class_module_type_extension_doc = fun (extension: Syn.Cst.extension) ->
  let payload_doc =
    match extension.payload with
    | None -> Doc.empty
    | Some (Syn.Cst.Payload.Opaque { tokens }) -> Doc.concat (List.map doc_of_token tokens)
  in
  Doc.concat
    [
      Doc.lbracket;
      extension.sigil_tokens |> List.map doc_of_token |> Doc.concat;
      doc_of_ident extension.name;
      payload_doc;
      Doc.rbracket;
    ]

and render_shared_attribute_payload_doc = fun (attribute: Syn.Cst.attribute) ->
  match attribute.payload with
  | None -> Doc.empty
  | Some (Syn.Cst.Payload.Opaque { tokens }) -> Doc.concat
    (List.map (fun token -> Doc.text (Syn.Cst.Token.full_text token)) tokens)

and render_attribute_doc = fun ~floating (attribute: Syn.Cst.attribute) ->
  Doc.concat
    [
      Doc.lbracket;
      attribute.sigil_tokens |> List.map doc_of_token |> Doc.concat;
      doc_of_ident attribute.name;
      render_shared_attribute_payload_doc attribute;
      Doc.rbracket;
    ]

and render_attribute = fun attribute -> render_attribute_doc ~floating:false attribute

and render_floating_attribute = fun attribute -> render_attribute_doc ~floating:true attribute

and doc_of_syntax_node_full_text = fun syntax_node ->
  let pieces =
    Ceibo.Red.SyntaxNode.tokens syntax_node
    |> List.concat_map
      (fun syntax_token ->
        let leading = Ceibo.Red.SyntaxToken.leading_trivia syntax_token
        |> List.map Ceibo.Red.SyntaxTrivia.text in
        leading @ [ Ceibo.Red.SyntaxToken.text syntax_token ])
  in
  Doc.text (String.concat "" pieces)

and render_first_class_module_type_doc = function
  | Syn.Cst.ModuleType.Path path ->
      doc_of_ident path
  | Syn.Cst.ModuleType.TypeOf { of_token; module_path; _ } ->
      Doc.concat
        [
          kw_module;
          Doc.space;
          kw_type;
          Doc.space;
          doc_of_token of_token;
          Doc.space;
          doc_of_ident module_path
        ]
  | Syn.Cst.ModuleType.Signature { syntax_node; _ } ->
      doc_of_syntax_node_full_text syntax_node
  | Syn.Cst.ModuleType.Functor { parameters; result; _ } ->
      Doc.concat
        [
          Doc.text "functor";
          Doc.space;
          Doc.join Doc.space (List.map render_first_class_functor_parameter parameters);
          Doc.space;
          Doc.arrow;
          Doc.space;
          render_first_class_module_type_doc result;
        ]
  | Syn.Cst.ModuleType.With { base; constraints; _ } ->
      let first, rest =
        match constraints with
        | [] -> (Doc.empty, [])
        | first :: rest -> (render_first_class_module_type_constraint ~keyword:kw_with first, rest)
      in
      Doc.concat
        (render_first_class_module_type_doc base
        :: Doc.space
        :: first
        :: List.map
          (fun constraint_ ->
            Doc.concat
              [ Doc.space; render_first_class_module_type_constraint ~keyword:kw_and constraint_; ])
          rest)
  | Syn.Cst.ModuleType.Parenthesized { opening_token; inner; closing_token; _ } ->
      Doc.concat
        [
          doc_of_token opening_token;
          render_first_class_module_type_doc inner;
          doc_of_token closing_token
        ]
  | Syn.Cst.ModuleType.Attribute { module_type; attribute; _ } ->
      Doc.concat
        [ render_first_class_module_type_doc module_type; Doc.space; render_attribute attribute ]
  | Syn.Cst.ModuleType.Extension extension ->
      render_first_class_module_type_extension_doc extension

and render_core_type = function
  | Syn.Cst.CoreType.Wildcard { wildcard_token; _ } ->
      doc_of_token wildcard_token
  | Syn.Cst.CoreType.Var { sigil_token; name_token; _ } ->
      Doc.concat
        [
          Option.unwrap_or (Option.map doc_of_token sigil_token) ~default:Doc.empty;
          doc_of_token name_token
        ]
  | Syn.Cst.CoreType.Constr { constructor_path; arguments; _ } ->
      let head = doc_of_ident constructor_path in
      (
        match arguments with
        | [] ->
            head
        | [ Syn.Cst.CoreType.Tuple { elements; _ } ] ->
            Doc.concat
              [
                Doc.lparen;
                join_map (Doc.concat [ Doc.comma; Doc.space ]) render_core_type elements;
                Doc.rparen;
                Doc.space;
                head
              ]
        | [ argument ] ->
            let argument =
              if core_type_needs_parens_in_application argument then
                Doc.concat [ Doc.lparen; render_core_type argument; Doc.rparen ]
              else
                render_core_type argument
            in
            Doc.concat [ argument; Doc.space; head ]
        | arguments ->
            Doc.concat
              [
                Doc.lparen;
                join_map (Doc.concat [ Doc.comma; Doc.space ]) render_core_type arguments;
                Doc.rparen;
                Doc.space;
                head
              ]
      )
  | Syn.Cst.CoreType.Class { hash_token; class_path; arguments; _ } ->
      let head = Doc.concat [ doc_of_token hash_token; doc_of_ident class_path ] in
      (
        match arguments with
        | [] ->
            head
        | [ Syn.Cst.CoreType.Tuple { elements; _ } ] ->
            Doc.concat
              [
                Doc.lparen;
                join_map (Doc.concat [ Doc.comma; Doc.space ]) render_core_type elements;
                Doc.rparen;
                Doc.space;
                head
              ]
        | [ argument ] ->
            let argument =
              if core_type_needs_parens_in_application argument then
                Doc.concat [ Doc.lparen; render_core_type argument; Doc.rparen ]
              else
                render_core_type argument
            in
            Doc.concat [ argument; Doc.space; head ]
        | arguments ->
            Doc.concat
              [
                Doc.lparen;
                join_map (Doc.concat [ Doc.comma; Doc.space ]) render_core_type arguments;
                Doc.rparen;
                Doc.space;
                head
              ]
      )
  | Syn.Cst.CoreType.Alias { type_; sigil_token; name_token; _ } ->
      let alias_doc =
        match sigil_token with
        | Some sigil_token -> Doc.concat [ doc_of_token sigil_token; doc_of_token name_token ]
        | None -> doc_of_token name_token
      in
      Doc.concat [ render_core_type type_; Doc.space; Doc.text "as"; Doc.space; alias_doc ]
  | Syn.Cst.CoreType.Attribute { type_; attribute; _ } ->
      Doc.concat [ render_core_type type_; Doc.space; render_attribute attribute ]
  | Syn.Cst.CoreType.Poly { type_keyword_token; binders; body; _ } ->
      let prefix =
        match type_keyword_token with
        | Some type_keyword_token -> Doc.concat [ doc_of_token type_keyword_token; Doc.space ]
        | None -> Doc.empty
      in
      Doc.concat
        [
          prefix;
          join_map (Doc.concat [ Doc.space ]) render_type_binder binders;
          Doc.text ".";
          Doc.space;
          render_core_type body
        ]
  | Syn.Cst.CoreType.Arrow { label; parameter_type; result_type; _ } ->
      let render_arrow_parameter label parameter_type =
        let parameter_type =
          match parameter_type with
          | Syn.Cst.CoreType.Arrow _ -> Doc.concat
            [ Doc.lparen; render_core_type parameter_type; Doc.rparen ]
          | _ -> render_core_type parameter_type
        in
        Doc.concat [ render_arrow_label label; parameter_type ]
      in
      let rec collect params label parameter_type result_type =
        let params = params @ [ render_arrow_parameter label parameter_type ] in
        match result_type with
        | Syn.Cst.CoreType.Arrow { label; parameter_type; result_type; _ } -> collect
          params
          label
          parameter_type
          result_type
        | result_type -> (params, render_core_type result_type)
      in
      let parameters, result = collect [] label parameter_type result_type in
      let parts = parameters @ [ result ] in
      Doc.group (join_map (Doc.concat [ Doc.space; Doc.arrow; Doc.break () ]) (fun doc -> doc) parts)
  | Syn.Cst.CoreType.Tuple { elements; _ } ->
      Doc.group
        (join_map (Doc.concat [ Doc.space; star; Doc.break ~flat:" " () ]) render_core_type elements)
  | Syn.Cst.CoreType.Parenthesized { inner; _ } ->
      Doc.concat [ Doc.lparen; render_core_type inner; Doc.rparen ]
  | Syn.Cst.CoreType.PolyVariant poly_variant ->
      render_poly_variant_type poly_variant
  | Syn.Cst.CoreType.Record { opening_token; fields; closing_token; _ } ->
      render_record_type ~opening_token ~closing_token fields
  | Syn.Cst.CoreType.FirstClassModule { opening_token; package_type; closing_token; _ } ->
      Doc.concat
        [
          doc_of_token opening_token;
          kw_module;
          Doc.space;
          render_package_type_doc package_type;
          doc_of_token closing_token;
        ]
  | Syn.Cst.CoreType.Object { opening_token; fields; closing_token; _ } ->
      render_object_type ~opening_token ~closing_token fields
  | Syn.Cst.CoreType.Extension extension ->
      render_core_type_extension_doc extension

and render_record_core_type_field = fun (field: Syn.Cst.record_type_field) ->
  let type_doc = render_core_type field.field_type in
  let separator =
    if core_type_prefers_multiline field.field_type then
      Doc.line
    else
      Doc.break ()
  in
  let prefix =
    if Option.is_some field.mutable_token then
      let mutable_doc =
        match field.mutable_token with
        | Some mutable_token -> doc_of_token mutable_token
        | None -> kw_mutable
      in
      Doc.concat [ mutable_doc; Doc.space; doc_of_token field.field_name ]
    else
      doc_of_token field.field_name
  in
  render_tight_doc_rhs prefix (doc_of_token field.colon_token) (Doc.concat [ separator; type_doc ])

and render_tight_token_rhs = fun head token rhs ->
  Doc.group
    (Doc.concat [ head; doc_of_token token; Doc.indent 2 (Doc.concat [ Doc.break (); rhs ]) ])

and render_tight_doc_rhs = fun head separator rhs ->
  Doc.group (Doc.concat [ head; separator; Doc.indent 2 (Doc.concat [ Doc.break (); rhs ]) ])

and render_tight_colon_rhs = fun head colon_token rhs -> render_tight_token_rhs head colon_token rhs

and render_tight_colon_block_rhs = fun head colon_token rhs ->
  Doc.concat [ head; doc_of_token colon_token; Doc.space; rhs ]

and render_parenthesized_tight_token_rhs = fun head token rhs ->
  Doc.group
    (Doc.concat
      [
        Doc.lparen;
        Doc.indent 2 (Doc.concat [ Doc.break ~flat:"" (); render_tight_token_rhs head token rhs ]);
        Doc.break ~flat:"" ();
        Doc.rparen;
      ])

and render_parenthesized_tight_doc_rhs = fun head separator rhs ->
  Doc.group
    (Doc.concat
      [
        Doc.lparen;
        Doc.indent 2 (Doc.concat [ Doc.break ~flat:"" (); render_tight_doc_rhs head separator rhs ]);
        Doc.break ~flat:"" ();
        Doc.rparen;
      ])

and render_member_head = fun keyword modifier_tokens name_token ->
  Doc.join
    Doc.space
    ([ keyword ] @ List.map doc_of_token modifier_tokens @ [ doc_of_token name_token ])

and render_record_type_field_separator = fun (field: Syn.Cst.record_type_field) ->
  match field.semicolon_token with
  | Some semicolon_token -> doc_of_token semicolon_token
  | None -> Doc.semi

and render_record_type = fun ~opening_token ~closing_token fields ->
  let rec render_fields = function
    | [] -> Doc.empty
    | [ (field: Syn.Cst.record_type_field) ] -> Doc.concat
      [ render_record_core_type_field field; render_record_type_field_separator field ]
    | (field: Syn.Cst.record_type_field) :: rest -> Doc.concat
      [
        render_record_core_type_field field;
        render_record_type_field_separator field;
        Doc.line;
        render_fields rest;
      ]
  in
  Doc.concat
    [
      doc_of_token opening_token;
      Doc.line;
      Doc.indent 2 (render_fields fields);
      Doc.line;
      doc_of_token closing_token
    ]

and render_record_definition_field = fun (field: Syn.Cst.RecordField.t) ->
  let field_type = Syn.Cst.RecordField.field_type field in
  let type_doc = render_core_type field_type in
  let separator =
    if core_type_prefers_multiline field_type then
      Doc.line
    else
      Doc.break ()
  in
  let prefix =
    match Syn.Cst.RecordField.mutable_token field with
    | Some mutable_token -> Doc.concat
      [
        doc_of_token mutable_token;
        Doc.space;
        doc_of_token (Syn.Cst.RecordField.field_name_token field)
      ]
    | None -> doc_of_token (Syn.Cst.RecordField.field_name_token field)
  in
  render_tight_doc_rhs
    prefix
    (doc_of_token (Syn.Cst.RecordField.colon_token field))
    (Doc.concat [ separator; type_doc ])

and render_record_definition_field_separator = fun (field: Syn.Cst.RecordField.t) ->
  match field.semicolon_token with
  | Some semicolon_token -> doc_of_token semicolon_token
  | None -> Doc.semi

and render_record_definition_field_entry = fun ?(include_trailing_semicolon = true) (
  field: Syn.Cst.RecordField.t
) ->
  let body =
    if include_trailing_semicolon then
      Doc.concat
        [ render_record_definition_field field; render_record_definition_field_separator field ]
    else
      render_record_definition_field field
  in
  body

and render_record_definition_body_item = fun ~remaining ->
  function
  | Syn.CstBuilder.RecordField field ->
      let _ = remaining in
      render_record_definition_field_entry ~include_trailing_semicolon:true field
  | Syn.CstBuilder.Comment comment ->
      Doc.text (Syn.Cst.Comment.text comment)
  | Syn.CstBuilder.Docstring docstring ->
      Doc.text (Syn.Cst.Docstring.text docstring)
  | Syn.CstBuilder.TrailingComment comment ->
      Doc.text (Syn.Cst.Comment.text comment)
  | Syn.CstBuilder.TrailingDocstring docstring ->
      Doc.text (Syn.Cst.Docstring.text docstring)

and render_record_definition_body = fun fields ->
  let items = Syn.CstBuilder.record_field_items_of_fields fields in
  let rec attach_to_last last extra =
    match List.rev last with
    | [] -> [ extra ]
    | previous :: rest -> List.rev (Doc.concat [ previous; Doc.space; extra ] :: rest)
  in
  let rec render_items = fun rendered ->
    function
    | [] ->
        rendered
    | Syn.CstBuilder.RecordField field :: rest ->
        let field_doc = render_record_definition_body_item
          ~remaining:rest
          (Syn.CstBuilder.RecordField field) in
        render_items (rendered @ [ field_doc ]) rest
    | Syn.CstBuilder.TrailingComment comment :: rest ->
        let comment_doc = render_record_definition_body_item
          ~remaining:rest
          (Syn.CstBuilder.TrailingComment comment) in
        render_items (attach_to_last rendered (Doc.concat [ Doc.space; comment_doc ])) rest
    | Syn.CstBuilder.TrailingDocstring docstring :: rest ->
        let docstring_doc = render_record_definition_body_item
          ~remaining:rest
          (Syn.CstBuilder.TrailingDocstring docstring) in
        render_items (attach_to_last rendered (Doc.concat [ Doc.space; docstring_doc ])) rest
    | Syn.CstBuilder.Comment comment :: rest ->
        let comment_doc = render_record_definition_body_item
          ~remaining:rest
          (Syn.CstBuilder.Comment comment) in
        render_items (rendered @ [ comment_doc ]) rest
    | Syn.CstBuilder.Docstring docstring :: rest ->
        let docstring_doc = render_record_definition_body_item
          ~remaining:rest
          (Syn.CstBuilder.Docstring docstring) in
        render_items (rendered @ [ docstring_doc ]) rest
  in
  items |> render_items [] |> Doc.join Doc.line

and render_record_definition = fun fields ->
  let body = render_record_definition_body fields in
  Doc.concat [ Doc.lbrace; Doc.line; Doc.indent 2 body; Doc.line; Doc.rbrace ]

and render_tokenized_record_definition = fun ~opening_token ~closing_token fields ->
  let body = fields |> render_record_definition_body in
  Doc.concat
    [ doc_of_token opening_token; Doc.line; Doc.indent 2 body; Doc.line; doc_of_token closing_token ]

and render_inline_record_definition = fun fields ->
  let rec render_fields = function
    | [] -> Doc.empty
    | [ field ] -> render_record_definition_field field
    | field :: rest -> Doc.concat
      [
        render_record_definition_field field;
        render_record_definition_field_separator field;
        Doc.break ~flat:" " ();
        render_fields rest;
      ]
  in
  if List.is_empty fields then
    Doc.concat [ Doc.lbrace; Doc.rbrace ]
  else
    Doc.group
      (Doc.concat
        [
          Doc.lbrace;
          Doc.indent 2 (Doc.concat [ Doc.break ~flat:" " (); render_fields fields ]);
          Doc.break ~flat:" " ();
          Doc.rbrace
        ])

and render_tokenized_inline_record_definition = fun ~opening_token ~closing_token fields ->
  let rec render_fields = function
    | [] -> Doc.empty
    | [ field ] -> render_record_definition_field field
    | field :: rest -> Doc.concat
      [
        render_record_definition_field field;
        render_record_definition_field_separator field;
        Doc.break ~flat:" " ();
        render_fields rest;
      ]
  in
  if List.is_empty fields then
    Doc.concat [ doc_of_token opening_token; doc_of_token closing_token ]
  else
    Doc.group
      (Doc.concat
        [
          doc_of_token opening_token;
          Doc.indent 2 (Doc.concat [ Doc.break ~flat:" " (); render_fields fields ]);
          Doc.break ~flat:" " ();
          doc_of_token closing_token
        ])

and render_object_type_field = fun (field: Syn.Cst.object_type_field) ->
  render_tight_colon_rhs
    (doc_of_token field.field_name)
    field.colon_token
    (render_core_type field.field_type)

and render_object_type_field_entry = fun (field: Syn.Cst.object_type_field) ->
  Doc.concat
    [ render_object_type_field field; (
        match field.semicolon_token with
        | Some semicolon_token -> doc_of_token semicolon_token
        | None -> Doc.empty
      ) ]

and render_object_type = fun ~opening_token ~closing_token fields ->
  Doc.concat
    [
      doc_of_token opening_token;
      Doc.line;
      Doc.indent 2 (join_map Doc.line render_object_type_field_entry fields);
      Doc.line;
      doc_of_token closing_token
    ]

and render_poly_variant_field = function
  | Syn.Cst.RowField.Tag tag ->
      let head =
        match tag.bar_token with
        | Some bar_token -> Doc.concat
          [ doc_of_token bar_token; Doc.space; Doc.text "`"; doc_of_token tag.tag_name ]
        | None -> Doc.concat [ Doc.text "`"; doc_of_token tag.tag_name ]
      in
      (
        match tag.payload_type with
        | None -> head
        | Some payload_type ->
            let separator_token =
              match tag.separator_token with
              | Some separator_token -> separator_token
              | None -> unsupported "polyvariant tag payload missing separator token"
            in
            let separator_leading_trivia = pending_doc_of_token_leading_trivia separator_token in
            let payload_leading_trivia = pending_doc_of_trivia_before_node
              ~after:(Syn.Cst.Token.span separator_token).end_
              (Syn.Cst.CoreType.syntax_node payload_type) in
            if separator_leading_trivia = None && payload_leading_trivia = None then
              Doc.concat
                [
                  head;
                  Doc.space;
                  doc_of_token separator_token;
                  Doc.space;
                  render_core_type payload_type;
                ]
            else
              let separator_doc = doc_of_token separator_token |> doc_with_leading_trivia separator_leading_trivia in
              let payload_doc = render_core_type payload_type |> doc_with_leading_trivia payload_leading_trivia in
              Doc.concat
                [ head; Doc.line; Doc.indent 2 separator_doc; Doc.line; Doc.indent 2 payload_doc; ]
      )
  | Syn.Cst.RowField.Inherit { bar_token; type_; _ } -> (
      match bar_token with
      | Some bar_token -> Doc.concat [ doc_of_token bar_token; Doc.space; render_core_type type_ ]
      | None -> render_core_type type_
    )

and render_poly_variant_type = fun ?(field_indent = 2) poly_variant ->
  let open_doc =
    match Syn.Cst.PolyVariant.kind poly_variant with
    | Syn.Cst.PolyVariantBound.Exact -> doc_of_token poly_variant.opening_token
    | Syn.Cst.PolyVariantBound.UpperBound { marker_token } -> Doc.concat
      [ doc_of_token poly_variant.opening_token; doc_of_token marker_token ]
    | Syn.Cst.PolyVariantBound.LowerBound { marker_token } -> Doc.concat
      [ doc_of_token poly_variant.opening_token; doc_of_token marker_token ]
  in
  let fields =
    let rec render_fields = fun previous_boundary_end ->
      function
      | [] -> []
      | field :: rest ->
          let leading =
            match field with
            | Syn.Cst.RowField.Tag tag -> (
                match tag.bar_token with
                | Some bar_token -> Syn.Cst.leading_trivia_after_token_before_node
                  ~after:previous_boundary_end
                  bar_token
                  (Syn.Cst.RowField.syntax_node field)
                |> pending_doc_of_trivia
                | None -> None
              )
            | Syn.Cst.RowField.Inherit _ -> None
          in
          let rendered = render_poly_variant_field field |> doc_with_leading_trivia leading in
          let next_boundary_end = (Syn.Cst.token_body_span (Syn.Cst.RowField.syntax_node field)).end_ in
          rendered :: render_fields next_boundary_end rest
    in
    render_fields
      (Syn.Cst.token_body_span (Syn.Cst.PolyVariant.syntax_node poly_variant)).start
      (Syn.Cst.PolyVariant.fields poly_variant)
  in
  Doc.concat
    [
      open_doc;
      Doc.line;
      Doc.indent field_indent (Doc.join Doc.line fields);
      Doc.line;
      doc_of_token poly_variant.closing_token
    ]

let poly_variant_has_inherit_field = fun poly_variant ->
  Syn.Cst.PolyVariant.fields poly_variant |> List.exists
    (
      function
      | Syn.Cst.RowField.Inherit _ -> true
      | Syn.Cst.RowField.Tag _ -> false
    )

let render_type_constraint = fun (constraint_: Syn.Cst.type_constraint) ->
  Doc.concat
    [
      kw_constraint;
      Doc.space;
      render_core_type constraint_.left;
      doc_of_token constraint_.equals_token;
      render_core_type constraint_.right
    ]

let render_variant_constructor_arguments = fun ?(prefer_multiline_inline_record = false) ->
  function
  | Syn.Cst.ConstructorArguments.Tuple types -> Doc.group
    (join_map (Doc.concat [ Doc.space; star; Doc.break ~flat:" " () ]) render_core_type types)
  | Syn.Cst.ConstructorArguments.Record { opening_token; fields; closing_token } ->
      let field_items = Syn.CstBuilder.record_field_items_of_fields fields in
      let has_standalone_record_trivia =
        field_items
        |> List.exists
          (
            function
            | Syn.CstBuilder.RecordField _ -> false
            | Syn.CstBuilder.Comment _
            | Syn.CstBuilder.Docstring _
            | Syn.CstBuilder.TrailingComment _
            | Syn.CstBuilder.TrailingDocstring _ -> true
          )
      in
      if List.is_empty fields then
        Doc.indent 2 (render_tokenized_record_definition ~opening_token ~closing_token fields)
      else if has_standalone_record_trivia then
        Doc.indent 2 (render_tokenized_record_definition ~opening_token ~closing_token fields)
      else if prefer_multiline_inline_record then
        Doc.indent 2 (render_tokenized_record_definition ~opening_token ~closing_token fields)
      else
        Doc.indent 2 (render_tokenized_inline_record_definition ~opening_token ~closing_token fields)

let render_variant_constructor = fun ?(prefer_multiline_inline_record = false) constructor ->
  let bar_leading_trivia =
    match Syn.Cst.VariantConstructor.bar_token constructor with
    | Some bar_token -> pending_doc_of_token_leading_trivia bar_token
    | None -> None
  in
  let head =
    match Syn.Cst.VariantConstructor.bar_token constructor with
    | Some bar_token -> Doc.concat
      [
        doc_of_token bar_token |> doc_with_leading_trivia bar_leading_trivia;
        Doc.space;
        doc_of_token (Syn.Cst.VariantConstructor.constructor_name_token constructor);
      ]
    | None -> doc_of_token (Syn.Cst.VariantConstructor.constructor_name_token constructor)
  in
  let inline_separator_or_multiline_block ~fallback_separator_doc ~separator_token ~next_syntax_node ~render_next =
    let separator_leading_trivia =
      match separator_token with
      | Some separator_token -> pending_doc_of_token_leading_trivia separator_token
      | None -> None
    in
    let next_leading_trivia =
      match separator_token, next_syntax_node with
      | Some separator_token, Some next_syntax_node -> pending_doc_of_trivia_before_node
        ~after:(Syn.Cst.Token.span separator_token).end_
        next_syntax_node
      | _ -> None
    in
    let separator_doc =
      match separator_token with
      | Some separator_token -> doc_of_token separator_token
      | None -> fallback_separator_doc
    in
    if separator_leading_trivia = None && next_leading_trivia = None then
      (Doc.concat [ Doc.space; separator_doc; Doc.space; render_next ], false)
    else
      let separator_doc = separator_doc |> doc_with_leading_trivia separator_leading_trivia in
      let next_doc = render_next |> doc_with_leading_trivia next_leading_trivia in
      (Doc.concat [ Doc.line; Doc.indent 2 separator_doc; Doc.line; Doc.indent 2 next_doc; ], true)
  in
  let body =
    match Syn.Cst.VariantConstructor.arguments constructor, Syn.Cst.VariantConstructor.result_type constructor with
    | Some arguments, Some result_type ->
        let payload = render_variant_constructor_arguments ~prefer_multiline_inline_record arguments in
        let separator_token =
          match Syn.Cst.VariantConstructor.separator_token constructor with
          | Some separator_token -> separator_token
          | None -> unsupported "variant constructor payload missing separator token"
        in
        let payload_doc, payload_multiline = inline_separator_or_multiline_block
          ~fallback_separator_doc:Doc.empty
          ~separator_token:(Some separator_token)
          ~next_syntax_node:((Syn.Cst.VariantConstructor.payload_type constructor
          |> Option.map Syn.Cst.CoreType.syntax_node))
          ~render_next:payload in
        let arrow_token = Syn.Cst.VariantConstructor.arrow_token constructor in
        let arrow_leading_trivia =
          match arrow_token with
          | Some arrow_token -> pending_doc_of_token_leading_trivia arrow_token
          | None -> None
        in
        let result_leading_trivia =
          match arrow_token with
          | Some arrow_token -> pending_doc_of_trivia_before_node
            ~after:(Syn.Cst.Token.span arrow_token).end_
            (Syn.Cst.CoreType.syntax_node result_type)
          | None -> None
        in
        let result_rhs = Doc.concat
          [ payload; Doc.space; (
              match arrow_token with
              | Some arrow_token -> doc_of_token arrow_token
              | None -> arrow
            ); Doc.space; render_core_type result_type; ]
        in
        if
          Syn.Cst.Token.text separator_token = ":"
          && arrow_leading_trivia = None
          && result_leading_trivia = None
        then
          render_tight_token_rhs head separator_token result_rhs
        else if
          not payload_multiline && arrow_leading_trivia = None && result_leading_trivia = None
        then
          Doc.concat
            [ head; payload_doc; Doc.space; (
                match arrow_token with
                | Some arrow_token -> doc_of_token arrow_token
                | None -> arrow
              ); Doc.space; render_core_type result_type; ]
        else
          let arrow_doc =
            (
              match arrow_token with
              | Some arrow_token -> doc_of_token arrow_token
              | None -> arrow
            )
            |> doc_with_leading_trivia arrow_leading_trivia
          in
          let result_doc = render_core_type result_type |> doc_with_leading_trivia result_leading_trivia in
          Doc.concat
            [
              head;
              payload_doc;
              Doc.line;
              Doc.indent 2 arrow_doc;
              Doc.line;
              Doc.indent 2 result_doc;
            ]
    | Some arguments, None ->
        let payload = render_variant_constructor_arguments ~prefer_multiline_inline_record arguments in
        let separator_token =
          match Syn.Cst.VariantConstructor.separator_token constructor with
          | Some separator_token -> separator_token
          | None -> unsupported "variant constructor payload missing separator token"
        in
        let payload_doc, _payload_multiline = inline_separator_or_multiline_block
          ~fallback_separator_doc:Doc.empty
          ~separator_token:(Some separator_token)
          ~next_syntax_node:((Syn.Cst.VariantConstructor.payload_type constructor
          |> Option.map Syn.Cst.CoreType.syntax_node))
          ~render_next:payload in
        Doc.concat [ head; payload_doc; ]
    | None, Some result_type ->
        let separator_token =
          match Syn.Cst.VariantConstructor.separator_token constructor with
          | Some separator_token -> separator_token
          | None -> unsupported "variant constructor result type missing separator token"
        in
        if Syn.Cst.Token.text separator_token = ":" then
          render_tight_token_rhs head separator_token (render_core_type result_type)
        else
          let result_doc, _result_multiline = inline_separator_or_multiline_block
            ~fallback_separator_doc:Doc.empty
            ~separator_token:(Some separator_token)
            ~next_syntax_node:(Some (Syn.Cst.CoreType.syntax_node result_type))
            ~render_next:(render_core_type result_type) in
          Doc.concat [ head; result_doc; ]
    | None, None ->
        head
  in
  body

let render_variant_definition = fun constructors ->
  let constructor_docs = constructors
  |> List.map (fun constructor -> render_variant_constructor constructor) in
  constructor_docs |> Doc.join Doc.line

let render_type_definition = function
  | Syn.Cst.TypeDefinition.Abstract -> None
  | Syn.Cst.TypeDefinition.Alias { manifest; _ } -> Some (render_core_type manifest)
  | Syn.Cst.TypeDefinition.Record { opening_token; fields; closing_token; _ } -> Some (render_tokenized_record_definition
    ~opening_token
    ~closing_token
    fields)
  | Syn.Cst.TypeDefinition.Variant { syntax_node=_; constructors } -> Some (render_variant_definition
    constructors)
  | Syn.Cst.TypeDefinition.PolyVariant poly_variant -> Some (render_poly_variant_type poly_variant)
  | Syn.Cst.TypeDefinition.Extensible _ -> Some (Doc.text "..")
  | Syn.Cst.TypeDefinition.FirstClassModule { opening_token; package_type; closing_token; _ } -> Some (Doc.concat
    [
      doc_of_token opening_token;
      kw_module;
      Doc.space;
      render_package_type_doc package_type;
      doc_of_token closing_token;
    ])
  | Syn.Cst.TypeDefinition.Object { opening_token; fields; closing_token; _ } -> Some (render_object_type
    ~opening_token
    ~closing_token
    fields)

type type_definition_layout =
  | Inline_definition
  | Inline_opening_definition
  | Broken_definition
  | Broken_definition_no_outer_indent

let type_definition_layout = fun decl ->
  match Syn.Cst.TypeDeclaration.type_definition decl with
  | Syn.Cst.TypeDefinition.Record _
  | Syn.Cst.TypeDefinition.Object _ ->
      Inline_opening_definition
  | Syn.Cst.TypeDefinition.PolyVariant poly_variant -> (
      match Syn.Cst.PolyVariant.kind poly_variant with
      | Syn.Cst.PolyVariantBound.Exact ->
          if poly_variant_has_inherit_field poly_variant then
            Inline_opening_definition
          else
            Broken_definition_no_outer_indent
      | Syn.Cst.PolyVariantBound.UpperBound _
      | Syn.Cst.PolyVariantBound.LowerBound _ -> Inline_opening_definition
    )
  | Syn.Cst.TypeDefinition.Variant _ ->
      Broken_definition
  | Syn.Cst.TypeDefinition.Alias { manifest; _ } ->
      if core_type_prefers_multiline manifest then
        Broken_definition
      else
        Inline_definition
  | Syn.Cst.TypeDefinition.FirstClassModule _
  | Syn.Cst.TypeDefinition.Extensible _ ->
      Inline_opening_definition
  | Syn.Cst.TypeDefinition.Abstract ->
      Inline_definition

let render_single_type_declaration_with_keyword = fun ~leading_after _keyword decl ->
  let type_name = Syn.Cst.TypeDeclaration.type_name decl in
  let type_definition = Syn.Cst.TypeDeclaration.type_definition decl in
  let params = render_type_parameters (Syn.Cst.TypeDeclaration.type_params decl) in
  let keyword =
    let keyword = doc_of_declaration_token_with_filtered_leading_trivia
      ~after:leading_after
      (Syn.Cst.TypeDeclaration.keyword_token decl) in
    match Syn.Cst.TypeDeclaration.nonrec_token decl with
    | Some nonrec_token -> Doc.concat [ keyword; Doc.space; doc_of_token nonrec_token ]
    | None -> keyword
  in
  let header =
    if params = Doc.empty then
      Doc.concat [ keyword; Doc.space; doc_of_ident type_name ]
    else
      Doc.concat [ keyword; Doc.space; params; Doc.space; doc_of_ident type_name ]
  in
  let header =
    match (
      Syn.Cst.TypeDeclaration.manifest_alias decl,
      Syn.Cst.TypeDeclaration.manifest_equals_token decl
    ) with
    | Some manifest_alias, Some manifest_equals_token -> Doc.concat
      [
        header;
        Doc.space;
        doc_of_token manifest_equals_token;
        Doc.space;
        render_core_type manifest_alias;
      ]
    | Some _, None -> unsupported "type declaration manifest alias missing equals token"
    | None, _ -> header
  in
  let definition =
    match Syn.Cst.TypeDeclaration.private_flag decl with
    | Syn.Cst.PrivateFlag.Public -> render_type_definition type_definition
    | Syn.Cst.PrivateFlag.Private _ -> Option.map
      (fun definition -> Doc.concat [ kw_private; Doc.space; definition ])
      (render_type_definition type_definition)
  in
  let with_definition =
    match definition with
    | None -> header
    | Some definition -> (
        let definition_equals_token =
          match Syn.Cst.TypeDeclaration.definition_equals_token decl with
          | Some definition_equals_token -> definition_equals_token
          | None -> unsupported "type declaration definition missing equals token"
        in
        match type_definition_layout decl with
        | Inline_definition -> Doc.concat
          [ header; Doc.space; doc_of_token definition_equals_token; Doc.space; definition; ]
        | Inline_opening_definition -> Doc.concat
          [ header; Doc.space; doc_of_token definition_equals_token; Doc.space; definition; ]
        | Broken_definition -> Doc.concat
          [
            header;
            Doc.space;
            doc_of_token definition_equals_token;
            Doc.line;
            Doc.indent 2 definition;
          ]
        | Broken_definition_no_outer_indent -> Doc.concat
          [ header; Doc.space; doc_of_token definition_equals_token; Doc.line; definition; ]
      )
  in
  let with_constraints = Syn.Cst.TypeDeclaration.constraints decl
  |> List.fold_left
    (fun acc constraint_ ->
      Doc.concat [ acc; Doc.line; Doc.indent 2 (render_type_constraint constraint_) ])
    with_definition in
  with_constraints

let render_type_declaration_member_with_keyword = fun ~leading_after keyword decl ->
  render_single_type_declaration_with_keyword ~leading_after keyword decl

let render_type_declaration_with_keyword = fun ?(leading_after = 0) keyword decl ->
  let and_declarations = Syn.Cst.TypeDeclaration.and_declarations decl in
  let base =
    if and_declarations = [] then
      render_type_declaration_member_with_keyword ~leading_after keyword decl
    else
      let rec render_items = fun previous_end ->
        function
        | [] -> []
        | declaration :: rest ->
            let rendered = render_type_declaration_member_with_keyword ~leading_after:0 kw_and declaration in
            let next_previous_end =
              let span = Syn.Cst.token_body_span declaration.Syn.Cst.TypeDeclaration.syntax_node in
              span.end_
            in
            rendered :: render_items next_previous_end rest
      in
      let first = render_type_declaration_member_with_keyword ~leading_after keyword decl in
      let first_end =
        let span = Syn.Cst.token_body_span decl.Syn.Cst.TypeDeclaration.syntax_node in
        span.end_
      in
      Doc.join blank_line (first :: render_items first_end and_declarations)
  in
  match Syn.Cst.TypeDeclaration.attributes decl with
  | [] -> base
  | attributes -> Doc.concat [ base; Doc.space; join_map Doc.space render_attribute attributes ]

let render_type_extension = fun (decl: Syn.Cst.TypeExtension.t) ->
  let params = render_type_parameters (Syn.Cst.TypeExtension.type_params decl) in
  let extension_operator = Syn.Cst.TypeExtension.extension_operator_tokens decl
  |> List.map doc_of_token
  |> Doc.concat in
  let header =
    if params = Doc.empty then
      Doc.concat
        [
          kw_type;
          Doc.space;
          doc_of_ident (Syn.Cst.TypeExtension.type_name decl);
          Doc.space;
          extension_operator;
        ]
    else
      Doc.concat
        [
          kw_type;
          Doc.space;
          params;
          Doc.space;
          doc_of_ident (Syn.Cst.TypeExtension.type_name decl);
          Doc.space;
          extension_operator;
        ]
  in
  let constructors = render_variant_definition (Syn.Cst.TypeExtension.constructors decl) in
  Doc.concat [ header; Doc.line; Doc.indent 2 constructors ]

let render_external_declaration = fun (decl: Syn.Cst.external_declaration) ->
  let primitive_names = decl.primitive_name_tokens |> List.map doc_of_token |> Doc.join Doc.space in
  let attributes =
    match decl.attributes with
    | [] -> Doc.empty
    | attributes -> Doc.concat
      [ Doc.space; attributes |> List.map render_attribute |> Doc.join Doc.space ]
  in
  let head = Doc.concat [ kw_external; Doc.space; render_declaration_name decl.name_tokens ] in
  let type_doc = render_tight_colon_rhs head decl.colon_token (render_core_type decl.type_) in
  Doc.group
    (Doc.concat
      [
        type_doc;
        Doc.indent
          2
          (Doc.concat
            [
              Doc.break ();
              doc_of_token_with_leading_trivia decl.equals_token;
              Doc.space;
              primitive_names;
              attributes;
            ]);
      ])

let doc_with_pattern_attributes = fun pattern doc ->
  match Syn.Cst.Pattern.attributes pattern with
  | [] -> doc
  | attributes -> Doc.concat [ doc; Doc.space; join_map Doc.space render_attribute attributes ]

let rec render_pattern = fun pattern ->
  let doc =
    match pattern with
    | Syn.Cst.Pattern.Identifier { name_token; _ } ->
        doc_of_token name_token
    | Syn.Cst.Pattern.Wildcard _ ->
        Doc.text "_"
    | Syn.Cst.Pattern.Extension extension ->
        let extension = extension.extension in
        let payload_doc =
          match extension.payload with
          | None -> Doc.empty
          | Some (Syn.Cst.Payload.Opaque { tokens }) -> Doc.concat (List.map doc_of_token tokens)
        in
        Doc.concat
          [
            Doc.lbracket;
            extension.sigil_tokens |> List.map doc_of_token |> Doc.concat;
            doc_of_ident extension.name;
            payload_doc;
            Doc.rbracket;
          ]
    | Syn.Cst.Pattern.Literal { literal; _ } ->
        render_literal literal
    | Syn.Cst.Pattern.Lazy { pattern; _ } ->
        Doc.concat [ kw_lazy; Doc.space; render_pattern pattern ]
    | Syn.Cst.Pattern.Constructor { constructor_path; arguments; _ } ->
        let head = doc_of_ident constructor_path in
        (
          match arguments with
          | [] -> head
          | arguments -> Doc.concat
            [
              head;
              Doc.space;
              join_map (Doc.concat [ Doc.comma; Doc.space ]) render_pattern arguments
            ]
        )
    | Syn.Cst.Pattern.Operator { operator_tokens; _ } ->
        let operator = operator_tokens |> List.map doc_of_token |> Doc.concat in
        Doc.concat [ Doc.lparen; Doc.space; operator; Doc.space; Doc.rparen ]
    | Syn.Cst.Pattern.FirstClassModule {
      opening_token;
      binding;
      colon_token;
      package_type;
      closing_token;
      _
    } ->
        let binding_doc =
          match binding with
          | Syn.Cst.Named { name_token } -> doc_of_token name_token
          | Syn.Cst.Anonymous { wildcard_token } -> doc_of_token wildcard_token
        in
        let constraint_doc =
          match package_type with
          | None -> Doc.empty
          | Some package_type ->
              let colon_token =
                match colon_token with
                | Some colon_token -> colon_token
                | None -> unsupported "first-class module pattern package type missing colon token"
              in
              Doc.concat
                [
                  Doc.space;
                  doc_of_token colon_token;
                  Doc.space;
                  render_package_type_doc package_type;
                ]
        in
        Doc.concat
          [
            doc_of_token opening_token;
            kw_module;
            Doc.space;
            binding_doc;
            constraint_doc;
            doc_of_token closing_token
          ]
    | Syn.Cst.Pattern.PolyVariantInherit { type_path; _ } ->
        Doc.concat [ hash; doc_of_ident type_path ]
    | Syn.Cst.Pattern.Tuple { elements; _ } ->
        Doc.concat
          [ Doc.lparen; join_map (Doc.concat [ Doc.comma; Doc.space ])
              (fun (element: Syn.Cst.tuple_pattern_element) ->
                match element.label_token with
                | None -> render_pattern element.pattern
                | Some label_token -> Doc.concat
                  [ doc_of_token label_token; render_pattern element.pattern ])
              elements; Doc.rparen ]
    | Syn.Cst.Pattern.List {
      opening_token;
      elements;
      separator_tokens;
      closing_token;
      _
    } ->
        if elements = [] then
          Doc.concat [ doc_of_token opening_token; doc_of_token closing_token ]
        else
          let edge_space =
            if List.length elements = 1 then
              " "
            else
              ""
          in
          let rec render_elements elements separator_tokens =
            match elements, separator_tokens with
            | [], [] -> Doc.empty
            | [ element ], [] -> render_pattern element
            | element :: rest, separator_token :: rest_separators -> Doc.concat
              [
                render_pattern element;
                doc_of_token separator_token;
                Doc.break ~flat:edge_space ();
                render_elements rest rest_separators;
              ]
            | _ -> unsupported "list pattern elements missing separator tokens"
          in
          Doc.group
            (Doc.concat
              [
                doc_of_token opening_token;
                Doc.indent
                  2
                  (Doc.concat
                    [ Doc.break ~flat:edge_space (); render_elements elements separator_tokens ]);
                Doc.break ~flat:edge_space ();
                doc_of_token closing_token
              ])
    | Syn.Cst.Pattern.Array {
      opening_token;
      elements;
      separator_tokens;
      closing_token;
      _
    } ->
        let rec render_elements elements separator_tokens =
          match elements, separator_tokens with
          | [], [] -> Doc.empty
          | [ element ], [] -> render_pattern element
          | element :: rest, separator_token :: rest_separators -> Doc.concat
            [
              render_pattern element;
              doc_of_token separator_token;
              Doc.space;
              render_elements rest rest_separators;
            ]
          | _ -> unsupported "array pattern elements missing separator tokens"
        in
        Doc.concat
          [
            doc_of_token opening_token;
            render_elements elements separator_tokens;
            doc_of_token closing_token
          ]
    | Syn.Cst.Pattern.Record {
      opening_token;
      fields;
      separator_tokens;
      closedness;
      closing_token;
      _
    } ->
        let fields =
          fields
          |> List.map
            (fun (field: Syn.Cst.record_pattern_field) ->
              match field.pattern with
              | None -> doc_of_ident field.field_path
              | Some pattern ->
                  let equals_token =
                    match field.equals_token with
                    | Some equals_token -> equals_token
                    | None -> unsupported "record pattern field missing equals token"
                  in
                  Doc.concat
                    [
                      doc_of_ident field.field_path;
                      doc_of_token equals_token;
                      render_pattern pattern;
                    ])
        in
        let fields =
          match closedness with
          | Syn.Cst.Closed -> fields
          | Syn.Cst.Open { wildcard_token } -> fields @ [ doc_of_token wildcard_token ]
        in
        let rec render_fields fields separator_tokens separator_doc =
          match fields, separator_tokens with
          | [], [] -> Doc.empty
          | [ field ], [] -> field
          | field :: rest, separator_token :: rest_separators -> Doc.concat
            [
              field;
              doc_of_token separator_token;
              separator_doc;
              render_fields rest rest_separators separator_doc;
            ]
          | _ -> unsupported "record pattern fields missing separator tokens"
        in
        if List.length fields > 4 then
          Doc.concat
            [
              doc_of_token opening_token;
              Doc.line;
              Doc.indent 2 (render_fields fields separator_tokens Doc.line);
              Doc.line;
              doc_of_token closing_token
            ]
        else
          Doc.group
            (Doc.concat
              [
                doc_of_token opening_token;
                Doc.indent
                  2
                  (Doc.concat
                    [
                      Doc.break ~flat:" " ();
                      render_fields fields separator_tokens (Doc.break ~flat:" " ())
                    ]);
                Doc.break ~flat:" " ();
                doc_of_token closing_token
              ])
    | Syn.Cst.Pattern.Cons { head; tail; _ } ->
        Doc.concat [ render_pattern head; Doc.space; Doc.text "::"; Doc.space; render_pattern tail ]
    | Syn.Cst.Pattern.Or { alternatives; separator_tokens; _ } ->
        let rec render_alternatives left separators right =
          match left, separators, right with
          | [], _, [] -> Doc.empty
          | [ pattern ], [], [] -> render_pattern pattern
          | pattern :: remaining_patterns, separator_token :: remaining_separators, [] -> Doc.concat
            [
              render_pattern pattern;
              Doc.space;
              doc_of_token separator_token;
              Doc.space;
              render_alternatives remaining_patterns remaining_separators [];
            ]
          | _ -> unsupported "or-pattern alternatives missing separator tokens"
        in
        render_alternatives alternatives separator_tokens []
    | Syn.Cst.Pattern.Alias { pattern; name_token; _ } ->
        Doc.concat
          [ render_pattern pattern; Doc.space; Doc.text "as"; Doc.space; doc_of_token name_token ]
    | Syn.Cst.Pattern.Typed { pattern; colon_token; type_; _ } ->
        render_parenthesized_tight_token_rhs
          (render_pattern pattern)
          colon_token
          (render_core_type type_)
    | Syn.Cst.Pattern.Effect { effect_pattern; continuation; _ } ->
        Doc.concat
          [
            Doc.text "effect";
            Doc.space;
            render_pattern effect_pattern;
            Doc.space;
            render_pattern continuation;
          ]
    | Syn.Cst.Pattern.LocalOpen {
      module_path;
      dot_token;
      opening_token;
      pattern;
      closing_token;
      _
    } ->
        let rendered_pattern =
          match opening_token, closing_token, pattern with
          | Some _, Some _, Syn.Cst.Pattern.Tuple { elements; _ } ->
              Doc.group
                (
                  join_map (Doc.concat [ Doc.comma; Doc.space ])
                    (fun (element: Syn.Cst.tuple_pattern_element) ->
                      match element.label_token with
                      | None -> render_pattern element.pattern
                      | Some label_token -> Doc.concat
                        [ doc_of_token label_token; render_pattern element.pattern ])
                    elements
                )
          | _ -> render_pattern pattern
        in
        Doc.concat
          [ doc_of_ident module_path; doc_of_token dot_token; (
              match opening_token with
              | Some opening_token -> doc_of_token opening_token
              | None -> Doc.empty
            ); rendered_pattern; (
              match closing_token with
              | Some closing_token -> doc_of_token closing_token
              | None -> Doc.empty
            ); ]
    | Syn.Cst.Pattern.Exception { keyword_token; pattern; _ } ->
        Doc.concat [ doc_of_token keyword_token; Doc.space; render_pattern pattern ]
    | Syn.Cst.Pattern.Range { lower; upper; _ } ->
        Doc.concat
          [ render_literal lower; Doc.space; Doc.text ".."; Doc.space; render_literal upper ]
    | Syn.Cst.Pattern.Parenthesized { inner; _ } -> (
        match inner with
        | Syn.Cst.Pattern.Identifier { name_token; _ } when Syn.Cst.Token.is_operator_like_name name_token -> Doc.concat
          [ Doc.lparen; Doc.space; doc_of_token name_token; Doc.space; Doc.rparen ]
        | Syn.Cst.Pattern.Tuple _
        | Syn.Cst.Pattern.List _
        | Syn.Cst.Pattern.Array _
        | Syn.Cst.Pattern.Record _ -> render_pattern inner
        | _ -> Doc.concat [ Doc.lparen; render_pattern inner; Doc.rparen ]
      )
    | Syn.Cst.Pattern.PolyVariant { tag_token; payload; _ } ->
        let head = Doc.concat [ Doc.text "`"; doc_of_token tag_token ] in
        (
          match payload with
          | None -> head
          | Some payload -> Doc.concat [ head; Doc.space; render_pattern payload ]
        )
  in
  doc_with_pattern_attributes pattern doc

let pattern_requires_parens_in_named_parameter = function
  | Syn.Cst.Pattern.Identifier _
  | Syn.Cst.Pattern.Wildcard _
  | Syn.Cst.Pattern.Record _
  | Syn.Cst.Pattern.Parenthesized _ -> false
  | _ -> true

let rec pattern_is_simple_function_parameter = function
  | Syn.Cst.Pattern.Identifier _
  | Syn.Cst.Pattern.Constructor { arguments=[]; _ }
  | Syn.Cst.Pattern.Literal { literal=Syn.Cst.Literal.Unit _; _ }
  | Syn.Cst.Pattern.Wildcard _ -> true
  | Syn.Cst.Pattern.Typed { pattern; _ }
  | Syn.Cst.Pattern.Parenthesized { inner=pattern; _ } -> pattern_is_simple_function_parameter pattern
  | _ -> false

let rec pattern_supports_binding_header_parameters = function
  | Syn.Cst.Pattern.Identifier _
  | Syn.Cst.Pattern.Operator _ -> true
  | Syn.Cst.Pattern.Parenthesized { inner; _ } -> pattern_supports_binding_header_parameters inner
  | Syn.Cst.Pattern.Typed { pattern; _ } -> pattern_supports_binding_header_parameters pattern
  | _ -> false

let parameters_mix_complex_positional_and_named = fun parameters ->
  let has_named = List.exists Syn.Cst.Parameter.is_named parameters in
  let has_complex_positional =
    List.exists
      (
        function
        | Syn.Cst.Parameter.Positional { pattern; _ } -> not
          (pattern_is_simple_function_parameter pattern)
        | Syn.Cst.Parameter.Labeled _
        | Syn.Cst.Parameter.Optional _
        | Syn.Cst.Parameter.LocallyAbstract _ -> false
      )
      parameters
  in
  has_named && has_complex_positional

let is_simple_expression = function
  | Syn.Cst.Expression.Path _
  | Syn.Cst.Expression.Literal _
  | Syn.Cst.Expression.Operator _
  | Syn.Cst.Expression.Unreachable _
  | Syn.Cst.Expression.Extension _
  | Syn.Cst.Expression.PolyVariant _ -> true
  | Syn.Cst.Expression.Constructor { payload=None; _ } -> true
  | _ -> false

let expression_needs_parens_for_postfix_attributes = function
  | Syn.Cst.Expression.Apply _
  | Syn.Cst.Expression.Infix _
  | Syn.Cst.Expression.Constructor { payload=Some _; _ }
  | Syn.Cst.Expression.PolyVariant { payload=Some _; _ } -> true
  | Syn.Cst.Expression.Parenthesized _ -> false
  | _ -> false

let expression_needs_parens_in_apply = function
  | Syn.Cst.Expression.If _
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.LetOperator _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Sequence _
  | Syn.Cst.Expression.Fun _
  | Syn.Cst.Expression.Function _
  | Syn.Cst.Expression.Infix _
  | Syn.Cst.Expression.TypeAscription _ -> true
  | _ -> false

let rec expression_needs_parens_in_labeled_argument = function
  | Syn.Cst.Expression.Parenthesized {
    inner=(Syn.Cst.Expression.Fun _ | Syn.Cst.Expression.Function _);
    _
  } -> false
  | Syn.Cst.Expression.Parenthesized { inner; _ } -> expression_needs_parens_in_labeled_argument inner
  | Syn.Cst.Expression.Apply _ -> true
  | Syn.Cst.Expression.PolyVariant { payload=Some _; _ } -> true
  | expression -> expression_needs_parens_in_apply expression

let rec expression_needs_parens_in_constructor = function
  | Syn.Cst.Expression.TypeAscription _
  | Syn.Cst.Expression.Polymorphic _ -> false
  | Syn.Cst.Expression.Parenthesized {
    inner=Syn.Cst.Expression.PolyVariant { payload=Some _; _ };
    _
  } -> true
  | Syn.Cst.Expression.Parenthesized _ -> false
  | Syn.Cst.Expression.PolyVariant { payload=Some _; _ } -> true
  | expression -> expression_needs_parens_in_apply expression

let expression_requires_spaced_delimited_local_open = function
  | Syn.Cst.Expression.Path { path; _ } -> (
      match Syn.Cst.Ident.last_segment path with
      | Some name_token -> Syn.Cst.Token.is_operator_like_name name_token
      | None -> false
    )
  | _ -> false

let rec expression_prefers_multiline_layout = function
  | Syn.Cst.Expression.If if_ -> if_prefers_multiline_layout if_
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.Function _
  | Syn.Cst.Expression.LetOperator _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Sequence _ -> true
  | Syn.Cst.Expression.Fun { body=Syn.Cst.Expression body; _ } -> expression_prefers_multiline_layout
    body
  | Syn.Cst.Expression.Fun { body=Syn.Cst.Cases _; _ } -> true
  | Syn.Cst.Expression.Parenthesized { grouping=Syn.Cst.BeginEnd; _ } -> true
  | Syn.Cst.Expression.Parenthesized { inner; _ } -> expression_prefers_multiline_layout inner
  | _ -> false

and if_prefers_multiline_layout = fun (
  { condition; then_branch; else_branch; _ }: Syn.Cst.if_expression
) ->
  let else_prefers_multiline =
    match else_branch with
    | Some (Syn.Cst.Expression.If _) -> false
    | Some else_branch -> branch_prefers_multiline_layout else_branch
    | None -> false
  in
  expression_prefers_multiline_layout condition || branch_prefers_multiline_layout then_branch || else_prefers_multiline

and branch_prefers_multiline_layout = function
  | Syn.Cst.Expression.If if_ -> true
  | Syn.Cst.Expression.Parenthesized { grouping=Syn.Cst.BeginEnd; _ } -> true
  | Syn.Cst.Expression.Parenthesized { inner; _ } -> expression_prefers_multiline_layout inner
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.Function _
  | Syn.Cst.Expression.Fun _
  | Syn.Cst.Expression.LetOperator _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Sequence _
  | _ -> false

let case_body_prefers_multiline = fun ({ body; _ }: Syn.Cst.match_case) ->
  expression_prefers_multiline_layout body

let rec function_body_prefers_multiline = function
  | Syn.Cst.Expression.If if_ -> if_prefers_multiline_layout if_
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.LetOperator _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Sequence _
  | Syn.Cst.Expression.Parenthesized { grouping=Syn.Cst.BeginEnd; _ } -> true
  | Syn.Cst.Expression.Function { cases; _ } -> List.exists case_body_prefers_multiline cases
  | Syn.Cst.Expression.Fun { body=Syn.Cst.Expression body; _ } -> function_body_prefers_multiline body
  | Syn.Cst.Expression.Fun { body=Syn.Cst.Cases _; _ } -> false
  | Syn.Cst.Expression.Apply apply -> qualified_multi_argument_apply_prefers_multiline apply
  | Syn.Cst.Expression.Parenthesized { inner; _ } -> function_body_prefers_multiline inner
  | _ -> false

and qualified_multi_argument_apply_prefers_multiline = fun (
  { callee; argument; _ }: Syn.Cst.apply_expression
) ->
  let rec argument_count = fun count ->
    function
    | Syn.Cst.Expression.Apply { callee; _ } -> argument_count (count + 1) callee
    | _ -> count
  in
  let rec head_is_qualified_path = function
    | Syn.Cst.Expression.Apply { callee; _ } ->
        head_is_qualified_path callee
    | Syn.Cst.Expression.Path { path; _ } ->
        List.length (Syn.Cst.Ident.segments path) > 1
    | Syn.Cst.Expression.FieldAccess { receiver; _ } -> (
        match receiver with
        | Syn.Cst.Expression.Path _
        | Syn.Cst.Expression.FieldAccess _ -> true
        | _ -> false
      )
    | _ ->
        false
  in
  let rec has_non_positional_argument = fun acc ->
    function
    | Syn.Cst.Expression.Apply { callee; argument; _ } ->
        let acc =
          acc
          || match argument with
          | Syn.Cst.Positional _ -> false
          | Syn.Cst.Labeled _
          | Syn.Cst.Optional _ -> true
        in
        has_non_positional_argument acc callee
    | _ -> acc
  in
  let acc =
    match argument with
    | Syn.Cst.Positional _ -> false
    | Syn.Cst.Labeled _
    | Syn.Cst.Optional _ -> true
  in
  argument_count 1 callee > 1
  && head_is_qualified_path callee
  && not (has_non_positional_argument acc callee)

let rec expression_is_pipeline = function
  | Syn.Cst.Expression.Infix { operator_tokens; left; right; _ } ->
      (
        match operator_tokens with
        | [ operator_token ] -> Syn.Cst.Token.fixed_operator operator_token = Some Syn.Cst.Token.PipeForward
        | _ -> false
      ) || expression_is_pipeline left || expression_is_pipeline right
  | Syn.Cst.Expression.Parenthesized { inner; _ } -> expression_is_pipeline inner
  | _ -> false

let rec expression_is_boolean_infix = function
  | Syn.Cst.Expression.Infix { operator_tokens; left; right; _ } ->
      (
        match operator_tokens with
        | [ operator_token ] -> (
            match Syn.Cst.Token.fixed_operator operator_token with
            | Some Syn.Cst.Token.BooleanAnd
            | Some Syn.Cst.Token.BooleanOr -> true
            | _ -> false
          )
        | _ -> false
      ) || expression_is_boolean_infix left || expression_is_boolean_infix right
  | Syn.Cst.Expression.Parenthesized { inner; _ } -> expression_is_boolean_infix inner
  | _ -> false

let rec expression_is_function_like = function
  | Syn.Cst.Expression.Function _ -> true
  | Syn.Cst.Expression.Parenthesized { inner; _ } -> expression_is_function_like inner
  | _ -> false

let rec infix_chain_term_count = function
  | Syn.Cst.Expression.Infix { left; right; _ } -> infix_chain_term_count left
  + infix_chain_term_count right
  | _ -> 1

let max_inline_infix_terms_after_equals = 8

let infix_expression_is_simple_after_equals = fun (infix: Syn.Cst.infix_expression) ->
  infix_chain_term_count (Syn.Cst.Expression.Infix infix) <= max_inline_infix_terms_after_equals

let rec expression_is_simple_after_equals = function
  | Syn.Cst.Expression.Infix infix -> infix_expression_is_simple_after_equals infix
  | Syn.Cst.Expression.Path _
  | Syn.Cst.Expression.Literal _
  | Syn.Cst.Expression.Operator _
  | Syn.Cst.Expression.Unreachable _
  | Syn.Cst.Expression.Extension _
  | Syn.Cst.Expression.Constructor _
  | Syn.Cst.Expression.PolyVariant _
  | Syn.Cst.Expression.Prefix _
  | Syn.Cst.Expression.Tuple _
  | Syn.Cst.Expression.List _
  | Syn.Cst.Expression.Array _
  | Syn.Cst.Expression.Record _
  | Syn.Cst.Expression.FieldAccess _
  | Syn.Cst.Expression.Index _
  | Syn.Cst.Expression.TypeAscription _
  | Syn.Cst.Expression.MethodCall _
  | Syn.Cst.Expression.New _
  | Syn.Cst.Expression.LocalOpen _ -> true
  | Syn.Cst.Expression.Apply apply -> apply_expression_is_simple_after_equals apply
  | Syn.Cst.Expression.Parenthesized { inner; _ } -> expression_is_simple_after_equals inner
  | _ -> false

and apply_argument_is_simple_after_equals = function
  | Syn.Cst.Positional value -> expression_is_simple_after_equals value
  | Syn.Cst.Labeled { value=Some value; _ }
  | Syn.Cst.Optional { value=Some value; _ } -> expression_is_simple_after_equals value
  | Syn.Cst.Labeled { value=None; _ }
  | Syn.Cst.Optional { value=None; _ } -> true

and apply_expression_is_simple_after_equals = fun ({ callee; argument; _ }: Syn.Cst.apply_expression) ->
  expression_is_simple_after_equals callee && apply_argument_is_simple_after_equals argument

let rec collapse_redundant_parenthesized_value_after_equals = function
  | Syn.Cst.Expression.Parenthesized { grouping=Syn.Cst.Parens; opening_token; inner; _ } as expression ->
      let inner_leading_trivia = Syn.Cst.leading_trivia_before_node
        ~after:(Syn.Cst.Token.span opening_token).end_
        (Syn.Cst.Expression.syntax_node inner) in
      if inner_leading_trivia = [] then
        collapse_redundant_parenthesized_value_after_equals inner
      else
        expression
  | (Syn.Cst.Expression.Infix _ as expression) ->
      expression
  | expression when expression_is_simple_after_equals expression ->
      expression
  | expression ->
      expression

let expression_requires_break_after_equals = function
  | Syn.Cst.Expression.Function _
  | Syn.Cst.Expression.If _
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.While _
  | Syn.Cst.Expression.For _
  | Syn.Cst.Expression.LetOperator _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Sequence _
  | Syn.Cst.Expression.LetModule _
  | Syn.Cst.Expression.LocalOpen _ -> true
  | _ -> false

let expression_can_use_delimited_local_open_sugar = function
  | Syn.Cst.Expression.List _
  | Syn.Cst.Expression.Array _
  | Syn.Cst.Expression.Record _
  | Syn.Cst.Expression.Tuple _
  | Syn.Cst.Expression.Parenthesized _ -> true
  | _ -> false

let rec collapse_redundant_parenthesized_expression = function
  | Syn.Cst.Expression.Parenthesized { grouping=Syn.Cst.Parens; inner; _ } -> collapse_redundant_parenthesized_expression
    inner
  | Syn.Cst.Expression.Parenthesized { grouping=Syn.Cst.BeginEnd; _ } -> None
  | Syn.Cst.Expression.Operator _ -> None
  | Syn.Cst.Expression.Prefix { operator_token; operand=Syn.Cst.Expression.Literal literal; _;  } when (
    match Syn.Cst.Token.fixed_operator operator_token with
    | Some Syn.Cst.Token.PrefixMinus
    | Some Syn.Cst.Token.PrefixNegate -> true
    | _ -> false
  ) -> Some (`NegativeLiteral literal)
  | expression when is_simple_expression expression -> Some (`Expression expression)
  | _ -> None

let same_operator_tokens = fun left right ->
  List.length left = List.length right && List.for_all2 Syn.Cst.Token.same_text left right

let doc_of_operator_tokens = fun operator_tokens -> operator_tokens |> List.map doc_of_token |> Doc.concat

let infix_chain = fun operator_tokens expression ->
  let rec collect = fun acc ->
    function
    | Syn.Cst.Expression.Infix { left; operator_tokens=next_operator_tokens; right; _ } when same_operator_tokens
      next_operator_tokens
      operator_tokens -> collect (collect acc left) right
    | expression -> acc @ [ expression ]
  in
  collect [] expression

let apply_argument_syntax_node = function
  | Syn.Cst.Positional expression -> Syn.Cst.Expression.syntax_node expression
  | Syn.Cst.Labeled argument -> argument.syntax_node
  | Syn.Cst.Optional argument -> argument.syntax_node

type lowerer = {
  render_structure_items:
    ?trailing_phrase_separator_tokens:Syn.Cst.Token.t list list ->
    source_node:Syn.Cst.syntax_node ->
    Syn.Cst.StructureItem.t list ->
    Doc.t;
  render_signature_items: source_node:Syn.Cst.syntax_node -> Syn.Cst.SignatureItem.t list -> Doc.t;
}

let make_lowerer =
  let rec render_expression expression =
    let doc =
      match expression with
      | Syn.Cst.Expression.Path { path; _ } ->
          doc_of_ident path
      | Syn.Cst.Expression.Extension extension ->
          render_extension_doc extension
      | Syn.Cst.Expression.Unreachable unreachable ->
          doc_of_token unreachable.dot_token
      | Syn.Cst.Expression.Literal literal ->
          render_literal literal
      | Syn.Cst.Expression.Object object_ ->
          render_object_expression object_
      | Syn.Cst.Expression.Constructor { constructor_path; payload; _ } ->
          let head = doc_of_ident constructor_path in
          (
            match payload with
            | None -> head
            | Some payload ->
                let payload =
                  if expression_needs_parens_in_constructor payload then
                    Doc.concat [ Doc.lparen; render_expression payload; Doc.rparen ]
                  else
                    render_expression payload
                in
                Doc.concat [ head; Doc.space; payload ]
          )
      | Syn.Cst.Expression.ModulePack {
        opening_token;
        closing_token;
        module_expression;
        colon_token;
        package_type;
        _
      } ->
          let constraint_doc =
            match package_type with
            | None -> Doc.empty
            | Some package_type ->
                let colon_token =
                  match colon_token with
                  | Some colon_token -> colon_token
                  | None -> unsupported "module pack package type missing colon token"
                in
                Doc.concat
                  [
                    Doc.space;
                    doc_of_token colon_token;
                    Doc.space;
                    render_package_type_doc package_type
                  ]
          in
          Doc.concat
            [
              doc_of_token opening_token;
              kw_module;
              Doc.space;
              render_module_expression_doc module_expression;
              constraint_doc;
              doc_of_token closing_token;
            ]
      | Syn.Cst.Expression.Assert { asserted; _ } ->
          Doc.concat [ kw_assert; Doc.space; render_expression asserted ]
      | Syn.Cst.Expression.Lazy { body; _ } ->
          Doc.concat [ kw_lazy; Doc.space; render_expression body ]
      | Syn.Cst.Expression.While { condition; body; _ } ->
          Doc.concat
            [
              kw_while;
              Doc.space;
              render_expression condition;
              Doc.space;
              kw_do;
              Doc.line;
              Doc.indent 2 (render_block_expression body);
              Doc.line;
              kw_done;
            ]
      | Syn.Cst.Expression.For {
        iterator_token;
        equals_token;
        start_expr;
        direction;
        end_expr;
        body;
        _
      } ->
          let direction_doc =
            match direction with
            | Syn.Cst.To { direction_token }
            | Syn.Cst.Downto { direction_token } -> doc_of_token direction_token
          in
          Doc.concat
            [
              kw_for;
              Doc.space;
              doc_of_token iterator_token;
              Doc.space;
              doc_of_token equals_token;
              Doc.space;
              render_expression start_expr;
              Doc.space;
              direction_doc;
              Doc.space;
              render_expression end_expr;
              Doc.space;
              kw_do;
              Doc.line;
              Doc.indent 2 (render_block_expression body);
              Doc.line;
              kw_done;
            ]
      | Syn.Cst.Expression.Operator { operator_tokens; _ } ->
          let operator = operator_tokens |> List.map doc_of_token |> Doc.concat in
          Doc.concat [ Doc.lparen; Doc.space; operator; Doc.space; Doc.rparen ]
      | Syn.Cst.Expression.Tuple { elements; _ } ->
          let rendered_elements = List.map render_expression elements in
          let prefers_multiline = List.exists Doc.is_multiline rendered_elements in
          if prefers_multiline then
            let lines = join_map (Doc.concat [ Doc.comma; Doc.line ]) render_expression elements in
            Doc.concat [ Doc.lparen; Doc.line; Doc.indent 2 lines; Doc.line; Doc.rparen; ]
          else
            Doc.group
              (Doc.concat
                [
                  Doc.lparen;
                  Doc.indent
                    2
                    (Doc.concat
                      [
                        Doc.break ~flat:"" ();
                        join_map (Doc.concat [ Doc.comma; Doc.break () ]) render_expression elements;
                      ]);
                  Doc.break ~flat:"" ();
                  Doc.rparen;
                ])
      | Syn.Cst.Expression.List {
        syntax_node;
        opening_token;
        elements;
        separator_tokens;
        closing_token;
        _
      } ->
          if elements = [] then
            Doc.concat [ doc_of_token opening_token; doc_of_token closing_token ]
          else if List.length elements >= multiline_list_threshold then
            render_multiline_list_expression ~opening_token ~separator_tokens ~closing_token elements
          else
            let rec render_elements elements separator_tokens =
              match elements, separator_tokens with
              | [], [] -> Doc.empty
              | [ element ], [] -> render_expression element
              | [ element ], [ separator_token ] -> Doc.concat
                [ render_expression element; doc_of_token separator_token ]
              | element :: rest, separator_token :: rest_separators -> Doc.concat
                [
                  render_expression element;
                  doc_of_token separator_token;
                  Doc.break ();
                  render_elements rest rest_separators;
                ]
              | _ -> unsupported "list expression elements missing separator tokens"
            in
            Doc.group
              (Doc.concat
                [
                  doc_of_token opening_token;
                  Doc.indent
                    2
                    (Doc.concat
                      [ Doc.break ~flat:" " (); render_elements elements separator_tokens; ]);
                  Doc.break ~flat:" " ();
                  doc_of_token closing_token;
                ])
      | Syn.Cst.Expression.Array {
        opening_token;
        elements;
        separator_tokens;
        closing_token;
        _
      } ->
          let rec render_elements elements separator_tokens =
            match elements, separator_tokens with
            | [], [] -> Doc.empty
            | [ element ], [] -> render_expression element
            | [ element ], [ separator_token ] -> Doc.concat
              [ render_expression element; doc_of_token separator_token ]
            | element :: rest, separator_token :: rest_separators -> Doc.concat
              [
                render_expression element;
                doc_of_token separator_token;
                Doc.break ();
                render_elements rest rest_separators;
              ]
            | _ -> unsupported "array expression elements missing separator tokens"
          in
          Doc.group
            (Doc.concat
              [
                doc_of_token opening_token;
                Doc.indent
                  2
                  (Doc.concat [ Doc.break ~flat:"" (); render_elements elements separator_tokens; ]);
                Doc.break ~flat:"" ();
                doc_of_token closing_token;
              ])
      | Syn.Cst.Expression.Parenthesized { inner; _ } ->
          render_parenthesized_expression expression
      | Syn.Cst.Expression.Prefix { operator_token; operand; _ } -> (
          match operand with
          | Syn.Cst.Expression.Literal literal when (
            match Syn.Cst.Token.fixed_operator operator_token with
            | Some Syn.Cst.Token.PrefixMinus
            | Some Syn.Cst.Token.PrefixNegate -> true
            | _ -> false
          ) -> Doc.concat [ Doc.lparen; Doc.text "-"; render_literal literal; Doc.rparen ]
          | _ ->
              let operand_doc =
                match operand with
                | _ when expression_needs_parens_in_apply operand -> Doc.concat
                  [ Doc.lparen; render_expression operand; Doc.rparen ]
                | _ -> render_expression operand
              in
              Doc.concat [ doc_of_token operator_token; operand_doc ]
        )
      | Syn.Cst.Expression.FieldAssign { target; operator_token; value; _ } ->
          Doc.concat
            [
              render_expression (Syn.Cst.Expression.FieldAccess target);
              Doc.space;
              doc_of_token operator_token;
              Doc.space;
              render_expression value;
            ]
      | Syn.Cst.Expression.Assign { target; operator_token; value; _ } ->
          Doc.concat
            [
              render_expression target;
              Doc.space;
              doc_of_token operator_token;
              Doc.space;
              render_expression value;
            ]
      | Syn.Cst.Expression.Infix infix ->
          render_infix_expression infix
      | Syn.Cst.Expression.Apply apply ->
          render_apply_expression apply
      | Syn.Cst.Expression.If if_ ->
          render_if_expression if_
      | Syn.Cst.Expression.Match match_ ->
          render_match_expression
            ~keyword_token:match_.keyword_token
            ~scrutinee:match_.scrutinee
            ~with_token:match_.with_token
            ~cases:match_.cases
      | Syn.Cst.Expression.Try try_ ->
          render_match_expression
            ~keyword_token:try_.keyword_token
            ~scrutinee:try_.body
            ~with_token:try_.with_token
            ~cases:try_.cases
      | Syn.Cst.Expression.Function function_ ->
          render_function_expression function_
      | Syn.Cst.Expression.Fun fun_ ->
          render_fun_expression fun_
      | Syn.Cst.Expression.LetOperator let_operator ->
          render_let_operator_expression let_operator
      | Syn.Cst.Expression.Let let_ ->
          render_let_expression let_
      | Syn.Cst.Expression.LetException let_exception ->
          render_let_exception_expression let_exception
      | Syn.Cst.Expression.LetModule let_module ->
          render_let_module_expression let_module
      | Syn.Cst.Expression.LocalOpen local_open ->
          render_local_open_expression local_open
      | Syn.Cst.Expression.Sequence sequence ->
          render_sequence_expression sequence
      | Syn.Cst.Expression.Record record ->
          render_record_expression record
      | Syn.Cst.Expression.MethodCall { receiver; hash_token; method_name; _ } ->
          let receiver =
            match receiver with
            | Syn.Cst.Expression.If _
            | Syn.Cst.Expression.Match _
            | Syn.Cst.Expression.Try _
            | Syn.Cst.Expression.LetOperator _
            | Syn.Cst.Expression.Let _
            | Syn.Cst.Expression.Sequence _
            | Syn.Cst.Expression.Fun _
            | Syn.Cst.Expression.Function _ -> Doc.concat
              [ Doc.lparen; render_expression receiver; Doc.rparen ]
            | _ -> render_expression receiver
          in
          Doc.concat [ receiver; doc_of_token hash_token; doc_of_token method_name ]
      | Syn.Cst.Expression.New { class_path; _ } ->
          Doc.concat [ kw_new; Doc.space; doc_of_ident class_path ]
      | Syn.Cst.Expression.ObjectOverride override ->
          render_object_override_expression override
      | Syn.Cst.Expression.InstanceVariableAssign assign ->
          Doc.concat
            [
              doc_of_token assign.name_token;
              Doc.space;
              doc_of_token assign.operator_token;
              Doc.space;
              render_expression assign.value;
            ]
      | Syn.Cst.Expression.FieldAccess { receiver; field_name; _ } ->
          let receiver =
            match receiver with
            | Syn.Cst.Expression.If _
            | Syn.Cst.Expression.Match _
            | Syn.Cst.Expression.Try _
            | Syn.Cst.Expression.LetOperator _
            | Syn.Cst.Expression.Let _
            | Syn.Cst.Expression.Sequence _
            | Syn.Cst.Expression.Fun _
            | Syn.Cst.Expression.Function _ -> Doc.concat
              [ Doc.lparen; render_expression receiver; Doc.rparen ]
            | _ -> render_expression receiver
          in
          Doc.concat [ receiver; Doc.text "."; doc_of_token field_name ]
      | Syn.Cst.Expression.Index index ->
          render_index_expression index
      | Syn.Cst.Expression.TypeAscription { expression; kind; _ } -> (
          match kind with
          | Syn.Cst.Type { colon_token; type_ } -> render_parenthesized_tight_token_rhs
            (render_expression expression)
            colon_token
            (render_core_type type_)
          | Syn.Cst.Coerce { coercion_tokens; type_ } ->
              render_parenthesized_tight_doc_rhs (render_expression expression)
                (coercion_tokens |> List.map doc_of_token |> Doc.concat)
                (render_core_type type_)
          | Syn.Cst.ConstraintCoerce { colon_token; from_type; coercion_tokens; to_type } -> render_parenthesized_tight_token_rhs
            (render_expression expression)
            colon_token
            (Doc.concat
              [
                render_core_type from_type;
                Doc.space;
                coercion_tokens |> List.map doc_of_token |> Doc.concat;
                Doc.space;
                render_core_type to_type;
              ])
        )
      | Syn.Cst.Expression.Polymorphic { expression; colon_token; type_; _ } ->
          render_parenthesized_tight_token_rhs
            (render_expression expression)
            colon_token
            (render_core_type type_)
      | Syn.Cst.Expression.PolyVariant { tag_token; payload; _ } ->
          let head = Doc.concat [ Doc.text "`"; doc_of_token tag_token ] in
          (
            match payload with
            | None -> head
            | Some payload ->
                let payload =
                  if expression_needs_parens_in_constructor payload then
                    Doc.concat [ Doc.lparen; render_expression payload; Doc.rparen ]
                  else
                    render_expression payload
                in
                Doc.concat [ head; Doc.space; payload ]
          )
    in
    doc_with_expression_attributes expression doc
  and render_extension_payload_doc_with_context ~context (extension: Syn.Cst.extension) =
    match extension.payload with
    | None -> Doc.empty
    | Some (Syn.Cst.Payload.Opaque { tokens }) -> Doc.concat
      (List.map (fun token -> Doc.text (Syn.Cst.Token.full_text token)) tokens)
  and render_extension_doc (extension: Syn.Cst.extension) = Doc.concat
    [
      Doc.lbracket;
      extension.sigil_tokens |> List.map doc_of_token |> Doc.concat;
      doc_of_ident extension.name;
      render_extension_payload_doc_with_context ~context:extension_payload_context extension;
      Doc.rbracket;
    ]
  and render_attribute_payload_doc (attribute: Syn.Cst.attribute) =
    match attribute.payload with
    | None -> Doc.empty
    | Some (Syn.Cst.Payload.Opaque { tokens }) -> Doc.concat
      (List.map (fun token -> Doc.text (Syn.Cst.Token.full_text token)) tokens)
  and render_attribute_doc ~floating (attribute: Syn.Cst.attribute) = Doc.concat
    [
      Doc.lbracket;
      attribute.sigil_tokens |> List.map doc_of_token |> Doc.concat;
      doc_of_ident attribute.name;
      render_attribute_payload_doc attribute;
      Doc.rbracket;
    ]
  and render_attribute attribute = render_attribute_doc ~floating:false attribute
  and render_floating_attribute attribute = render_attribute_doc ~floating:true attribute
  and doc_with_expression_attributes expression doc =
    match Syn.Cst.Expression.attributes expression with
    | [] -> doc
    | attributes ->
        let doc =
          if expression_needs_parens_for_postfix_attributes expression then
            Doc.concat [ Doc.lparen; doc; Doc.rparen ]
          else
            doc
        in
        Doc.concat [ doc; Doc.space; join_map Doc.space render_attribute attributes ]
  and render_record_field (field: Syn.Cst.record_expression_field) =
    match field.source with
    | Syn.Cst.Punned -> doc_of_ident field.field_path
    | Syn.Cst.Explicit ->
        let equals_token =
          match field.equals_token with
          | Some equals_token -> equals_token
          | None -> unsupported "record expression field missing equals token"
        in
        let body_doc =
          if expression_requires_break_after_equals field.value then
            render_block_expression field.value
          else
            render_expression field.value
        in
        if Doc.is_multiline body_doc || expression_requires_break_after_equals field.value then
          Doc.concat
            [
              doc_of_ident field.field_path;
              Doc.space;
              doc_of_token equals_token;
              Doc.line;
              Doc.indent 2 body_doc;
            ]
        else
          Doc.concat
            [
              doc_of_ident field.field_path;
              Doc.space;
              doc_of_token equals_token;
              Doc.space;
              body_doc;
            ]
  and render_record_expression_field_separator = function
    | Some separator_token -> doc_of_token separator_token
    | None -> Doc.semi
  and doc_with_object_member_attributes attributes doc =
    match attributes with
    | [] -> doc
    | attributes -> Doc.concat [ doc; Doc.space; join_map Doc.space render_attribute attributes ]
  and render_object_member_body ~head ~equals_token expression =
    let body_doc =
      if expression_requires_break_after_equals expression then
        render_block_expression expression
      else
        render_expression expression
    in
    if Doc.is_multiline body_doc || expression_requires_break_after_equals expression then
      Doc.concat [ head; Doc.space; doc_of_token equals_token; Doc.line; Doc.indent 2 body_doc ]
    else
      Doc.concat [ head; Doc.space; doc_of_token equals_token; Doc.space; body_doc ]
  and render_object_method ({
      name_token;
      body;
      equals_token;
      type_;
      colon_token;
      attributes;
      modifier_tokens;
      _;

    }: Syn.Cst.object_method) =
    let head = render_member_head kw_method modifier_tokens name_token in
    let doc =
      match type_ with
      | None -> render_object_member_body ~head ~equals_token body
      | Some type_ ->
          let colon_token =
            match colon_token with
            | Some colon_token -> colon_token
            | None -> unsupported "object method type annotation missing colon token"
          in
          render_object_member_body
            ~head:(render_tight_colon_rhs head colon_token (render_core_type type_))
            ~equals_token
            body
    in
    doc_with_object_member_attributes attributes doc
  and render_object_value ({
      name_token;
      value;
      equals_token;
      type_;
      colon_token;
      attributes;
      modifier_tokens;
      _;

    }: Syn.Cst.object_value) =
    let head = render_member_head kw_val modifier_tokens name_token in
    let doc =
      match type_ with
      | None -> render_object_member_body ~head ~equals_token value
      | Some type_ ->
          let colon_token =
            match colon_token with
            | Some colon_token -> colon_token
            | None -> unsupported "object value type annotation missing colon token"
          in
          render_object_member_body
            ~head:(render_tight_colon_rhs head colon_token (render_core_type type_))
            ~equals_token
            value
    in
    doc_with_object_member_attributes attributes doc
  and render_object_inherit ({ expression; attributes; _ }: Syn.Cst.object_inherit) =
    let doc = Doc.concat [ kw_inherit; Doc.space; render_expression expression ] in
    doc_with_object_member_attributes attributes doc
  and render_object_initializer ({ body; _ }: Syn.Cst.object_initializer) =
    let body_doc =
      if expression_prefers_multiline_layout body then
        render_block_expression body
      else
        render_expression body
    in
    if Doc.is_multiline body_doc || expression_prefers_multiline_layout body then
      Doc.concat [ kw_initializer; Doc.line; Doc.indent 2 body_doc ]
    else
      Doc.concat [ kw_initializer; Doc.space; body_doc ]
  and render_object_member = function
    | Syn.Cst.ObjectMember.Method method_ -> render_object_method method_
    | Syn.Cst.ObjectMember.Value value -> render_object_value value
    | Syn.Cst.ObjectMember.Inherit inherit_ -> render_object_inherit inherit_
    | Syn.Cst.ObjectMember.Extension extension -> render_extension_doc extension
    | Syn.Cst.ObjectMember.Initializer initializer_ -> render_object_initializer initializer_
  and render_object_expression_body_item = function
    | Syn.CstBuilder.ObjectMember member -> render_object_member member
    | Syn.CstBuilder.Comment comment -> Doc.text (Syn.Cst.Comment.text comment)
    | Syn.CstBuilder.Docstring docstring -> Doc.text (Syn.Cst.Docstring.text docstring)
  and render_object_expression ({ syntax_node; self_pattern; members; _ }: Syn.Cst.object_expression) =
    let header =
      match self_pattern with
      | None -> kw_object
      | Some self_pattern -> Doc.concat
        [ kw_object; Doc.space; Doc.lparen; render_pattern self_pattern; Doc.rparen ]
    in
    let body_items = Syn.CstBuilder.object_member_items_of_members ~source_node:syntax_node members in
    if List.is_empty body_items then
      Doc.concat [ header; Doc.space; kw_end ]
    else
      Doc.concat
        [
          header;
          Doc.line;
          Doc.indent 2 (join_map Doc.line render_object_expression_body_item body_items);
          Doc.line;
          kw_end;
        ]
  and render_class_type_field = function
    | Syn.Cst.ClassTypeField.Inherit { class_type; _ } ->
        Doc.concat [ kw_inherit; Doc.space; render_class_type_doc class_type ]
    | Syn.Cst.ClassTypeField.Value {
      name_token;
      type_;
      colon_token;
      modifier_tokens;
      _
    } ->
        let head = render_member_head kw_val modifier_tokens name_token in
        render_tight_colon_rhs head colon_token (render_core_type type_)
    | Syn.Cst.ClassTypeField.Method {
      name_token;
      type_;
      colon_token;
      modifier_tokens;
      _
    } ->
        let head = render_member_head kw_method modifier_tokens name_token in
        render_tight_colon_rhs head colon_token (render_core_type type_)
    | Syn.Cst.ClassTypeField.Constraint { left; equals_token; right; _ } ->
        Doc.concat
          [
            kw_constraint;
            Doc.space;
            render_core_type left;
            Doc.space;
            doc_of_token equals_token;
            render_core_type right;
          ]
    | Syn.Cst.ClassTypeField.Attribute { field; attribute; _ } ->
        Doc.concat [ render_class_type_field field; Doc.space; render_attribute attribute ]
    | Syn.Cst.ClassTypeField.Extension extension ->
        render_extension_doc extension
  and render_class_type_body_item = function
    | Syn.CstBuilder.ClassTypeField field -> render_class_type_field field
    | Syn.CstBuilder.Comment comment -> Doc.text (Syn.Cst.Comment.text comment)
    | Syn.CstBuilder.Docstring docstring -> Doc.text (Syn.Cst.Docstring.text docstring)
  and render_class_type_doc = function
    | Syn.Cst.ClassType.Path path ->
        doc_of_ident path
    | Syn.Cst.ClassType.Signature { syntax_node; fields } ->
        let body_items = Syn.CstBuilder.class_type_field_items_of_fields ~source_node:syntax_node fields in
        if List.is_empty body_items then
          Doc.concat [ kw_object; Doc.space; kw_end ]
        else
          Doc.concat
            [
              kw_object;
              Doc.line;
              Doc.indent 2 (join_map Doc.line render_class_type_body_item body_items);
              Doc.line;
              kw_end;
            ]
    | Syn.Cst.ClassType.Arrow { label; parameter_type; result_type; _ } ->
        let render_arrow_parameter label parameter_type =
          let parameter_type =
            match parameter_type with
            | Syn.Cst.CoreType.Arrow _ -> Doc.concat
              [ Doc.lparen; render_core_type parameter_type; Doc.rparen ]
            | _ -> render_core_type parameter_type
          in
          Doc.concat [ render_arrow_label label; parameter_type ]
        in
        let rec collect params label parameter_type result_type =
          let params = params @ [ render_arrow_parameter label parameter_type ] in
          match result_type with
          | Syn.Cst.ClassType.Arrow { label; parameter_type; result_type; _ } -> collect
            params
            label
            parameter_type
            result_type
          | result_type -> (params, render_class_type_doc result_type)
        in
        let parameters, result = collect [] label parameter_type result_type in
        let parts = parameters @ [ result ] in
        Doc.group
          (join_map (Doc.concat [ Doc.space; Doc.arrow; Doc.break () ]) (fun doc -> doc) parts)
    | Syn.Cst.ClassType.Parenthesized { opening_token; inner; closing_token; _ } ->
        Doc.concat
          [ doc_of_token opening_token; render_class_type_doc inner; doc_of_token closing_token ]
    | Syn.Cst.ClassType.Attribute { class_type; attribute; _ } ->
        Doc.concat [ render_class_type_doc class_type; Doc.space; render_attribute attribute ]
    | Syn.Cst.ClassType.Extension extension ->
        render_extension_doc extension
  and render_class_member_body ~head ~equals_token expression =
    let body_doc =
      if expression_requires_break_after_equals expression then
        render_block_expression expression
      else
        render_expression expression
    in
    if Doc.is_multiline body_doc || expression_requires_break_after_equals expression then
      Doc.concat [ head; Doc.space; doc_of_token equals_token; Doc.line; Doc.indent 2 body_doc ]
    else
      Doc.concat [ head; Doc.space; doc_of_token equals_token; Doc.space; body_doc ]
  and render_class_method ({
      name_token;
      concrete_equals_token;
      definition;
      virtual_colon_token;
      modifier_tokens;
      _;

    }: Syn.Cst.class_method) =
    let head = render_member_head kw_method modifier_tokens name_token in
    match definition with
    | Syn.Cst.VirtualMethod { type_; _ } ->
        let virtual_colon_token =
          match virtual_colon_token with
          | Some virtual_colon_token -> virtual_colon_token
          | None -> unsupported "virtual class method missing colon token"
        in
        render_tight_colon_rhs head virtual_colon_token (render_core_type type_)
    | Syn.Cst.ConcreteMethod { body; type_=None } ->
        let equals_token =
          match concrete_equals_token with
          | Some equals_token -> equals_token
          | None -> unsupported "concrete class method missing equals token"
        in
        render_class_member_body ~head ~equals_token body
    | Syn.Cst.ConcreteMethod { body; type_=Some (colon_token, type_) } ->
        let equals_token =
          match concrete_equals_token with
          | Some equals_token -> equals_token
          | None -> unsupported "concrete class method missing equals token"
        in
        render_class_member_body
          ~head:(render_tight_colon_rhs head colon_token (render_core_type type_))
          ~equals_token
          body
  and render_class_value ({
      name_token;
      concrete_equals_token;
      definition;
      virtual_colon_token;
      modifier_tokens;
      _;

    }: Syn.Cst.class_value) =
    let head = render_member_head kw_val modifier_tokens name_token in
    match definition with
    | Syn.Cst.VirtualValue { type_; _ } ->
        let virtual_colon_token =
          match virtual_colon_token with
          | Some virtual_colon_token -> virtual_colon_token
          | None -> unsupported "virtual class value missing colon token"
        in
        render_tight_colon_rhs head virtual_colon_token (render_core_type type_)
    | Syn.Cst.ConcreteValue { value; type_=None } ->
        let equals_token =
          match concrete_equals_token with
          | Some equals_token -> equals_token
          | None -> unsupported "concrete class value missing equals token"
        in
        render_class_member_body ~head ~equals_token value
    | Syn.Cst.ConcreteValue { value; type_=Some (colon_token, type_) } ->
        let equals_token =
          match concrete_equals_token with
          | Some equals_token -> equals_token
          | None -> unsupported "concrete class value missing equals token"
        in
        render_class_member_body
          ~head:(render_tight_colon_rhs head colon_token (render_core_type type_))
          ~equals_token
          value
  and render_class_inherit ({ class_expression; _ }: Syn.Cst.class_inherit) = Doc.concat
    [ kw_inherit; Doc.space; render_class_expression class_expression ]
  and render_class_constraint ({ left; equals_token; right; _ }: Syn.Cst.class_constraint) = Doc.concat
    [
      kw_constraint;
      Doc.space;
      render_core_type left;
      Doc.space;
      doc_of_token equals_token;
      render_core_type right;
    ]
  and render_class_initializer ({ body; _ }: Syn.Cst.class_initializer) =
    let body_doc =
      if expression_prefers_multiline_layout body then
        render_block_expression body
      else
        render_expression body
    in
    if Doc.is_multiline body_doc || expression_prefers_multiline_layout body then
      Doc.concat [ kw_initializer; Doc.line; Doc.indent 2 body_doc ]
    else
      Doc.concat [ kw_initializer; Doc.space; body_doc ]
  and render_class_field = function
    | Syn.Cst.ClassField.Method method_ -> render_class_method method_
    | Syn.Cst.ClassField.Value value -> render_class_value value
    | Syn.Cst.ClassField.Inherit inherit_ -> render_class_inherit inherit_
    | Syn.Cst.ClassField.Constraint constraint_ -> render_class_constraint constraint_
    | Syn.Cst.ClassField.Initializer initializer_ -> render_class_initializer initializer_
    | Syn.Cst.ClassField.Attribute { field; attribute; _ } -> Doc.concat
      [ render_class_field field; Doc.space; render_attribute attribute ]
    | Syn.Cst.ClassField.Extension extension -> render_extension_doc extension
  and render_class_expression_body_item = function
    | Syn.CstBuilder.ClassField field -> render_class_field field
    | Syn.CstBuilder.Comment comment -> Doc.text (Syn.Cst.Comment.text comment)
    | Syn.CstBuilder.Docstring docstring -> Doc.text (Syn.Cst.Docstring.text docstring)
  and class_expression_needs_parens_in_apply_head = function
    | Syn.Cst.ClassExpression.Path _
    | Syn.Cst.ClassExpression.Structure _
    | Syn.Cst.ClassExpression.Parenthesized _
    | Syn.Cst.ClassExpression.LocalOpen _
    | Syn.Cst.ClassExpression.Extension _ -> false
    | Syn.Cst.ClassExpression.Attribute { class_expression; _ } -> class_expression_needs_parens_in_apply_head
      class_expression
    | Syn.Cst.ClassExpression.Fun _
    | Syn.Cst.ClassExpression.Apply _
    | Syn.Cst.ClassExpression.Let _
    | Syn.Cst.ClassExpression.Constraint _ -> true
  and render_class_apply_expression ({ callee; argument; _ }: Syn.Cst.class_apply_expression) =
    let rec collect_arguments = fun acc ->
      function
      | Syn.Cst.ClassExpression.Apply { callee; argument; _ } -> collect_arguments
        (argument :: acc)
        callee
      | class_expression -> (class_expression, acc)
    in
    let head, arguments = collect_arguments [ argument ] callee in
    let rendered_head =
      if class_expression_needs_parens_in_apply_head head then
        Doc.concat [ Doc.lparen; render_class_expression head; Doc.rparen ]
      else
        render_class_expression head
    in
    let rendered_arguments = arguments |> List.map render_apply_argument in
    Doc.group
      (Doc.concat
        (rendered_head :: List.map (fun argument -> Doc.concat [ Doc.break (); argument ]) rendered_arguments))
  and render_class_fun_expression ({ parameters; body; _ }: Syn.Cst.class_fun_expression) =
    let parameters = parameters |> List.map render_parameter in
    let body = render_class_expression body in
    let body_prefers_multiline = Doc.is_multiline body in
    if body_prefers_multiline || List.length parameters = 0 then
      Doc.concat
        [ kw_fun; (
            if List.length parameters = 0 then
              Doc.empty
            else
              Doc.concat [ Doc.space; Doc.join Doc.space parameters ]
          ); Doc.space; Doc.arrow; Doc.line; Doc.indent 2 body; ]
    else
      Doc.group
        (Doc.concat
          [
            kw_fun;
            Doc.space;
            Doc.join Doc.space parameters;
            Doc.space;
            Doc.arrow;
            Doc.indent 2 (Doc.concat [ Doc.break (); body ]);
          ])
  and render_class_let_expression ({
      keyword_token;
      rec_token;
      equals_token;
      binding_pattern;
      parameters;
      bound_value;
      and_binding;
      in_token;
      body;
      _
    }: Syn.Cst.class_let_expression) =
    let leading_value_trivia = pending_doc_of_trivia_before_node
      ~after:(Syn.Cst.Token.span equals_token).end_
      (Syn.Cst.Expression.syntax_node bound_value) in
    let first_binding = render_local_binding
      ~local_context:true
      ~source_has_explicit_fun:false
      ~keyword_token
      ~rec_token
      ~equals_token
      ~leading_binding_trivia:None
      ~leading_value_trivia
      ~pattern:binding_pattern
      ~parameters
      ~value:bound_value in
    let and_bindings = Option.to_list and_binding
    |> List.concat_map let_binding_group_items
    |> List.map
      (fun (binding: Syn.Cst.let_binding) ->
        render_local_binding
          ~local_context:true
          ~source_has_explicit_fun:false
          ~keyword_token:binding.keyword_token
          ~rec_token:binding.rec_token
          ~equals_token:binding.equals_token
          ~leading_binding_trivia:(pending_doc_of_token_leading_trivia binding.keyword_token)
          ~leading_value_trivia:(pending_doc_of_trivia_before_node
            ~after:(Syn.Cst.Token.span binding.equals_token).end_
            (Syn.Cst.LetBinding.value_syntax_node binding))
          ~pattern:binding.binding_pattern
          ~parameters:binding.parameters
          ~value:binding.value) in
    let bindings = Doc.concat
      (first_binding :: List.map (fun binding -> Doc.concat [ Doc.line; binding ]) and_bindings) in
    let body_doc = render_class_expression body in
    if Doc.is_multiline first_binding then
      Doc.concat [ bindings; Doc.line; doc_of_token in_token; Doc.line; body_doc; ]
    else
      Doc.concat [ bindings; Doc.space; doc_of_token in_token; Doc.line; body_doc; ]
  and render_class_expression = function
    | Syn.Cst.ClassExpression.Path path ->
        doc_of_ident path
    | Syn.Cst.ClassExpression.Structure { syntax_node; self_pattern; fields } ->
        let header =
          match self_pattern with
          | None -> kw_object
          | Some self_pattern -> Doc.concat
            [ kw_object; Doc.space; Doc.lparen; render_pattern self_pattern; Doc.rparen ]
        in
        let body_items = Syn.CstBuilder.class_field_items_of_fields ~source_node:syntax_node fields in
        if List.is_empty body_items then
          Doc.concat [ header; Doc.space; kw_end ]
        else
          Doc.concat
            [
              header;
              Doc.line;
              Doc.indent 2 (join_map Doc.line render_class_expression_body_item body_items);
              Doc.line;
              kw_end;
            ]
    | Syn.Cst.ClassExpression.Fun fun_ ->
        render_class_fun_expression fun_
    | Syn.Cst.ClassExpression.Apply apply ->
        render_class_apply_expression apply
    | Syn.Cst.ClassExpression.Let let_ ->
        render_class_let_expression let_
    | Syn.Cst.ClassExpression.Constraint { class_expression; colon_token; class_type; _ } ->
        Doc.concat
          [
            Doc.lparen;
            render_class_expression class_expression;
            doc_of_token colon_token;
            render_class_type_doc class_type;
            Doc.rparen;
          ]
    | Syn.Cst.ClassExpression.LocalOpen (Syn.Cst.LetOpen {
      let_token;
      open_token;
      module_path;
      in_token;
      body;
      _
    }) ->
        Doc.concat
          [
            doc_of_token let_token;
            Doc.space;
            doc_of_token open_token;
            Doc.space;
            doc_of_ident module_path;
            Doc.space;
            doc_of_token in_token;
            Doc.space;
            render_class_expression body;
          ]
    | Syn.Cst.ClassExpression.LocalOpen (Syn.Cst.Delimited {
      module_path;
      dot_token;
      opening_token;
      body;
      closing_token;
      _
    }) -> (
        match opening_token, closing_token with
        | Some opening_token, Some closing_token -> Doc.concat
          [
            doc_of_ident module_path;
            doc_of_token dot_token;
            doc_of_token opening_token;
            render_class_expression body;
            doc_of_token closing_token;
          ]
        | None, None -> Doc.concat
          [ doc_of_ident module_path; doc_of_token dot_token; render_class_expression body ]
        | _ -> panic "render_class_expression: mismatched class local-open delimiters"
      )
    | Syn.Cst.ClassExpression.Parenthesized { opening_token; inner; closing_token; _ } ->
        Doc.concat
          [ doc_of_token opening_token; render_class_expression inner; doc_of_token closing_token ]
    | Syn.Cst.ClassExpression.Attribute { class_expression; attribute; _ } ->
        Doc.concat
          [ render_class_expression class_expression; Doc.space; render_attribute attribute ]
    | Syn.Cst.ClassExpression.Extension extension ->
        render_extension_doc extension
  and render_object_override_expression ({
      opening_token;
      fields;
      separator_tokens;
      closing_token;
      _
    }: Syn.Cst.object_override_expression) =
    let fields =
      fields
      |> List.map
        (fun ({ field_name; equals_token; value; _ }: Syn.Cst.object_override_field) ->
          match value with
          | None -> doc_of_token field_name
          | Some value ->
              let equals_token =
                match equals_token with
                | Some equals_token -> equals_token
                | None -> unsupported "object override field missing equals token"
              in
              Doc.concat
                [ doc_of_token field_name; doc_of_token equals_token; render_expression value ])
    in
    let rec render_fields fields separator_tokens break_doc =
      match fields, separator_tokens with
      | [], [] -> Doc.empty
      | [ field ], [] -> field
      | field :: rest, separator_token :: rest_separators -> Doc.concat
        [
          field;
          doc_of_token separator_token;
          break_doc;
          render_fields rest rest_separators break_doc;
        ]
      | _ -> unsupported "object override fields missing separator tokens"
    in
    if fields = [] then
      Doc.concat [ doc_of_token opening_token; doc_of_token closing_token ]
    else if List.length fields > 4 then
      Doc.concat
        [
          doc_of_token opening_token;
          Doc.line;
          Doc.indent 2 (render_fields fields separator_tokens Doc.line);
          Doc.line;
          doc_of_token closing_token;
        ]
    else
      Doc.group
        (Doc.concat
          [
            doc_of_token opening_token;
            Doc.indent
              2
              (Doc.concat
                [
                  Doc.break ~flat:" " ();
                  render_fields fields separator_tokens (Doc.break ~flat:" " ());
                ]);
            Doc.break ~flat:" " ();
            doc_of_token closing_token;
          ])
  and render_index_expression ({
      collection;
      opening_tokens;
      index;
      closing_token;
      _
    }: Syn.Cst.index_expression) =
    let collection_doc =
      match collection with
      | Syn.Cst.Expression.If _
      | Syn.Cst.Expression.Match _
      | Syn.Cst.Expression.Try _
      | Syn.Cst.Expression.LetOperator _
      | Syn.Cst.Expression.Let _
      | Syn.Cst.Expression.Sequence _
      | Syn.Cst.Expression.Fun _
      | Syn.Cst.Expression.Function _ -> Doc.concat
        [ Doc.lparen; render_expression collection; Doc.rparen ]
      | _ -> render_expression collection
    in
    Doc.concat
      [
        collection_doc;
        Doc.concat (List.map doc_of_token opening_tokens);
        render_expression index;
        doc_of_token closing_token;
      ]
  and render_record_expression = function
    | Syn.Cst.RecordExpression.Literal {
      opening_token;
      fields;
      separator_tokens;
      closing_token;
      _
    } ->
        let rendered_fields = List.map render_record_field fields in
        let rec render_fields ~include_trailing_separator fields separator_tokens break_doc =
          match fields, separator_tokens with
          | [], [] -> Doc.empty
          | [ field ], [] ->
              if include_trailing_separator then
                Doc.concat [ field; render_record_expression_field_separator None ]
              else
                field
          | [ field ], [ separator_token ] ->
              if include_trailing_separator then
                Doc.concat [ field; render_record_expression_field_separator (Some separator_token) ]
              else
                field
          | field :: rest, separator_token :: rest_separators -> Doc.concat
            [
              field;
              doc_of_token separator_token;
              break_doc;
              render_fields ~include_trailing_separator rest rest_separators break_doc;
            ]
          | _ -> unsupported "record literal fields missing separator tokens"
        in
        if fields = [] then
          Doc.concat [ doc_of_token opening_token; doc_of_token closing_token ]
        else if List.exists Doc.is_multiline rendered_fields || List.length fields > 4 then
          Doc.concat
            [
              doc_of_token opening_token;
              Doc.line;
              Doc.indent
                2
                (render_fields
                  ~include_trailing_separator:true
                  rendered_fields
                  separator_tokens
                  Doc.line);
              Doc.line;
              doc_of_token closing_token;
            ]
        else
          Doc.group
            (Doc.concat
              [
                doc_of_token opening_token;
                Doc.indent
                  2
                  (Doc.concat
                    [
                      Doc.break ~flat:" " ();
                      render_fields
                        ~include_trailing_separator:false
                        rendered_fields
                        separator_tokens
                        (Doc.break ~flat:" " ())
                    ]);
                Doc.break ~flat:" " ();
                doc_of_token closing_token;
              ])
    | Syn.Cst.RecordExpression.Update {
      opening_token;
      base;
      with_token;
      fields;
      separator_tokens;
      closing_token;
      _
    } ->
        let base_doc = render_expression base in
        let rendered_fields = List.map render_record_field fields in
        let rec render_fields ~include_trailing_separator fields separator_tokens break_doc =
          match fields, separator_tokens with
          | [], [] -> Doc.empty
          | [ field ], [] ->
              if include_trailing_separator then
                Doc.concat [ field; render_record_expression_field_separator None ]
              else
                field
          | [ field ], [ separator_token ] ->
              if include_trailing_separator then
                Doc.concat [ field; render_record_expression_field_separator (Some separator_token) ]
              else
                field
          | field :: rest, separator_token :: rest_separators -> Doc.concat
            [
              field;
              doc_of_token separator_token;
              break_doc;
              render_fields ~include_trailing_separator rest rest_separators break_doc;
            ]
          | _ -> unsupported "record update fields missing separator tokens"
        in
        if
          List.exists Doc.is_multiline rendered_fields
          || Doc.is_multiline base_doc
          || List.length fields > 4
        then
          Doc.concat
            [
              doc_of_token opening_token;
              Doc.line;
              Doc.indent
                2
                (Doc.concat
                  [
                    base_doc;
                    Doc.line;
                    doc_of_token with_token;
                    Doc.space;
                    render_fields
                      ~include_trailing_separator:true
                      rendered_fields
                      separator_tokens
                      Doc.line;
                  ]);
              Doc.line;
              doc_of_token closing_token;
            ]
        else
          Doc.group
            (Doc.concat
              [
                doc_of_token opening_token;
                Doc.indent
                  2
                  (Doc.concat
                    [
                      Doc.break ~flat:" " ();
                      base_doc;
                      Doc.break ();
                      doc_of_token with_token;
                      Doc.space;
                      render_fields
                        ~include_trailing_separator:false
                        rendered_fields
                        separator_tokens
                        (Doc.break ~flat:" " ());
                    ]);
                Doc.break ~flat:" " ();
                doc_of_token closing_token;
              ])
  and render_tuple_expression_bare elements = Doc.group
    (join_map (Doc.concat [ Doc.comma; Doc.break () ]) render_expression elements)
  and render_parenthesized_apply_payload (
    { opening_token; closing_token; inner; _ }: Syn.Cst.parenthesized_expression
  ) =
    let rendered_inner =
      match inner with
      | Syn.Cst.Expression.Tuple { elements; _ } -> render_tuple_expression_bare elements
      | _ -> render_expression inner
    in
    let rendered_inner = rendered_inner
    |> doc_with_leading_trivia
      (pending_doc_of_trivia
        (Syn.Cst.leading_trivia_before_node
          ~after:(Syn.Cst.Token.span opening_token).end_
          (Syn.Cst.Expression.syntax_node inner))) in
    Doc.concat [ doc_of_token opening_token; rendered_inner; doc_of_token closing_token ]
  and render_local_open_expression (local_open: Syn.Cst.local_open_expression) =
    match local_open with
    | Syn.Cst.LetOpen {
      let_token;
      open_token;
      module_path;
      in_token;
      body;
      _
    } ->
        let module_doc = doc_of_ident module_path in
        let body_doc = render_expression body in
        let head = Doc.concat
          [
            doc_of_token let_token;
            Doc.space;
            doc_of_token open_token;
            Doc.space;
            module_doc;
            Doc.space;
            doc_of_token in_token;
          ] in
        if Doc.is_multiline body_doc || expression_requires_break_after_equals body then
          Doc.concat [ head; Doc.line; Doc.indent 2 body_doc ]
        else
          Doc.concat [ head; Doc.space; body_doc ]
    | Syn.Cst.Delimited {
      module_path;
      dot_token;
      opening_token;
      body;
      closing_token;
      _
    } ->
        let module_doc = doc_of_ident module_path in
        let body_doc = render_expression body in
        (
          match opening_token, closing_token with
          | None, None -> Doc.concat [ module_doc; doc_of_token dot_token; body_doc ]
          | Some opening_token, Some closing_token ->
              if Doc.is_multiline body_doc then
                Doc.concat
                  [
                    module_doc;
                    doc_of_token dot_token;
                    doc_of_token opening_token;
                    Doc.line;
                    Doc.indent 2 body_doc;
                    Doc.line;
                    doc_of_token closing_token;
                  ]
              else if expression_requires_spaced_delimited_local_open body then
                Doc.concat
                  [
                    module_doc;
                    doc_of_token dot_token;
                    doc_of_token opening_token;
                    Doc.space;
                    body_doc;
                    Doc.space;
                    doc_of_token closing_token;
                  ]
              else
                Doc.concat
                  [
                    module_doc;
                    doc_of_token dot_token;
                    doc_of_token opening_token;
                    body_doc;
                    doc_of_token closing_token;
                  ]
          | _ -> panic "render_local_open_expression: mismatched delimited local-open tokens"
        )
  and render_multiline_list_expression ~opening_token ~separator_tokens ~closing_token elements =
    let rec render_body elements separator_tokens =
      match elements, separator_tokens with
      | [], [] -> Doc.empty
      | [ element ], [] -> render_expression element
      | [ element ], [ separator_token ] -> Doc.concat
        [ render_expression element; doc_of_token separator_token ]
      | element :: rest, separator_token :: rest_separators -> Doc.concat
        [
          render_expression element;
          doc_of_token separator_token;
          Doc.line;
          render_body rest rest_separators;
        ]
      | _ -> unsupported "list expression elements missing separator tokens"
    in
    let body = render_body elements separator_tokens in
    Doc.concat
      [
        doc_of_token opening_token;
        Doc.line;
        Doc.indent 2 body;
        Doc.line;
        doc_of_token closing_token;
      ]
  and render_apply_argument = function
    | Syn.Cst.Positional (Syn.Cst.Expression.Function _ as expression) ->
        Doc.concat
          [
            Doc.lparen;
            Doc.line;
            Doc.indent 2 (render_expression expression);
            Doc.line;
            Doc.rparen;
          ]
    | Syn.Cst.Positional (Syn.Cst.Expression.Parenthesized ({
      grouping=Syn.Cst.Parens;
      inner=(Syn.Cst.Expression.Tuple _ | Syn.Cst.Expression.PolyVariant { payload=Some _; _ });
      _;

    } as expression)) ->
        render_parenthesized_apply_payload expression
    | Syn.Cst.Positional expression ->
        if expression_needs_parens_in_apply expression then
          Doc.concat [ Doc.lparen; render_expression expression; Doc.rparen ]
        else
          render_expression expression
    | Syn.Cst.Labeled { sigil_token; label_token; value; _ } -> (
        match value with
        | None -> Doc.concat [ doc_of_token sigil_token; doc_of_token label_token ]
        | Some value ->
            let value =
              match value with
              | Syn.Cst.Expression.Parenthesized ({
                grouping=Syn.Cst.Parens;
                inner=(Syn.Cst.Expression.Apply _ | Syn.Cst.Expression.Tuple _ | Syn.Cst.Expression.PolyVariant {
                  payload=Some _;
                  _
                });
                _;

              } as expression) -> render_parenthesized_apply_payload expression
              | _ when expression_needs_parens_in_labeled_argument value -> Doc.concat
                [ Doc.lparen; render_expression value; Doc.rparen ]
              | _ -> render_expression value
            in
            Doc.concat [ doc_of_token sigil_token; doc_of_token label_token; Doc.text ":"; value; ]
      )
    | Syn.Cst.Optional { sigil_token; label_token; value; _ } -> (
        match value with
        | None -> Doc.concat [ doc_of_token sigil_token; doc_of_token label_token ]
        | Some value ->
            let value =
              match value with
              | Syn.Cst.Expression.Parenthesized ({
                grouping=Syn.Cst.Parens;
                inner=(Syn.Cst.Expression.Apply _ | Syn.Cst.Expression.Tuple _ | Syn.Cst.Expression.PolyVariant {
                  payload=Some _;
                  _
                });
                _;

              } as expression) -> render_parenthesized_apply_payload expression
              | _ when expression_needs_parens_in_labeled_argument value -> Doc.concat
                [ Doc.lparen; render_expression value; Doc.rparen ]
              | _ -> render_expression value
            in
            Doc.concat [ doc_of_token sigil_token; doc_of_token label_token; Doc.text ":"; value; ]
      )
  and apply_argument_prefers_break = function
    | Syn.Cst.Positional (Syn.Cst.Expression.Parenthesized {
      inner=(Syn.Cst.Expression.If _ | Syn.Cst.Expression.Match _ | Syn.Cst.Expression.Try _ | Syn.Cst.Expression.LetOperator _ | Syn.Cst.Expression.Let _ | Syn.Cst.Expression.Sequence _);
      _;

    }) ->
        true
    | Syn.Cst.Positional (Syn.Cst.Expression.Parenthesized {
      inner=Syn.Cst.Expression.Infix { operator_tokens; _ };
      _
    }) -> (
        match operator_tokens with
        | [ operator_token ] -> Syn.Cst.Token.fixed_operator operator_token = Some Syn.Cst.Token.PipeForward
        | _ -> false
      )
    | Syn.Cst.Positional expression ->
        expression_prefers_multiline_layout expression
    | Syn.Cst.Labeled { value=Some value; _ }
    | Syn.Cst.Optional { value=Some value; _ } ->
        expression_prefers_multiline_layout value
    | _ ->
        false
  and render_infix_expression ({
      syntax_node;
      left;
      operator_tokens;
      right;
      _
    }: Syn.Cst.infix_expression) =
    let parts = infix_chain operator_tokens
      (
        Syn.Cst.Expression.Infix {
          syntax_node;
          left;
          operator_tokens;
          right;
          attributes = [];
        }
      )
    in
    let render_part ~with_leading_trivia expression =
      let rendered = render_expression expression in
      if with_leading_trivia then
        rendered
        |> doc_with_boundary_trivia
          (boundary_trivia_before_node ~after:0 (Syn.Cst.Expression.syntax_node expression))
      else
        rendered
    in
    match parts with
    | [] -> Doc.empty
    | first :: rest ->
        let separator = Doc.concat
          [ Doc.break (); doc_of_operator_tokens operator_tokens; Doc.space ] in
        Doc.group
          (Doc.concat
            (render_part ~with_leading_trivia:false first
            :: List.concat_map
              (fun part -> [ separator; render_part ~with_leading_trivia:true part ])
              rest))
  and render_apply_expression ({ syntax_node; callee; argument; _ }: Syn.Cst.apply_expression) =
    let rec collect_arguments = fun acc ->
      function
      | Syn.Cst.Expression.Apply { callee; argument; _ } -> collect_arguments (argument :: acc) callee
      | expression -> (expression, acc)
    in
    let head, arguments = collect_arguments [ argument ] callee in
    let rendered_head =
      match head with
      | Syn.Cst.Expression.Parenthesized _ as expression -> render_parenthesized_expression expression
      | _ -> render_expression head
    in
    let rendered_arguments = arguments
    |> List.map
      (fun argument ->
        render_apply_argument argument
        |> doc_with_boundary_trivia
          (boundary_trivia_before_node ~after:0 (apply_argument_syntax_node argument))) in
    let rendered_argument_pairs = List.combine arguments rendered_arguments in
    if
      List.exists
        (fun ((argument, doc)) -> apply_argument_prefers_break argument || Doc.is_multiline doc)
        rendered_argument_pairs
    then
      let rec split_inline_prefix = fun acc ->
        function
        | (argument, doc) :: rest when not (apply_argument_prefers_break argument)
        && not (Doc.is_multiline doc) -> split_inline_prefix (doc :: acc) rest
        | rest -> (List.rev acc, rest)
      in
      let inline_arguments, multiline_arguments = split_inline_prefix [] rendered_argument_pairs in
      let head_with_inline_arguments = Doc.concat
        (rendered_head :: List.map (fun argument -> Doc.concat [ Doc.space; argument ]) inline_arguments) in
      (
        match multiline_arguments with
        | [] -> head_with_inline_arguments
        | multiline_arguments ->
            Doc.concat
              [ head_with_inline_arguments; Doc.line; Doc.indent 2
                  (multiline_arguments |> List.map snd |> Doc.join Doc.line); ]
      )
    else
      Doc.group
        (Doc.concat
          [
            rendered_head;
            Doc.indent
              2
              (Doc.concat
                (List.map (fun argument -> Doc.concat [ Doc.break (); argument ]) rendered_arguments));
          ])
  and render_if_expression ({
      syntax_node;
      keyword_token;
      then_token;
      else_token;
      condition;
      then_branch;
      else_branch;
      _
    }: Syn.Cst.if_expression) = render_if_expression_block
    {
      syntax_node;
      keyword_token;
      then_token;
      else_token;
      condition;
      then_branch;
      else_branch;
      attributes = [];
    }
  and render_case ?(force_multiline_body = false) ?(force_leading_bar = false) (
    case: Syn.Cst.match_case
  ) =
    let body_trivia = pending_doc_of_trivia
      (Syn.Cst.leading_trivia_before_node
        ~after:(Syn.Cst.Token.span case.arrow_token).end_
        (Syn.Cst.Expression.syntax_node case.body)) in
    let body = render_expression case.body |> doc_with_leading_trivia body_trivia in
    let prefix =
      match case.bar_token with
      | Some token -> Doc.concat [ doc_of_token token; Doc.space ]
      | None when force_leading_bar -> Doc.concat [ Doc.bar; Doc.space ]
      | None -> Doc.empty
    in
    let rendered_pattern =
      match case.pattern with
      | Syn.Cst.Pattern.Tuple { elements; _ } ->
          join_map (Doc.concat [ Doc.comma; Doc.space ])
            (fun (element: Syn.Cst.tuple_pattern_element) ->
              match element.label_token with
              | None -> render_pattern element.pattern
              | Some label_token -> Doc.concat
                [ doc_of_token label_token; render_pattern element.pattern ])
            elements
      | pattern -> render_pattern pattern
    in
    let render_branch pattern =
      let guard =
        match case.guard, case.when_token with
        | Some guard, Some when_token -> Doc.concat
          [ Doc.space; doc_of_token when_token; Doc.space; render_expression guard ]
        | Some guard, None -> Doc.concat [ Doc.space; kw_when; Doc.space; render_expression guard ]
        | None, _ -> Doc.empty
      in
      match case.body with
      | Syn.Cst.Expression.Parenthesized _ when Doc.is_multiline body -> Doc.concat
        [
          prefix;
          pattern;
          guard;
          Doc.space;
          doc_of_token case.arrow_token;
          Doc.space;
          Doc.indent 2 body;
        ]
      | _ when force_multiline_body
      || Doc.is_multiline body
      || expression_prefers_multiline_layout case.body -> Doc.concat
        [
          prefix;
          pattern;
          guard;
          Doc.space;
          doc_of_token case.arrow_token;
          Doc.line;
          Doc.indent 4 body;
        ]
      | _ -> Doc.concat
        [ prefix; pattern; guard; Doc.space; doc_of_token case.arrow_token; Doc.space; body ]
    in
    match case.pattern with
    | Syn.Cst.Pattern.Or { alternatives; _ } -> (
        match List.rev alternatives with
        | [] -> Doc.empty
        | last :: rest_reversed ->
            let leading = rest_reversed
            |> List.rev
            |> List.map
              (fun alternative -> Doc.concat [ prefix; render_pattern alternative; Doc.line ]) in
            Doc.concat (leading @ [ render_branch (render_pattern last) ])
      )
    | _ -> render_branch rendered_pattern
  and render_match_expression ~keyword_token ~scrutinee ~with_token ~cases =
    let force_multiline_cases = List.length cases > 2 && List.exists case_body_prefers_multiline cases in
    let scrutinee_doc =
      match scrutinee with
      | Syn.Cst.Expression.Tuple { elements; _ } -> join_map
        (Doc.concat [ Doc.comma; Doc.space ])
        render_expression
        elements
      | _ when expression_prefers_multiline_layout scrutinee -> render_block_expression scrutinee
      | _ -> render_expression scrutinee
    in
    let head = Doc.concat [ doc_of_token keyword_token; Doc.space; scrutinee_doc; ] in
    if expression_prefers_multiline_layout scrutinee || Doc.is_multiline scrutinee_doc then
      Doc.concat
        [
          doc_of_token keyword_token;
          Doc.line;
          Doc.indent 2 scrutinee_doc;
          Doc.line;
          doc_of_token with_token;
          Doc.line;
          join_map
            Doc.line
            (render_case ~force_multiline_body:force_multiline_cases ~force_leading_bar:true)
            cases;
        ]
    else
      Doc.concat
        [
          head;
          Doc.space;
          doc_of_token with_token;
          Doc.line;
          join_map
            Doc.line
            (render_case ~force_multiline_body:force_multiline_cases ~force_leading_bar:true)
            cases;
        ]
  and flatten_fun_expression ({ parameters; return_type; body; _ }: Syn.Cst.fun_expression) =
    let rec loop = fun acc ->
      function
      | Syn.Cst.Expression (Syn.Cst.Expression.Fun ({ return_type=Some _; _ } as body)) ->
          (List.rev acc, Syn.Cst.Expression (Syn.Cst.Expression.Fun body))
      | Syn.Cst.Cases _ as body -> (List.rev acc, body)
      | Syn.Cst.Expression (Syn.Cst.Expression.Fun { parameters; body; return_type=None; _ }) -> loop
        (List.rev_append parameters acc)
        body
      | Syn.Cst.Expression expression -> (List.rev acc, Syn.Cst.Expression expression)
    in
    match return_type with
    | Some _ -> (parameters, body)
    | None -> loop (List.rev parameters) body
  and render_fun_expression ({
      keyword_token;
      colon_token;
      arrow_token;
      parameters=_;
      return_type;
      body=_;
      _
    } as fun_: Syn.Cst.fun_expression) =
    let parameters = fun_.parameters in
    let body = fun_.body in
    let parameters = parameters |> List.map render_parameter in
    let return_type_doc = return_type |> Option.map render_core_type in
    let has_multiline_parameter = List.exists Doc.is_multiline parameters in
    let body = render_fun_body body in
    let body_syntax_node =
      match fun_.body with
      | Syn.Cst.Expression expression -> Syn.Cst.Expression.syntax_node expression
      | Syn.Cst.Cases cases -> cases.syntax_node
    in
    let body_trivia = pending_doc_of_trivia
      (Syn.Cst.leading_trivia_before_node ~after:(Syn.Cst.Token.span arrow_token).end_ body_syntax_node) in
    let body = doc_with_leading_trivia body_trivia body in
    let body_prefers_multiline =
      match body with
      | _ when Doc.is_multiline body -> true
      | _ -> (
          match fun_.body with
          | Syn.Cst.Expression expression -> function_body_prefers_multiline expression
          | Syn.Cst.Cases _ -> true
        )
    in
    let head =
      let parameters =
        if parameters = [] then
          Doc.empty
        else
          Doc.concat [ Doc.space; Doc.join Doc.space parameters ]
      in
      let head = Doc.concat [ doc_of_token keyword_token; parameters ] in
      match return_type_doc with
      | Some return_type_doc -> Doc.concat
        [
          head;
          Doc.space;
          (
            match colon_token with
            | Some colon_token -> doc_of_token colon_token
            | None -> Doc.colon
          );
          Doc.space;
          return_type_doc;
        ]
      | None -> head
    in
    if has_multiline_parameter then
      Doc.concat
        [
          doc_of_token keyword_token;
          Doc.line;
          Doc.indent 2 (
            let parameter_doc = Doc.join Doc.space parameters in
            match return_type_doc with
            | Some return_type_doc -> Doc.concat
              [
                parameter_doc;
                Doc.space;
                (
                  match colon_token with
                  | Some colon_token -> doc_of_token colon_token
                  | None -> Doc.colon
                );
                Doc.space;
                return_type_doc;
                Doc.space;
                doc_of_token arrow_token;
              ]
            | None -> Doc.concat [ parameter_doc; Doc.space; doc_of_token arrow_token ]
          );
          Doc.line;
          Doc.indent 2 body;
        ]
    else if body_prefers_multiline || List.length parameters = 0 then
      Doc.concat
        [ head; Doc.space; doc_of_token arrow_token; Doc.line; Doc.indent 2 body; ]
    else
      render_fun_arrow_body
        ~keyword:head
        ~parameters:Doc.empty
        ~arrow_token
        ~body
  and render_function_expression ({ keyword_token; cases; _ }: Syn.Cst.function_expression) =
    let force_multiline_cases = List.length cases > 2 && List.exists case_body_prefers_multiline cases in
    Doc.concat
      [
        doc_of_token keyword_token;
        Doc.line;
        join_map
          Doc.line
          (render_case ~force_multiline_body:force_multiline_cases ~force_leading_bar:true)
          cases;
      ]
  and render_function_expression_unindented (
    { keyword_token; cases; _ }: Syn.Cst.function_expression
  ) =
    let force_multiline_cases = List.length cases > 2 && List.exists case_body_prefers_multiline cases in
    Doc.concat
      [
        doc_of_token keyword_token;
        Doc.line;
        join_map
          Doc.line
          (render_case ~force_multiline_body:force_multiline_cases ~force_leading_bar:true)
          cases;
      ]
  and render_fun_body = function
    | Syn.Cst.Expression (Syn.Cst.Expression.Tuple { elements; _ }) ->
        render_tuple_expression_bare elements
    | Syn.Cst.Expression body ->
        if expression_prefers_multiline_layout body then
          render_block_expression body
        else
          render_expression body
    | Syn.Cst.Cases { cases; _ } ->
        let force_multiline_cases =
          List.length cases > 2 && List.exists case_body_prefers_multiline cases in
        Doc.concat
          [
            kw_function;
            Doc.line;
            join_map
              Doc.line
              (render_case ~force_multiline_body:force_multiline_cases ~force_leading_bar:true)
              cases;
          ]
  and render_block_expression = function
    | Syn.Cst.Expression.If if_ -> render_if_expression_block if_
    | Syn.Cst.Expression.Match match_ -> render_match_expression
      ~keyword_token:match_.keyword_token
      ~scrutinee:match_.scrutinee
      ~with_token:match_.with_token
      ~cases:match_.cases
    | Syn.Cst.Expression.Try try_ -> render_match_expression
      ~keyword_token:try_.keyword_token
      ~scrutinee:try_.body
      ~with_token:try_.with_token
      ~cases:try_.cases
    | Syn.Cst.Expression.LetOperator let_operator -> render_let_operator_expression let_operator
    | Syn.Cst.Expression.Let let_ -> render_let_expression let_
    | Syn.Cst.Expression.Sequence sequence -> render_sequence_expression sequence
    | Syn.Cst.Expression.Function function_ -> render_function_expression_unindented function_
    | Syn.Cst.Expression.Fun fun_ -> render_fun_expression fun_
    | Syn.Cst.Expression.Parenthesized _ as expression -> render_parenthesized_expression expression
    | expression -> render_expression expression
  and render_if_expression_block ({
      keyword_token;
      then_token;
      else_token;
      condition;
      then_branch;
      else_branch;
      _
    }: Syn.Cst.if_expression) =
    let condition_doc = render_expression condition in
    let then_branch_leading_trivia = Syn.Cst.leading_trivia_before_node
      ~after:(Syn.Cst.Token.span then_token).end_
      (Syn.Cst.Expression.syntax_node then_branch) in
    let then_doc =
      if branch_prefers_multiline_layout then_branch then
        render_block_expression then_branch
      else
        render_expression then_branch
    in
    let then_doc = doc_with_leading_trivia (pending_doc_of_trivia then_branch_leading_trivia) then_doc in
    let then_branch_trailing_trivia =
      match else_token with
      | Some else_token -> Syn.Cst.leading_trivia_after
        ~after:((Syn.Cst.token_body_span (Syn.Cst.Expression.syntax_node then_branch)).end_)
        else_token
      | None -> []
    in
    let then_trivia =
      if List.is_empty then_branch_trailing_trivia then
        None
      else
        pending_doc_of_trivia then_branch_trailing_trivia
    in
    let then_doc = doc_with_trailing_trivia then_doc then_trivia in
    let head = Doc.group
      (Doc.concat
        [
          doc_of_token keyword_token;
          Doc.indent 2 (Doc.concat [ Doc.break (); condition_doc ]);
          Doc.break ();
          doc_of_token then_token;
        ]) in
    match else_branch, else_token with
    | None, _ -> (
        match then_branch with
        | Syn.Cst.Expression.Sequence { expressions=first :: rest; separator_token; _ } when not
          (rest = []) ->
            let first_doc = Doc.concat
              [
                head;
                Doc.line;
                Doc.indent 2 (render_expression first);
                doc_of_token separator_token
              ] in
            let tail_doc =
              rest
              |> List.mapi
                (fun index expression ->
                  let suffix =
                    if index < List.length rest - 1 then
                      doc_of_token separator_token
                    else
                      Doc.empty
                  in
                  Doc.concat [ render_expression expression; suffix ])
              |> Doc.join Doc.line
            in
            Doc.concat [ first_doc; Doc.line; tail_doc ]
        | _ -> Doc.concat [ head; Doc.line; Doc.indent 2 then_doc ]
      )
    | Some (Syn.Cst.Expression.If nested_if), Some else_token ->
        let else_branch_leading_trivia = Syn.Cst.leading_trivia_before_node
          ~after:(Syn.Cst.Token.span else_token).end_
          (Syn.Cst.Expression.syntax_node (Syn.Cst.Expression.If nested_if)) in
        let else_trivia = pending_doc_of_trivia else_branch_leading_trivia in
        Doc.concat
          [ head; Doc.line; Doc.indent 2 then_doc; Doc.line; doc_of_token else_token; (
              match else_trivia with
              | None -> Doc.space
              | Some _ -> Doc.line
            ); (
              match else_trivia with
              | None -> render_if_expression_block nested_if
              | Some trivia -> Doc.indent
                2
                (doc_with_leading_trivia (Some trivia) (render_if_expression_block nested_if))
            ); ]
    | Some else_branch, Some else_token ->
        let else_branch_leading_trivia = Syn.Cst.leading_trivia_before_node
          ~after:(Syn.Cst.Token.span else_token).end_
          (Syn.Cst.Expression.syntax_node else_branch) in
        let else_doc =
          if branch_prefers_multiline_layout else_branch then
            render_block_expression else_branch
          else
            render_expression else_branch
        in
        let else_trivia = pending_doc_of_trivia else_branch_leading_trivia in
        let else_doc = doc_with_leading_trivia else_trivia else_doc in
        (
          match else_branch with
          | Syn.Cst.Expression.Parenthesized { inner=Syn.Cst.Expression.Sequence _; _ } -> Doc.concat
            [
              head;
              Doc.line;
              Doc.indent 2 then_doc;
              Doc.line;
              doc_of_token else_token;
              Doc.space;
              else_doc;
            ]
          | _ -> Doc.concat
            [
              head;
              Doc.line;
              Doc.indent 2 then_doc;
              Doc.line;
              doc_of_token else_token;
              Doc.line;
              Doc.indent 2 else_doc;
            ]
        )
    | Some else_branch, None ->
        let else_doc = render_expression else_branch in
        Doc.concat
          [
            head;
            Doc.line;
            Doc.indent 2 then_doc;
            Doc.line;
            kw_else;
            Doc.line;
            Doc.indent 2 else_doc
          ]
  and render_fun_arrow_body ~keyword ~parameters ~arrow_token ~body = Doc.group
    (
      Doc.concat
        [ keyword; (
            if parameters = Doc.empty then
              Doc.empty
            else
              Doc.concat [ Doc.space; parameters ]
          ); Doc.space; doc_of_token arrow_token; Doc.concat [ Doc.break (); Doc.indent 2 body ]; ]
    )
  and render_parenthesized_expression = function
    | Syn.Cst.Expression.Parenthesized {
      opening_token;
      closing_token;
      grouping;
      inner;
      _
    } ->
        let inner_leading_trivia = Syn.Cst.leading_trivia_before_node
          ~after:(Syn.Cst.Token.span opening_token).end_
          (Syn.Cst.Expression.syntax_node inner) in
        let rendered_inner = render_expression inner
        |> doc_with_leading_trivia (pending_doc_of_trivia inner_leading_trivia) in
        let has_inner_leading_trivia =
          match inner_leading_trivia with
          | [] -> false
          | _ -> true
        in
        (
          match grouping with
          | Syn.Cst.BeginEnd -> Doc.concat
            [
              doc_of_token opening_token;
              Doc.line;
              Doc.indent 2 rendered_inner;
              Doc.line;
              doc_of_token closing_token;
            ]
          | Syn.Cst.Parens -> (
              match inner with
              | Syn.Cst.Expression.Parenthesized _ when not has_inner_leading_trivia ->
                  render_expression inner
              | Syn.Cst.Expression.Tuple _
              | Syn.Cst.Expression.List _
              | Syn.Cst.Expression.Array _
              | Syn.Cst.Expression.Record _ ->
                  if has_inner_leading_trivia then
                    Doc.concat
                      [
                        doc_of_token opening_token;
                        Doc.line;
                        Doc.indent 2 rendered_inner;
                        Doc.line;
                        doc_of_token closing_token;
                      ]
                  else
                    render_expression inner
              | _ when expression_is_function_like inner ->
                  Doc.concat
                    [
                      doc_of_token opening_token;
                      Doc.line;
                      Doc.indent 2 rendered_inner;
                      Doc.line;
                      doc_of_token closing_token;
                    ]
              | _ -> (
                  match collapse_redundant_parenthesized_expression inner with
                  | Some (`NegativeLiteral literal) ->
                      if has_inner_leading_trivia then
                        Doc.concat
                          [
                            doc_of_token opening_token;
                            Doc.line;
                            Doc.indent 2 rendered_inner;
                            Doc.line;
                            doc_of_token closing_token;
                          ]
                      else
                        Doc.concat
                          [
                            doc_of_token opening_token;
                            Doc.text "-";
                            render_literal literal;
                            doc_of_token closing_token;
                          ]
                  | Some (`Expression expression) ->
                      if has_inner_leading_trivia then
                        Doc.concat
                          [
                            doc_of_token opening_token;
                            Doc.line;
                            Doc.indent 2 rendered_inner;
                            Doc.line;
                            doc_of_token closing_token;
                          ]
                      else
                        render_expression expression
                  | None ->
                      if
                        expression_prefers_multiline_layout inner || Doc.is_multiline rendered_inner
                      then
                        (
                          match inner with
                          | Syn.Cst.Expression.Function _
                          | Syn.Cst.Expression.Fun _ -> Doc.concat
                            [
                              doc_of_token opening_token;
                              rendered_inner;
                              doc_of_token closing_token;
                            ]
                          | _ -> Doc.concat
                            [
                              doc_of_token opening_token;
                              Doc.line;
                              Doc.indent 2 rendered_inner;
                              Doc.line;
                              doc_of_token closing_token;
                            ]
                        )
                      else
                        Doc.concat
                          [ doc_of_token opening_token; rendered_inner; doc_of_token closing_token; ]
                )
            )
        )
    | expression -> render_expression expression
  and render_let_module_expression ({
      module_name_token;
      equals_token;
      module_expression;
      body;
      _
    }: Syn.Cst.let_module_expression) =
    let header = Doc.concat
      [
        kw_let;
        Doc.space;
        kw_module;
        Doc.space;
        doc_of_token module_name_token;
        Doc.space;
        doc_of_token equals_token;
        Doc.space;
        render_module_expression_doc module_expression;
        Doc.space;
        kw_in;
      ]
    in
    Doc.concat [ header; Doc.line; render_expression body ]
  and render_exception_declaration (decl: Syn.Cst.exception_declaration) =
    let rhs_doc =
      match decl.rhs with
      | None ->
          Doc.empty
      | Some (Syn.Cst.Alias { equals_token; alias }) ->
          let leading_equals_trivia = pending_doc_of_token_leading_trivia equals_token in
          let leading_alias_trivia = pending_doc_of_trivia_before_node
            ~after:(Syn.Cst.Token.span equals_token).end_
            (Syn.Cst.Ident.syntax_node alias) in
          if leading_equals_trivia = None && leading_alias_trivia = None then
            Doc.concat [ Doc.space; doc_of_token equals_token; Doc.space; doc_of_ident alias ]
          else
            let equals_doc = doc_of_token equals_token |> doc_with_leading_trivia leading_equals_trivia in
            let alias_doc = doc_of_ident alias |> doc_with_leading_trivia leading_alias_trivia in
            Doc.concat [ Doc.line; Doc.indent 2 equals_doc; Doc.line; Doc.indent 2 alias_doc; ]
      | Some (Syn.Cst.Payload { of_token; payload_type }) ->
          let leading_of_trivia = pending_doc_of_token_leading_trivia of_token in
          let leading_payload_trivia = pending_doc_of_trivia_before_node
            ~after:(Syn.Cst.Token.span of_token).end_
            (Syn.Cst.CoreType.syntax_node payload_type) in
          if leading_of_trivia = None && leading_payload_trivia = None then
            Doc.concat [ Doc.space; doc_of_token of_token; Doc.space; render_core_type payload_type ]
          else
            let of_doc = doc_of_token of_token |> doc_with_leading_trivia leading_of_trivia in
            let payload_doc = render_core_type payload_type |> doc_with_leading_trivia leading_payload_trivia in
            Doc.concat [ Doc.line; Doc.indent 2 of_doc; Doc.line; Doc.indent 2 payload_doc; ]
    in
    Doc.concat [ doc_of_token decl.keyword_token; Doc.space; doc_of_token decl.name_token; rhs_doc ]
  and render_let_exception_expression (
    { exception_declaration; body; _ }: Syn.Cst.let_exception_expression
  ) =
    let exception_doc = render_exception_declaration exception_declaration in
    Doc.concat
      [ kw_let; Doc.space; exception_doc; Doc.space; kw_in; Doc.line; render_expression body; ]
  and render_binding_operator_binding ({
      keyword_token;
      operator_token;
      equals_token;
      binding_pattern;
      bound_value;
      _
    }: Syn.Cst.binding_operator_binding) =
    let header = Doc.concat
      [
        doc_of_token keyword_token;
        doc_of_token operator_token;
        Doc.space;
        render_pattern binding_pattern;
      ] in
    let leading_value_trivia = pending_doc_of_trivia_before_node
      ~after:(Syn.Cst.Token.span equals_token).end_
      (Syn.Cst.Expression.syntax_node bound_value) in
    let rendered_value = render_expression bound_value |> doc_with_leading_trivia leading_value_trivia in
    let keep_value_after_equals = local_binding_value_stays_after_equals
      ~leading_value_trivia
      ~has_parameters:false
      ~keep_header_parameters:false
      bound_value
    |> adjust_local_binding_value_after_equals ~rendered_value bound_value in
    let keep_value_after_equals = keep_value_after_equals && not (Doc.is_multiline rendered_value) in
    if keep_value_after_equals then
      Doc.concat [ header; Doc.space; doc_of_token equals_token; Doc.space; rendered_value ]
    else
      Doc.concat
        [ header; Doc.space; doc_of_token equals_token; Doc.line; Doc.indent 2 rendered_value ]
  and render_let_operator_expression (
    { binding; in_token; body; _ }: Syn.Cst.let_operator_expression
  ) =
    let and_bindings =
      match binding.and_binding with
      | Some next -> binding_operator_group_items next
      | None -> []
    in
    let bindings = Doc.concat
      (render_binding_operator_binding binding
      :: List.map
        (fun binding -> Doc.concat [ Doc.line; binding ])
        (List.map render_binding_operator_binding and_bindings)) in
    let body_trivia = pending_doc_of_trivia_before_node
      ~after:(Syn.Cst.Token.span in_token).end_
      (Syn.Cst.Expression.syntax_node body) in
    let body_doc = render_expression body |> doc_with_leading_trivia body_trivia in
    if Doc.is_multiline bindings then
      Doc.concat [ bindings; Doc.line; doc_of_token in_token; Doc.line; body_doc ]
    else
      Doc.concat [ bindings; Doc.space; doc_of_token in_token; Doc.line; body_doc ]
  and render_sequence_expression ({ separator_tokens; expressions; _ }: Syn.Cst.sequence_expression) =
    let separator_token_at index = List.nth_opt separator_tokens index in
    let rec render_sequence_items = fun previous_expression index ->
      function
      | [] -> []
      | expression :: rest ->
          let leading_trivia =
            match previous_expression with
            | None -> None
            | Some previous_expression -> (
                match separator_token_at (index - 1) with
                | Some separator_token -> pending_doc_of_trivia
                  (Syn.Cst.leading_trivia_after_token_before_node
                    ~after:((Syn.Cst.token_body_span
                      (Syn.Cst.Expression.syntax_node previous_expression)).end_)
                    separator_token
                    (Syn.Cst.Expression.syntax_node expression))
                | None -> None
              )
          in
          let suffix =
            match separator_token_at index with
            | Some separator_token -> doc_of_token separator_token
            | None -> Doc.empty
          in
          Doc.concat
            [ doc_with_leading_trivia leading_trivia (render_expression expression); suffix ]
          :: render_sequence_items (Some expression) (index + 1) rest
    in
    render_sequence_items None 0 expressions |> Doc.join Doc.line
  and render_binding_header ~keyword_token ~rec_token pattern =
    let rec_part =
      match rec_token with
      | None -> []
      | Some token -> [ Doc.space; doc_of_token token ]
    in
    let pattern =
      match pattern with
      | Syn.Cst.Pattern.Tuple { elements; _ } ->
          join_map (Doc.concat [ Doc.comma; Doc.space ])
            (fun (element: Syn.Cst.tuple_pattern_element) ->
              match element.label_token with
              | None -> render_pattern element.pattern
              | Some label_token -> Doc.concat
                [ doc_of_token label_token; render_pattern element.pattern ])
            elements
      | _ -> render_pattern pattern
    in
    Doc.concat ([ doc_of_token keyword_token ] @ rec_part @ [ Doc.space; pattern ])
  and split_typed_binding_value = function
    | Syn.Cst.Expression.TypeAscription { expression; kind=Syn.Cst.Type { colon_token; type_ }; _ } -> (
      expression,
      Some (colon_token, type_)
    )
    | Syn.Cst.Expression.Polymorphic { expression; colon_token; type_; _ } -> (
      expression,
      Some (colon_token, type_)
    )
    | expression -> (expression, None)
  and split_typed_binding_pattern = function
    | Syn.Cst.Pattern.Typed { pattern; colon_token; type_; _ } -> (
      pattern,
      Some (colon_token, type_)
    )
    | pattern -> (pattern, None)
  and render_positional_parameter_pattern pattern =
    let pattern_doc = render_pattern pattern in
    let pattern_doc_is_already_parenthesized =
      match pattern with
      | Syn.Cst.Pattern.Typed _ -> true
      | Syn.Cst.Pattern.Tuple _
      | Syn.Cst.Pattern.List _
      | Syn.Cst.Pattern.Array _
      | Syn.Cst.Pattern.Record _ -> true
      | Syn.Cst.Pattern.Parenthesized {
        inner=(Syn.Cst.Pattern.Tuple _ | Syn.Cst.Pattern.List _ | Syn.Cst.Pattern.Array _ | Syn.Cst.Pattern.Record _);
        _
      } -> true
      | Syn.Cst.Pattern.Parenthesized _ -> true
      | _ -> false
    in
    if pattern_is_simple_function_parameter pattern || pattern_doc_is_already_parenthesized then
      pattern_doc
    else
      Doc.concat [ Doc.lparen; pattern_doc; Doc.rparen ]
  and render_named_parameter_binding_pattern_internal ~include_type pattern =
    let pattern, type_ = split_typed_binding_pattern pattern in
    let pattern_doc =
      if pattern_requires_parens_in_named_parameter pattern then
        Doc.concat [ Doc.lparen; render_pattern pattern; Doc.rparen ]
      else
        render_pattern pattern
    in
    match type_ with
    | Some (colon_token, type_) when include_type -> Doc.concat
      [ pattern_doc; doc_of_token colon_token; render_core_type type_ ]
    | Some _
    | None -> pattern_doc
  and render_named_parameter_binding_pattern pattern = render_named_parameter_binding_pattern_internal
    ~include_type:true
    pattern
  and named_parameter_binding_pattern_can_be_elided pattern =
    let pattern, _ = split_typed_binding_pattern pattern in
    match pattern with
    | Syn.Cst.Pattern.Identifier _ -> true
    | _ -> false
  and render_named_parameter_internal ~include_type ~sigil_token ~label_token ~colon_token ~binding_name_matches_label ~binding_pattern =
    match binding_pattern with
    | None ->
        Doc.concat [ doc_of_token sigil_token; doc_of_token label_token ]
    | Some pattern when binding_name_matches_label && named_parameter_binding_pattern_can_be_elided pattern ->
        let _, type_ = split_typed_binding_pattern pattern in
        (
          match include_type, type_ with
          | true, Some _ -> Doc.concat
            [
              doc_of_token sigil_token;
              Doc.lparen;
              render_named_parameter_binding_pattern pattern;
              Doc.rparen;
            ]
          | _ -> Doc.concat [ doc_of_token sigil_token; doc_of_token label_token ]
        )
    | Some pattern ->
        let colon_token =
          match colon_token with
          | Some colon_token -> colon_token
          | None -> unsupported "named parameter binding missing colon token"
        in
        Doc.concat
          [
            doc_of_token sigil_token;
            doc_of_token label_token;
            doc_of_token colon_token;
            render_named_parameter_binding_pattern_internal ~include_type pattern;
          ]
  and render_named_parameter ~sigil_token ~label_token ~colon_token ~binding_name_matches_label ~binding_pattern = render_named_parameter_internal
    ~include_type:true
    ~sigil_token
    ~label_token
    ~colon_token
    ~binding_name_matches_label
    ~binding_pattern
  and render_unsugared_named_parameter ~sigil_token ~label_token ~colon_token ~binding_name_matches_label ~binding_pattern = render_named_parameter_internal
    ~include_type:false
    ~sigil_token
    ~label_token
    ~colon_token
    ~binding_name_matches_label
    ~binding_pattern
  and render_optional_parameter_with_default_internal ~include_type ~sigil_token ~label_token ~colon_token ~equals_token ~binding_name_matches_label ~binding_pattern ~default_value =
    let binding_doc =
      match binding_pattern with
      | Some pattern -> render_named_parameter_binding_pattern_internal ~include_type pattern
      | None -> doc_of_token label_token
    in
    if binding_name_matches_label then
      let equals_token =
        match equals_token with
        | Some equals_token -> equals_token
        | None -> unsupported "optional parameter default missing equals token"
      in
      Doc.concat
        [
          doc_of_token sigil_token;
          Doc.lparen;
          binding_doc;
          Doc.space;
          doc_of_token equals_token;
          Doc.space;
          render_expression default_value;
          Doc.rparen;
        ]
    else
      let colon_token =
        match colon_token with
        | Some colon_token -> colon_token
        | None -> unsupported "optional parameter alias missing colon token"
      in
      let equals_token =
        match equals_token with
        | Some equals_token -> equals_token
        | None -> unsupported "optional parameter default missing equals token"
      in
      Doc.concat
        [
          doc_of_token sigil_token;
          doc_of_token label_token;
          doc_of_token colon_token;
          Doc.lparen;
          binding_doc;
          Doc.space;
          doc_of_token equals_token;
          Doc.space;
          render_expression default_value;
          Doc.rparen;
        ]
  and render_optional_parameter_with_default ~sigil_token ~label_token ~colon_token ~equals_token ~binding_name_matches_label ~binding_pattern ~default_value = render_optional_parameter_with_default_internal
    ~include_type:true
    ~sigil_token
    ~label_token
    ~colon_token
    ~equals_token
    ~binding_name_matches_label
    ~binding_pattern
    ~default_value
  and render_unsugared_optional_parameter_with_default ~sigil_token ~label_token ~colon_token ~equals_token ~binding_name_matches_label ~binding_pattern ~default_value = render_optional_parameter_with_default_internal
    ~include_type:false
    ~sigil_token
    ~label_token
    ~colon_token
    ~equals_token
    ~binding_name_matches_label
    ~binding_pattern
    ~default_value
  and render_unsugared_binding_parameter = function
    | Syn.Cst.Parameter.Positional { pattern; _ } ->
        let pattern, _ = split_typed_binding_pattern pattern in
        render_positional_parameter_pattern pattern
    | Syn.Cst.Parameter.Labeled {
      sigil_token;
      label_token;
      colon_token;
      binding_name_matches_label;
      binding_pattern;
      _
    } ->
        render_unsugared_named_parameter
          ~sigil_token
          ~label_token
          ~colon_token
          ~binding_name_matches_label
          ~binding_pattern
    | Syn.Cst.Parameter.Optional {
      sigil_token;
      label_token;
      colon_token;
      equals_token;
      binding_name_matches_label;
      binding_pattern;
      default_value;
      _
    } -> (
        match default_value with
        | Some default_value -> render_unsugared_optional_parameter_with_default
          ~sigil_token
          ~label_token
          ~colon_token
          ~equals_token
          ~binding_name_matches_label
          ~binding_pattern
          ~default_value
        | None -> render_unsugared_named_parameter
          ~sigil_token
          ~label_token
          ~colon_token
          ~binding_name_matches_label
          ~binding_pattern
      )
    | Syn.Cst.Parameter.LocallyAbstract parameter ->
        render_parameter (Syn.Cst.Parameter.LocallyAbstract parameter)
  and render_parameter = function
    | Syn.Cst.Parameter.Positional { pattern; _ } ->
        render_positional_parameter_pattern pattern
    | Syn.Cst.Parameter.Labeled {
      sigil_token;
      label_token;
      colon_token;
      binding_name_matches_label;
      binding_pattern;
      _
    } ->
        render_named_parameter ~sigil_token ~label_token ~colon_token ~binding_name_matches_label ~binding_pattern
    | Syn.Cst.Parameter.Optional {
      sigil_token;
      label_token;
      colon_token;
      equals_token;
      binding_name_matches_label;
      binding_pattern;
      default_value;
      _
    } -> (
        match default_value with
        | Some default_value -> render_optional_parameter_with_default
          ~sigil_token
          ~label_token
          ~colon_token
          ~equals_token
          ~binding_name_matches_label
          ~binding_pattern
          ~default_value
        | None -> render_named_parameter
          ~sigil_token
          ~label_token
          ~colon_token
          ~binding_name_matches_label
          ~binding_pattern
      )
    | Syn.Cst.Parameter.LocallyAbstract { binders; _ } ->
        Doc.concat
          [
            Doc.lparen;
            kw_type;
            Doc.space;
            join_map (Doc.concat [ Doc.space ]) render_type_binder binders;
            Doc.rparen;
          ]
  and render_binding_value ~leading_body_trivia ~force_multiline_body ~parameters ~value =
    match parameters with
    | [] -> (
        let value = collapse_redundant_parenthesized_value_after_equals value in
        match value with
        | Syn.Cst.Expression.Fun ({ keyword_token; arrow_token; _ } as fun_) when force_multiline_body ->
            let parameters, body = flatten_fun_expression fun_ in
            let parameters = parameters |> List.map render_parameter |> Doc.join Doc.space in
            let body = render_fun_body body in
            Doc.concat
              [ doc_of_token keyword_token; (
                  if parameters = Doc.empty then
                    Doc.empty
                  else
                    Doc.concat [ Doc.space; parameters ]
                ); Doc.space; doc_of_token arrow_token; Doc.line; Doc.indent 2 body; ]
        | _ ->
            if expression_requires_break_after_equals value then
              render_block_expression value
            else
              render_expression value
      )
    | parameters ->
        let force_multiline_body =
          force_multiline_body || parameters_mix_complex_positional_and_named parameters in
        let parameters = parameters |> List.map render_parameter |> Doc.join Doc.space in
        let has_multiline_parameters = Doc.is_multiline parameters in
        let body =
          match value with
          | Syn.Cst.Expression.Tuple { elements; _ } -> render_tuple_expression_bare elements
          | _ -> render_expression value
        in
        let body = doc_with_leading_trivia leading_body_trivia body in
        if has_multiline_parameters then
          Doc.concat
            [
              kw_fun;
              Doc.line;
              Doc.indent 2 (Doc.concat [ parameters; Doc.space; Doc.arrow ]);
              Doc.line;
              Doc.indent 2 body;
            ]
        else if
          force_multiline_body || function_body_prefers_multiline value || Doc.is_multiline body
        then
          Doc.concat
            [ kw_fun; Doc.space; parameters; Doc.space; Doc.arrow; Doc.line; Doc.indent 2 body; ]
        else
          Doc.group
            (Doc.concat
              [
                kw_fun;
                Doc.space;
                parameters;
                Doc.space;
                Doc.arrow;
                Doc.indent 2 (Doc.concat [ Doc.break (); body ]);
              ])
  and render_binding_value_with_parameter_doc ~leading_body_trivia ~force_multiline_body ~parameter_doc ~value =
    let body =
      match value with
      | Syn.Cst.Expression.Tuple { elements; _ } -> render_tuple_expression_bare elements
      | _ -> render_expression value
    in
    let body = doc_with_leading_trivia leading_body_trivia body in
    if force_multiline_body || function_body_prefers_multiline value || Doc.is_multiline body then
      Doc.concat
        [ kw_fun; Doc.space; parameter_doc; Doc.space; Doc.arrow; Doc.line; Doc.indent 2 body; ]
    else
      Doc.group
        (Doc.concat
          [
            kw_fun;
            Doc.space;
            parameter_doc;
            Doc.space;
            Doc.arrow;
            Doc.indent 2 (Doc.concat [ Doc.break (); body ]);
          ])
  and local_binding_value_stays_after_equals ~leading_value_trivia ~has_parameters ~keep_header_parameters value =
    if Option.is_some leading_value_trivia && (not has_parameters || keep_header_parameters) then
      false
    else if has_parameters && not keep_header_parameters then
      true
    else
      match value with
      | Syn.Cst.Expression.Fun _
      | Syn.Cst.Expression.Function _ -> true
      | _ when expression_is_boolean_infix value -> false
      | _ when expression_requires_break_after_equals value -> false
      | _ -> expression_is_simple_after_equals value
  and adjust_local_binding_value_after_equals ~rendered_value value stays_after_equals =
    match value with
    | Syn.Cst.Expression.Fun _
    | Syn.Cst.Expression.Function _ -> stays_after_equals
    | _ when expression_is_pipeline value && Doc.is_multiline rendered_value -> false
    | _ -> stays_after_equals
  and local_binding_keeps_parameters_in_header ~parameters ~rendered_type_annotation ~synthesized_type_annotation =
    not (parameters = []) && not synthesized_type_annotation
  and local_binding_forces_multiline_body ~local_context ~rec_token value =
    local_context
    && Option.is_some rec_token
    && (
      expression_prefers_multiline_layout value || match value with
      | Syn.Cst.Expression.Fun _ -> true
      | _ -> false
    )
  and render_local_binding ~local_context ~source_has_explicit_fun ~keyword_token ~rec_token ~equals_token ~leading_binding_trivia ~(leading_value_trivia:Doc.t option) ~pattern ~parameters ~value =
    let pattern, type_annotation_from_pattern = split_typed_binding_pattern pattern in
    let value, type_annotation = split_typed_binding_value value in
    let type_annotation =
      match type_annotation with
      | Some _ -> type_annotation
      | None -> type_annotation_from_pattern
    in
    let lifted_parameters, lifted_body_expression =
      match parameters, value, type_annotation with
      | _, _, Some _ ->
          ([], None)
      | [], Syn.Cst.Expression.Fun fun_, None when not source_has_explicit_fun ->
          let fun_parameters, fun_body = flatten_fun_expression fun_ in
          (
            match fun_body with
            | Syn.Cst.Expression body_expression -> (fun_parameters, Some body_expression)
            | Syn.Cst.Cases _ -> ([], None)
          )
      | _ ->
          ([], None)
    in
    let parameters = parameters @ lifted_parameters in
    let value =
      match lifted_body_expression with
      | Some body_expression -> body_expression
      | None -> value
    in
    let rendered_type_annotation, parameters, synthesized_type_annotation =
      match type_annotation with
      | None -> (None, parameters, false)
      | Some (colon_token, type_) -> (
        Some (Some colon_token, render_core_type type_),
        parameters,
        false
      )
    in
    let header = render_binding_header ~keyword_token ~rec_token pattern in
    let parameter_doc =
      parameters
      |> List.map
        (
          if synthesized_type_annotation then
            render_unsugared_binding_parameter
          else
            render_parameter
        )
      |> Doc.join Doc.space
    in
    let keep_header_parameters = local_binding_keeps_parameters_in_header
      ~parameters
      ~rendered_type_annotation
      ~synthesized_type_annotation in
    let header =
      if keep_header_parameters then
        Doc.concat [ header; Doc.space; parameter_doc ]
      else
        header
    in
    let header =
      match rendered_type_annotation with
      | None -> header
      | Some (colon_token, type_doc) -> (
          match colon_token with
          | Some colon_token -> render_tight_token_rhs header colon_token type_doc
          | None -> render_tight_doc_rhs header Doc.colon type_doc
        )
    in
    let force_multiline_body = local_binding_forces_multiline_body ~local_context ~rec_token value in
    let keep_value_after_equals = local_binding_value_stays_after_equals
      ~leading_value_trivia
      ~has_parameters:(not (parameters = []))
      ~keep_header_parameters
      value in
    let rendered_value =
      match value with
      | Syn.Cst.Expression.Function function_ when parameters = [] && not keep_value_after_equals ->
          render_function_expression_unindented function_
      | _ when not (parameters = []) ->
          if keep_header_parameters then
            if expression_requires_break_after_equals value then
              render_block_expression value
            else
              render_expression value
          else if synthesized_type_annotation then
            render_binding_value_with_parameter_doc
              ~leading_body_trivia:leading_value_trivia
              ~force_multiline_body
              ~parameter_doc
              ~value
          else
            render_binding_value
              ~leading_body_trivia:leading_value_trivia
              ~force_multiline_body
              ~parameters
              ~value
      | _ ->
          render_binding_value ~leading_body_trivia:None ~force_multiline_body ~parameters:[] ~value
    in
    let rendered_value =
      if parameters = [] || keep_header_parameters then
        doc_with_leading_trivia leading_value_trivia rendered_value
      else
        rendered_value
    in
    let keep_value_after_equals = adjust_local_binding_value_after_equals ~rendered_value value keep_value_after_equals in
    let rendered_binding =
      if keep_value_after_equals then
        (
          match value with
          | Syn.Cst.Expression.Fun _ -> Doc.concat
            [ header; Doc.space; doc_of_token equals_token; Doc.space; rendered_value; ]
          | Syn.Cst.Expression.Function _ -> Doc.concat
            [ header; Doc.space; doc_of_token equals_token; Doc.space; Doc.indent 2 rendered_value; ]
          | _ -> Doc.concat
            [ header; Doc.space; doc_of_token equals_token; Doc.space; rendered_value; ]
        )
      else if
        not (expression_is_simple_after_equals value)
        || expression_prefers_multiline_layout value
        || Doc.is_multiline rendered_value
      then
        let rendered_value =
          match value with
          | Syn.Cst.Expression.Infix ({ operator_tokens; _ } as infix) when parameters = [] || keep_header_parameters ->
              let parts = infix_chain operator_tokens (Syn.Cst.Expression.Infix infix) in
              let rendered_value = join_map
                (Doc.concat [ Doc.line; doc_of_operator_tokens operator_tokens; Doc.space ])
                render_expression
                parts in
              doc_with_leading_trivia leading_value_trivia rendered_value
          | _ -> rendered_value
        in
        Doc.concat
          [ header; Doc.space; doc_of_token equals_token; Doc.line; Doc.indent 2 rendered_value; ]
      else
        Doc.group
          (Doc.concat
            [
              header;
              Doc.space;
              doc_of_token equals_token;
              Doc.indent 2 (Doc.concat [ Doc.break (); rendered_value ]);
            ])
    in
    doc_with_leading_trivia leading_binding_trivia rendered_binding
  and render_let_expression ({
      keyword_token;
      rec_token;
      equals_token;
      binding_pattern;
      parameters;
      bound_value;
      and_binding;
      body;
      in_token;
      _
    }: Syn.Cst.let_expression) =
    let leading_value_trivia = pending_doc_of_trivia_before_node
      ~after:(Syn.Cst.Token.span equals_token).end_
      (Syn.Cst.Expression.syntax_node bound_value) in
    let first_binding = render_local_binding
      ~local_context:true
      ~source_has_explicit_fun:false
      ~keyword_token
      ~rec_token
      ~equals_token
      ~leading_binding_trivia:None
      ~leading_value_trivia
      ~pattern:binding_pattern
      ~parameters
      ~value:bound_value in
    let and_bindings = Option.to_list and_binding
    |> List.concat_map let_binding_group_items
    |> List.map
      (fun (binding: Syn.Cst.let_binding) ->
        render_local_binding
          ~local_context:true
          ~source_has_explicit_fun:false
          ~keyword_token:binding.keyword_token
          ~rec_token:binding.rec_token
          ~equals_token:binding.equals_token
          ~leading_binding_trivia:(pending_doc_of_token_leading_trivia binding.keyword_token)
          ~leading_value_trivia:(pending_doc_of_trivia_before_node
            ~after:(Syn.Cst.Token.span binding.equals_token).end_
            (Syn.Cst.LetBinding.value_syntax_node binding))
          ~pattern:binding.binding_pattern
          ~parameters:binding.parameters
          ~value:binding.value) in
    let bindings = Doc.concat
      (first_binding :: List.map (fun binding -> Doc.concat [ Doc.line; binding ]) and_bindings) in
    let body_trivia = pending_doc_of_trivia_before_node
      ~after:(Syn.Cst.Token.span in_token).end_
      (Syn.Cst.Expression.syntax_node body) in
    let body_doc = render_expression body |> doc_with_leading_trivia body_trivia in
    if Doc.is_multiline first_binding then
      Doc.concat [ bindings; Doc.line; doc_of_token in_token; Doc.line; body_doc; ]
    else
      Doc.concat [ bindings; Doc.space; doc_of_token in_token; Doc.line; body_doc; ]
  and render_let_binding_group_item ?leading_binding_trivia_override (binding: Syn.Cst.let_binding) =
    let source_has_explicit_fun = binding_has_explicit_fun_rhs binding in
    let leading_binding_trivia =
      match leading_binding_trivia_override with
      | Some leading_binding_trivia -> leading_binding_trivia
      | None -> pending_doc_of_token_leading_trivia binding.keyword_token
    in
    render_local_binding
      ~local_context:false
      ~keyword_token:binding.keyword_token
      ~source_has_explicit_fun
      ~rec_token:binding.rec_token
      ~equals_token:binding.equals_token
      ~leading_binding_trivia
      ~leading_value_trivia:(pending_doc_of_trivia_before_node
        ~after:(Syn.Cst.Token.span binding.equals_token).end_
        (Syn.Cst.LetBinding.value_syntax_node binding))
      ~pattern:binding.binding_pattern
      ~parameters:binding.parameters
      ~value:binding.value
  and render_let_binding (binding: Syn.Cst.let_binding) =
    let first = render_let_binding_group_item ~leading_binding_trivia_override:None binding in
    let trailing = Syn.Cst.LetBinding.and_bindings binding
    |> List.map
      (fun and_binding -> Doc.concat [ blank_line; render_let_binding_group_item and_binding; ]) in
    Doc.concat (first :: trailing)
  and nested_signature_items_from_module_type module_type =
    match Syn.CstBuilder.signature_items_of_module_type module_type with
    | Ok items -> items
    | Error error -> unsupported_with_context_entries
      ~context:((Context_label "module_type"
      :: Context_syntax_kind error.syntax_kind
      :: List.map (fun label -> Context_label label) error.context))
      error.message
  and nested_structure_items_from_module_expression module_expression =
    match Syn.CstBuilder.structure_items_of_module_expression module_expression with
    | Ok items -> items
    | Error error -> unsupported_with_context_entries
      ~context:((Context_label "module_expression"
      :: Context_syntax_kind error.syntax_kind
      :: List.map (fun label -> Context_label label) error.context))
      error.message
  and render_module_type_constraint ~keyword (constraint_: Syn.Cst.module_type_constraint) =
    let separator = Doc.concat [ Doc.space; doc_of_token constraint_.separator_token; Doc.space ] in
    Doc.concat
      [
        keyword;
        Doc.space;
        kw_type;
        Doc.space;
        render_core_type constraint_.constrained_type;
        separator;
        render_core_type constraint_.replacement_type;
      ]
  and render_functor_parameter (
    { name_token; colon_token; module_type; _ }: Syn.Cst.functor_parameter
  ) = Doc.concat
    [
      Doc.lparen;
      doc_of_token name_token;
      Doc.space;
      doc_of_token colon_token;
      Doc.space;
      render_module_type_doc module_type;
      Doc.rparen;
    ]
  and render_module_type_doc = function
    | Syn.Cst.ModuleType.Path path ->
        doc_of_ident path
    | Syn.Cst.ModuleType.TypeOf { of_token; module_path; _ } ->
        Doc.concat
          [
            kw_module;
            Doc.space;
            kw_type;
            Doc.space;
            doc_of_token of_token;
            Doc.space;
            doc_of_ident module_path
          ]
    | (Syn.Cst.ModuleType.Signature { syntax_node; _ } as module_type) ->
        let body = render_signature_items
          ~source_node:syntax_node
          (nested_signature_items_from_module_type module_type) in
        Doc.concat [ Doc.text "sig"; Doc.line; Doc.indent 2 body; Doc.line; Doc.text "end"; ]
    | Syn.Cst.ModuleType.Functor { parameters; result; _ } ->
        Doc.concat
          [
            Doc.text "functor";
            Doc.space;
            Doc.join Doc.space (List.map render_functor_parameter parameters);
            Doc.space;
            Doc.arrow;
            Doc.space;
            render_module_type_doc result;
          ]
    | Syn.Cst.ModuleType.With { base; constraints; _ } ->
        let first, rest =
          match constraints with
          | [] -> (Doc.empty, [])
          | first :: rest -> (render_module_type_constraint ~keyword:kw_with first, rest)
        in
        Doc.concat
          (render_module_type_doc base
          :: Doc.space
          :: first
          :: List.map
            (fun constraint_ ->
              Doc.concat [ Doc.space; render_module_type_constraint ~keyword:kw_and constraint_; ])
            rest)
    | Syn.Cst.ModuleType.Parenthesized { opening_token; inner; closing_token; _ } ->
        Doc.concat
          [ doc_of_token opening_token; render_module_type_doc inner; doc_of_token closing_token ]
    | Syn.Cst.ModuleType.Attribute { module_type; attribute; _ } ->
        Doc.concat [ render_module_type_doc module_type; Doc.space; render_attribute attribute ]
    | Syn.Cst.ModuleType.Extension extension ->
        render_extension_doc extension
  and render_module_application_argument = function
    | Syn.Cst.ModuleExpression.Parenthesized { opening_token; inner; closing_token; _ } -> Doc.concat
      [ doc_of_token opening_token; render_module_expression_doc inner; doc_of_token closing_token ]
    | argument -> Doc.concat [ Doc.lparen; render_module_expression_doc argument; Doc.rparen ]
  and render_module_expression_doc = function
    | Syn.Cst.ModuleExpression.Path path ->
        doc_of_ident path
    | (Syn.Cst.ModuleExpression.Structure { syntax_node; _ } as module_expression) ->
        let body = render_structure_items
          ~source_node:syntax_node
          (nested_structure_items_from_module_expression module_expression) in
        Doc.concat [ Doc.text "struct"; Doc.line; Doc.indent 2 body; Doc.line; Doc.text "end"; ]
    | Syn.Cst.ModuleExpression.Functor { parameters; body; _ } ->
        Doc.concat
          [
            Doc.text "functor";
            Doc.space;
            Doc.join Doc.space (List.map render_functor_parameter parameters);
            Doc.space;
            Doc.arrow;
            Doc.space;
            render_module_expression_doc body;
          ]
    | Syn.Cst.ModuleExpression.Apply { callee; argument; _ } ->
        Doc.concat
          [
            render_module_expression_doc callee;
            Doc.space;
            render_module_application_argument argument;
          ]
    | Syn.Cst.ModuleExpression.ApplyUnit { callee; _ } ->
        Doc.concat [ render_module_expression_doc callee; Doc.space; Doc.lparen; Doc.rparen ]
    | Syn.Cst.ModuleExpression.Constraint { module_expression; colon_token; module_type; _ } ->
        Doc.concat
          [
            render_module_expression_doc module_expression;
            Doc.space;
            doc_of_token colon_token;
            Doc.space;
            render_module_type_doc module_type;
          ]
    | Syn.Cst.ModuleExpression.ModuleUnpack {
      opening_token;
      expression;
      colon_token;
      package_type;
      closing_token;
      _
    } ->
        let constraint_doc =
          match package_type with
          | None -> Doc.empty
          | Some package_type ->
              let colon_token =
                match colon_token with
                | Some colon_token -> colon_token
                | None -> unsupported "module unpack package type missing colon token"
              in
              Doc.concat
                [
                  Doc.space;
                  doc_of_token colon_token;
                  Doc.space;
                  render_package_type_doc package_type
                ]
        in
        Doc.concat
          [
            doc_of_token opening_token;
            kw_val;
            Doc.space;
            render_expression expression;
            constraint_doc;
            doc_of_token closing_token;
          ]
    | Syn.Cst.ModuleExpression.Parenthesized { opening_token; inner; closing_token; _ } ->
        Doc.concat
          [
            doc_of_token opening_token;
            render_module_expression_doc inner;
            doc_of_token closing_token
          ]
    | Syn.Cst.ModuleExpression.Attribute { module_expression; attribute; _ } ->
        Doc.concat
          [ render_module_expression_doc module_expression; Doc.space; render_attribute attribute ]
    | Syn.Cst.ModuleExpression.Extension extension ->
        render_extension_doc extension
  and render_module_signature_with_keyword _keyword_doc (decl: Syn.Cst.ModuleSignature.t) =
    let rest = Syn.Cst.ModuleSignature.and_declarations decl in
    Doc.join
      blank_line
      (render_module_signature_header ~include_keyword_leading_trivia:false decl
      :: List.map (render_module_signature_header ~include_keyword_leading_trivia:true) rest)
  and render_module_signature_header ~include_keyword_leading_trivia (
    decl: Syn.Cst.ModuleSignature.t
  ) =
    let keyword_token = Syn.Cst.ModuleSignature.keyword_token decl in
    let rec_token = Syn.Cst.ModuleSignature.rec_token decl in
    let module_name = Syn.Cst.ModuleSignature.module_name_token decl in
    let functor_parameters = Syn.Cst.ModuleSignature.functor_parameters decl in
    let header = Doc.concat
      [ (
          if include_keyword_leading_trivia then
            doc_of_token_with_leading_trivia keyword_token
          else
            doc_of_token keyword_token
        ); (
          match rec_token with
          | None -> Doc.empty
          | Some rec_token -> Doc.concat [ Doc.space; doc_of_token_with_leading_trivia rec_token ]
        ); Doc.space; doc_of_token_with_leading_trivia module_name; (
          if functor_parameters = [] then
            Doc.empty
          else
            Doc.concat
              [
                Doc.space;
                Doc.join Doc.space (List.map render_functor_parameter functor_parameters);
              ]
        ); ]
    in
    match Syn.Cst.ModuleSignature.definition decl with
    | Syn.Cst.ModuleSignature.Signature module_type ->
        let colon_token =
          match Syn.Cst.ModuleSignature.colon_token decl with
          | Some colon_token -> colon_token
          | None -> unsupported "module signature with module type missing colon token"
        in
        render_tight_colon_block_rhs header colon_token (render_module_type_doc module_type)
    | Syn.Cst.ModuleSignature.Alias module_expression ->
        let equals_token =
          match Syn.Cst.ModuleSignature.equals_token decl with
          | Some equals_token -> equals_token
          | None -> unsupported "module signature alias missing equals token"
        in
        Doc.concat
          [
            header;
            Doc.space;
            doc_of_token_with_leading_trivia equals_token;
            Doc.space;
            render_module_expression_doc module_expression
          ]
  and render_module_structure_with_keyword _keyword_doc (decl: Syn.Cst.ModuleStructure.t) =
    let rest = Syn.Cst.ModuleStructure.and_declarations decl in
    Doc.join
      blank_line
      (render_module_structure_header ~include_keyword_leading_trivia:false decl
      :: List.map (render_module_structure_header ~include_keyword_leading_trivia:true) rest)
  and render_module_structure_header ~include_keyword_leading_trivia (
    decl: Syn.Cst.ModuleStructure.t
  ) =
    let keyword_token = Syn.Cst.ModuleStructure.keyword_token decl in
    let rec_token = Syn.Cst.ModuleStructure.rec_token decl in
    let module_name = Syn.Cst.ModuleStructure.module_name_token decl in
    let functor_parameters = Syn.Cst.ModuleStructure.functor_parameters decl in
    let module_type = Syn.Cst.ModuleStructure.module_type decl in
    let module_expression = Syn.Cst.ModuleStructure.module_expression decl in
    let header = Doc.concat
      [ (
          if include_keyword_leading_trivia then
            doc_of_token_with_leading_trivia keyword_token
          else
            doc_of_token keyword_token
        ); (
          match rec_token with
          | None -> Doc.empty
          | Some rec_token -> Doc.concat [ Doc.space; doc_of_token_with_leading_trivia rec_token ]
        ); Doc.space; doc_of_token_with_leading_trivia module_name; (
          if functor_parameters = [] then
            Doc.empty
          else
            Doc.concat
              [
                Doc.space;
                Doc.join Doc.space (List.map render_functor_parameter functor_parameters);
              ]
        ); ]
    in
    let header =
      match module_type with
      | None -> header
      | Some module_type ->
          let colon_token =
            match Syn.Cst.ModuleStructure.colon_token decl with
            | Some colon_token -> colon_token
            | None -> unsupported "module structure with module type missing colon token"
          in
          render_tight_colon_block_rhs header colon_token (render_module_type_doc module_type)
    in
    let equals_token = Syn.Cst.ModuleStructure.equals_token decl in
    match module_expression with
    | Syn.Cst.ModuleExpression.Constraint { module_expression; _ } when Option.is_some module_type -> Doc.concat
      [
        header;
        Doc.space;
        doc_of_token_with_leading_trivia equals_token;
        Doc.space;
        render_module_expression_doc module_expression
      ]
    | module_expression -> Doc.concat
      [
        header;
        Doc.space;
        doc_of_token_with_leading_trivia equals_token;
        Doc.space;
        render_module_expression_doc module_expression
      ]
  and render_module_type_declaration ~leading_after ({
      module_keyword_token;
      type_keyword_token;
      module_type_name;
      equals_token;
      module_type;
      _
    }: Syn.Cst.ModuleTypeDeclaration.t) =
    let header = Doc.concat
      [
        doc_of_token_with_filtered_leading_trivia ~after:leading_after module_keyword_token;
        Doc.space;
        doc_of_token type_keyword_token;
        Doc.space;
        doc_of_token module_type_name;
      ] in
    match module_type with
    | None -> header
    | Some module_type ->
        let equals_token =
          match equals_token with
          | Some equals_token -> equals_token
          | None -> unsupported "module type declaration missing equals token"
        in
        Doc.concat
          [
            header;
            Doc.space;
            doc_of_token_with_leading_trivia equals_token;
            Doc.space;
            render_module_type_doc module_type;
          ]
  and render_open_target = function
    | Syn.Cst.OpenStatement.Path path -> doc_of_ident path
    | Syn.Cst.OpenStatement.ModuleExpression expression -> render_module_expression_doc expression
  and render_include_statement ({ keyword_token; target; _ }: Syn.Cst.include_statement) =
    let target =
      match target with
      | Syn.Cst.ModuleExpression expression -> render_module_expression_doc expression
      | Syn.Cst.ModuleType module_type -> render_module_type_doc module_type
    in
    Doc.concat [ doc_of_token keyword_token; Doc.space; target ]
  and is_module_alias_structure_item = function
    | Syn.Cst.StructureItem.ModuleDeclaration {
      functor_parameters=[];
      module_type=None;
      module_expression=Syn.Cst.ModuleExpression.Path _;
      _;

    } -> true
    | _ -> false
  and is_open_structure_item = function
    | item when is_module_alias_structure_item item -> true
    | Syn.Cst.StructureItem.OpenStatement _ -> true
    | _ -> false
  and is_open_signature_item = function
    | Syn.Cst.SignatureItem.OpenStatement _ -> true
    | _ -> false
  and render_structure_entry ~trailing_suffix ~leading_after item =
    let doc =
      let base_doc =
        match item with
        | Syn.Cst.StructureItem.TypeDeclaration decl -> render_type_declaration_with_keyword
          ~leading_after
          kw_type
          decl
        | Syn.Cst.StructureItem.ModuleTypeDeclaration decl -> render_module_type_declaration
          ~leading_after
          decl
        | Syn.Cst.StructureItem.ClassDeclaration decl -> render_class_definition ~leading_after decl
        | Syn.Cst.StructureItem.ClassTypeDeclaration decl -> render_class_type_declaration
          ~leading_after
          decl
        | _ -> render_structure_item item
      in
      match trailing_suffix with
      | None -> base_doc
      | Some suffix -> Doc.concat [ base_doc; suffix ]
    in
    let is_trivia =
      match item with
      | Syn.Cst.StructureItem.Docstring docstring -> not (Syn.Cst.Docstring.is_section docstring)
      | Syn.Cst.StructureItem.Comment _ -> false
      | _ -> false
    in
    let tight_after = false in
    let is_docstring =
      match item with
      | Syn.Cst.StructureItem.Docstring docstring -> not (Syn.Cst.Docstring.is_section docstring)
      | _ -> false
    in
    let is_type_declaration =
      match item with
      | Syn.Cst.StructureItem.TypeDeclaration _ -> true
      | _ -> false
    in
    let compact_before =
      match item with
      | Syn.Cst.StructureItem.Attribute _ -> true
      | _ -> false
    in
    (
      doc,
      is_open_structure_item item,
      is_trivia,
      tight_after,
      false,
      is_docstring,
      is_type_declaration,
      compact_before
    )
  and render_signature_entry ~trailing_suffix ~leading_after item =
    let doc =
      let base_doc =
        match item with
        | Syn.Cst.SignatureItem.TypeDeclaration decl -> render_type_declaration_with_keyword
          ~leading_after
          kw_type
          decl
        | Syn.Cst.SignatureItem.ValueDeclaration decl -> render_signature_value_declaration
          ~leading_after
          decl
        | Syn.Cst.SignatureItem.ModuleTypeDeclaration decl -> render_module_type_declaration
          ~leading_after
          decl
        | Syn.Cst.SignatureItem.ClassDeclaration decl -> render_class_declaration ~leading_after decl
        | Syn.Cst.SignatureItem.ClassTypeDeclaration decl -> render_class_type_declaration
          ~leading_after
          decl
        | _ -> render_signature_item item
      in
      match trailing_suffix with
      | None -> base_doc
      | Some suffix -> Doc.concat [ base_doc; suffix ]
    in
    let is_trivia =
      match item with
      | Syn.Cst.SignatureItem.Docstring docstring -> not (Syn.Cst.Docstring.is_section docstring)
      | Syn.Cst.SignatureItem.Comment _ -> false
      | _ -> false
    in
    let tight_after =
      match item with
      | Syn.Cst.SignatureItem.TypeDeclaration _ -> true
      | _ -> false
    in
    let compact_after = false in
    let is_docstring =
      match item with
      | Syn.Cst.SignatureItem.Docstring docstring -> not (Syn.Cst.Docstring.is_section docstring)
      | _ -> false
    in
    (doc, is_open_signature_item item, is_trivia, tight_after, false, compact_after, is_docstring)
  and render_structure_item = function
    | Syn.Cst.StructureItem.LetBinding binding -> render_let_binding binding
    | Syn.Cst.StructureItem.TypeDeclaration decl -> render_type_declaration_with_keyword kw_type decl
    | Syn.Cst.StructureItem.TypeExtension decl -> render_type_extension decl
    | Syn.Cst.StructureItem.ExternalDeclaration decl -> render_external_declaration decl
    | Syn.Cst.StructureItem.ModuleDeclaration decl -> render_module_structure_with_keyword
      kw_module
      decl
    | Syn.Cst.StructureItem.ModuleTypeDeclaration decl -> render_module_type_declaration
      ~leading_after:0
      decl
    | Syn.Cst.StructureItem.IncludeStatement stmt -> render_include_statement stmt
    | Syn.Cst.StructureItem.OpenStatement open_ ->
        Doc.concat
          [ doc_of_token (Syn.Cst.OpenStatement.keyword_token open_); (
              match open_.bang_token with
              | None -> Doc.empty
              | Some bang_token -> doc_of_token bang_token
            ); Doc.space; render_open_target open_.target; ]
    | Syn.Cst.StructureItem.Attribute attribute -> render_floating_attribute attribute
    | Syn.Cst.StructureItem.Docstring docstring -> doc_of_token (Syn.Cst.Docstring.token docstring)
    | Syn.Cst.StructureItem.Comment comment -> doc_of_token (Syn.Cst.Comment.token comment)
    | Syn.Cst.StructureItem.ExceptionDeclaration decl -> render_exception_declaration decl
    | Syn.Cst.StructureItem.Expression expression -> render_expression expression
    | Syn.Cst.StructureItem.Extension extension -> render_extension_doc extension
    | Syn.Cst.StructureItem.ClassDeclaration decl -> render_class_definition ~leading_after:0 decl
    | Syn.Cst.StructureItem.ClassTypeDeclaration decl -> render_class_type_declaration
      ~leading_after:0
      decl
  and render_signature_value_declaration ~leading_after decl =
    let head = Doc.concat
      [
        doc_of_declaration_token_with_filtered_leading_trivia
          ~after:leading_after
          (Syn.Cst.ValueDeclaration.keyword_token decl);
        Doc.space;
        render_value_declaration_name decl;
      ] in
    let base = render_tight_colon_rhs
      head
      (Syn.Cst.ValueDeclaration.colon_token decl)
      (render_core_type decl.type_) in
    match Syn.Cst.ValueDeclaration.trailing_comment decl with
    | Some comment -> Doc.concat [ base; Doc.space; doc_of_token (Syn.Cst.Comment.token comment) ]
    | None -> base
  and render_signature_item item =
    match item with
    | Syn.Cst.SignatureItem.TypeDeclaration decl -> render_type_declaration_with_keyword kw_type decl
    | Syn.Cst.SignatureItem.TypeExtension decl -> render_type_extension decl
    | Syn.Cst.SignatureItem.ModuleDeclaration decl -> render_module_signature_with_keyword
      kw_module
      decl
    | Syn.Cst.SignatureItem.ModuleTypeDeclaration decl -> render_module_type_declaration
      ~leading_after:0
      decl
    | Syn.Cst.SignatureItem.IncludeStatement stmt -> render_include_statement stmt
    | Syn.Cst.SignatureItem.OpenStatement open_ ->
        Doc.concat
          [ doc_of_token (Syn.Cst.OpenStatement.keyword_token open_); (
              match open_.bang_token with
              | None -> Doc.empty
              | Some bang_token -> doc_of_token bang_token
            ); Doc.space; render_open_target open_.target; ]
    | Syn.Cst.SignatureItem.Attribute attribute -> render_floating_attribute attribute
    | Syn.Cst.SignatureItem.Docstring docstring -> doc_of_token (Syn.Cst.Docstring.token docstring)
    | Syn.Cst.SignatureItem.Comment comment -> doc_of_token (Syn.Cst.Comment.token comment)
    | Syn.Cst.SignatureItem.ValueDeclaration decl -> render_signature_value_declaration
      ~leading_after:0
      decl
    | Syn.Cst.SignatureItem.ExternalDeclaration decl -> render_external_declaration decl
    | Syn.Cst.SignatureItem.ExceptionDeclaration decl -> render_exception_declaration decl
    | Syn.Cst.SignatureItem.Extension extension -> render_extension_doc extension
    | Syn.Cst.SignatureItem.ClassDeclaration decl -> render_class_declaration ~leading_after:0 decl
    | Syn.Cst.SignatureItem.ClassTypeDeclaration decl -> render_class_type_declaration
      ~leading_after:0
      decl
  and render_class_declaration ~leading_after (declaration: Syn.Cst.ClassDeclaration.t) =
    let type_params = Syn.Cst.ClassDeclaration.type_params declaration in
    let declaration_extension = Syn.Cst.ClassDeclaration.declaration_extension declaration in
    let declaration_attributes = Syn.Cst.ClassDeclaration.declaration_attributes declaration in
    let class_name = Syn.Cst.ClassDeclaration.class_name_token declaration in
    let keyword_token = Syn.Cst.ClassDeclaration.keyword_token declaration in
    let keyword =
      match declaration_extension with
      | None -> doc_of_token_with_filtered_leading_trivia ~after:leading_after keyword_token
      | Some extension -> Doc.concat
        [
          doc_of_token_with_filtered_leading_trivia ~after:leading_after keyword_token;
          extension.sigil_tokens |> List.map doc_of_token |> Doc.concat;
          doc_of_ident extension.name;
          render_extension_payload_doc_with_context ~context:extension_payload_context extension;
        ]
    in
    let head =
      let params = render_type_parameters type_params in
      if params = Doc.empty then
        Doc.concat
          [ keyword; Doc.space; join_map Doc.space render_attribute declaration_attributes; (
              if declaration_attributes = [] then
                Doc.empty
              else
                Doc.space
            ); doc_of_token class_name ]
      else
        Doc.concat
          [ keyword; Doc.space; join_map Doc.space render_attribute declaration_attributes; (
              if declaration_attributes = [] then
                Doc.empty
              else
                Doc.space
            ); params; Doc.space; doc_of_token class_name; ]
    in
    Doc.concat
      [
        head;
        doc_of_token (Syn.Cst.ClassDeclaration.colon_token declaration);
        render_class_type_doc (Syn.Cst.ClassDeclaration.class_type declaration);
      ]
  and render_class_definition ~leading_after (declaration: Syn.Cst.ClassDefinition.t) =
    let type_params = Syn.Cst.ClassDefinition.type_params declaration in
    let declaration_extension = Syn.Cst.ClassDefinition.declaration_extension declaration in
    let declaration_attributes = Syn.Cst.ClassDefinition.declaration_attributes declaration in
    let class_name = Syn.Cst.ClassDefinition.class_name_token declaration in
    let keyword_token = Syn.Cst.ClassDefinition.keyword_token declaration in
    let keyword =
      match declaration_extension with
      | None -> doc_of_token_with_filtered_leading_trivia ~after:leading_after keyword_token
      | Some extension -> Doc.concat
        [
          doc_of_token_with_filtered_leading_trivia ~after:leading_after keyword_token;
          extension.sigil_tokens |> List.map doc_of_token |> Doc.concat;
          doc_of_ident extension.name;
          render_extension_payload_doc_with_context ~context:extension_payload_context extension;
        ]
    in
    let head =
      let params = render_type_parameters type_params in
      if params = Doc.empty then
        Doc.concat
          [ keyword; Doc.space; join_map Doc.space render_attribute declaration_attributes; (
              if declaration_attributes = [] then
                Doc.empty
              else
                Doc.space
            ); doc_of_token class_name ]
      else
        Doc.concat
          [ keyword; Doc.space; join_map Doc.space render_attribute declaration_attributes; (
              if declaration_attributes = [] then
                Doc.empty
              else
                Doc.space
            ); params; Doc.space; doc_of_token class_name; ]
    in
    match Syn.Cst.ClassDefinition.class_type declaration with
    | Some class_type ->
        let colon_token =
          match Syn.Cst.ClassDefinition.colon_token declaration with
          | Some colon_token -> colon_token
          | None -> unsupported "class definition with class type missing colon token"
        in
        Doc.concat
          [
            head;
            doc_of_token colon_token;
            render_class_type_doc class_type;
            Doc.space;
            doc_of_token_with_leading_trivia (Syn.Cst.ClassDefinition.equals_token declaration);
            Doc.space;
            render_class_expression (Syn.Cst.ClassDefinition.class_body declaration);
          ]
    | None -> Doc.concat
      [
        head;
        Doc.space;
        doc_of_token_with_leading_trivia (Syn.Cst.ClassDefinition.equals_token declaration);
        Doc.space;
        render_class_expression (Syn.Cst.ClassDefinition.class_body declaration);
      ]
  and render_class_type_declaration ~leading_after ({
      class_keyword_token;
      type_keyword_token;
      declaration_extension;
      declaration_attributes;
      class_type_name;
      equals_token;
      type_params;
      class_type_body;
      _;

    }: Syn.Cst.class_type_declaration) =
    let keyword =
      match declaration_extension with
      | None -> Doc.concat
        [
          doc_of_token_with_filtered_leading_trivia ~after:leading_after class_keyword_token;
          Doc.space;
          doc_of_token type_keyword_token;
        ]
      | Some extension -> Doc.concat
        [
          doc_of_token_with_filtered_leading_trivia ~after:leading_after class_keyword_token;
          Doc.space;
          doc_of_token type_keyword_token;
          extension.sigil_tokens |> List.map doc_of_token |> Doc.concat;
          doc_of_ident extension.name;
          render_extension_payload_doc_with_context ~context:extension_payload_context extension;
        ]
    in
    let head =
      let params = render_type_parameters type_params in
      if params = Doc.empty then
        Doc.concat
          [ keyword; Doc.space; join_map Doc.space render_attribute declaration_attributes; (
              if declaration_attributes = [] then
                Doc.empty
              else
                Doc.space
            ); doc_of_token class_type_name; ]
      else
        Doc.concat
          [ keyword; Doc.space; join_map Doc.space render_attribute declaration_attributes; (
              if declaration_attributes = [] then
                Doc.empty
              else
                Doc.space
            ); params; Doc.space; doc_of_token class_type_name; ]
    in
    Doc.concat
      [
        head;
        Doc.space;
        doc_of_token equals_token;
        Doc.space;
        render_class_type_doc class_type_body
      ]
  and render_structure_top_level_items ~trailing_phrase_separator_tokens ~items =
    let rec join_entries = function
      | [] ->
          Doc.empty
      | (doc, _, _, _, _, _, _, _) :: [] ->
          doc
      | (doc, is_open, is_trivia, tight_after, has_trailing_break, is_docstring, is_type_declaration, _compact_before) :: ((_, next_is_open, next_is_trivia, _, _, next_is_docstring, _, next_compact_before) :: _ as rest) ->
          let separator =
            if has_trailing_break then
              Doc.empty
            else if is_docstring && next_is_docstring then
              blank_line
            else if next_is_trivia then
              blank_line
            else if is_type_declaration && next_compact_before then
              Doc.line
            else if tight_after || is_trivia then
              Doc.line
            else if is_open && next_is_open then
              Doc.line
            else
              blank_line
          in
          Doc.concat [ doc; separator; join_entries rest ]
    in
    let rec loop_with_trailing acc previous_end items trailing_phrase_separator_tokens =
      yield ();
      match items with
      | [] -> join_entries (List.rev acc)
      | item :: rest ->
          let trailing_suffix =
            match trailing_phrase_separator_tokens with
            | trailing :: _ -> phrase_separator_doc_of_tokens trailing
            | [] -> None
          in
          let entry = render_structure_entry ~trailing_suffix ~leading_after:previous_end item in
          let next_previous_end =
            let span = Syn.Cst.token_body_span (Syn.Cst.StructureItem.syntax_node item) in
            span.end_
          in
          let remaining_trailing =
            match trailing_phrase_separator_tokens with
            | _ :: trailing_rest -> trailing_rest
            | [] -> []
          in
          loop_with_trailing (entry :: acc) next_previous_end rest remaining_trailing
    in
    loop_with_trailing [] 0 items trailing_phrase_separator_tokens
  and render_structure_items ?(trailing_phrase_separator_tokens = []) ~source_node items =
    let _source_node = source_node in
    render_structure_top_level_items ~trailing_phrase_separator_tokens ~items
  and render_signature_top_level_items ~items =
    let rec join_entries = function
      | [] ->
          Doc.empty
      | (doc, _, _, _, _, _, _) :: [] ->
          doc
      | (doc, is_open, is_trivia, tight_after, has_trailing_break, compact_after, is_docstring) :: ((_, next_is_open, next_is_trivia, _, _, _, next_is_docstring) :: _ as rest) ->
          let separator =
            if has_trailing_break then
              Doc.empty
            else if compact_after then
              Doc.empty
            else if is_docstring && next_is_docstring then
              blank_line
            else if next_is_trivia then
              blank_line
            else if tight_after || is_trivia then
              Doc.line
            else if is_open && next_is_open then
              Doc.line
            else
              blank_line
          in
          Doc.concat [ doc; separator; join_entries rest ]
    in
    let rec loop acc previous_end items =
      yield ();
      match items with
      | [] -> join_entries (List.rev acc)
      | item :: rest ->
          let entry = render_signature_entry ~trailing_suffix:None ~leading_after:previous_end item in
          let next_previous_end =
            let span = Syn.Cst.token_body_span (Syn.Cst.SignatureItem.syntax_node item) in
            span.end_
          in
          loop (entry :: acc) next_previous_end rest
    in
    loop [] 0 items
  and render_signature_items ~source_node:_ items = render_signature_top_level_items ~items
  in
  { render_structure_items; render_signature_items }

let source_file = fun source_file ->
  try
    let lowerer = make_lowerer in
    Ok (
      match source_file with
      | Syn.Cst.Implementation implementation -> lowerer.render_structure_items
        ~trailing_phrase_separator_tokens:implementation.trailing_phrase_separator_tokens
        ~source_node:implementation.syntax_node
        implementation.items
      | Syn.Cst.Interface interface -> lowerer.render_signature_items
        ~source_node:(Syn.Cst.SourceFile.syntax_node (Syn.Cst.Interface interface))
        interface.items
    )
  with
  | Unsupported err -> Error err

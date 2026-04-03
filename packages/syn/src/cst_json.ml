open Std
open Std.Data

let span_to_json = fun (span: Ceibo.Span.t) ->
  Json.Object [ ("start", Json.Int span.start); ("end", Json.Int span.end_) ]

let syntax_node_to_json = fun syntax_node ->
  Json.Object [
    ("kind", Json.String (Syntax_kind.to_string (Ceibo.Red.SyntaxNode.kind syntax_node)));
    ("span", span_to_json (Ceibo.Red.SyntaxNode.span syntax_node))
  ]

let syntax_token_to_json = fun syntax_token ->
  Json.Object [
    ("kind", Json.String (Syntax_kind.to_string (Ceibo.Red.SyntaxToken.kind syntax_token)));
    ("text", Json.String (Ceibo.Red.SyntaxToken.text syntax_token));
    ("span", span_to_json (Ceibo.Red.SyntaxToken.span syntax_token))
  ]

let option_to_json = fun to_json ->
  function
  | Some value -> to_json value
  | None -> Json.Null

let token_to_json = fun token -> syntax_token_to_json (Cst.Token.syntax_token token)

let docstring_to_json = fun (docstring: Cst.Docstring.t) ->
  Json.Object [
    ("syntax_node", syntax_node_to_json (Cst.Docstring.syntax_node docstring));
    ("docstring_token", token_to_json (Cst.Docstring.token docstring))
  ]

let comment_to_json = fun (comment: Cst.Comment.t) ->
  Json.Object [
    ("syntax_node", syntax_node_to_json (Cst.Comment.syntax_node comment));
    ("comment_token", token_to_json (Cst.Comment.token comment))
  ]

let trivia_to_json = function
  | Cst.Trivia.Docstring docstring -> Json.Object [
    ("tag", Json.String "docstring");
    ("value", docstring_to_json docstring)
  ]
  | Cst.Trivia.Comment comment -> Json.Object [
    ("tag", Json.String "comment");
    ("value", comment_to_json comment)
  ]

let expression_grouping_to_json = function
  | Cst.Parens -> Json.String "parens"
  | Cst.BeginEnd -> Json.String "begin_end"

let rec ident_to_json = function
  | Cst.Ident.Ident { syntax_node; name_token } -> Json.Object [
    ("tag", Json.String "ident");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("name_token", token_to_json name_token)
  ]
  | Cst.Ident.Qualified { syntax_node; prefix; dot_token; name_token } -> Json.Object [
    ("tag", Json.String "qualified");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("prefix", ident_to_json prefix);
    ("dot_token", token_to_json dot_token);
    ("name_token", token_to_json name_token)
  ]

let rec object_type_field_to_json = fun (
  { syntax_node; field_name; field_type }: Cst.object_type_field
) ->
  Json.Object [
    ("syntax_node", syntax_node_to_json syntax_node);
    ("field_name", token_to_json field_name);
    ("field_type", core_type_to_json field_type)
  ]

and record_type_field_to_json = fun
  ({
      syntax_node;
      field_name;
      mutable_token;
      field_type;
      semicolon_token;
      attributes
    }: Cst.record_type_field) ->
  let attributes = List.map attribute_to_json attributes in
  Json.Object (
    [
      ("syntax_node", syntax_node_to_json syntax_node);
      ("field_name", token_to_json field_name);
      ("mutable_token", option_to_json token_to_json mutable_token);
      ("field_type", core_type_to_json field_type);
      ("semicolon_token", option_to_json token_to_json semicolon_token)
    ]
    @ if attributes = [] then
      []
    else
      [ ("attributes", Json.Array attributes) ]
  )

and poly_variant_tag_to_json = fun
  ({
      syntax_node;
      attributes;
      bar_token;
      tag_name;
      separator_token;
      payload_type
    }: Cst.poly_variant_tag) ->
  let attributes = List.map attribute_to_json attributes in
  Json.Object (
    [
      ("syntax_node", syntax_node_to_json syntax_node);
      ("bar_token", option_to_json token_to_json bar_token);
      ("tag_name", token_to_json tag_name);
      ("separator_token", option_to_json token_to_json separator_token);
      ("payload_type", option_to_json core_type_to_json payload_type)
    ]
    @ if attributes = [] then
      []
    else
      [ ("attributes", Json.Array attributes) ]
  )

and poly_variant_bound_to_json = function
  | Cst.PolyVariantBound.Exact -> Json.Object [ ("tag", Json.String "exact") ]
  | Cst.PolyVariantBound.UpperBound { marker_token } -> Json.Object [
    ("tag", Json.String "upper_bound");
    ("marker_token", token_to_json marker_token)
  ]
  | Cst.PolyVariantBound.LowerBound { marker_token } -> Json.Object [
    ("tag", Json.String "lower_bound");
    ("marker_token", token_to_json marker_token)
  ]

and row_field_to_json = function
  | Cst.RowField.Tag tag -> Json.Object [
    ("tag", Json.String "tag");
    ("field", poly_variant_tag_to_json tag)
  ]
  | Cst.RowField.Inherit { bar_token; syntax_node; type_ } -> Json.Object [
    ("tag", Json.String "inherit");
    ("bar_token", option_to_json token_to_json bar_token);
    ("syntax_node", syntax_node_to_json syntax_node);
    ("type", core_type_to_json type_)
  ]

and type_binder_to_json = function
  | Cst.TypeBinder.Quoted { syntax_node; name_token } -> Json.Object [
    ("tag", Json.String "quoted");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("name_token", token_to_json name_token)
  ]
  | Cst.TypeBinder.Bare { name_token } -> Json.Object [
    ("tag", Json.String "bare");
    ("name_token", token_to_json name_token)
  ]

and string_delimiter_to_json = function
  | Cst.DoubleQuote -> Json.Object [ ("tag", Json.String "double_quote") ]
  | Cst.Quoted { marker } -> Json.Object [
    ("tag", Json.String "quoted");
    ("marker", Json.String marker)
  ]

and integer_base_to_json = function
  | Cst.Decimal -> Json.String "decimal"
  | Cst.Hexadecimal -> Json.String "hexadecimal"
  | Cst.Octal -> Json.String "octal"
  | Cst.Binary -> Json.String "binary"

and exponent_sign_to_json = function
  | Cst.Positive -> Json.String "positive"
  | Cst.Negative -> Json.String "negative"

and float_exponent_to_json = fun ({ marker; sign; digits }: Cst.float_exponent) ->
  Json.Object [
    ("marker", Json.String marker);
    ("sign", option_to_json exponent_sign_to_json sign);
    ("digits", Json.String digits)
  ]

and sign_token_field_to_json = function
  | None -> []
  | Some sign_token -> [ ("sign_token", token_to_json sign_token) ]

and constant_to_json = function
  | Cst.Constant.String {
    syntax_node;
    literal_token;
    delimiter;
    contents;
    terminated
  } -> Json.Object [
    ("tag", Json.String "string");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("literal_token", token_to_json literal_token);
    ("delimiter", string_delimiter_to_json delimiter);
    ("contents", Json.String contents);
    ("terminated", Json.Bool terminated)
  ]
  | Cst.Constant.Int {
    syntax_node;
    sign_token;
    literal_token;
    base;
    prefix;
    digits;
    suffix
  } -> Json.Object ([ ("tag", Json.String "int"); ("syntax_node", syntax_node_to_json syntax_node); ]
  @ sign_token_field_to_json sign_token
  @ [
    ("literal_token", token_to_json literal_token);
    ("base", integer_base_to_json base);
    ("prefix", option_to_json (fun text -> Json.String text) prefix);
    ("digits", Json.String digits);
    ("suffix", option_to_json (fun text -> Json.String text) suffix);
  ])
  | Cst.Constant.Float {
    syntax_node;
    sign_token;
    literal_token;
    integral_digits;
    fractional_digits;
    exponent;
    suffix
  } -> Json.Object ([
    ("tag", Json.String "float");
    ("syntax_node", syntax_node_to_json syntax_node);
  ]
  @ sign_token_field_to_json sign_token
  @ [
    ("literal_token", token_to_json literal_token);
    ("integral_digits", Json.String integral_digits);
    ("fractional_digits", Json.String fractional_digits);
    ("exponent", option_to_json float_exponent_to_json exponent);
    ("suffix", option_to_json (fun text -> Json.String text) suffix);
  ])
  | Cst.Constant.Char { syntax_node; literal_token; contents } -> Json.Object [
    ("tag", Json.String "char");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("literal_token", token_to_json literal_token);
    ("contents", Json.String contents)
  ]
  | Cst.Constant.Bool { syntax_node; literal_token; value } -> Json.Object [
    ("tag", Json.String "bool");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("literal_token", token_to_json literal_token);
    ("value", Json.Bool value)
  ]
  | Cst.Constant.Unit { syntax_node } -> Json.Object [
    ("tag", Json.String "unit");
    ("syntax_node", syntax_node_to_json syntax_node)
  ]

and arrow_label_to_json = function
  | Cst.ArrowLabel.Named { sigil_token; label_token } -> Json.Object [
    ("tag", Json.String "named");
    ("sigil_token", option_to_json token_to_json sigil_token);
    ("label_token", token_to_json label_token)
  ]
  | Cst.ArrowLabel.OptionalNamed { sigil_token; label_token } -> Json.Object [
    ("tag", Json.String "optional_named");
    ("sigil_token", token_to_json sigil_token);
    ("label_token", token_to_json label_token)
  ]

and core_type_to_json = function
  | Cst.CoreType.Wildcard { syntax_node; wildcard_token } -> Json.Object [
    ("tag", Json.String "wildcard");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("wildcard_token", token_to_json wildcard_token)
  ]
  | Cst.CoreType.Var { syntax_node; sigil_token; name_token } -> Json.Object [
    ("tag", Json.String "var");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("sigil_token", option_to_json token_to_json sigil_token);
    ("name_token", token_to_json name_token)
  ]
  | Cst.CoreType.Constr { syntax_node; constructor_path; arguments } -> Json.Object [
    ("tag", Json.String "constr");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("constructor_path", ident_to_json constructor_path);
    ("arguments", Json.Array (List.map core_type_to_json arguments))
  ]
  | Cst.CoreType.Class { syntax_node; hash_token; class_path; arguments } -> Json.Object [
    ("tag", Json.String "class");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("hash_token", token_to_json hash_token);
    ("class_path", ident_to_json class_path);
    ("arguments", Json.Array (List.map core_type_to_json arguments))
  ]
  | Cst.CoreType.Alias { syntax_node; type_; sigil_token; name_token } -> Json.Object [
    ("tag", Json.String "alias");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("type", core_type_to_json type_);
    ("sigil_token", option_to_json token_to_json sigil_token);
    ("name_token", token_to_json name_token)
  ]
  | Cst.CoreType.Attribute { syntax_node; type_; attribute } -> Json.Object [
    ("tag", Json.String "attribute");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("type", core_type_to_json type_);
    ("attribute", attribute_to_json attribute)
  ]
  | Cst.CoreType.Extension extension -> Json.Object [
    ("tag", Json.String "extension");
    ("extension", extension_to_json extension)
  ]
  | Cst.CoreType.Poly {
    syntax_node;
    type_keyword_token;
    dot_token;
    binders;
    body
  } -> Json.Object [
    ("tag", Json.String "poly");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("type_keyword_token", option_to_json token_to_json type_keyword_token);
    ("dot_token", token_to_json dot_token);
    ("binders", Json.Array (List.map type_binder_to_json binders));
    ("body", core_type_to_json body)
  ]
  | Cst.CoreType.Arrow { syntax_node; label; parameter_type; result_type } -> Json.Object [
    ("tag", Json.String "arrow");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("label", option_to_json arrow_label_to_json label);
    ("parameter_type", core_type_to_json parameter_type);
    ("result_type", core_type_to_json result_type)
  ]
  | Cst.CoreType.Tuple { syntax_node; elements } -> Json.Object [
    ("tag", Json.String "tuple");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("elements", Json.Array (List.map core_type_to_json elements))
  ]
  | Cst.CoreType.Parenthesized { syntax_node; inner } -> Json.Object [
    ("tag", Json.String "parenthesized");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("inner", core_type_to_json inner)
  ]
  | Cst.CoreType.PolyVariant poly_variant -> Json.Object [
    ("tag", Json.String "poly_variant");
    ("syntax_node", syntax_node_to_json (Cst.PolyVariant.syntax_node poly_variant));
    ("kind", poly_variant_bound_to_json (Cst.PolyVariant.kind poly_variant));
    ("fields", Json.Array (List.map row_field_to_json (Cst.PolyVariant.fields poly_variant)))
  ]
  | Cst.CoreType.Record { syntax_node; fields } -> Json.Object [
    ("tag", Json.String "record");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("fields", Json.Array (List.map record_type_field_to_json fields))
  ]
  | Cst.CoreType.FirstClassModule { syntax_node; package_type } -> Json.Object [
    ("tag", Json.String "first_class_module");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("package_type", package_type_to_json package_type)
  ]
  | Cst.CoreType.Object { syntax_node; fields } -> Json.Object [
    ("tag", Json.String "object");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("fields", Json.Array (List.map object_type_field_to_json fields))
  ]

and module_type_constraint_to_json = fun (
  { syntax_node; constrained_type; replacement_type; separator_token }: Cst.module_type_constraint
) ->
  Json.Object [
    ("syntax_node", syntax_node_to_json syntax_node);
    ("constrained_type", core_type_to_json constrained_type);
    ("replacement_type", core_type_to_json replacement_type);
    ("separator_token", token_to_json separator_token)
  ]

and package_type_to_json = fun (
  { syntax_node; module_type_path; constraints; attribute }: Cst.package_type
) ->
  Json.Object [
    ("syntax_node", syntax_node_to_json syntax_node);
    ("module_type_path", ident_to_json module_type_path);
    ("constraints", Json.Array (List.map module_type_constraint_to_json constraints));
    ("attribute", option_to_json attribute_to_json attribute)
  ]

and functor_parameter_to_json = fun ({ syntax_node; name_token; module_type }: Cst.functor_parameter) ->
  Json.Object [
    ("syntax_node", syntax_node_to_json syntax_node);
    ("name_token", token_to_json name_token);
    ("module_type", module_type_to_json module_type)
  ]

and module_type_to_json = function
  | Cst.ModuleType.Path path -> Json.Object [
    ("tag", Json.String "path");
    ("path", ident_to_json path)
  ]
  | Cst.ModuleType.TypeOf { syntax_node; module_path } -> Json.Object [
    ("tag", Json.String "type_of");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("module_path", ident_to_json module_path)
  ]
  | Cst.ModuleType.Signature { syntax_node; signature_syntax_node } -> Json.Object [
    ("tag", Json.String "signature");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("signature_syntax_node", syntax_node_to_json signature_syntax_node)
  ]
  | Cst.ModuleType.Functor { syntax_node; parameters; result } -> Json.Object [
    ("tag", Json.String "functor");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("parameters", Json.Array (List.map functor_parameter_to_json parameters));
    ("result", module_type_to_json result)
  ]
  | Cst.ModuleType.With { syntax_node; base; constraints } -> Json.Object [
    ("tag", Json.String "with");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("base", module_type_to_json base);
    ("constraints", Json.Array (List.map module_type_constraint_to_json constraints))
  ]
  | Cst.ModuleType.Parenthesized { syntax_node; inner } -> Json.Object [
    ("tag", Json.String "parenthesized");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("inner", module_type_to_json inner)
  ]
  | Cst.ModuleType.Attribute { syntax_node; module_type; attribute } -> Json.Object [
    ("tag", Json.String "attribute");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("module_type", module_type_to_json module_type);
    ("attribute", attribute_to_json attribute)
  ]
  | Cst.ModuleType.Extension extension -> Json.Object [
    ("tag", Json.String "extension");
    ("extension", extension_to_json extension)
  ]

and module_expression_to_json = function
  | Cst.ModuleExpression.Path path -> Json.Object [
    ("tag", Json.String "path");
    ("path", ident_to_json path)
  ]
  | Cst.ModuleExpression.Structure { syntax_node; item_syntax_nodes } -> Json.Object [
    ("tag", Json.String "structure");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("item_syntax_nodes", Json.Array (List.map syntax_node_to_json item_syntax_nodes))
  ]
  | Cst.ModuleExpression.Functor { syntax_node; parameters; body } -> Json.Object [
    ("tag", Json.String "functor");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("parameters", Json.Array (List.map functor_parameter_to_json parameters));
    ("body", module_expression_to_json body)
  ]
  | Cst.ModuleExpression.Apply { syntax_node; callee; argument } -> Json.Object [
    ("tag", Json.String "apply");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("callee", module_expression_to_json callee);
    ("argument", module_expression_to_json argument)
  ]
  | Cst.ModuleExpression.ApplyUnit { syntax_node; callee } -> Json.Object [
    ("tag", Json.String "apply_unit");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("callee", module_expression_to_json callee)
  ]
  | Cst.ModuleExpression.Constraint { syntax_node; module_expression; module_type } -> Json.Object [
    ("tag", Json.String "constraint");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("module_expression", module_expression_to_json module_expression);
    ("module_type", module_type_to_json module_type)
  ]
  | Cst.ModuleExpression.ModuleUnpack { syntax_node; expression; package_type } -> Json.Object [
    ("tag", Json.String "unpack");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("expression", expression_to_json expression);
    ("package_type", option_to_json package_type_to_json package_type)
  ]
  | Cst.ModuleExpression.Parenthesized { syntax_node; inner } -> Json.Object [
    ("tag", Json.String "parenthesized");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("inner", module_expression_to_json inner)
  ]
  | Cst.ModuleExpression.Attribute { syntax_node; module_expression; attribute } -> Json.Object [
    ("tag", Json.String "attribute");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("module_expression", module_expression_to_json module_expression);
    ("attribute", attribute_to_json attribute)
  ]
  | Cst.ModuleExpression.Extension extension -> Json.Object [
    ("tag", Json.String "extension");
    ("extension", extension_to_json extension)
  ]

and exception_declaration_to_json = fun (decl: Cst.exception_declaration) ->
  let rhs =
    match decl.rhs with
    | None -> Json.Null
    | Some (Cst.Alias { equals_token; alias }) -> Json.Object [
      ("tag", Json.String "alias");
      ("equals_token", token_to_json equals_token);
      ("alias", ident_to_json alias)
    ]
    | Some (Cst.Payload { of_token; payload_type }) -> Json.Object [
      ("tag", Json.String "payload");
      ("of_token", token_to_json of_token);
      ("payload_type", core_type_to_json payload_type)
    ]
  in
  Json.Object [
    ("syntax_node", syntax_node_to_json decl.syntax_node);
    ("keyword_token", token_to_json decl.keyword_token);
    ("name_token", token_to_json decl.name_token);
    ("rhs", rhs)
  ]

and pattern_literal_to_json = fun literal -> constant_to_json literal

and pattern_attribute_fields = fun attributes ->
  match attributes with
  | [] -> []
  | _ -> [ ("attributes", Json.Array (List.map attribute_to_json attributes)) ]

and pattern_to_json = function
  | Cst.Pattern.Identifier { syntax_node; name_token; attributes } ->
      Json.Object ([
        ("tag", Json.String "identifier");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("name_token", token_to_json name_token)
      ]
      @ pattern_attribute_fields attributes)
  | Cst.Pattern.Wildcard { syntax_node; attributes } ->
      Json.Object ([
        ("tag", Json.String "wildcard");
        ("syntax_node", syntax_node_to_json syntax_node)
      ]
      @ pattern_attribute_fields attributes)
  | Cst.Pattern.Extension { syntax_node; extension; attributes } ->
      Json.Object ([
        ("tag", Json.String "extension");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("extension", extension_to_json extension)
      ]
      @ pattern_attribute_fields attributes)
  | Cst.Pattern.Literal { syntax_node; literal; attributes } ->
      Json.Object ([
        ("tag", Json.String "literal");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("literal", pattern_literal_to_json literal)
      ]
      @ pattern_attribute_fields attributes)
  | Cst.Pattern.Lazy { syntax_node; pattern; attributes } ->
      Json.Object ([
        ("tag", Json.String "lazy");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("pattern", pattern_to_json pattern)
      ]
      @ pattern_attribute_fields attributes)
  | Cst.Pattern.Exception { syntax_node; keyword_token; pattern; attributes } ->
      Json.Object ([
        ("tag", Json.String "exception");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("keyword_token", token_to_json keyword_token);
        ("pattern", pattern_to_json pattern)
      ]
      @ pattern_attribute_fields attributes)
  | Cst.Pattern.Range { syntax_node; lower; upper; attributes } ->
      Json.Object ([
        ("tag", Json.String "range");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("lower", pattern_literal_to_json lower);
        ("upper", pattern_literal_to_json upper)
      ]
      @ pattern_attribute_fields attributes)
  | Cst.Pattern.Operator { syntax_node; operator_tokens; attributes } ->
      Json.Object ([
        ("tag", Json.String "operator");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("operator_tokens", Json.Array (List.map token_to_json operator_tokens))
      ]
      @ pattern_attribute_fields attributes)
  | Cst.Pattern.FirstClassModule { syntax_node; binding; package_type; attributes } ->
      let binding_fields =
        match binding with
        | Cst.Named { name_token } -> [ ("name_token", token_to_json name_token) ]
        | Cst.Anonymous { wildcard_token } -> [ ("wildcard_token", token_to_json wildcard_token) ]
      in
      Json.Object ([
        ("tag", Json.String "first_class_module");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("package_type", option_to_json package_type_to_json package_type)
      ]
      @ binding_fields
      @ pattern_attribute_fields attributes)
  | Cst.Pattern.PolyVariant { syntax_node; tag_token; payload; attributes } ->
      Json.Object ([
        ("tag", Json.String "poly_variant");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("tag_token", token_to_json tag_token);
        ("payload", option_to_json pattern_to_json payload)
      ]
      @ pattern_attribute_fields attributes)
  | Cst.Pattern.PolyVariantInherit { syntax_node; type_path; attributes } ->
      Json.Object ([
        ("tag", Json.String "poly_variant_inherit");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("type_path", ident_to_json type_path)
      ]
      @ pattern_attribute_fields attributes)
  | Cst.Pattern.Constructor {
    syntax_node;
    constructor_path;
    existentials;
    arguments;
    attributes
  } ->
      Json.Object ([
        ("tag", Json.String "constructor");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("constructor_path", ident_to_json constructor_path);
        ("existentials", option_to_json constructor_pattern_existentials_to_json existentials);
        ("arguments", Json.Array (List.map pattern_to_json arguments))
      ]
      @ pattern_attribute_fields attributes)
  | Cst.Pattern.Tuple { syntax_node; elements; open_tail; attributes } ->
      Json.Object ([
        ("tag", Json.String "tuple");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("elements", Json.Array (List.map tuple_pattern_element_to_json elements));
        ("open_tail", option_to_json tuple_pattern_open_tail_to_json open_tail)
      ]
      @ pattern_attribute_fields attributes)
  | Cst.Pattern.List { syntax_node; elements; attributes } ->
      Json.Object ([
        ("tag", Json.String "list");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("elements", Json.Array (List.map pattern_to_json elements))
      ]
      @ pattern_attribute_fields attributes)
  | Cst.Pattern.Array { syntax_node; elements; attributes } ->
      Json.Object ([
        ("tag", Json.String "array");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("elements", Json.Array (List.map pattern_to_json elements))
      ]
      @ pattern_attribute_fields attributes)
  | Cst.Pattern.Record { syntax_node; fields; closedness; attributes } ->
      Json.Object ([
        ("tag", Json.String "record");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("fields", Json.Array (List.map record_pattern_field_to_json fields));
        ("closedness", record_pattern_closedness_to_json closedness)
      ]
      @ pattern_attribute_fields attributes)
  | Cst.Pattern.Cons { syntax_node; head; tail; attributes } ->
      Json.Object ([
        ("tag", Json.String "cons");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("head", pattern_to_json head);
        ("tail", pattern_to_json tail)
      ]
      @ pattern_attribute_fields attributes)
  | Cst.Pattern.Or { syntax_node; alternatives; attributes } ->
      Json.Object ([
        ("tag", Json.String "or");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("alternatives", Json.Array (List.map pattern_to_json alternatives))
      ]
      @ pattern_attribute_fields attributes)
  | Cst.Pattern.Alias { syntax_node; pattern; name_token; attributes } ->
      Json.Object ([
        ("tag", Json.String "alias");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("pattern", pattern_to_json pattern);
        ("name_token", token_to_json name_token)
      ]
      @ pattern_attribute_fields attributes)
  | Cst.Pattern.Typed { syntax_node; pattern; type_; attributes } ->
      Json.Object ([
        ("tag", Json.String "typed");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("pattern", pattern_to_json pattern);
        ("type", core_type_to_json type_)
      ]
      @ pattern_attribute_fields attributes)
  | Cst.Pattern.Effect { syntax_node; effect_pattern; continuation; attributes } ->
      Json.Object ([
        ("tag", Json.String "effect");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("effect", pattern_to_json effect_pattern);
        ("continuation", pattern_to_json continuation)
      ]
      @ pattern_attribute_fields attributes)
  | Cst.Pattern.LocalOpen { syntax_node; module_path; pattern; attributes } ->
      Json.Object ([
        ("tag", Json.String "local_open");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("module_path", ident_to_json module_path);
        ("pattern", pattern_to_json pattern)
      ]
      @ pattern_attribute_fields attributes)
  | Cst.Pattern.Parenthesized { syntax_node; inner; attributes } ->
      Json.Object ([
        ("tag", Json.String "parenthesized");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("inner", pattern_to_json inner)
      ]
      @ pattern_attribute_fields attributes)

and constructor_pattern_existentials_to_json = fun (
  { syntax_node; binders }: Cst.constructor_pattern_existentials
) ->
  Json.Object [
    ("syntax_node", syntax_node_to_json syntax_node);
    ("binders", Json.Array (List.map type_binder_to_json binders))
  ]

and record_pattern_field_to_json = fun field ->
  Json.Object [
    ("syntax_node", syntax_node_to_json field.syntax_node);
    ("field_path", ident_to_json field.field_path);
    ("pattern", option_to_json pattern_to_json field.pattern)
  ]

and tuple_pattern_element_to_json = fun ({ label_token; pattern }: Cst.tuple_pattern_element) ->
  Json.Object [
    ("label_token", option_to_json token_to_json label_token);
    ("pattern", pattern_to_json pattern)
  ]

and tuple_pattern_open_tail_to_json = fun ({ dotdot_token }: Cst.tuple_pattern_open_tail) ->
  Json.Object [ ("dotdot_token", token_to_json dotdot_token) ]

and record_pattern_closedness_to_json = function
  | Cst.Closed -> Json.Object [ ("tag", Json.String "closed") ]
  | Cst.Open { wildcard_token } -> Json.Object [
    ("tag", Json.String "open");
    ("wildcard_token", token_to_json wildcard_token)
  ]

and record_expression_field_source_to_json = function
  | Cst.Explicit -> Json.String "explicit"
  | Cst.Punned -> Json.String "punned"

and parameter_to_json = function
  | Cst.Parameter.Positional { syntax_node; pattern; name_token } -> Json.Object [
    ("tag", Json.String "positional");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("pattern", pattern_to_json pattern);
    ("name_token", option_to_json token_to_json name_token)
  ]
  | Cst.Parameter.Labeled {
    syntax_node;
    sigil_token;
    label_token;
    binding_name_token;
    binding_name_matches_label;
    binding_pattern
  } -> Json.Object [
    ("tag", Json.String "labeled");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("sigil_token", token_to_json sigil_token);
    ("label_token", token_to_json label_token);
    ("binding_name_token", option_to_json token_to_json binding_name_token);
    ("binding_name_matches_label", Json.Bool binding_name_matches_label);
    ("binding_pattern", option_to_json pattern_to_json binding_pattern)
  ]
  | Cst.Parameter.Optional {
    syntax_node;
    sigil_token;
    label_token;
    binding_name_token;
    binding_name_matches_label;
    default_value;
    binding_pattern
  } -> Json.Object [
    ("tag", Json.String "optional");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("sigil_token", token_to_json sigil_token);
    ("label_token", token_to_json label_token);
    ("binding_name_token", option_to_json token_to_json binding_name_token);
    ("binding_name_matches_label", Json.Bool binding_name_matches_label);
    ("default_value", option_to_json expression_to_json default_value);
    ("binding_pattern", option_to_json pattern_to_json binding_pattern)
  ]
  | Cst.Parameter.LocallyAbstract { syntax_node; binders } -> Json.Object [
    ("tag", Json.String "locally_abstract");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("binders", Json.Array (List.map type_binder_to_json binders))
  ]

and literal_to_json = fun literal -> constant_to_json literal

and attribute_to_json = fun (attr: Cst.attribute) ->
  Json.Object [
    ("syntax_node", syntax_node_to_json attr.syntax_node);
    ("sigil_tokens", Json.Array (List.map token_to_json attr.sigil_tokens));
    ("name", ident_to_json attr.name);
    ("payload", option_to_json payload_to_json attr.payload)
  ]

and payload_to_json = function
  | Cst.Payload.Opaque { tokens } -> Json.Object [
    ("tag", Json.String "opaque");
    ("tokens", Json.Array (List.map token_to_json tokens));
  ]

and extension_to_json = fun (ext: Cst.extension) ->
  Json.Object [
    ("syntax_node", syntax_node_to_json ext.syntax_node);
    ("sigil_tokens", Json.Array (List.map token_to_json ext.sigil_tokens));
    ("name", ident_to_json ext.name);
    ("payload", option_to_json payload_to_json ext.payload)
  ]

and binding_operator_binding_to_json = fun
  ({
      keyword_token;
      operator_token;
      equals_token;
      binding_pattern;
      bound_value;
      and_binding
    }: Cst.binding_operator_binding) ->
  Json.Object [
    ("keyword_token", token_to_json keyword_token);
    ("operator_token", token_to_json operator_token);
    ("equals_token", token_to_json equals_token);
    ("binding_pattern", pattern_to_json binding_pattern);
    ("bound_value", expression_to_json bound_value);
    ("and_binding", option_to_json binding_operator_binding_to_json and_binding)
  ]

and modifier_tokens_to_json = fun tokens -> Json.Array (List.map token_to_json tokens)

and method_definition_to_json = function
  | Cst.ConcreteMethod { body; type_ } ->
      let colon_token, type_ =
        match type_ with
        | Some (colon_token, type_) -> (Some colon_token, Some type_)
        | None -> (None, None)
      in
      Json.Object [
        ("tag", Json.String "concrete");
        ("body", expression_to_json body);
        ("colon_token", option_to_json token_to_json colon_token);
        ("type", option_to_json core_type_to_json type_)
      ]
  | Cst.VirtualMethod { virtual_token; type_ } -> Json.Object [
    ("tag", Json.String "virtual");
    ("virtual_token", token_to_json virtual_token);
    ("type", core_type_to_json type_)
  ]

and value_definition_to_json = function
  | Cst.ConcreteValue { value; type_ } ->
      let colon_token, type_ =
        match type_ with
        | Some (colon_token, type_) -> (Some colon_token, Some type_)
        | None -> (None, None)
      in
      Json.Object [
        ("tag", Json.String "concrete");
        ("value", expression_to_json value);
        ("colon_token", option_to_json token_to_json colon_token);
        ("type", option_to_json core_type_to_json type_)
      ]
  | Cst.VirtualValue { virtual_token; type_ } -> Json.Object [
    ("tag", Json.String "virtual");
    ("virtual_token", token_to_json virtual_token);
    ("type", core_type_to_json type_)
  ]

and object_member_to_json = function
  | Cst.ObjectMember.Method {
    syntax_node;
    attributes;
    name_token;
    body;
    type_;
    colon_token;
    modifier_tokens
  } -> Json.Object [
    ("tag", Json.String "method");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("attributes", Json.Array (List.map attribute_to_json attributes));
    ("name_token", token_to_json name_token);
    ("body", expression_to_json body);
    ("colon_token", option_to_json token_to_json colon_token);
    ("type", option_to_json core_type_to_json type_);
    ("modifier_tokens", modifier_tokens_to_json modifier_tokens)
  ]
  | Cst.ObjectMember.Value {
    syntax_node;
    attributes;
    name_token;
    value;
    type_;
    colon_token;
    modifier_tokens
  } -> Json.Object [
    ("tag", Json.String "value");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("attributes", Json.Array (List.map attribute_to_json attributes));
    ("name_token", token_to_json name_token);
    ("value", expression_to_json value);
    ("colon_token", option_to_json token_to_json colon_token);
    ("type", option_to_json core_type_to_json type_);
    ("modifier_tokens", modifier_tokens_to_json modifier_tokens)
  ]
  | Cst.ObjectMember.Inherit { syntax_node; attributes; expression } -> Json.Object [
    ("tag", Json.String "inherit");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("attributes", Json.Array (List.map attribute_to_json attributes));
    ("expression", expression_to_json expression)
  ]
  | Cst.ObjectMember.Extension extension -> Json.Object [
    ("tag", Json.String "extension");
    ("extension", extension_to_json extension)
  ]
  | Cst.ObjectMember.Initializer { syntax_node; body } -> Json.Object [
    ("tag", Json.String "initializer");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("body", expression_to_json body)
  ]

and for_direction_to_json = function
  | Cst.To { direction_token } -> Json.Object [
    ("tag", Json.String "to");
    ("token", token_to_json direction_token)
  ]
  | Cst.Downto { direction_token } -> Json.Object [
    ("tag", Json.String "downto");
    ("token", token_to_json direction_token)
  ]

and expression_attribute_fields = fun expression ->
  match Cst.Expression.attributes expression with
  | [] -> []
  | attributes -> [ ("attributes", Json.Array (List.map attribute_to_json attributes)) ]

and type_ascription_kind_to_json = function
  | Cst.Type { colon_token; type_ } -> Json.Object [
    ("tag", Json.String "type");
    ("colon_token", token_to_json colon_token);
    ("type", core_type_to_json type_);
  ]
  | Cst.Coerce { coercion_tokens; type_ } -> Json.Object [
    ("tag", Json.String "coerce");
    ("coercion_tokens", Json.Array (List.map token_to_json coercion_tokens));
    ("type", core_type_to_json type_);
  ]
  | Cst.ConstraintCoerce { colon_token; from_type; coercion_tokens; to_type } -> Json.Object [
    ("tag", Json.String "constraint_coerce");
    ("colon_token", token_to_json colon_token);
    ("from_type", core_type_to_json from_type);
    ("coercion_tokens", Json.Array (List.map token_to_json coercion_tokens));
    ("to_type", core_type_to_json to_type);
  ]

and expression_to_json = fun expression ->
  let node_is_recursive syntax_node rec_token =
    Option.is_some rec_token
    || match Ceibo.Red.SyntaxNode.parent syntax_node with
    | Some parent ->
        Ceibo.Red.SyntaxNode.direct_tokens parent |> List.exists
          (fun token ->
            String.equal (Ceibo.Red.SyntaxToken.text token) "rec")
    | None -> false
  in
  match expression with
  | Cst.Expression.Path { syntax_node; path; _ } ->
      Json.Object ([
        ("tag", Json.String "path");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("path", ident_to_json path)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Constructor { syntax_node; constructor_path; payload; _ } ->
      Json.Object ([
        ("tag", Json.String "constructor");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("constructor_path", ident_to_json constructor_path);
        ("payload", option_to_json expression_to_json payload)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Operator { syntax_node; operator_tokens; _ } ->
      Json.Object ([
        ("tag", Json.String "operator");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("operator_tokens", Json.Array (List.map token_to_json operator_tokens))
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Literal literal ->
      Json.Object ([ ("tag", Json.String "literal"); ("literal", literal_to_json literal) ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Unreachable { syntax_node; dot_token; _ } ->
      Json.Object ([
        ("tag", Json.String "unreachable");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("dot_token", token_to_json dot_token)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Extension ext ->
      Json.Object ([ ("tag", Json.String "extension"); ("extension", extension_to_json ext) ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Object { syntax_node; self_pattern; members; _ } ->
      Json.Object ([
        ("tag", Json.String "object");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("self_pattern", option_to_json pattern_to_json self_pattern);
        ("members", Json.Array (List.map object_member_to_json members))
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.PolyVariant { syntax_node; tag_token; payload; _ } ->
      Json.Object ([
        ("tag", Json.String "poly_variant");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("tag_token", token_to_json tag_token);
        ("payload", option_to_json expression_to_json payload)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.ModulePack { syntax_node; module_expression; package_type; _ } ->
      Json.Object ([
        ("tag", Json.String "first_class_module");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("module_expression", module_expression_to_json module_expression);
        ("package_type", option_to_json package_type_to_json package_type)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.LetModule {
    syntax_node;
    module_name_token;
    module_expression;
    body;
    _
  } ->
      Json.Object ([
        ("tag", Json.String "let_module");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("module_name_token", token_to_json module_name_token);
        ("module_expression", module_expression_to_json module_expression);
        ("body", expression_to_json body)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.LetException { syntax_node; exception_declaration; body; _ } ->
      Json.Object ([
        ("tag", Json.String "let_exception");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("exception_declaration", exception_declaration_to_json exception_declaration);
        ("body", expression_to_json body)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Assert { syntax_node; asserted; _ } ->
      Json.Object ([
        ("tag", Json.String "assert");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("asserted", expression_to_json asserted)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Lazy { syntax_node; body; _ } ->
      Json.Object ([
        ("tag", Json.String "lazy");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("body", expression_to_json body)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.While { syntax_node; condition; body; _ } ->
      Json.Object ([
        ("tag", Json.String "while");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("condition", expression_to_json condition);
        ("body", expression_to_json body)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.For {
    syntax_node;
    iterator_token;
    start_expr;
    direction;
    end_expr;
    body;
    _
  } ->
      Json.Object ([
        ("tag", Json.String "for");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("iterator_token", token_to_json iterator_token);
        ("start_expr", expression_to_json start_expr);
        ("direction", for_direction_to_json direction);
        ("end_expr", expression_to_json end_expr);
        ("body", expression_to_json body)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Apply { syntax_node; callee; argument; _ } ->
      Json.Object ([
        ("tag", Json.String "apply");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("callee", expression_to_json callee);
        ("argument", apply_argument_to_json argument)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.MethodCall {
    syntax_node;
    receiver;
    hash_token;
    method_name;
    _
  } ->
      Json.Object ([
        ("tag", Json.String "method_call");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("receiver", expression_to_json receiver);
        ("hash_token", token_to_json hash_token);
        ("method_name", token_to_json method_name)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.New { syntax_node; class_path; _ } ->
      Json.Object ([
        ("tag", Json.String "new");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("class_path", ident_to_json class_path)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Prefix { syntax_node; operator_token; operand; _ } ->
      Json.Object ([
        ("tag", Json.String "prefix");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("operator_token", token_to_json operator_token);
        ("operand", expression_to_json operand)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.FieldAccess { syntax_node; receiver; field_name; _ } ->
      Json.Object ([
        ("tag", Json.String "field_access");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("receiver", expression_to_json receiver);
        ("field_name", token_to_json field_name)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Index {
    syntax_node;
    collection;
    opening_tokens;
    index;
    closing_token;
    _
  } ->
      Json.Object ([
        ("tag", Json.String "index");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("collection", expression_to_json collection);
        ("opening_tokens", Json.Array (List.map token_to_json opening_tokens));
        ("index", expression_to_json index);
        ("closing_token", token_to_json closing_token)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.ObjectOverride { syntax_node; fields; _ } ->
      Json.Object ([
        ("tag", Json.String "object_override");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("fields", Json.Array (List.map object_override_field_to_json fields))
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.InstanceVariableAssign {
    syntax_node;
    name_token;
    operator_token;
    value;
    _
  } ->
      Json.Object ([
        ("tag", Json.String "instance_variable_assign");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("name_token", token_to_json name_token);
        ("operator_token", token_to_json operator_token);
        ("value", expression_to_json value)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.FieldAssign {
    syntax_node;
    target;
    operator_token;
    value;
    _
  } ->
      Json.Object ([
        ("tag", Json.String "field_assign");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("target", expression_to_json (Cst.Expression.FieldAccess target));
        ("operator_token", token_to_json operator_token);
        ("value", expression_to_json value)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Assign {
    syntax_node;
    target;
    operator_token;
    value;
    _
  } ->
      Json.Object ([
        ("tag", Json.String "assign");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("target", expression_to_json target);
        ("operator_token", token_to_json operator_token);
        ("value", expression_to_json value)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Infix {
    syntax_node;
    left;
    operator_tokens;
    right;
    _
  } ->
      Json.Object ([
        ("tag", Json.String "infix");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("left", expression_to_json left);
        ("operator_tokens", Json.Array (List.map token_to_json operator_tokens));
        ("right", expression_to_json right)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.TypeAscription { syntax_node; expression=inner; kind; _ } ->
      Json.Object ([
        ("tag", Json.String "type_ascription");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("expression", expression_to_json inner);
        ("kind", type_ascription_kind_to_json kind)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Polymorphic { syntax_node; expression=inner; type_; _ } ->
      Json.Object ([
        ("tag", Json.String "polymorphic");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("expression", expression_to_json inner);
        ("type", core_type_to_json type_)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Sequence {
    syntax_node;
    separator_token;
    separator_tokens;
    expressions;
    _
  } ->
      Json.Object ([
        ("tag", Json.String "sequence");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("separator_token", token_to_json separator_token);
        ("separator_tokens", Json.Array (List.map token_to_json separator_tokens));
        ("expressions", Json.Array (List.map expression_to_json expressions))
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Tuple { syntax_node; elements; _ } ->
      Json.Object ([
        ("tag", Json.String "tuple");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("elements", Json.Array (List.map expression_to_json elements))
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.List { syntax_node; elements; _ } ->
      Json.Object ([
        ("tag", Json.String "list");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("elements", Json.Array (List.map expression_to_json elements))
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Array { syntax_node; elements; _ } ->
      Json.Object ([
        ("tag", Json.String "array");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("elements", Json.Array (List.map expression_to_json elements))
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Record (Cst.RecordExpression.Literal { syntax_node; fields; _ }) ->
      Json.Object ([
        ("tag", Json.String "record");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("shape", Json.String "literal");
        ("fields", Json.Array (List.map record_expression_field_to_json fields))
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Record (Cst.RecordExpression.Update { syntax_node; base; fields; _ }) ->
      Json.Object ([
        ("tag", Json.String "record");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("shape", Json.String "update");
        ("base", expression_to_json base);
        ("fields", Json.Array (List.map record_expression_field_to_json fields))
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.LocalOpen (Cst.LetOpen {
    syntax_node;
    let_token;
    open_token;
    module_path;
    in_token;
    body;
    _
  }) ->
      Json.Object ([
        ("tag", Json.String "local_open");
        ("shape", Json.String "let_open");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("let_token", token_to_json let_token);
        ("open_token", token_to_json open_token);
        ("module_path", ident_to_json module_path);
        ("in_token", token_to_json in_token);
        ("body", expression_to_json body)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.LocalOpen (Cst.Delimited {
    syntax_node;
    module_path;
    dot_token;
    opening_token;
    body;
    closing_token;
    _
  }) ->
      Json.Object ([
        ("tag", Json.String "local_open");
        ("shape", Json.String "delimited");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("module_path", ident_to_json module_path);
        ("dot_token", token_to_json dot_token);
        ("opening_token", option_to_json token_to_json opening_token);
        ("body", expression_to_json body);
        ("closing_token", option_to_json token_to_json closing_token)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Fun {
    syntax_node;
    keyword_token;
    colon_token;
    arrow_token;
    parameters;
    return_type;
    body;
    _
  } ->
      let fields = [
        ("tag", Json.String "fun");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("keyword_token", token_to_json keyword_token);
        ("arrow_token", token_to_json arrow_token);
        ("parameters", Json.Array (List.map parameter_to_json parameters));
        ("body", fun_body_to_json body)
      ] in
      let fields =
        match colon_token with
        | Some colon_token -> fields @ [ ("colon_token", token_to_json colon_token) ]
        | None -> fields
      in
      let fields =
        match return_type with
        | Some return_type -> fields @ [ ("return_type", core_type_to_json return_type) ]
        | None -> fields
      in
      Json.Object (fields @ expression_attribute_fields expression)
  | Cst.Expression.Function { syntax_node; keyword_token; cases; _ } ->
      Json.Object ([
        ("tag", Json.String "function");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("keyword_token", token_to_json keyword_token);
        ("cases", Json.Array (List.map match_case_to_json cases))
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.LetOperator {
    syntax_node;
    binding;
    in_token;
    body;
    _
  } ->
      Json.Object ([
        ("tag", Json.String "let_operator");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("binding", binding_operator_binding_to_json binding);
        ("in_token", token_to_json in_token);
        ("body", expression_to_json body)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Let {
    syntax_node;
    keyword_token;
    rec_token;
    equals_token;
    in_token;
    binding_pattern;
    parameters;
    bound_value;
    and_binding;
    body;
    _
  } ->
      Json.Object (
        [
          ("tag", Json.String "let");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("keyword_token", token_to_json keyword_token);
          ("rec_token", option_to_json token_to_json rec_token);
          ("equals_token", token_to_json equals_token);
          ("in_token", token_to_json in_token);
          ("binding_pattern", pattern_to_json binding_pattern);
          ("parameters", Json.Array (List.map parameter_to_json parameters));
          ("bound_value", expression_to_json bound_value);
          ("and_binding", option_to_json let_binding_to_json and_binding);
          ("body", expression_to_json body);
          ("is_recursive", Json.Bool (node_is_recursive syntax_node rec_token));
        ] @ expression_attribute_fields expression
      )
  | Cst.Expression.Match {
    syntax_node;
    keyword_token;
    with_token;
    scrutinee;
    cases;
    _
  } ->
      Json.Object ([
        ("tag", Json.String "match");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("keyword_token", token_to_json keyword_token);
        ("with_token", token_to_json with_token);
        ("scrutinee", expression_to_json scrutinee);
        ("cases", Json.Array (List.map match_case_to_json cases))
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Try {
    syntax_node;
    keyword_token;
    with_token;
    body;
    cases;
    _
  } ->
      Json.Object ([
        ("tag", Json.String "try");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("keyword_token", token_to_json keyword_token);
        ("with_token", token_to_json with_token);
        ("body", expression_to_json body);
        ("cases", Json.Array (List.map match_case_to_json cases))
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.If {
    syntax_node;
    keyword_token;
    then_token;
    else_token;
    condition;
    then_branch;
    else_branch;
    _
  } ->
      Json.Object ([
        ("tag", Json.String "if");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("keyword_token", token_to_json keyword_token);
        ("then_token", token_to_json then_token);
        ("else_token", option_to_json token_to_json else_token);
        ("condition", expression_to_json condition);
        ("then_branch", expression_to_json then_branch);
        ("else_branch", option_to_json expression_to_json else_branch)
      ]
      @ expression_attribute_fields expression)
  | Cst.Expression.Parenthesized {
    syntax_node;
    opening_token;
    closing_token;
    grouping;
    inner;
    _
  } ->
      Json.Object ([
        ("tag", Json.String "parenthesized");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("opening_token", token_to_json opening_token);
        ("closing_token", token_to_json closing_token);
        ("grouping", expression_grouping_to_json grouping);
        ("inner", expression_to_json inner)
      ]
      @ expression_attribute_fields expression)

and apply_argument_to_json = function
  | Cst.Positional expr -> Json.Object [
    ("tag", Json.String "positional");
    ("value", expression_to_json expr)
  ]
  | Cst.Labeled { syntax_node; sigil_token; label_token; value } -> Json.Object [
    ("tag", Json.String "labeled");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("sigil_token", token_to_json sigil_token);
    ("label_token", token_to_json label_token);
    ("value", option_to_json expression_to_json value)
  ]
  | Cst.Optional { syntax_node; sigil_token; label_token; value } -> Json.Object [
    ("tag", Json.String "optional");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("sigil_token", token_to_json sigil_token);
    ("label_token", token_to_json label_token);
    ("value", option_to_json expression_to_json value)
  ]

and record_expression_field_to_json = fun field ->
  Json.Object [
    ("syntax_node", syntax_node_to_json field.syntax_node);
    ("field_path", ident_to_json field.field_path);
    ("field_name", token_to_json field.field_name);
    ("value", expression_to_json field.value);
    ("source", record_expression_field_source_to_json field.source)
  ]

and object_override_field_to_json = fun field ->
  Json.Object [
    ("syntax_node", syntax_node_to_json field.syntax_node);
    ("field_name", token_to_json field.field_name);
    ("value", option_to_json expression_to_json field.value)
  ]

and function_case_body_to_json = fun ({ syntax_node; cases }: Cst.function_case_body) ->
  Json.Object [
    ("syntax_node", syntax_node_to_json syntax_node);
    ("cases", Json.Array (List.map match_case_to_json cases))
  ]

and fun_body_to_json = function
  | Cst.Expression body -> Json.Object [
    ("tag", Json.String "expression");
    ("expression", expression_to_json body)
  ]
  | Cst.Cases cases -> Json.Object [
    ("tag", Json.String "cases");
    ("cases_body", function_case_body_to_json cases)
  ]

and match_case_to_json = fun
  {
    syntax_node;
    bar_token;
    when_token;
    arrow_token;
    pattern;
    guard;
    body
  } ->
  Json.Object [
    ("syntax_node", syntax_node_to_json syntax_node);
    ("bar_token", option_to_json token_to_json bar_token);
    ("when_token", option_to_json token_to_json when_token);
    ("arrow_token", token_to_json arrow_token);
    ("pattern", pattern_to_json pattern);
    ("guard", option_to_json expression_to_json guard);
    ("body", expression_to_json body)
  ]

and let_binding_to_json = fun binding ->
  Json.Object [
    ("syntax_node", syntax_node_to_json (Cst.LetBinding.syntax_node binding));
    ("keyword_token", token_to_json (Cst.LetBinding.keyword_token binding));
    ("rec_token", option_to_json token_to_json (Cst.LetBinding.rec_token binding));
    ("equals_token", token_to_json (Cst.LetBinding.equals_token binding));
    ("attributes", Json.Array (List.map attribute_to_json (Cst.LetBinding.attributes binding)));
    ("binding_pattern", pattern_to_json (Cst.LetBinding.binding_pattern binding));
    ("parameters", Json.Array (List.map parameter_to_json (Cst.LetBinding.parameters binding)));
    ("value", expression_to_json (Cst.LetBinding.value binding));
    ("and_binding", option_to_json let_binding_to_json (Cst.LetBinding.and_binding binding));
    ("is_recursive", Json.Bool (Cst.LetBinding.is_recursive binding));
  ]

let type_variable_to_json = fun type_variable ->
  Json.Object [
    ("syntax_node", syntax_node_to_json (Cst.TypeVariable.syntax_node type_variable));
    ("name_token", token_to_json (Cst.TypeVariable.name_token type_variable))
  ]

let type_parameter_variance_to_json = function
  | Cst.TypeParameterVariance.Covariant { marker_token } -> Json.Object [
    ("tag", Json.String "covariant");
    ("marker_token", token_to_json marker_token)
  ]
  | Cst.TypeParameterVariance.Contravariant { marker_token } -> Json.Object [
    ("tag", Json.String "contravariant");
    ("marker_token", token_to_json marker_token)
  ]

let type_parameter_to_json = fun type_parameter ->
  Json.Object [
    ("syntax_node", syntax_node_to_json (Cst.TypeParameter.syntax_node type_parameter));
    (
      "variance",
      option_to_json type_parameter_variance_to_json (Cst.TypeParameter.variance type_parameter)
    );
    (
      "injectivity_token",
      option_to_json token_to_json (Cst.TypeParameter.injectivity_token type_parameter)
    );
    (
      "type_variable",
      option_to_json type_variable_to_json (Cst.TypeParameter.type_variable type_parameter)
    )
  ]

let record_field_to_json = fun field ->
  let attributes = Cst.RecordField.attributes field |> List.map attribute_to_json in
  Json.Object (
    [
      ("syntax_node", syntax_node_to_json (Cst.RecordField.syntax_node field));
      ("field_name", token_to_json (Cst.RecordField.field_name_token field));
      ("mutable_token", option_to_json token_to_json (Cst.RecordField.mutable_token field));
      ("field_type", core_type_to_json (Cst.RecordField.field_type field))
    ]
    @ (
      if attributes = [] then
        []
      else
        [ ("attributes", Json.Array attributes) ]
    )
  )

let constructor_arguments_to_json = function
  | Cst.ConstructorArguments.Tuple elements -> Json.Object [
    ("tag", Json.String "tuple");
    ("elements", Json.Array (List.map core_type_to_json elements))
  ]
  | Cst.ConstructorArguments.Record { opening_token; fields; closing_token } -> Json.Object [
    ("tag", Json.String "record");
    ("opening_token", token_to_json opening_token);
    ("fields", Json.Array (List.map record_field_to_json fields));
    ("closing_token", token_to_json closing_token)
  ]

let variant_constructor_to_json = fun constr ->
  let attributes = Cst.VariantConstructor.attributes constr |> List.map attribute_to_json in
  let arguments = Cst.VariantConstructor.arguments constr |> Option.map constructor_arguments_to_json in
  let result_type = Cst.VariantConstructor.result_type constr |> Option.map core_type_to_json in
  Json.Object (
    [
      ("syntax_node", syntax_node_to_json (Cst.VariantConstructor.syntax_node constr));
      ("bar_token", option_to_json token_to_json (Cst.VariantConstructor.bar_token constr));
      ("constructor_name", token_to_json (Cst.VariantConstructor.constructor_name_token constr))
    ]
    @ (
      if attributes = [] then
        []
      else
        [ ("attributes", Json.Array attributes) ]
    )
    @ (
      match arguments with
      | Some arguments -> [ ("arguments", arguments) ]
      | None -> []
    )
    @ [
      (
        "separator_token",
        option_to_json token_to_json (Cst.VariantConstructor.separator_token constr)
      );
      ("payload_type", option_to_json core_type_to_json (Cst.VariantConstructor.payload_type constr))
    ]
    @ (
      match result_type with
      | Some result_type -> [ ("result_type", result_type) ]
      | None -> []
    )
    @ [ ("arrow_token", option_to_json token_to_json (Cst.VariantConstructor.arrow_token constr)) ]
  )

let type_constraint_to_json = fun ({ syntax_node; left; right }: Cst.type_constraint) ->
  Json.Object [
    ("syntax_node", syntax_node_to_json syntax_node);
    ("left", core_type_to_json left);
    ("right", core_type_to_json right)
  ]

let private_flag_to_json = function
  | Cst.PrivateFlag.Public -> Json.Object [ ("tag", Json.String "public") ]
  | Cst.PrivateFlag.Private { private_token } -> Json.Object [
    ("tag", Json.String "private");
    ("private_token", token_to_json private_token)
  ]

let type_definition_to_json = function
  | Cst.TypeDefinition.Abstract -> Json.Object [ ("tag", Json.String "abstract") ]
  | Cst.TypeDefinition.Alias { syntax_node; manifest } -> Json.Object [
    ("tag", Json.String "alias");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("manifest", core_type_to_json manifest)
  ]
  | Cst.TypeDefinition.Extensible { syntax_node } -> Json.Object [
    ("tag", Json.String "extensible");
    ("syntax_node", syntax_node_to_json syntax_node)
  ]
  | Cst.TypeDefinition.FirstClassModule { syntax_node; package_type } -> Json.Object [
    ("tag", Json.String "first_class_module");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("package_type", package_type_to_json package_type)
  ]
  | Cst.TypeDefinition.Object { syntax_node; fields } -> Json.Object [
    ("tag", Json.String "object");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("fields", Json.Array (List.map object_type_field_to_json fields))
  ]
  | Cst.TypeDefinition.Record { syntax_node; fields } -> Json.Object [
    ("tag", Json.String "record");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("fields", Json.Array (List.map record_field_to_json fields))
  ]
  | Cst.TypeDefinition.Variant { syntax_node; constructors } -> Json.Object [
    ("tag", Json.String "variant");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("constructors", Json.Array (List.map variant_constructor_to_json constructors))
  ]
  | Cst.TypeDefinition.PolyVariant poly_variant -> Json.Object [
    ("tag", Json.String "poly_variant");
    ("syntax_node", syntax_node_to_json (Cst.PolyVariant.syntax_node poly_variant));
    ("kind", poly_variant_bound_to_json (Cst.PolyVariant.kind poly_variant));
    ("fields", Json.Array (List.map row_field_to_json (Cst.PolyVariant.fields poly_variant)))
  ]

let rec type_declaration_to_json = fun decl ->
  let constraints = Cst.TypeDeclaration.constraints decl |> List.map type_constraint_to_json in
  let attributes = Cst.TypeDeclaration.attributes decl |> List.map attribute_to_json in
  Json.Object (
    [
      ("syntax_node", syntax_node_to_json (Cst.TypeDeclaration.syntax_node decl));
      ("keyword_token", token_to_json (Cst.TypeDeclaration.keyword_token decl));
      ("type_name", ident_to_json (Cst.TypeDeclaration.type_name decl));
      (
        "type_params",
        Json.Array (List.map type_parameter_to_json (Cst.TypeDeclaration.type_params decl))
      );
      ("type_definition", type_definition_to_json (Cst.TypeDeclaration.type_definition decl))
    ]
    @ (
      match Cst.TypeDeclaration.manifest_alias decl with
      | None -> []
      | Some manifest_alias -> [ ("manifest_alias", core_type_to_json manifest_alias) ]
    )
    @ (
      match Cst.TypeDeclaration.private_flag decl with
      | Cst.PrivateFlag.Public -> []
      | private_flag -> [ ("private_flag", private_flag_to_json private_flag) ]
    )
    @ (
      if constraints = [] then
        []
      else
        [ ("constraints", Json.Array constraints) ]
    )
    @ (
      if attributes = [] then
        []
      else
        [ ("attributes", Json.Array attributes) ]
    )
    @ (
      match Cst.TypeDeclaration.next_and_declaration decl with
      | None -> []
      | Some next -> [ ("next_and_declaration", type_declaration_to_json next) ]
    )
    @ (
      match Cst.TypeDeclaration.nonrec_token decl with
      | Some nonrec_token -> [ ("nonrec_token", token_to_json nonrec_token) ]
      | None -> []
    )
    @ [
      (
        "destructive_substitution_token",
        option_to_json token_to_json (Cst.TypeDeclaration.destructive_substitution_token decl)
      )
    ]
  )

let type_extension_to_json = fun decl ->
  Json.Object [
    ("syntax_node", syntax_node_to_json (Cst.TypeExtension.syntax_node decl));
    ("type_name", ident_to_json (Cst.TypeExtension.type_name decl));
    (
      "type_params",
      Json.Array (List.map type_parameter_to_json (Cst.TypeExtension.type_params decl))
    );
    (
      "constructors",
      Json.Array (List.map variant_constructor_to_json (Cst.TypeExtension.constructors decl))
    )
  ]

let rec module_signature_to_json = fun decl ->
  let definition_json =
    match Cst.ModuleSignature.definition decl with
    | Cst.ModuleSignature.Signature module_type -> Json.Object [
      ("kind", Json.String "signature");
      ("module_type", module_type_to_json module_type);
    ]
    | Cst.ModuleSignature.Alias module_expression -> Json.Object [
      ("kind", Json.String "alias");
      ("module_expression", module_expression_to_json module_expression);
    ]
  in
  Json.Object [
    ("syntax_node", syntax_node_to_json (Cst.ModuleSignature.syntax_node decl));
    ("keyword_token", token_to_json (Cst.ModuleSignature.keyword_token decl));
    ("rec_token", option_to_json token_to_json (Cst.ModuleSignature.rec_token decl));
    ("module_name", token_to_json (Cst.ModuleSignature.module_name_token decl));
    (
      "functor_parameters",
      Json.Array (List.map functor_parameter_to_json (Cst.ModuleSignature.functor_parameters decl))
    );
    ("definition", definition_json);
    (
      "next_and_declaration",
      option_to_json module_signature_to_json (Cst.ModuleSignature.next_and_declaration decl)
    );
    ("is_recursive", Json.Bool (Cst.ModuleSignature.is_recursive decl))
  ]

let rec module_structure_to_json = fun decl ->
  Json.Object [
    ("syntax_node", syntax_node_to_json (Cst.ModuleStructure.syntax_node decl));
    ("keyword_token", token_to_json (Cst.ModuleStructure.keyword_token decl));
    ("rec_token", option_to_json token_to_json (Cst.ModuleStructure.rec_token decl));
    ("module_name", token_to_json (Cst.ModuleStructure.module_name_token decl));
    (
      "functor_parameters",
      Json.Array (List.map functor_parameter_to_json (Cst.ModuleStructure.functor_parameters decl))
    );
    ("module_type", option_to_json module_type_to_json (Cst.ModuleStructure.module_type decl));
    ("module_expression", module_expression_to_json (Cst.ModuleStructure.module_expression decl));
    (
      "next_and_declaration",
      option_to_json module_structure_to_json (Cst.ModuleStructure.next_and_declaration decl)
    );
    ("is_recursive", Json.Bool (Cst.ModuleStructure.is_recursive decl))
  ]

let module_type_declaration_to_json = fun decl ->
  Json.Object [
    ("syntax_node", syntax_node_to_json (Cst.ModuleTypeDeclaration.syntax_node decl));
    ("module_type_name", token_to_json (Cst.ModuleTypeDeclaration.module_type_name_token decl));
    ("equals_token", option_to_json token_to_json (Cst.ModuleTypeDeclaration.equals_token decl));
    ("module_type", option_to_json module_type_to_json (Cst.ModuleTypeDeclaration.module_type decl))
  ]

let open_statement_to_json = fun stmt ->
  let open_target_to_json = function
    | Cst.OpenStatement.Path path -> Json.Object [
      ("tag", Json.String "path");
      ("value", ident_to_json path)
    ]
    | Cst.OpenStatement.ModuleExpression module_expression -> Json.Object [
      ("tag", Json.String "module_expression");
      ("value", module_expression_to_json module_expression)
    ]
  in
  Json.Object [
    ("syntax_node", syntax_node_to_json (Cst.OpenStatement.syntax_node stmt));
    ("keyword_token", token_to_json (Cst.OpenStatement.keyword_token stmt));
    ("target", open_target_to_json (Cst.OpenStatement.target stmt));
    ("module_path", option_to_json ident_to_json (Cst.OpenStatement.module_path stmt));
    ("bang_token", option_to_json token_to_json (Cst.OpenStatement.bang_token stmt))
  ]

let value_declaration_to_json = fun (decl: Cst.value_declaration) ->
  Json.Object [
    ("syntax_node", syntax_node_to_json (Cst.ValueDeclaration.syntax_node decl));
    ("keyword_token", token_to_json (Cst.ValueDeclaration.keyword_token decl));
    ("name_tokens", Json.Array (List.map token_to_json (Cst.ValueDeclaration.name_tokens decl)));
    ("colon_token", token_to_json (Cst.ValueDeclaration.colon_token decl));
    ("type", core_type_to_json (Cst.ValueDeclaration.type_ decl));
    ("trailing_comment", option_to_json comment_to_json (Cst.ValueDeclaration.trailing_comment decl))
  ]

let external_declaration_to_json = fun (decl: Cst.external_declaration) ->
  Json.Object [
    ("syntax_node", syntax_node_to_json decl.syntax_node);
    ("name_tokens", Json.Array (List.map token_to_json decl.name_tokens));
    ("colon_token", token_to_json decl.colon_token);
    ("type", core_type_to_json decl.type_);
    ("equals_token", token_to_json decl.equals_token);
    ("primitive_name_tokens", Json.Array (List.map token_to_json decl.primitive_name_tokens));
    ("attributes", Json.Array (List.map attribute_to_json decl.attributes))
  ]

let rec class_field_to_json = function
  | Cst.ClassField.Method {
    syntax_node;
    name_token;
    virtual_colon_token;
    definition;
    modifier_tokens
  } -> Json.Object [
    ("tag", Json.String "method");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("name_token", token_to_json name_token);
    ("virtual_colon_token", option_to_json token_to_json virtual_colon_token);
    ("definition", method_definition_to_json definition);
    ("modifier_tokens", modifier_tokens_to_json modifier_tokens)
  ]
  | Cst.ClassField.Value {
    syntax_node;
    name_token;
    virtual_colon_token;
    definition;
    modifier_tokens
  } -> Json.Object [
    ("tag", Json.String "value");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("name_token", token_to_json name_token);
    ("virtual_colon_token", option_to_json token_to_json virtual_colon_token);
    ("definition", value_definition_to_json definition);
    ("modifier_tokens", modifier_tokens_to_json modifier_tokens)
  ]
  | Cst.ClassField.Inherit { syntax_node; class_expression } -> Json.Object [
    ("tag", Json.String "inherit");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("class_expression", class_expression_to_json class_expression)
  ]
  | Cst.ClassField.Constraint { syntax_node; left; right } -> Json.Object [
    ("tag", Json.String "constraint");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("left", core_type_to_json left);
    ("right", core_type_to_json right)
  ]
  | Cst.ClassField.Initializer { syntax_node; body } -> Json.Object [
    ("tag", Json.String "initializer");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("body", expression_to_json body)
  ]
  | Cst.ClassField.Attribute { syntax_node; field; attribute } -> Json.Object [
    ("tag", Json.String "attribute");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("field", class_field_to_json field);
    ("attribute", attribute_to_json attribute)
  ]
  | Cst.ClassField.Extension extension -> Json.Object [
    ("tag", Json.String "extension");
    ("extension", extension_to_json extension)
  ]

and class_expression_to_json =
  let node_is_recursive syntax_node rec_token =
    Option.is_some rec_token
    || match Ceibo.Red.SyntaxNode.parent syntax_node with
    | Some parent ->
        Ceibo.Red.SyntaxNode.direct_tokens parent |> List.exists
          (fun token ->
            String.equal (Ceibo.Red.SyntaxToken.text token) "rec")
    | None -> false
  in
  function
  | Cst.ClassExpression.Path path -> Json.Object [
    ("tag", Json.String "path");
    ("path", ident_to_json path)
  ]
  | Cst.ClassExpression.Structure { syntax_node; self_pattern; fields } -> Json.Object [
    ("tag", Json.String "structure");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("self_pattern", option_to_json pattern_to_json self_pattern);
    ("fields", Json.Array (List.map class_field_to_json fields))
  ]
  | Cst.ClassExpression.Fun { syntax_node; parameters; body } -> Json.Object [
    ("tag", Json.String "fun");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("parameters", Json.Array (List.map parameter_to_json parameters));
    ("body", class_expression_to_json body)
  ]
  | Cst.ClassExpression.Apply { syntax_node; callee; argument } -> Json.Object [
    ("tag", Json.String "apply");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("callee", class_expression_to_json callee);
    ("argument", apply_argument_to_json argument)
  ]
  | Cst.ClassExpression.Let {
    syntax_node;
    keyword_token;
    rec_token;
    equals_token;
    in_token;
    binding_pattern;
    parameters;
    bound_value;
    and_binding;
    body
  } ->
      Json.Object [
        ("tag", Json.String "let");
        ("syntax_node", syntax_node_to_json syntax_node);
        ("keyword_token", token_to_json keyword_token);
        ("rec_token", option_to_json token_to_json rec_token);
        ("equals_token", token_to_json equals_token);
        ("in_token", token_to_json in_token);
        ("binding_pattern", pattern_to_json binding_pattern);
        ("parameters", Json.Array (List.map parameter_to_json parameters));
        ("bound_value", expression_to_json bound_value);
        ("and_binding", option_to_json let_binding_to_json and_binding);
        ("body", class_expression_to_json body);
        ("is_recursive", Json.Bool (node_is_recursive syntax_node rec_token));
      ]
  | Cst.ClassExpression.Constraint { syntax_node; class_expression; class_type } -> Json.Object [
    ("tag", Json.String "constraint");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("class_expression", class_expression_to_json class_expression);
    ("class_type", class_type_to_json class_type)
  ]
  | Cst.ClassExpression.LocalOpen (Cst.LetOpen {
    syntax_node;
    let_token;
    open_token;
    module_path;
    in_token;
    body
  }) -> Json.Object [
    ("tag", Json.String "local_open");
    ("shape", Json.String "let_open");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("let_token", token_to_json let_token);
    ("open_token", token_to_json open_token);
    ("module_path", ident_to_json module_path);
    ("in_token", token_to_json in_token);
    ("body", class_expression_to_json body)
  ]
  | Cst.ClassExpression.LocalOpen (Cst.Delimited {
    syntax_node;
    module_path;
    dot_token;
    opening_token;
    body;
    closing_token
  }) -> Json.Object [
    ("tag", Json.String "local_open");
    ("shape", Json.String "delimited");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("module_path", ident_to_json module_path);
    ("dot_token", token_to_json dot_token);
    ("opening_token", option_to_json token_to_json opening_token);
    ("body", class_expression_to_json body);
    ("closing_token", option_to_json token_to_json closing_token)
  ]
  | Cst.ClassExpression.Parenthesized { syntax_node; inner } -> Json.Object [
    ("tag", Json.String "parenthesized");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("inner", class_expression_to_json inner)
  ]
  | Cst.ClassExpression.Attribute { syntax_node; class_expression; attribute } -> Json.Object [
    ("tag", Json.String "attribute");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("class_expression", class_expression_to_json class_expression);
    ("attribute", attribute_to_json attribute)
  ]
  | Cst.ClassExpression.Extension extension -> Json.Object [
    ("tag", Json.String "extension");
    ("extension", extension_to_json extension)
  ]

and class_type_field_to_json = function
  | Cst.ClassTypeField.Inherit { syntax_node; class_type } -> Json.Object [
    ("tag", Json.String "inherit");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("class_type", class_type_to_json class_type)
  ]
  | Cst.ClassTypeField.Value {
    syntax_node;
    name_token;
    colon_token;
    type_;
    modifier_tokens
  } -> Json.Object [
    ("tag", Json.String "value");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("name_token", token_to_json name_token);
    ("colon_token", token_to_json colon_token);
    ("type", core_type_to_json type_);
    ("modifier_tokens", modifier_tokens_to_json modifier_tokens)
  ]
  | Cst.ClassTypeField.Method {
    syntax_node;
    name_token;
    colon_token;
    type_;
    modifier_tokens
  } -> Json.Object [
    ("tag", Json.String "method");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("name_token", token_to_json name_token);
    ("colon_token", token_to_json colon_token);
    ("type", core_type_to_json type_);
    ("modifier_tokens", modifier_tokens_to_json modifier_tokens)
  ]
  | Cst.ClassTypeField.Constraint { syntax_node; left; right } -> Json.Object [
    ("tag", Json.String "constraint");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("left", core_type_to_json left);
    ("right", core_type_to_json right)
  ]
  | Cst.ClassTypeField.Attribute { syntax_node; field; attribute } -> Json.Object [
    ("tag", Json.String "attribute");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("field", class_type_field_to_json field);
    ("attribute", attribute_to_json attribute)
  ]
  | Cst.ClassTypeField.Extension extension -> Json.Object [
    ("tag", Json.String "extension");
    ("extension", extension_to_json extension)
  ]

and class_type_to_json = function
  | Cst.ClassType.Path path -> Json.Object [
    ("tag", Json.String "path");
    ("path", ident_to_json path)
  ]
  | Cst.ClassType.Signature { syntax_node; fields } -> Json.Object [
    ("tag", Json.String "signature");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("fields", Json.Array (List.map class_type_field_to_json fields))
  ]
  | Cst.ClassType.Arrow { syntax_node; label; parameter_type; result_type } -> Json.Object [
    ("tag", Json.String "arrow");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("label", option_to_json arrow_label_to_json label);
    ("parameter_type", core_type_to_json parameter_type);
    ("result_type", class_type_to_json result_type)
  ]
  | Cst.ClassType.Parenthesized { syntax_node; inner } -> Json.Object [
    ("tag", Json.String "parenthesized");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("inner", class_type_to_json inner)
  ]
  | Cst.ClassType.Attribute { syntax_node; class_type; attribute } -> Json.Object [
    ("tag", Json.String "attribute");
    ("syntax_node", syntax_node_to_json syntax_node);
    ("class_type", class_type_to_json class_type);
    ("attribute", attribute_to_json attribute)
  ]
  | Cst.ClassType.Extension extension -> Json.Object [
    ("tag", Json.String "extension");
    ("extension", extension_to_json extension)
  ]

let class_declaration_to_json = fun (decl: Cst.ClassDeclaration.t) ->
  Json.Object [
    ("tag", Json.String "declaration");
    ("syntax_node", syntax_node_to_json (Cst.ClassDeclaration.syntax_node decl));
    (
      "type_params",
      Json.Array (List.map type_parameter_to_json (Cst.ClassDeclaration.type_params decl))
    );
    (
      "declaration_extension",
      option_to_json extension_to_json (Cst.ClassDeclaration.declaration_extension decl)
    );
    (
      "declaration_attributes",
      Json.Array (List.map attribute_to_json (Cst.ClassDeclaration.declaration_attributes decl))
    );
    ("class_name", token_to_json (Cst.ClassDeclaration.class_name_token decl));
    ("class_type", class_type_to_json (Cst.ClassDeclaration.class_type decl))
  ]

let class_definition_to_json = fun (decl: Cst.ClassDefinition.t) ->
  Json.Object [
    ("tag", Json.String "definition");
    ("syntax_node", syntax_node_to_json (Cst.ClassDefinition.syntax_node decl));
    (
      "type_params",
      Json.Array (List.map type_parameter_to_json (Cst.ClassDefinition.type_params decl))
    );
    (
      "declaration_extension",
      option_to_json extension_to_json (Cst.ClassDefinition.declaration_extension decl)
    );
    (
      "declaration_attributes",
      Json.Array (List.map attribute_to_json (Cst.ClassDefinition.declaration_attributes decl))
    );
    ("class_name", token_to_json (Cst.ClassDefinition.class_name_token decl));
    ("class_type", option_to_json class_type_to_json (Cst.ClassDefinition.class_type decl));
    ("class_body", class_expression_to_json (Cst.ClassDefinition.class_body decl))
  ]

let class_type_declaration_to_json = fun
  ({
      syntax_node;
      type_params;
      declaration_extension;
      declaration_attributes;
      class_type_name;
      class_type_body;

    }: Cst.class_type_declaration) ->
  Json.Object [
    ("syntax_node", syntax_node_to_json syntax_node);
    ("type_params", Json.Array (List.map type_parameter_to_json type_params));
    ("declaration_extension", option_to_json extension_to_json declaration_extension);
    ("declaration_attributes", Json.Array (List.map attribute_to_json declaration_attributes));
    ("class_type_name", token_to_json class_type_name);
    ("class_type_body", class_type_to_json class_type_body)
  ]

let include_statement_to_json = fun (stmt: Cst.include_statement) ->
  let target =
    match stmt.target with
    | Cst.ModuleExpression module_expression -> Json.Object [
      ("tag", Json.String "module_expression");
      ("value", module_expression_to_json module_expression)
    ]
    | Cst.ModuleType module_type -> Json.Object [
      ("tag", Json.String "module_type");
      ("value", module_type_to_json module_type)
    ]
  in
  Json.Object [
    ("syntax_node", syntax_node_to_json stmt.syntax_node);
    ("keyword_token", token_to_json stmt.keyword_token);
    ("target", target)
  ]

let structure_item_to_json = function
  | Cst.StructureItem.TypeDeclaration decl -> Json.Object [
    ("tag", Json.String "type_declaration");
    ("item", type_declaration_to_json decl)
  ]
  | Cst.StructureItem.TypeExtension decl -> Json.Object [
    ("tag", Json.String "type_extension");
    ("item", type_extension_to_json decl)
  ]
  | Cst.StructureItem.LetBinding binding -> Json.Object [
    ("tag", Json.String "let_binding");
    ("item", let_binding_to_json binding)
  ]
  | Cst.StructureItem.Expression expr -> Json.Object [
    ("tag", Json.String "expression");
    ("item", expression_to_json expr)
  ]
  | Cst.StructureItem.ClassDeclaration decl -> Json.Object [
    ("tag", Json.String "class_declaration");
    ("item", class_definition_to_json decl)
  ]
  | Cst.StructureItem.Attribute attribute -> Json.Object [
    ("tag", Json.String "attribute_item");
    ("attribute", attribute_to_json attribute)
  ]
  | Cst.StructureItem.Extension extension -> Json.Object [
    ("tag", Json.String "extension_item");
    ("extension", extension_to_json extension)
  ]
  | Cst.StructureItem.ClassTypeDeclaration decl -> Json.Object [
    ("tag", Json.String "class_type_declaration");
    ("item", class_type_declaration_to_json decl)
  ]
  | Cst.StructureItem.ModuleDeclaration decl -> Json.Object [
    ("tag", Json.String "module_declaration");
    ("item", module_structure_to_json decl)
  ]
  | Cst.StructureItem.ModuleTypeDeclaration decl -> Json.Object [
    ("tag", Json.String "module_type_declaration");
    ("item", module_type_declaration_to_json decl)
  ]
  | Cst.StructureItem.OpenStatement stmt -> Json.Object [
    ("tag", Json.String "open_statement");
    ("item", open_statement_to_json stmt)
  ]
  | Cst.StructureItem.Docstring docstring -> Json.Object [
    ("tag", Json.String "docstring");
    ("item", docstring_to_json docstring)
  ]
  | Cst.StructureItem.Comment comment -> Json.Object [
    ("tag", Json.String "comment");
    ("item", comment_to_json comment)
  ]
  | Cst.StructureItem.ExternalDeclaration decl -> Json.Object [
    ("tag", Json.String "external_declaration");
    ("item", external_declaration_to_json decl)
  ]
  | Cst.StructureItem.IncludeStatement stmt -> Json.Object [
    ("tag", Json.String "include_statement");
    ("item", include_statement_to_json stmt)
  ]
  | Cst.StructureItem.ExceptionDeclaration decl -> Json.Object [
    ("tag", Json.String "exception_declaration");
    ("item", exception_declaration_to_json decl)
  ]

let signature_item_to_json = function
  | Cst.SignatureItem.TypeDeclaration decl -> Json.Object [
    ("tag", Json.String "type_declaration");
    ("item", type_declaration_to_json decl)
  ]
  | Cst.SignatureItem.TypeExtension decl -> Json.Object [
    ("tag", Json.String "type_extension");
    ("item", type_extension_to_json decl)
  ]
  | Cst.SignatureItem.Attribute attribute -> Json.Object [
    ("tag", Json.String "attribute_item");
    ("attribute", attribute_to_json attribute)
  ]
  | Cst.SignatureItem.Extension extension -> Json.Object [
    ("tag", Json.String "extension_item");
    ("extension", extension_to_json extension)
  ]
  | Cst.SignatureItem.ClassDeclaration decl -> Json.Object [
    ("tag", Json.String "class_declaration");
    ("item", class_declaration_to_json decl)
  ]
  | Cst.SignatureItem.ClassTypeDeclaration decl -> Json.Object [
    ("tag", Json.String "class_type_declaration");
    ("item", class_type_declaration_to_json decl)
  ]
  | Cst.SignatureItem.ModuleDeclaration decl -> Json.Object [
    ("tag", Json.String "module_declaration");
    ("item", module_signature_to_json decl)
  ]
  | Cst.SignatureItem.ModuleTypeDeclaration decl -> Json.Object [
    ("tag", Json.String "module_type_declaration");
    ("item", module_type_declaration_to_json decl)
  ]
  | Cst.SignatureItem.OpenStatement stmt -> Json.Object [
    ("tag", Json.String "open_statement");
    ("item", open_statement_to_json stmt)
  ]
  | Cst.SignatureItem.Docstring docstring -> Json.Object [
    ("tag", Json.String "docstring");
    ("item", docstring_to_json docstring)
  ]
  | Cst.SignatureItem.Comment comment -> Json.Object [
    ("tag", Json.String "comment");
    ("item", comment_to_json comment)
  ]
  | Cst.SignatureItem.ValueDeclaration decl -> Json.Object [
    ("tag", Json.String "value_declaration");
    ("item", value_declaration_to_json decl)
  ]
  | Cst.SignatureItem.ExternalDeclaration decl -> Json.Object [
    ("tag", Json.String "external_declaration");
    ("item", external_declaration_to_json decl)
  ]
  | Cst.SignatureItem.IncludeStatement stmt -> Json.Object [
    ("tag", Json.String "include_statement");
    ("item", include_statement_to_json stmt)
  ]
  | Cst.SignatureItem.ExceptionDeclaration decl -> Json.Object [
    ("tag", Json.String "exception_declaration");
    ("item", exception_declaration_to_json decl)
  ]

let of_source_file = fun source_file ->
  let items =
    match source_file with
    | Cst.Implementation { items; _ } -> List.map structure_item_to_json items
    | Cst.Interface { items; _ } -> List.map signature_item_to_json items
  in
  Json.Object [ (
      "kind",
      Json.String (
        match Cst.SourceFile.kind source_file with
        | `Implementation -> "implementation"
        | `Interface -> "interface"
      )
    ); ("syntax_node", syntax_node_to_json (Cst.SourceFile.syntax_node source_file)); (
      "items",
      Json.Array items
    ) ]

let of_error = fun (error: Cst_builder.error) ->
  Json.Object [
    ("message", Json.String error.message);
    ("syntax_kind", Json.String (Syntax_kind.to_string error.syntax_kind));
    ("span", span_to_json error.span);
    ("context", Json.Array (List.map Json.string error.context))
  ]

let of_result = function
  | Ok source_file -> Json.Object [
    ("status", Json.String "ok");
    ("cst", of_source_file source_file)
  ]
  | Error error -> Json.Object [ ("status", Json.String "error"); ("error", of_error error) ]

open Std
open Std.Collections
module SynAst = Syn.Ast
module SurfacePath = Model.Surface_path

type origin = {
  span: Syn.Ceibo.Span.t;
  kind: Syn.SyntaxKind.t;
}

type file_kind =
[
  `Implementation
  | `Interface
]

type path = SurfacePath.t

type literal =
  | Int
  | Float
  | Char
  | String
  | Bool
  | Unit
  | Unknown

type core_type = {
  origin: origin;
  kind: core_type_kind;
}

and core_type_kind =
  | Wildcard
  | Var of string option
  | Path of path
  | Apply of { argument: core_type; constructor: core_type }
  | Arrow of { left: core_type; right: core_type }
  | Tuple of core_type list
  | Labeled of core_type
  | Poly of { parameters: string list; body: core_type }
  | PolyVariant of string list
  | Parenthesized of core_type

type type_parameter = string option

type type_constructor = {
  origin: origin;
  name: string;
  payload: core_type option;
}

type record_field_declaration = {
  origin: origin;
  name: string;
  type_annotation: core_type;
}

type type_definition = {
  origin: origin;
  kind: type_definition_kind;
}

and type_definition_kind =
  | Abstract
  | Alias of core_type
  | Variant of type_constructor list
  | Record of record_field_declaration list

type type_declaration = {
  origin: origin;
  name: string;
  parameters: type_parameter list;
  definition: type_definition;
}

type parameter = {
  origin: origin;
  kind: parameter_kind;
}

and parameter_kind =
  | Labeled of { label: string; pattern: pattern option }
  | Optional of { label: string; pattern: pattern option }
  | OptionalDefault of { label: string; pattern: pattern option; default: expression }

and pattern = {
  origin: origin;
  kind: pattern_kind;
}

and record_pattern_field = {
  origin: origin;
  name: path;
  pattern: pattern option;
}

and pattern_kind =
  | Wildcard
  | Path of path
  | Apply of { callee: pattern; argument: pattern }
  | Literal of literal
  | PolyVariant of string
  | Tuple of pattern list
  | List of pattern list
  | Record of record_pattern_field list
  | Or of { left: pattern; right: pattern }
  | Cons of { head: pattern; tail: pattern }
  | Constraint of { pattern: pattern; annotation: core_type }
  | Alias of { pattern: pattern; alias: pattern }
  | Attribute of pattern
  | Parenthesized of pattern
  | LabeledParameter of parameter
  | OptionalParameter of parameter
  | OptionalParameterDefault of parameter

and let_binding = {
  origin: origin;
  pattern: pattern;
  parameters: pattern list;
  body: expression;
  type_annotation: core_type option;
}

and expression = {
  origin: origin;
  type_hint: core_type option;
  kind: expression_kind;
}

and expression_kind =
  | Literal of literal
  | Path of path
  | Tuple of expression list
  | List of expression list
  | PolyVariant of string
  | Record of record_expression_field list
  | RecordUpdate of { base: expression; fields: record_expression_field list }
  | FieldAccess of { receiver: expression; field: path }
  | Sequence of { left: expression; right: expression }
  | If of { condition: expression; then_branch: expression; else_branch: expression option }
  | Match of { scrutinee: expression; cases: match_case list }
  | Function of { parameters: pattern list; body: function_body }
  | Apply of { callee: expression; arguments: argument list }
  | Infix of { left: expression; operator: path; right: expression }
  | Let of { first_binding: let_binding; body: expression }
  | Assert of expression

and function_body =
  | Body of expression
  | Cases of match_case list

and match_case = {
  origin: origin;
  pattern: pattern;
  guard: expression option;
  body: expression;
}

and record_expression_field = {
  origin: origin;
  name: path;
  value: expression;
}

and argument = {
  origin: origin;
  kind: argument_kind;
}

and argument_kind =
  | Positional of expression
  | Labeled of { label: string; value: expression option }
  | Optional of { label: string; value: expression option }

type let_declaration = {
  origin: origin;
  recursive: bool;
  bindings: let_binding list;
}

type value_declaration = {
  origin: origin;
  name: string;
  type_annotation: core_type;
}

type external_declaration = {
  origin: origin;
  name: string;
  type_annotation: core_type;
}

type structure_item = {
  origin: origin;
  kind: structure_item_kind;
}

and structure_item_kind =
  | Let of let_declaration
  | Type of type_declaration list
  | Expression of expression
  | External of external_declaration

type signature_item = {
  origin: origin;
  kind: signature_item_kind;
}

and signature_item_kind =
  | Value of value_declaration
  | Type of type_declaration list
  | External of external_declaration

type t = {
  origin: origin;
  kind: source_file_kind;
}

and source_file_kind =
  | Implementation of structure_item list
  | Interface of signature_item list
  | Empty of file_kind

let core_type_origin = fun (type_: core_type) -> type_.origin

let parameter_origin = fun (parameter: parameter) -> parameter.origin

let pattern_origin = fun (pattern: pattern) -> pattern.origin

let expression_origin = fun (expression: expression) -> expression.origin

let match_case_origin = fun (match_case: match_case) -> match_case.origin

let structure_item_origin = fun (item: structure_item) -> item.origin

let signature_item_origin = fun (item: signature_item) -> item.origin

let span_from_token_body = fun token ->
  let _start, end_ = SynAst.Token.raw_range token in
  let width = SynAst.Token.width token in
  (
    (
      if end_ >= width then
        end_ - width
      else
        0
    ),
    end_
  )

let span_from_node = fun node ->
  match SynAst.Node.first_descendant_token node with
  | None ->
      let start, end_ = SynAst.Node.raw_range node in
      Syn.Ceibo.Span.make ~start ~end_
  | Some first ->
      let start, _ = span_from_token_body first in
      let last_end = ref start in
      SynAst.Node.for_each_token node
        ~fn:(fun token ->
          let _, end_ = span_from_token_body token in
          last_end := end_);
      Syn.Ceibo.Span.make ~start ~end_:!last_end

let origin_from_node = fun node -> { span = span_from_node node; kind = SynAst.Node.kind node }

let token_text = SynAst.Token.text

let path_from_syn_path = fun path ->
  let segments = ref [] in
  SynAst.Path.for_each_ident path ~fn:(fun token -> segments := token_text token :: !segments);
  SurfacePath.from_segments (List.reverse !segments)

let path_from_tokens = fun tokens ->
  tokens |> List.map ~fn:token_text |> String.concat "" |> SurfacePath.from_name

let literal_from_token = fun token ->
  match SynAst.Token.kind token with
  | Syn.SyntaxKind.INT -> Int
  | FLOAT -> Float
  | CHAR -> Char
  | STRING -> String
  | TRUE_KW
  | FALSE_KW -> Bool
  | _ -> Unit

let node_summary = fun node -> Syn.SyntaxKind.to_string (SynAst.Node.kind node)

let child_patterns = fun pattern ->
  let children = ref [] in
  SynAst.Pattern.for_each_child_pattern pattern ~fn:(fun child -> children := child :: !children);
  List.reverse !children

let direct_child_patterns = fun node ->
  let children = ref [] in
  SynAst.Node.for_each_child_node node
    ~fn:(fun child ->
      match SynAst.Pattern.cast child with
      | Some pattern -> children := pattern :: !children
      | None -> ());
  List.reverse !children

let child_exprs = fun expression ->
  let children = ref [] in
  SynAst.Expr.for_each_child_expr expression ~fn:(fun child -> children := child :: !children);
  List.reverse !children

let make_core_type = fun origin (kind: core_type_kind) -> ({ origin; kind }: core_type)

let make_type_definition = fun origin (kind: type_definition_kind) ->
  ({ origin; kind }: type_definition)

let make_parameter = fun origin (kind: parameter_kind) -> ({ origin; kind }: parameter)

let make_pattern = fun origin (kind: pattern_kind) -> ({ origin; kind }: pattern)

let make_expression = fun origin (kind: expression_kind) -> { origin; type_hint = None; kind }

let make_argument = fun origin (kind: argument_kind) -> ({ origin; kind }: argument)

let make_structure_item = fun origin (kind: structure_item_kind) ->
  ({ origin; kind }: structure_item)

let make_signature_item = fun origin (kind: signature_item_kind) ->
  ({ origin; kind }: signature_item)

let make_source_file = fun origin (kind: source_file_kind) -> ({ origin; kind }: t)

let with_expression_type_hint = fun type_hint (expression: expression) ->
  { expression with type_hint = Some type_hint }

exception Build_failed of Diagnostics.Diagnostic.t

let build_failed = fun origin summary ->
  raise
    (Build_failed (Diagnostics.Diagnostic.UnsupportedSyntax {
      span = origin.span;
      kind = origin.kind;
      summary
    }))

let require_some = fun origin summary value ->
  match value with
  | Some value -> value
  | None -> build_failed origin summary

let unsupported_node = fun node summary -> build_failed (origin_from_node node) summary

let poly_type_parameters = fun type_expr ->
  let names = ref [] in
  SynAst.TypeExpr.for_each_poly_type_name
    type_expr
    ~fn:(fun token -> names := token_text token :: !names);
  List.reverse !names

let poly_variant_tags_from_node = fun node ->
  let tags = ref [] in
  let saw_backtick = ref false in
  SynAst.Node.for_each_token node
    ~fn:(fun token ->
      match SynAst.Token.kind token with
      | Syn.SyntaxKind.BACKTICK ->
          saw_backtick := true
      | IDENT when !saw_backtick ->
          tags := token_text token :: !tags;
          saw_backtick := false
      | _ ->
          saw_backtick := false);
  List.reverse !tags

let poly_variant_tag_from_node = fun origin node ->
  match poly_variant_tags_from_node node with
  | tag :: _ -> tag
  | [] -> build_failed origin "missing polymorphic variant tag"

let rec build_core_type = fun type_expr ->
  let origin = origin_from_node type_expr in
  (match SynAst.TypeExpr.view type_expr with
    | SynAst.TypeExpr.Wildcard ->
        make_core_type origin Wildcard
    | SynAst.TypeExpr.Var { name } ->
        make_core_type origin (Var (Option.map name ~fn:token_text))
    | SynAst.TypeExpr.Path { path } ->
        make_core_type origin (Path (path_from_syn_path path))
    | SynAst.TypeExpr.Apply { argument; constructor } ->
        make_core_type
          origin
          (Apply {
            argument = build_core_type
              (require_some origin "missing type application argument" argument);
            constructor = build_core_type
              (require_some origin "missing type application constructor" constructor)
          })
    | SynAst.TypeExpr.Arrow { left; right } ->
        make_core_type
          origin
          (Arrow {
            left = build_core_type (require_some origin "missing arrow parameter type" left);
            right = build_core_type (require_some origin "missing arrow result type" right)
          })
    | SynAst.TypeExpr.Tuple { left; right; _ } ->
        make_core_type origin (Tuple (tuple_type_elements origin left right))
    | SynAst.TypeExpr.Labeled { annotation; _ } ->
        make_core_type
          origin
          (Labeled (build_core_type
            (require_some origin "missing labeled type annotation" annotation)))
    | SynAst.TypeExpr.Poly { body } ->
        make_core_type
          origin
          (Poly {
            parameters = poly_type_parameters type_expr;
            body = build_core_type (require_some origin "missing polymorphic type body" body)
          })
    | SynAst.TypeExpr.Parenthesized { inner } ->
        make_core_type
          origin
          (Parenthesized (build_core_type (require_some origin "missing parenthesized type" inner)))
    | SynAst.TypeExpr.Opaque node -> (
        match poly_variant_tags_from_node node with
        | _ :: _ as tags -> make_core_type origin (PolyVariant tags)
        | [] -> (
            match SynAst.TypeExpr.inner_without_attribute_suffix type_expr with
            | Some inner -> build_core_type inner
            | None -> unsupported_node node (node_summary node)
          )
      )
    | SynAst.TypeExpr.Error node ->
        unsupported_node node (node_summary node)
    | SynAst.TypeExpr.Unknown node ->
        unsupported_node node (node_summary node): core_type)

and tuple_type_elements = fun origin left right ->
  let rec flatten type_expr =
    match SynAst.TypeExpr.view type_expr with
    | SynAst.TypeExpr.Tuple { left; right; _ } -> tuple_type_elements
      (origin_from_node type_expr)
      left
      right
    | _ -> [ build_core_type type_expr ]
  in
  List.append
    (flatten (require_some origin "missing tuple type left" left))
    (flatten (require_some origin "missing tuple type right" right))

and child_type_exprs = fun type_expr ->
  let children = ref [] in
  SynAst.TypeExpr.for_each_child_type
    type_expr
    ~fn:(fun child -> children := build_core_type child :: !children);
  List.reverse !children

let rec build_parameter = fun parameter ->
  let origin = origin_from_node parameter in
  (match SynAst.Parameter.view parameter with
    | SynAst.Parameter.Labeled { label; pattern } -> make_parameter
      origin
      (Labeled {
        label = token_text (require_some origin "missing labeled parameter label" label);
        pattern = Option.map pattern ~fn:build_pattern
      })
    | SynAst.Parameter.Optional { label; pattern } -> make_parameter
      origin
      (Optional {
        label = token_text (require_some origin "missing optional parameter label" label);
        pattern = Option.map pattern ~fn:build_pattern
      })
    | SynAst.Parameter.OptionalDefault { label; pattern; default } -> make_parameter
      origin
      (OptionalDefault {
        label = token_text (require_some origin "missing optional parameter label" label);
        pattern = Option.map pattern ~fn:build_pattern;
        default = build_expression (require_some origin "missing optional parameter default" default)
      })
    | SynAst.Parameter.Unknown node -> unsupported_node node (node_summary node): parameter)

and build_pattern = fun syntax_pattern ->
  let origin = origin_from_node syntax_pattern in
  (match SynAst.Pattern.view syntax_pattern with
    | SynAst.Pattern.Wildcard ->
        make_pattern origin Wildcard
    | SynAst.Pattern.Path { path } ->
        make_pattern origin (Path (path_from_syn_path path))
    | SynAst.Pattern.Apply { callee; argument } ->
        make_pattern
          origin
          (Apply {
            callee = build_pattern (require_some origin "missing pattern callee" callee);
            argument = build_pattern (require_some origin "missing pattern argument" argument)
          })
    | SynAst.Pattern.Literal { token } ->
        make_pattern
          origin
          (Literal (Option.map token ~fn:literal_from_token |> Option.unwrap_or ~default:Unknown))
    | SynAst.Pattern.PolyVariant ->
        make_pattern origin (PolyVariant (poly_variant_tag_from_node origin syntax_pattern))
    | SynAst.Pattern.Tuple ->
        make_pattern origin (Tuple (child_patterns syntax_pattern |> List.map ~fn:build_pattern))
    | SynAst.Pattern.List ->
        make_pattern origin (List (child_patterns syntax_pattern |> List.map ~fn:build_pattern))
    | SynAst.Pattern.Record ->
        let record = SynAst.RecordPattern.cast syntax_pattern |> require_some origin "invalid record pattern" in
        make_pattern origin (Record (build_record_pattern_fields record))
    | SynAst.Pattern.Or { left; right } ->
        make_pattern
          origin
          (Or {
            left = build_pattern (require_some origin "missing left or-pattern" left);
            right = build_pattern (require_some origin "missing right or-pattern" right)
          })
    | SynAst.Pattern.Cons { head; tail } ->
        make_pattern
          origin
          (Cons {
            head = build_pattern (require_some origin "missing cons head" head);
            tail = build_pattern (require_some origin "missing cons tail" tail)
          })
    | SynAst.Pattern.Parenthesized { inner=Some inner } ->
        make_pattern origin (Parenthesized (build_pattern inner))
    | SynAst.Pattern.Parenthesized { inner=None } ->
        make_pattern origin (Literal Unit)
    | SynAst.Pattern.Constraint { pattern; annotation } ->
        make_pattern
          origin
          (Constraint {
            pattern = build_pattern (require_some origin "missing constrained pattern" pattern);
            annotation = build_core_type
              (require_some origin "missing pattern type annotation" annotation)
          })
    | SynAst.Pattern.Alias { pattern; alias } ->
        make_pattern
          origin
          (Alias {
            pattern = build_pattern (require_some origin "missing aliased pattern" pattern);
            alias = build_pattern (require_some origin "missing pattern alias" alias)
          })
    | SynAst.Pattern.Attribute { inner } ->
        make_pattern
          origin
          (Attribute (build_pattern (require_some origin "missing attributed pattern" inner)))
    | SynAst.Pattern.LabeledParam parameter ->
        make_pattern origin (LabeledParameter (build_parameter parameter))
    | SynAst.Pattern.OptionalParam parameter ->
        make_pattern origin (OptionalParameter (build_parameter parameter))
    | SynAst.Pattern.OptionalParamDefault parameter ->
        make_pattern origin (OptionalParameterDefault (build_parameter parameter))
    | SynAst.Pattern.Error node ->
        unsupported_node node (node_summary node)
    | SynAst.Pattern.Unknown node ->
        unsupported_node node (node_summary node)
    | SynAst.Pattern.Array
    | SynAst.Pattern.Extension
    | SynAst.Pattern.LocalOpen
    | SynAst.Pattern.LocallyAbstractType
    | SynAst.Pattern.FirstClassModule
    | SynAst.Pattern.Interval _
    | SynAst.Pattern.Lazy _
    | SynAst.Pattern.Exception _ ->
        build_failed origin (Syn.SyntaxKind.to_string origin.kind): pattern)

and build_record_pattern_field = fun (field: SynAst.RecordPattern.field) ->
  let origin = origin_from_node field.node in
  (
    {
      origin;
      name = path_from_syn_path (require_some origin "missing record pattern field name" field.path);
      pattern = Option.map field.pattern ~fn:build_pattern
    }:
      record_pattern_field
  )

and build_record_pattern_fields = fun record ->
  let fields = ref [] in
  SynAst.RecordPattern.for_each_field
    record
    ~fn:(fun field -> fields := build_record_pattern_field field :: !fields);
  List.reverse !fields

and build_let_binding = fun binding ->
  let origin = origin_from_node binding in
  let parameters = ref [] in
  SynAst.LetBinding.for_each_parameter
    binding
    ~fn:(fun parameter -> parameters := build_pattern parameter :: !parameters);
  ({
      origin;
      pattern = build_pattern
        (require_some origin "missing let binding pattern" (SynAst.LetBinding.pattern binding));
      parameters = List.reverse !parameters;
      body = build_expression
        (require_some origin "missing let binding body" (SynAst.LetBinding.body binding));
      type_annotation = Option.map (SynAst.LetBinding.type_annotation binding) ~fn:build_core_type;
    }: let_binding)

and build_match_case = fun match_case ->
  let origin = origin_from_node match_case in
  let view = SynAst.MatchCase.view match_case in
  (
    {
      origin;
      pattern = build_pattern (require_some origin "missing match case pattern" view.pattern);
      guard = Option.map view.guard ~fn:build_expression;
      body = build_expression (require_some origin "missing match case body" view.body)
    }:
      match_case
  )

and build_match_cases = fun syntax_expression ->
  let cases = ref [] in
  SynAst.Expr.for_each_match_case
    syntax_expression
    ~fn:(fun match_case -> cases := build_match_case match_case :: !cases);
  List.reverse !cases

and build_record_expression_field = fun (field: SynAst.RecordExpr.field) ->
  let origin = origin_from_node field.node in
  (
    {
      origin;
      name = path_from_syn_path (require_some origin "missing record field name" field.path);
      value = build_expression (require_some origin "missing record field value" field.value)
    }:
      record_expression_field
  )

and build_record_expression_fields = fun record ->
  let fields = ref [] in
  SynAst.RecordExpr.for_each_field
    record
    ~fn:(fun field -> fields := build_record_expression_field field :: !fields);
  List.reverse !fields

and build_expression = fun syntax_expression ->
  let origin = origin_from_node syntax_expression in
  (match SynAst.Expr.view syntax_expression with
    | SynAst.Expr.Literal { token } ->
        make_expression
          origin
          (Literal (Option.map token ~fn:literal_from_token |> Option.unwrap_or ~default:Unknown))
    | SynAst.Expr.Path { path } ->
        make_expression origin (Path (path_from_syn_path path))
    | SynAst.Expr.Parenthesized { inner=Some inner } ->
        build_expression inner
    | SynAst.Expr.Parenthesized { inner=None } ->
        make_expression origin (Literal Unit)
    | SynAst.Expr.Attribute { inner=Some inner } ->
        build_expression inner
    | SynAst.Expr.Attribute { inner=None } ->
        build_failed origin "missing attributed expression"
    | SynAst.Expr.Typed { expr=Some expr; annotation=Some annotation } ->
        with_expression_type_hint (build_core_type annotation) (build_expression expr)
    | SynAst.Expr.Typed { expr=Some expr; annotation=None } ->
        build_expression expr
    | SynAst.Expr.Typed { expr=None; _ } ->
        build_failed origin "missing typed expression"
    | SynAst.Expr.Tuple ->
        make_expression
          origin
          (Tuple (child_exprs syntax_expression |> List.map ~fn:build_expression))
    | SynAst.Expr.List ->
        make_expression
          origin
          (List (child_exprs syntax_expression |> List.map ~fn:build_expression))
    | SynAst.Expr.PolyVariant { payload=None } ->
        make_expression origin (PolyVariant (poly_variant_tag_from_node origin syntax_expression))
    | SynAst.Expr.PolyVariant { payload=Some _ } ->
        build_failed origin "polymorphic variant payload"
    | SynAst.Expr.Record ->
        let record = SynAst.RecordExpr.cast syntax_expression |> require_some origin "invalid record expression" in
        (
          match SynAst.RecordExpr.base record with
          | Some base -> make_expression
            origin
            (RecordUpdate {
              base = build_expression base;
              fields = build_record_expression_fields record
            })
          | None -> make_expression origin (Record (build_record_expression_fields record))
        )
    | SynAst.Expr.RecordUpdate ->
        let record = SynAst.RecordExpr.cast syntax_expression |> require_some origin "invalid record update" in
        make_expression
          origin
          (RecordUpdate {
            base = build_expression
              (require_some origin "missing record update base" (SynAst.RecordExpr.base record));
            fields = build_record_expression_fields record
          })
    | SynAst.Expr.FieldAccess { target=Some target; field=Some field } ->
        make_expression
          origin
          (FieldAccess {
            receiver = build_expression target;
            field = SurfacePath.from_name (token_text field)
          })
    | SynAst.Expr.FieldAccess _ ->
        build_failed origin "incomplete field access"
    | SynAst.Expr.Sequence { left=Some left; right=Some right } ->
        make_expression
          origin
          (Sequence { left = build_expression left; right = build_expression right })
    | SynAst.Expr.Sequence _ ->
        build_failed origin "incomplete sequence expression"
    | SynAst.Expr.If { condition=Some condition; then_branch=Some then_branch; else_branch } ->
        make_expression
          origin
          (If {
            condition = build_expression condition;
            then_branch = build_expression then_branch;
            else_branch = Option.map else_branch ~fn:build_expression
          })
    | SynAst.Expr.If _ ->
        build_failed origin "incomplete if expression"
    | SynAst.Expr.Match { scrutinee=Some scrutinee; first_case=Some _ } ->
        make_expression
          origin
          (Match {
            scrutinee = build_expression scrutinee;
            cases = build_match_cases syntax_expression
          })
    | SynAst.Expr.Match _ ->
        build_failed origin "incomplete match expression"
    | SynAst.Expr.Fun { body=Some body } ->
        make_expression
          origin
          (Function {
            parameters = direct_child_patterns syntax_expression |> List.map ~fn:build_pattern;
            body = Body (build_expression body)
          })
    | SynAst.Expr.Fun { body=None } ->
        build_failed origin "missing function body"
    | SynAst.Expr.Function { first_case=Some _ } ->
        make_expression
          origin
          (Function { parameters = []; body = Cases (build_match_cases syntax_expression) })
    | SynAst.Expr.Function { first_case=None } ->
        build_failed origin "missing function cases"
    | SynAst.Expr.Apply { callee=Some callee; argument } ->
        let arguments = [
          build_argument (require_some origin "missing application argument" argument)
        ] in
        make_expression origin (Apply { callee = build_expression callee; arguments })
    | SynAst.Expr.Apply { callee=None; _ } ->
        build_failed origin "missing application callee"
    | SynAst.Expr.Infix { left=Some left; operator=Some operator; right=Some right } ->
        make_expression
          origin
          (Infix {
            left = build_expression left;
            operator = SurfacePath.from_name (token_text operator);
            right = build_expression right
          })
    | SynAst.Expr.Infix _ ->
        build_failed origin "incomplete infix expression"
    | SynAst.Expr.Prefix { operator=Some operator; operand=Some operand } ->
        let callee = make_expression origin (Path (SurfacePath.from_name (token_text operator))) in
        let argument = build_expression operand in
        make_expression
          origin
          (Apply { callee; arguments = [ make_argument argument.origin (Positional argument) ] })
    | SynAst.Expr.Prefix _ ->
        build_failed origin "incomplete prefix expression"
    | SynAst.Expr.Let { first_binding=Some first_binding; body=Some body } ->
        make_expression
          origin
          (Let { first_binding = build_let_binding first_binding; body = build_expression body })
    | SynAst.Expr.Let _ ->
        build_failed origin "incomplete let expression"
    | SynAst.Expr.Assert { argument=Some argument } ->
        make_expression origin (Assert (build_expression argument))
    | SynAst.Expr.Assert { argument=None } ->
        build_failed origin "missing assert argument"
    | SynAst.Expr.LabeledArg _ ->
        build_failed origin "labeled argument outside application"
    | SynAst.Expr.OptionalArg _ ->
        build_failed origin "optional argument outside application"
    | SynAst.Expr.Error node ->
        unsupported_node node (node_summary node)
    | SynAst.Expr.Unknown node ->
        unsupported_node node (node_summary node)
    | SynAst.Expr.Array
    | SynAst.Expr.Extension
    | SynAst.Expr.FirstClassModule
    | SynAst.Expr.LocalOpen _
    | SynAst.Expr.LetModule _
    | SynAst.Expr.LetException _
    | SynAst.Expr.BindingOperator _
    | SynAst.Expr.Unreachable
    | SynAst.Expr.Object
    | SynAst.Expr.New
    | SynAst.Expr.Try _
    | SynAst.Expr.While _
    | SynAst.Expr.For _
    | SynAst.Expr.Lazy _
    | SynAst.Expr.Assign _
    | SynAst.Expr.MethodCall _
    | SynAst.Expr.ArrayIndex _
    | SynAst.Expr.StringIndex _ ->
        build_failed origin (Syn.SyntaxKind.to_string origin.kind): expression)

and build_argument = fun syntax_expression ->
  let origin = origin_from_node syntax_expression in
  (match SynAst.Expr.view syntax_expression with
    | SynAst.Expr.LabeledArg { label; value } -> make_argument
      origin
      (Labeled {
        label = token_text (require_some origin "missing labeled argument label" label);
        value = Option.map value ~fn:build_expression
      })
    | SynAst.Expr.OptionalArg { label; value } -> make_argument
      origin
      (Optional {
        label = token_text (require_some origin "missing optional argument label" label);
        value = Option.map value ~fn:build_expression
      })
    | _ -> make_argument origin (Positional (build_expression syntax_expression)): argument)

let build_let_declaration = fun declaration ->
  let bindings = ref [] in
  SynAst.LetDeclaration.for_each_binding
    declaration
    ~fn:(fun binding -> bindings := build_let_binding binding :: !bindings);
  (
    {
      origin = origin_from_node declaration;
      recursive = Option.is_some (SynAst.LetDeclaration.rec_token declaration);
      bindings = List.reverse !bindings
    }:
      let_declaration
  )

let name_from_declaration_tokens = fun for_each_token fallback ->
  let tokens = ref [] in
  for_each_token ~fn:(fun token -> tokens := token :: !tokens);
  match List.reverse !tokens with
  | [] -> Option.map fallback ~fn:token_text
  | tokens -> Some (path_from_tokens tokens |> SurfacePath.to_string)

let build_value_declaration = fun declaration ->
  let origin = origin_from_node declaration in
  (
    {
      origin;
      name = name_from_declaration_tokens
        (SynAst.ValueDeclaration.for_each_name_token declaration)
        (SynAst.ValueDeclaration.name declaration)
      |> require_some origin "missing value declaration name";
      type_annotation = SynAst.ValueDeclaration.type_annotation declaration
      |> require_some origin "missing value declaration type annotation"
      |> build_core_type
    }:
      value_declaration
  )

let build_external_declaration = fun declaration ->
  let origin = origin_from_node declaration in
  (
    {
      origin;
      name = name_from_declaration_tokens
        (SynAst.ExternalDeclaration.for_each_name_token declaration)
        (SynAst.ExternalDeclaration.name declaration)
      |> require_some origin "missing external declaration name";
      type_annotation = SynAst.ExternalDeclaration.type_annotation declaration
      |> require_some origin "missing external declaration type annotation"
      |> build_core_type
    }:
      external_declaration
  )

let build_type_parameter = function
  | SynAst.TypeDeclaration.Named { name; _ } -> Some (token_text name)
  | SynAst.TypeDeclaration.Wildcard _ -> None

let build_type_constructor = fun constructor ->
  let origin = origin_from_node constructor in
  (
    {
      origin;
      name = SynAst.VariantConstructor.name constructor
      |> require_some origin "missing variant constructor name"
      |> token_text;
      payload = Option.map (SynAst.VariantConstructor.payload_type constructor) ~fn:build_core_type
    }:
      type_constructor
  )

let build_record_field_declaration = fun field ->
  let origin = origin_from_node field in
  (
    {
      origin;
      name = SynAst.RecordField.name field |> require_some origin "missing record field name" |> token_text;
      type_annotation = SynAst.RecordField.type_annotation field
      |> require_some origin "missing record field type annotation"
      |> build_core_type
    }:
      record_field_declaration
  )

let build_record_field_declarations = fun record ->
  let fields = ref [] in
  SynAst.RecordType.for_each_field
    record
    ~fn:(fun field -> fields := build_record_field_declaration field :: !fields);
  List.reverse !fields

let build_type_declaration_member = fun member ->
  let origin = origin_from_node (SynAst.TypeDeclaration.Member.declaration member) in
  let parameters = ref [] in
  SynAst.TypeDeclaration.Member.for_each_parameter
    member
    ~fn:(fun parameter -> parameters := build_type_parameter parameter :: !parameters);
  let definition: type_definition =
    match SynAst.TypeDeclaration.Member.manifest member, SynAst.TypeDeclaration.Member.variant_type member, SynAst.TypeDeclaration.Member.record_type
      member with
    | Some manifest, _, _ ->
        make_type_definition origin (Alias (build_core_type manifest))
    | None, Some variant, _ ->
        let constructors = ref [] in
        SynAst.VariantType.for_each_constructor
          variant
          ~fn:(fun constructor -> constructors := build_type_constructor constructor :: !constructors);
        make_type_definition origin (Variant (List.reverse !constructors))
    | None, None, Some record ->
        make_type_definition origin (Record (build_record_field_declarations record))
    | None, None, None ->
        make_type_definition origin Abstract
  in
  (
    {
      origin;
      name = SynAst.TypeDeclaration.Member.name member
      |> require_some origin "missing type declaration name"
      |> token_text;
      parameters = List.reverse !parameters;
      definition
    }:
      type_declaration
  )

let build_type_declarations = fun declaration ->
  let declarations = ref [] in
  SynAst.TypeDeclaration.for_each_member
    declaration
    ~fn:(fun member -> declarations := build_type_declaration_member member :: !declarations);
  List.reverse !declarations

let build_structure_item = fun item ->
  let origin = origin_from_node item in
  (match SynAst.StructureItem.view item with
    | Let declaration ->
        make_structure_item origin (Let (build_let_declaration declaration))
    | Expr expr_item -> (
        match SynAst.ExprItem.expr expr_item with
        | Some expression -> make_structure_item origin (Expression (build_expression expression))
        | None -> build_failed origin "missing structure expression"
      )
    | External declaration ->
        make_structure_item origin (External (build_external_declaration declaration))
    | Type declaration ->
        make_structure_item origin (Type (build_type_declarations declaration))
    | TypeExtension declaration ->
        build_failed
          (origin_from_node declaration)
          (Syn.SyntaxKind.to_string (SynAst.Node.kind declaration))
    | Exception declaration ->
        build_failed
          (origin_from_node declaration)
          (Syn.SyntaxKind.to_string (SynAst.Node.kind declaration))
    | Class declaration ->
        build_failed
          (origin_from_node declaration)
          (Syn.SyntaxKind.to_string (SynAst.Node.kind declaration))
    | Attribute attribute ->
        build_failed
          (origin_from_node attribute)
          (Syn.SyntaxKind.to_string (SynAst.Node.kind attribute))
    | Extension extension ->
        build_failed
          (origin_from_node extension)
          (Syn.SyntaxKind.to_string (SynAst.Node.kind extension))
    | Module _
    | ModuleType _
    | Open _
    | Include _ ->
        build_failed origin (Syn.SyntaxKind.to_string origin.kind)
    | Error node ->
        unsupported_node node (node_summary node)
    | Unknown node ->
        unsupported_node node (node_summary node): structure_item)

let build_signature_item = fun item ->
  let origin = origin_from_node item in
  (match SynAst.SignatureItem.view item with
    | Value declaration -> make_signature_item origin (Value (build_value_declaration declaration))
    | External declaration -> make_signature_item
      origin
      (External (build_external_declaration declaration))
    | Type declaration -> make_signature_item origin (Type (build_type_declarations declaration))
    | TypeExtension declaration -> build_failed
      (origin_from_node declaration)
      (Syn.SyntaxKind.to_string (SynAst.Node.kind declaration))
    | Exception declaration -> build_failed
      (origin_from_node declaration)
      (Syn.SyntaxKind.to_string (SynAst.Node.kind declaration))
    | Class declaration -> build_failed
      (origin_from_node declaration)
      (Syn.SyntaxKind.to_string (SynAst.Node.kind declaration))
    | Attribute attribute -> build_failed
      (origin_from_node attribute)
      (Syn.SyntaxKind.to_string (SynAst.Node.kind attribute))
    | Extension extension -> build_failed
      (origin_from_node extension)
      (Syn.SyntaxKind.to_string (SynAst.Node.kind extension))
    | Module _
    | ModuleType _
    | Open _
    | Include _ -> build_failed origin (Syn.SyntaxKind.to_string origin.kind)
    | Error node -> unsupported_node node (node_summary node)
    | Unknown node -> unsupported_node node (node_summary node): signature_item)

let from_parse_result = fun ~source:_ (parse_result: Syn.Parser.parse_result) ->
  try
    let source_file = SynAst.SourceFile.make parse_result.tree in
    let kind =
      match parse_result.kind with
      | `Implementation -> `Implementation
      | `Interface -> `Interface
    in
    let ast =
      match SynAst.SourceFile.view source_file with
      | Implementation implementation ->
          let items = ref [] in
          SynAst.Implementation.for_each_item
            implementation
            ~fn:(fun item -> items := build_structure_item item :: !items);
          make_source_file (origin_from_node source_file) (Implementation (List.reverse !items))
      | Interface interface ->
          let items = ref [] in
          SynAst.Interface.for_each_item
            interface
            ~fn:(fun item -> items := build_signature_item item :: !items);
          make_source_file (origin_from_node source_file) (Interface (List.reverse !items))
      | Empty ->
          make_source_file (origin_from_node source_file) (Empty kind)
    in
    Ok ast
  with
  | Build_failed diagnostic -> Error [ diagnostic ]

let span_serializer = Serde.Ser.record
  (Serde.Ser.fields
    [
      Serde.Ser.field "start" Serde.Ser.int (fun (span: Syn.Ceibo.Span.t) -> span.start);
      Serde.Ser.field "end" Serde.Ser.int (fun (span: Syn.Ceibo.Span.t) -> span.end_);
    ])

let origin_serializer = Serde.Ser.record
  (Serde.Ser.fields
    [
      Serde.Ser.field "span" span_serializer (fun (origin: origin) -> origin.span);
      Serde.Ser.field
        "kind"
        (Serde.Ser.contramap Syn.SyntaxKind.to_string Serde.Ser.string)
        (fun (origin: origin) -> origin.kind);
    ])

let file_kind_serializer = Serde.Ser.variant
  [ Serde.Ser.Variant.unit "Implementation"
      (
        function
        | `Implementation -> true
        | `Interface -> false
      ); Serde.Ser.Variant.unit "Interface"
      (
        function
        | `Implementation -> false
        | `Interface -> true
      ); ]

let file_kind = function
  | { kind=Implementation _; _ } -> `Implementation
  | { kind=Interface _; _ } -> `Interface
  | { kind=Empty kind; _ } -> kind

let file_origin = fun file -> file.origin

let view_name = function
  | { kind=Implementation _; _ } -> "Implementation"
  | { kind=Interface _; _ } -> "Interface"
  | { kind=Empty _; _ } -> "Empty"

let item_count = function
  | { kind=Implementation items; _ } -> List.length items
  | { kind=Interface items; _ } -> List.length items
  | { kind=Empty _; _ } -> 0

let serializer = Serde.Ser.record
  (Serde.Ser.fields
    [
      Serde.Ser.field "kind" file_kind_serializer (fun (file: t) -> file_kind file);
      Serde.Ser.field "origin" origin_serializer (fun (file: t) -> file_origin file);
      Serde.Ser.field "view" Serde.Ser.string (fun (file: t) -> view_name file);
      Serde.Ser.field "item_count" Serde.Ser.int (fun (file: t) -> item_count file);
    ])

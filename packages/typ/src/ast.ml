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
  view: core_type_view;
}

and core_type_view =
  | TypeWildcard
  | TypeVar of string option
  | TypePath of path
  | TypeApply of { argument: core_type option; constructor: core_type option }
  | TypeArrow of { left: core_type option; right: core_type option }
  | TypeTuple of core_type list
  | TypeLabeled of { annotation: core_type option }
  | TypePoly of { body: core_type option }
  | TypeUnsupported of string
  | TypeError of string

type parameter = {
  origin: origin;
  view: parameter_view;
}

and parameter_view =
  | Labeled of { label: string option; pattern: pattern option }
  | Optional of { label: string option; pattern: pattern option }
  | OptionalDefault of { label: string option; pattern: pattern option; default: expr option }
  | UnknownParameter of string

and pattern = {
  origin: origin;
  view: pattern_view;
}

and pattern_view =
  | PatternWildcard
  | PatternPath of path
  | PatternApply of { callee: pattern option; argument: pattern option }
  | PatternLiteral of literal
  | PatternTuple of pattern list
  | PatternList of pattern list
  | PatternCons of { head: pattern option; tail: pattern option }
  | PatternConstraint of { pattern: pattern option; annotation: core_type option }
  | PatternAlias of { pattern: pattern option; alias: pattern option }
  | PatternAttribute of { inner: pattern option }
  | PatternLabeledParam of parameter
  | PatternOptionalParam of parameter
  | PatternOptionalParamDefault of parameter
  | PatternUnsupported of string
  | PatternError of string

and let_binding = {
  origin: origin;
  pattern: pattern option;
  parameters: pattern list;
  body: expr option;
  type_annotation: core_type option;
}

and expr = {
  origin: origin;
  view: expr_view;
}

and expr_view =
  | ExprLiteral of literal
  | ExprPath of path
  | ExprParenthesized of { inner: expr option }
  | ExprAttribute of { inner: expr option }
  | ExprTyped of { expr: expr option; annotation: core_type option }
  | ExprTuple of expr list
  | ExprList of expr list
  | ExprSequence of { left: expr option; right: expr option }
  | ExprIf of { condition: expr option; then_branch: expr option; else_branch: expr option }
  | ExprApply of { callee: expr option; argument: expr option }
  | ExprInfix of { left: expr option; operator: path option; right: expr option }
  | ExprPrefix of { operator: path option; operand: expr option }
  | ExprLet of { first_binding: let_binding option; body: expr option }
  | ExprAssert of { argument: expr option }
  | ExprLabeledArg of { label: string option; value: expr option }
  | ExprOptionalArg of { label: string option; value: expr option }
  | ExprUnsupported of string
  | ExprError of string

type let_declaration = {
  origin: origin;
  recursive: bool;
  bindings: let_binding list;
}

type value_declaration = {
  origin: origin;
  name: string option;
  type_annotation: core_type option;
}

type external_declaration = {
  origin: origin;
  name: string option;
  type_annotation: core_type option;
}

type structure_item = {
  origin: origin;
  view: structure_item_view;
}

and structure_item_view =
  | StructureLet of let_declaration
  | StructureExpr of expr option
  | StructureExternal of external_declaration
  | StructureUnsupported of string
  | StructureError of string

type signature_item = {
  origin: origin;
  view: signature_item_view;
}

and signature_item_view =
  | SignatureValue of value_declaration
  | SignatureExternal of external_declaration
  | SignatureUnsupported of string
  | SignatureError of string

type view =
  | Implementation of structure_item list
  | Interface of signature_item list
  | Empty

type t = {
  kind: file_kind;
  origin: origin;
  view: view;
}

let span_of_token_body = fun token ->
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

let span_of_node = fun node ->
  match SynAst.Node.first_descendant_token node with
  | None ->
      let start, end_ = SynAst.Node.raw_range node in
      Syn.Ceibo.Span.make ~start ~end_
  | Some first ->
      let start, _ = span_of_token_body first in
      let last_end = ref start in
      SynAst.Node.for_each_token node
        ~fn:(fun token ->
          let _, end_ = span_of_token_body token in
          last_end := end_);
      Syn.Ceibo.Span.make ~start ~end_:!last_end

let origin_of_node = fun node -> { span = span_of_node node; kind = SynAst.Node.kind node }

let token_text = SynAst.Token.text

let path_of_syn_path = fun path ->
  let segments = ref [] in
  SynAst.Path.for_each_ident path ~fn:(fun token -> segments := token_text token :: !segments);
  SurfacePath.of_segments (List.reverse !segments)

let path_of_tokens = fun tokens ->
  tokens |> List.map ~fn:token_text |> String.concat "" |> SurfacePath.of_name

let literal_of_token = fun token ->
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

let child_exprs = fun expression ->
  let children = ref [] in
  SynAst.Expr.for_each_child_expr expression ~fn:(fun child -> children := child :: !children);
  List.reverse !children

let rec build_core_type = fun type_expr ->
  let origin = origin_of_node type_expr in
  let view =
    match SynAst.TypeExpr.view type_expr with
    | SynAst.TypeExpr.Wildcard ->
        TypeWildcard
    | SynAst.TypeExpr.Var { name } ->
        TypeVar (Option.map name ~fn:token_text)
    | SynAst.TypeExpr.Path { path } ->
        TypePath (path_of_syn_path path)
    | SynAst.TypeExpr.Apply { argument; constructor } ->
        TypeApply {
          argument = Option.map argument ~fn:build_core_type;
          constructor = Option.map constructor ~fn:build_core_type
        }
    | SynAst.TypeExpr.Arrow { left; right } ->
        TypeArrow {
          left = Option.map left ~fn:build_core_type;
          right = Option.map right ~fn:build_core_type
        }
    | SynAst.TypeExpr.Tuple _ ->
        TypeTuple (child_type_exprs type_expr)
    | SynAst.TypeExpr.Labeled { annotation; _ } ->
        TypeLabeled { annotation = Option.map annotation ~fn:build_core_type }
    | SynAst.TypeExpr.Poly { body } ->
        TypePoly { body = Option.map body ~fn:build_core_type }
    | SynAst.TypeExpr.Parenthesized { inner=Some inner } ->
        (build_core_type inner).view
    | SynAst.TypeExpr.Parenthesized { inner=None } ->
        TypeError "missing parenthesized type"
    | SynAst.TypeExpr.Opaque node -> (
        match SynAst.TypeExpr.inner_without_attribute_suffix type_expr with
        | Some inner -> (build_core_type inner).view
        | None -> TypeUnsupported (node_summary node)
      )
    | SynAst.TypeExpr.Error node ->
        TypeError (node_summary node)
    | SynAst.TypeExpr.Unknown node ->
        TypeUnsupported (node_summary node)
  in
  ({ origin; view }: core_type)

and child_type_exprs = fun type_expr ->
  let children = ref [] in
  SynAst.TypeExpr.for_each_child_type
    type_expr
    ~fn:(fun child -> children := build_core_type child :: !children);
  List.reverse !children

let rec build_parameter = fun parameter ->
  let origin = origin_of_node parameter in
  let view =
    match SynAst.Parameter.view parameter with
    | SynAst.Parameter.Labeled { label; pattern } -> Labeled {
      label = Option.map label ~fn:token_text;
      pattern = Option.map pattern ~fn:build_pattern
    }
    | SynAst.Parameter.Optional { label; pattern } -> Optional {
      label = Option.map label ~fn:token_text;
      pattern = Option.map pattern ~fn:build_pattern
    }
    | SynAst.Parameter.OptionalDefault { label; pattern; default } -> OptionalDefault {
      label = Option.map label ~fn:token_text;
      pattern = Option.map pattern ~fn:build_pattern;
      default = Option.map default ~fn:build_expr
    }
    | SynAst.Parameter.Unknown node -> UnknownParameter (node_summary node)
  in
  ({ origin; view }: parameter)

and build_pattern = fun pattern ->
  let origin = origin_of_node pattern in
  let view =
    match SynAst.Pattern.view pattern with
    | SynAst.Pattern.Wildcard -> PatternWildcard
    | SynAst.Pattern.Path { path } -> PatternPath (path_of_syn_path path)
    | SynAst.Pattern.Apply { callee; argument } -> PatternApply {
      callee = Option.map callee ~fn:build_pattern;
      argument = Option.map argument ~fn:build_pattern
    }
    | SynAst.Pattern.Literal { token } -> PatternLiteral (Option.map token ~fn:literal_of_token
    |> Option.unwrap_or ~default:Unknown)
    | SynAst.Pattern.Tuple -> PatternTuple (child_patterns pattern |> List.map ~fn:build_pattern)
    | SynAst.Pattern.List -> PatternList (child_patterns pattern |> List.map ~fn:build_pattern)
    | SynAst.Pattern.Cons { head; tail } -> PatternCons {
      head = Option.map head ~fn:build_pattern;
      tail = Option.map tail ~fn:build_pattern
    }
    | SynAst.Pattern.Parenthesized { inner=Some inner } -> (build_pattern inner).view
    | SynAst.Pattern.Parenthesized { inner=None } -> PatternError "missing parenthesized pattern"
    | SynAst.Pattern.Constraint { pattern; annotation } -> PatternConstraint {
      pattern = Option.map pattern ~fn:build_pattern;
      annotation = Option.map annotation ~fn:build_core_type
    }
    | SynAst.Pattern.Alias { pattern; alias } -> PatternAlias {
      pattern = Option.map pattern ~fn:build_pattern;
      alias = Option.map alias ~fn:build_pattern
    }
    | SynAst.Pattern.Attribute { inner } -> PatternAttribute {
      inner = Option.map inner ~fn:build_pattern
    }
    | SynAst.Pattern.LabeledParam parameter -> PatternLabeledParam (build_parameter parameter)
    | SynAst.Pattern.OptionalParam parameter -> PatternOptionalParam (build_parameter parameter)
    | SynAst.Pattern.OptionalParamDefault parameter -> PatternOptionalParamDefault (build_parameter parameter)
    | SynAst.Pattern.Error node -> PatternError (node_summary node)
    | SynAst.Pattern.Unknown node -> PatternUnsupported (node_summary node)
    | SynAst.Pattern.Array
    | SynAst.Pattern.Record
    | SynAst.Pattern.PolyVariant
    | SynAst.Pattern.Extension
    | SynAst.Pattern.LocalOpen
    | SynAst.Pattern.LocallyAbstractType
    | SynAst.Pattern.FirstClassModule
    | SynAst.Pattern.Interval _
    | SynAst.Pattern.Or _
    | SynAst.Pattern.Lazy _
    | SynAst.Pattern.Exception _ -> PatternUnsupported (Syn.SyntaxKind.to_string origin.kind)
  in
  ({ origin; view }: pattern)

and build_let_binding = fun binding ->
  let parameters = ref [] in
  SynAst.LetBinding.for_each_parameter
    binding
    ~fn:(fun parameter -> parameters := build_pattern parameter :: !parameters);
  ({
      origin = origin_of_node binding;
      pattern = Option.map (SynAst.LetBinding.pattern binding) ~fn:build_pattern;
      parameters = List.reverse !parameters;
      body = Option.map (SynAst.LetBinding.body binding) ~fn:build_expr;
      type_annotation = Option.map (SynAst.LetBinding.type_annotation binding) ~fn:build_core_type;
    }: let_binding)

and build_expr = fun expression ->
  let origin = origin_of_node expression in
  let view =
    match SynAst.Expr.view expression with
    | SynAst.Expr.Literal { token } -> ExprLiteral (Option.map token ~fn:literal_of_token
    |> Option.unwrap_or ~default:Unknown)
    | SynAst.Expr.Path { path } -> ExprPath (path_of_syn_path path)
    | SynAst.Expr.Parenthesized { inner } -> ExprParenthesized {
      inner = Option.map inner ~fn:build_expr
    }
    | SynAst.Expr.Attribute { inner } -> ExprAttribute { inner = Option.map inner ~fn:build_expr }
    | SynAst.Expr.Typed { expr; annotation } -> ExprTyped {
      expr = Option.map expr ~fn:build_expr;
      annotation = Option.map annotation ~fn:build_core_type
    }
    | SynAst.Expr.Tuple -> ExprTuple (child_exprs expression |> List.map ~fn:build_expr)
    | SynAst.Expr.List -> ExprList (child_exprs expression |> List.map ~fn:build_expr)
    | SynAst.Expr.Sequence { left; right } -> ExprSequence {
      left = Option.map left ~fn:build_expr;
      right = Option.map right ~fn:build_expr
    }
    | SynAst.Expr.If { condition; then_branch; else_branch } -> ExprIf {
      condition = Option.map condition ~fn:build_expr;
      then_branch = Option.map then_branch ~fn:build_expr;
      else_branch = Option.map else_branch ~fn:build_expr
    }
    | SynAst.Expr.Apply { callee; argument } -> ExprApply {
      callee = Option.map callee ~fn:build_expr;
      argument = Option.map argument ~fn:build_expr
    }
    | SynAst.Expr.Infix { left; operator; right } -> ExprInfix {
      left = Option.map left ~fn:build_expr;
      operator = Option.map operator ~fn:(fun token -> SurfacePath.of_name (token_text token));
      right = Option.map right ~fn:build_expr
    }
    | SynAst.Expr.Prefix { operator; operand } -> ExprPrefix {
      operator = Option.map operator ~fn:(fun token -> SurfacePath.of_name (token_text token));
      operand = Option.map operand ~fn:build_expr
    }
    | SynAst.Expr.Let { first_binding; body } -> ExprLet {
      first_binding = Option.map first_binding ~fn:build_let_binding;
      body = Option.map body ~fn:build_expr
    }
    | SynAst.Expr.Assert { argument } -> ExprAssert { argument = Option.map argument ~fn:build_expr }
    | SynAst.Expr.LabeledArg { label; value } -> ExprLabeledArg {
      label = Option.map label ~fn:token_text;
      value = Option.map value ~fn:build_expr
    }
    | SynAst.Expr.OptionalArg { label; value } -> ExprOptionalArg {
      label = Option.map label ~fn:token_text;
      value = Option.map value ~fn:build_expr
    }
    | SynAst.Expr.Error node -> ExprError (node_summary node)
    | SynAst.Expr.Unknown node -> ExprUnsupported (node_summary node)
    | SynAst.Expr.FieldAccess _ -> ExprUnsupported "field access"
    | SynAst.Expr.Match _ -> ExprUnsupported "match expression"
    | SynAst.Expr.Function _ -> ExprUnsupported "function expression"
    | SynAst.Expr.Fun _ -> ExprUnsupported "function expression"
    | SynAst.Expr.Array
    | SynAst.Expr.Record
    | SynAst.Expr.RecordUpdate
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
    | SynAst.Expr.PolyVariant _
    | SynAst.Expr.ArrayIndex _
    | SynAst.Expr.StringIndex _ -> ExprUnsupported (Syn.SyntaxKind.to_string origin.kind)
  in
  ({ origin; view }: expr)

let build_let_declaration = fun declaration ->
  let bindings = ref [] in
  SynAst.LetDeclaration.for_each_binding
    declaration
    ~fn:(fun binding -> bindings := build_let_binding binding :: !bindings);
  (
    {
      origin = origin_of_node declaration;
      recursive = Option.is_some (SynAst.LetDeclaration.rec_token declaration);
      bindings = List.reverse !bindings
    }:
      let_declaration
  )

let name_of_declaration_tokens = fun for_each_token fallback ->
  let tokens = ref [] in
  for_each_token ~fn:(fun token -> tokens := token :: !tokens);
  match List.reverse !tokens with
  | [] -> Option.map fallback ~fn:token_text
  | tokens -> Some (path_of_tokens tokens |> SurfacePath.to_string)

let build_value_declaration = fun declaration ->
  (
    {
      origin = origin_of_node declaration;
      name = name_of_declaration_tokens
        (SynAst.ValueDeclaration.for_each_name_token declaration)
        (SynAst.ValueDeclaration.name declaration);
      type_annotation = Option.map (SynAst.ValueDeclaration.type_annotation declaration) ~fn:build_core_type
    }:
      value_declaration
  )

let build_external_declaration = fun declaration ->
  (
    {
      origin = origin_of_node declaration;
      name = name_of_declaration_tokens
        (SynAst.ExternalDeclaration.for_each_name_token declaration)
        (SynAst.ExternalDeclaration.name declaration);
      type_annotation = Option.map (SynAst.ExternalDeclaration.type_annotation declaration) ~fn:build_core_type
    }:
      external_declaration
  )

let build_structure_item = fun item ->
  let origin = origin_of_node item in
  let view =
    match SynAst.StructureItem.view item with
    | Let declaration -> StructureLet (build_let_declaration declaration)
    | Expr expr_item -> StructureExpr (Option.map (SynAst.ExprItem.expr expr_item) ~fn:build_expr)
    | External declaration -> StructureExternal (build_external_declaration declaration)
    | Type declaration -> StructureUnsupported (Syn.SyntaxKind.to_string
      (SynAst.Node.kind declaration))
    | TypeExtension declaration -> StructureUnsupported (Syn.SyntaxKind.to_string
      (SynAst.Node.kind declaration))
    | Exception declaration -> StructureUnsupported (Syn.SyntaxKind.to_string
      (SynAst.Node.kind declaration))
    | Class declaration -> StructureUnsupported (Syn.SyntaxKind.to_string
      (SynAst.Node.kind declaration))
    | Attribute attribute -> StructureUnsupported (Syn.SyntaxKind.to_string
      (SynAst.Node.kind attribute))
    | Extension extension -> StructureUnsupported (Syn.SyntaxKind.to_string
      (SynAst.Node.kind extension))
    | Module _
    | ModuleType _
    | Open _
    | Include _ -> StructureUnsupported (Syn.SyntaxKind.to_string origin.kind)
    | Error node -> StructureError (node_summary node)
    | Unknown node -> StructureUnsupported (node_summary node)
  in
  ({ origin; view }: structure_item)

let build_signature_item = fun item ->
  let origin = origin_of_node item in
  let view =
    match SynAst.SignatureItem.view item with
    | Value declaration -> SignatureValue (build_value_declaration declaration)
    | External declaration -> SignatureExternal (build_external_declaration declaration)
    | Type declaration -> SignatureUnsupported (Syn.SyntaxKind.to_string
      (SynAst.Node.kind declaration))
    | TypeExtension declaration -> SignatureUnsupported (Syn.SyntaxKind.to_string
      (SynAst.Node.kind declaration))
    | Exception declaration -> SignatureUnsupported (Syn.SyntaxKind.to_string
      (SynAst.Node.kind declaration))
    | Class declaration -> SignatureUnsupported (Syn.SyntaxKind.to_string
      (SynAst.Node.kind declaration))
    | Attribute attribute -> SignatureUnsupported (Syn.SyntaxKind.to_string
      (SynAst.Node.kind attribute))
    | Extension extension -> SignatureUnsupported (Syn.SyntaxKind.to_string
      (SynAst.Node.kind extension))
    | Module _
    | ModuleType _
    | Open _
    | Include _ -> SignatureUnsupported (Syn.SyntaxKind.to_string origin.kind)
    | Error node -> SignatureError (node_summary node)
    | Unknown node -> SignatureUnsupported (node_summary node)
  in
  ({ origin; view }: signature_item)

let of_parse_result = fun ~source:_ (parse_result: Syn.Parser.parse_result) ->
  let source_file = SynAst.SourceFile.make parse_result.tree in
  let kind =
    match parse_result.kind with
    | `Implementation -> `Implementation
    | `Interface -> `Interface
  in
  let view =
    match SynAst.SourceFile.view source_file with
    | Implementation implementation ->
        let items = ref [] in
        SynAst.Implementation.for_each_item
          implementation
          ~fn:(fun item -> items := build_structure_item item :: !items);
        Implementation (List.reverse !items)
    | Interface interface ->
        let items = ref [] in
        SynAst.Interface.for_each_item
          interface
          ~fn:(fun item -> items := build_signature_item item :: !items);
        Interface (List.reverse !items)
    | Empty ->
        Empty
  in
  ({ kind; origin = origin_of_node source_file; view }: t)

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

let view_name = function
  | Implementation _ -> "Implementation"
  | Interface _ -> "Interface"
  | Empty -> "Empty"

let item_count = function
  | Implementation items -> List.length items
  | Interface items -> List.length items
  | Empty -> 0

let serializer = Serde.Ser.record
  (Serde.Ser.fields
    [
      Serde.Ser.field "kind" file_kind_serializer (fun (file: t) -> file.kind);
      Serde.Ser.field "origin" origin_serializer (fun (file: t) -> file.origin);
      Serde.Ser.field "view" Serde.Ser.string (fun (file: t) -> view_name file.view);
      Serde.Ser.field "item_count" Serde.Ser.int (fun (file: t) -> item_count file.view);
    ])

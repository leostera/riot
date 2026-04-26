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

type type_tuple_separator =
[
  `Star
  | `Comma
  | `Unknown
]

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
  | Tuple of { separator: type_tuple_separator; elements: core_type list }
  | Labeled of core_type
  | Poly of { parameters: string list; body: core_type }
  | PolyVariant of poly_variant_type_field list
  | Package of package_type
  | Parenthesized of core_type

and poly_variant_type_field = {
  origin: origin;
  tag: string;
  payload: core_type option;
}

and package_type = {
  origin: origin;
  binder: string option;
  module_type: path;
  constraints: package_type_constraint list;
}

and package_type_constraint = {
  origin: origin;
  type_name: path;
  manifest: core_type;
}

type type_parameter = string option

type type_constructor = {
  origin: origin;
  name: string;
  payload: core_type option;
  result: core_type option;
  inline_record: record_field_declaration list option;
}

and record_field_declaration = {
  origin: origin;
  name: string;
  mutable_: bool;
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
  | PolyVariant of poly_variant_pattern
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
  | LocallyAbstractType of string list
  | FirstClassModule of { binder: string option; package_type: package_type option }

and poly_variant_pattern = {
  tag: string;
  payload: pattern option;
}

and let_binding = {
  origin: origin;
  pattern: pattern;
  parameters: pattern list;
  body: expression;
  type_annotation: core_type option;
}

and expression_type_hint_kind =
  | Annotation
  | Coercion

and expression_type_hint = {
  kind: expression_type_hint_kind;
  type_: core_type;
}

and expression = {
  origin: origin;
  type_hint: expression_type_hint option;
  kind: expression_kind;
}

and module_unpack = {
  origin: origin;
  expression: expression;
  package_type: package_type option;
}

and expression_kind =
  | Literal of literal
  | Path of path
  | Tuple of expression list
  | List of expression list
  | PolyVariant of poly_variant_expression
  | Record of record_expression_field list
  | RecordUpdate of { base: expression; fields: record_expression_field list }
  | FieldAccess of { receiver: expression; field: path }
  | Assign of { target: expression; value: expression }
  | Sequence of { left: expression; right: expression }
  | If of { condition: expression; then_branch: expression; else_branch: expression option }
  | Match of { scrutinee: expression; cases: match_case list }
  | Function of { parameters: pattern list; body: function_body }
  | Apply of { callee: expression; arguments: argument list }
  | Infix of { left: expression; operator: path; right: expression }
  | Let of { first_binding: let_binding; body: expression }
  | LetModule of {
      name: string;
      items: structure_item list;
      alias: path option;
      unpack: module_unpack option;
      body: expression
    }
  | LocalOpen of { module_path: path; body: expression }
  | FirstClassModule of { module_path: path; package_type: package_type option }
  | Assert of expression

and poly_variant_expression = {
  tag: string;
  payload: expression option;
}

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

and let_declaration = {
  origin: origin;
  recursive: bool;
  bindings: let_binding list;
}

and value_declaration = {
  origin: origin;
  name: string;
  type_annotation: core_type;
}

and external_declaration = {
  origin: origin;
  name: string;
  type_annotation: core_type;
}

and module_declaration = {
  origin: origin;
  name: string;
  parameters: functor_parameter list;
  items: structure_item list;
  alias: path option;
  module_type: path option;
  application: module_application option;
}

and functor_parameter = {
  origin: origin;
  name: string;
  module_type: path option;
}

and module_application = {
  callee: path;
  argument: path;
}

and module_type_declaration = {
  origin: origin;
  name: string;
  items: signature_item list;
}

and structure_item = {
  origin: origin;
  kind: structure_item_kind;
}

and structure_item_kind =
  | Let of let_declaration
  | Type of type_declaration list
  | Expression of expression
  | External of external_declaration
  | Module of module_declaration list
  | ModuleType of module_type_declaration
  | Include of path

and signature_item = {
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

let path_from_ident_tokens = fun tokens -> tokens |> List.map ~fn:token_text |> SurfacePath.from_segments

let ident_tokens = fun tokens ->
  tokens |> List.filter ~fn:(fun token -> Syn.SyntaxKind.(SynAst.Token.kind token = IDENT))

let path_from_ident_tokens_in_tokens = fun tokens -> tokens |> ident_tokens |> path_from_ident_tokens

let path_from_ident_tokens_in_node = fun node ->
  let tokens = ref [] in
  SynAst.Node.for_each_token node
    ~fn:(fun token ->
      match SynAst.Token.kind token with
      | Syn.SyntaxKind.IDENT -> tokens := token :: !tokens
      | _ -> ());
  path_from_ident_tokens (List.reverse !tokens)

let path_from_module_type_node = fun node ->
  let tokens = ref [] in
  let stop = ref false in
  SynAst.Node.for_each_token node
    ~fn:(fun token ->
      match SynAst.Token.kind token with
      | Syn.SyntaxKind.WITH_KW -> stop := true
      | Syn.SyntaxKind.IDENT when not !stop -> tokens := token :: !tokens
      | _ -> ());
  match List.reverse !tokens with
  | [] -> None
  | tokens -> Some (path_from_ident_tokens tokens)

let direct_child_tokens = fun node ->
  let tokens = ref [] in
  SynAst.Node.for_each_child_token node ~fn:(fun token -> tokens := token :: !tokens);
  List.reverse !tokens

let token_kind_is = fun token kind -> Syn.SyntaxKind.(SynAst.Token.kind token = kind)

let rec split_at_token_kind = fun kind tokens ->
  match tokens with
  | [] ->
      ([], None)
  | token :: rest when token_kind_is token kind ->
      ([], Some rest)
  | token :: rest ->
      let before, after = split_at_token_kind kind rest in
      (token :: before, after)

let tokens_until_any = fun stop_kinds tokens ->
  let rec loop acc tokens =
    match tokens with
    | [] -> List.reverse acc
    | token :: _ when List.exists (token_kind_is token) stop_kinds -> List.reverse acc
    | token :: rest -> loop (token :: acc) rest
  in
  loop [] tokens

let tokens_after_token_kind = fun kind tokens ->
  match split_at_token_kind kind tokens with
  | _, Some after -> Some after
  | _, None -> None

let drop_final_rparen = fun tokens ->
  match List.reverse tokens with
  | token :: rest when token_kind_is token Syn.SyntaxKind.RPAREN -> List.reverse rest
  | _ -> tokens

let type_source_from_tokens = fun tokens -> tokens |> List.map ~fn:token_text |> String.concat " "

let path_from_module_expr_node = fun node ->
  match SynAst.Node.kind node with
  | Syn.SyntaxKind.PATH_MODULE_EXPR ->
      Some (path_from_ident_tokens_in_node node)
  | Syn.SyntaxKind.MODULE_EXPR -> (
      match SynAst.Node.first_child_node node ~kind:Syn.SyntaxKind.PATH_MODULE_EXPR with
      | Some path -> Some (path_from_ident_tokens_in_node path)
      | None -> None
    )
  | _ ->
      None

let module_application_from_module_expr = fun node ->
  match SynAst.Node.kind node with
  | Syn.SyntaxKind.APPLY_MODULE_EXPR ->
      let paths = ref [] in
      SynAst.Node.for_each_child_node node
        ~fn:(fun child ->
          match path_from_module_expr_node child with
          | Some path -> paths := path :: !paths
          | None -> ());
      (
        match List.reverse !paths with
        | callee :: argument :: _ -> Some { callee; argument }
        | _ -> None
      )
  | _ -> None

let member_ident_tokens_between = fun member ~start ~stop ->
  let rec loop index tokens =
    if index >= stop then
      List.reverse tokens
    else
      match SynAst.ModuleDeclaration.Member.child_token_at member index with
      | Some token when Syn.SyntaxKind.(SynAst.Token.kind token = IDENT) -> loop
        (index + 1)
        (token :: tokens)
      | _ -> loop (index + 1) tokens
  in
  loop start []

let member_find_token = fun member ~start kind ->
  let rec loop index =
    if index >= SynAst.ModuleDeclaration.Member.child_count member then
      None
    else if SynAst.ModuleDeclaration.Member.child_token_kind_is member index kind then
      Some index
    else
      loop (index + 1)
  in
  loop start

let functor_parameters_from_member = fun origin member ->
  let child_count = SynAst.ModuleDeclaration.Member.child_count member in
  let rec loop index parameters =
    if index >= child_count then
      List.reverse parameters
    else
      match SynAst.ModuleDeclaration.Member.child_token_kind_is member index Syn.SyntaxKind.LPAREN, SynAst.ModuleDeclaration.Member.child_token_at
        member
        (index + 1), SynAst.ModuleDeclaration.Member.child_token_kind_is
        member
        (index + 2)
        Syn.SyntaxKind.COLON, member_find_token member ~start:(index + 3) Syn.SyntaxKind.RPAREN with
      | true, Some name, true, Some close_index when Syn.SyntaxKind.(SynAst.Token.kind name = IDENT) ->
          let module_type =
            match member_ident_tokens_between member ~start:(index + 3) ~stop:close_index with
            | [] -> None
            | tokens -> Some (path_from_ident_tokens tokens)
          in
          let parameter = { origin; name = token_text name; module_type } in
          loop (close_index + 1) (parameter :: parameters)
      | _ -> loop (index + 1) parameters
  in
  loop 0 []

let literal_from_token = fun token ->
  match SynAst.Token.kind token with
  | Syn.SyntaxKind.INT -> Int
  | FLOAT -> Float
  | CHAR -> Char
  | STRING -> String
  | TRUE_KW
  | FALSE_KW -> Bool
  | _ -> Unit

let type_tuple_separator_from_syn = function
  | SynAst.TypeExpr.Star -> `Star
  | SynAst.TypeExpr.Comma -> `Comma
  | SynAst.TypeExpr.UnknownSeparator -> `Unknown

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

let with_expression_type_hint = fun kind type_ (expression: expression) ->
  { expression with type_hint = Some { kind; type_ } }

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

let poly_variant_tag_names_from_node = fun node ->
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
  match poly_variant_tag_names_from_node node with
  | tag :: _ -> tag
  | [] -> build_failed origin "missing polymorphic variant tag"

type build_context = {
  mutable poly_variant_type_aliases: (string * poly_variant_type_field list) list;
}

let make_build_context = fun () -> { poly_variant_type_aliases = [] }

let poly_variant_type_fields_from_node = fun origin node ->
  poly_variant_tag_names_from_node node
  |> List.map ~fn:(fun tag -> ({ origin; tag; payload = None }: poly_variant_type_field))

let poly_variant_inherited_type_names_from_node = fun node ->
  let names = ref [] in
  let saw_backtick = ref false in
  SynAst.Node.for_each_token node
    ~fn:(fun token ->
      match SynAst.Token.kind token with
      | Syn.SyntaxKind.BACKTICK ->
          saw_backtick := true
      | IDENT when !saw_backtick ->
          saw_backtick := false
      | IDENT ->
          names := token_text token :: !names;
          saw_backtick := false
      | _ ->
          saw_backtick := false);
  List.reverse !names

let find_poly_variant_type_alias = fun context name ->
  List.find context.poly_variant_type_aliases
    ~fn:(fun (alias_name, _) ->
      String.equal alias_name name)

let inherited_poly_variant_type_fields_from_node = fun context origin node ->
  let names = poly_variant_inherited_type_names_from_node node in
  if List.is_empty names then
    []
  else
    names |> List.flat_map
      ~fn:(fun name ->
        match find_poly_variant_type_alias context name with
        | Some (_, fields) -> fields
        | None -> build_failed origin ("unknown polymorphic variant row " ^ name))

let opaque_type_is_token = fun kind type_expr ->
  match SynAst.TypeExpr.view type_expr with
  | SynAst.TypeExpr.Opaque node -> direct_child_tokens node
  |> List.exists (fun token -> token_kind_is token kind)
  | _ -> false

let type_expr_is_coercion = fun type_expr ->
  match SynAst.TypeExpr.view type_expr with
  | SynAst.TypeExpr.Apply { argument=Some argument; constructor=Some _ } -> opaque_type_is_token
    Syn.SyntaxKind.GT
    argument
  | _ -> false

let source_slice = fun source -> IO.IoVec.IoSlice.from_string source |> Result.expect ~msg:"failed to create typ AST parser source slice"

let rec build_core_type = fun context type_expr ->
  let origin = origin_from_node type_expr in
  (match SynAst.TypeExpr.view type_expr with
    | SynAst.TypeExpr.Wildcard ->
        make_core_type origin Wildcard
    | SynAst.TypeExpr.Var { name } ->
        make_core_type origin (Var (Option.map name ~fn:token_text))
    | SynAst.TypeExpr.Path { path } ->
        make_core_type origin (Path (path_from_syn_path path))
    | SynAst.TypeExpr.Apply { argument=Some argument; constructor=Some constructor } when opaque_type_is_token
      Syn.SyntaxKind.GT
      argument ->
        build_core_type context constructor
    | SynAst.TypeExpr.Apply { argument; constructor } ->
        make_core_type
          origin
          (Apply {
            argument = build_core_type
              context
              (require_some origin "missing type application argument" argument);
            constructor = build_core_type
              context
              (require_some origin "missing type application constructor" constructor)
          })
    | SynAst.TypeExpr.Arrow { left; right } ->
        make_core_type
          origin
          (Arrow {
            left = build_core_type context (require_some origin "missing arrow parameter type" left);
            right = build_core_type context (require_some origin "missing arrow result type" right)
          })
    | SynAst.TypeExpr.Tuple { left; right; separator } ->
        let separator = type_tuple_separator_from_syn separator in
        make_core_type
          origin
          (Tuple { separator; elements = tuple_type_elements context origin separator left right })
    | SynAst.TypeExpr.Labeled { annotation; _ } ->
        make_core_type
          origin
          (Labeled (build_core_type
            context
            (require_some origin "missing labeled type annotation" annotation)))
    | SynAst.TypeExpr.Poly { body } ->
        make_core_type
          origin
          (Poly {
            parameters = poly_type_parameters type_expr;
            body = build_core_type context (require_some origin "missing polymorphic type body" body)
          })
    | SynAst.TypeExpr.Parenthesized { inner } ->
        make_core_type
          origin
          (Parenthesized (build_core_type
            context
            (require_some origin "missing parenthesized type" inner)))
    | SynAst.TypeExpr.Opaque node -> (
        match poly_variant_type_fields_from_node origin node with
        | _ :: _ as fields -> make_core_type origin (PolyVariant fields)
        | [] -> (
            match inherited_poly_variant_type_fields_from_node context origin node with
            | _ :: _ as fields -> make_core_type origin (PolyVariant fields)
            | [] -> (
                match SynAst.TypeExpr.inner_without_attribute_suffix type_expr with
                | Some inner -> build_core_type context inner
                | None -> unsupported_node node (node_summary node)
              )
          )
      )
    | SynAst.TypeExpr.Error node ->
        unsupported_node node (node_summary node)
    | SynAst.TypeExpr.Unknown node ->
        unsupported_node node (node_summary node): core_type)

and tuple_type_elements = fun context origin separator left right ->
  let rec flatten type_expr =
    match SynAst.TypeExpr.view type_expr with
    | SynAst.TypeExpr.Tuple { left; right; separator=child_separator } when type_tuple_separator_from_syn
      child_separator
    = separator -> tuple_type_elements context (origin_from_node type_expr) separator left right
    | _ -> [ build_core_type context type_expr ]
  in
  List.append
    (flatten (require_some origin "missing tuple type left" left))
    (flatten (require_some origin "missing tuple type right" right))

and child_type_exprs = fun context type_expr ->
  let children = ref [] in
  SynAst.TypeExpr.for_each_child_type
    type_expr
    ~fn:(fun child -> children := build_core_type context child :: !children);
  List.reverse !children

and build_core_type_from_source = fun context origin source ->
  let parse_result = Syn.parse_interface (source_slice ("val __typ : " ^ source ^ "\n")) in
  let source_file = SynAst.SourceFile.make parse_result.tree in
  match SynAst.SourceFile.view source_file with
  | Interface interface ->
      let annotation = ref None in
      SynAst.Interface.for_each_item interface
        ~fn:(fun item ->
          match !annotation, SynAst.SignatureItem.view item with
          | None, Value declaration -> annotation := SynAst.ValueDeclaration.type_annotation declaration
          | _ -> ());
      build_core_type
        context
        (require_some origin ("failed to parse package constraint type: " ^ source) !annotation)
  | Implementation _
  | Empty -> build_failed origin ("failed to parse package constraint type: " ^ source)

and build_core_type_from_tokens = fun context origin tokens ->
  build_core_type_from_source context origin (type_source_from_tokens tokens)

and package_constraint_manifest_tokens = fun tokens ->
  let rec loop depth acc tokens =
    match tokens with
    | [] -> (List.reverse acc, [])
    | token :: rest when Int.equal depth 0 && token_kind_is token Syn.SyntaxKind.WITH_KW -> (
      List.reverse acc,
      tokens
    )
    | token :: rest when token_kind_is token Syn.SyntaxKind.LPAREN -> loop
      (depth + 1)
      (token :: acc)
      rest
    | token :: rest when token_kind_is token Syn.SyntaxKind.RPAREN -> loop
      (Int.max 0 (depth - 1))
      (token :: acc)
      rest
    | token :: rest -> loop depth (token :: acc) rest
  in
  loop 0 [] tokens

and build_package_constraints = fun context origin tokens ->
  let rec loop constraints tokens =
    match tokens with
    | [] ->
        List.reverse constraints
    | token :: rest when token_kind_is token Syn.SyntaxKind.WITH_KW ->
        loop constraints rest
    | token :: rest when token_kind_is token Syn.SyntaxKind.TYPE_KW ->
        let name_tokens, after_name = split_at_token_kind Syn.SyntaxKind.EQ rest in
        let manifest_tokens, rest =
          match after_name with
          | Some tokens -> package_constraint_manifest_tokens tokens
          | None -> build_failed origin "missing package type constraint manifest"
        in
        let constraint_: package_type_constraint = {
          origin;
          type_name = path_from_ident_tokens_in_tokens name_tokens;
          manifest = build_core_type_from_tokens context origin manifest_tokens
        } in
        loop (constraint_ :: constraints) rest
    | _ :: rest ->
        loop constraints rest
  in
  loop [] tokens

and build_package_type_from_ascription_tokens = fun context origin ?binder tokens ->
  let module_type_tokens, constraint_tokens = split_at_token_kind Syn.SyntaxKind.WITH_KW tokens in
  let module_type = path_from_ident_tokens_in_tokens module_type_tokens in
  let constraints =
    match constraint_tokens with
    | Some tokens -> build_package_constraints context origin tokens
    | None -> []
  in
  ({ origin; binder; module_type; constraints }: package_type)

and first_class_module_path_from_tokens = fun origin tokens ->
  let after_module = tokens_after_token_kind Syn.SyntaxKind.MODULE_KW tokens |> require_some origin "missing first-class module keyword" in
  after_module |> tokens_until_any [ Syn.SyntaxKind.COLON; Syn.SyntaxKind.RPAREN ] |> path_from_ident_tokens_in_tokens

and first_class_package_type_from_tokens = fun context origin ?binder tokens ->
  match tokens_after_token_kind Syn.SyntaxKind.COLON tokens with
  | None -> None
  | Some tokens ->
      let tokens = drop_final_rparen tokens in
      Some (build_package_type_from_ascription_tokens context origin ?binder tokens)

and first_class_module_unpack_expression_from_tokens = fun origin tokens ->
  let after_val = tokens_after_token_kind Syn.SyntaxKind.VAL_KW tokens |> require_some origin "missing first-class module unpack keyword" in
  let expression_tokens = tokens_until_any [ Syn.SyntaxKind.COLON; Syn.SyntaxKind.RPAREN ] after_val in
  match ident_tokens expression_tokens with
  | [] -> build_failed origin "missing first-class module unpack expression"
  | identifiers -> make_expression origin (Path (path_from_ident_tokens identifiers))

and first_class_module_unpack_from_tokens = fun context origin tokens ->
  (
    {
      origin;
      expression = first_class_module_unpack_expression_from_tokens origin tokens;
      package_type = first_class_package_type_from_tokens context origin tokens
    }:
      module_unpack
  )

and module_body_unpack = fun context node ->
  let tokens = direct_child_tokens node in
  if List.exists (fun token -> token_kind_is token Syn.SyntaxKind.VAL_KW) tokens then
    Some (first_class_module_unpack_from_tokens context (origin_from_node node) tokens)
  else
    None

let rec build_parameter = fun context parameter ->
  let origin = origin_from_node parameter in
  (match SynAst.Parameter.view parameter with
    | SynAst.Parameter.Labeled { label; pattern } -> make_parameter
      origin
      (Labeled {
        label = token_text (require_some origin "missing labeled parameter label" label);
        pattern = Option.map pattern ~fn:(build_pattern context)
      })
    | SynAst.Parameter.Optional { label; pattern } -> make_parameter
      origin
      (Optional {
        label = token_text (require_some origin "missing optional parameter label" label);
        pattern = Option.map pattern ~fn:(build_pattern context)
      })
    | SynAst.Parameter.OptionalDefault { label; pattern; default } -> make_parameter
      origin
      (OptionalDefault {
        label = token_text (require_some origin "missing optional parameter label" label);
        pattern = Option.map pattern ~fn:(build_pattern context);
        default = build_expression
          context
          (require_some origin "missing optional parameter default" default)
      })
    | SynAst.Parameter.Unknown node -> unsupported_node node (node_summary node): parameter)

and build_locally_abstract_type_pattern = fun origin syntax_pattern ->
  let pattern = SynAst.LocallyAbstractTypePattern.cast syntax_pattern |> require_some origin "invalid locally abstract type pattern" in
  let names = ref [] in
  SynAst.LocallyAbstractTypePattern.for_each_type_name
    pattern
    ~fn:(fun token -> names := token_text token :: !names);
  make_pattern origin (LocallyAbstractType (List.reverse !names))

and build_first_class_module_pattern = fun context origin syntax_pattern ->
  let pattern = SynAst.FirstClassModulePattern.cast syntax_pattern |> require_some origin "invalid first-class module pattern" in
  let binder =
    match SynAst.FirstClassModulePattern.binder pattern with
    | Some token when not (token_kind_is token Syn.SyntaxKind.UNDERSCORE) -> Some (token_text token)
    | _ -> None
  in
  let tokens = direct_child_tokens syntax_pattern in
  make_pattern
    origin
    (FirstClassModule {
      binder;
      package_type = first_class_package_type_from_tokens context origin ?binder tokens
    })

and build_pattern = fun context syntax_pattern ->
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
            callee = build_pattern context (require_some origin "missing pattern callee" callee);
            argument = build_pattern
              context
              (require_some origin "missing pattern argument" argument)
          })
    | SynAst.Pattern.Literal { token } ->
        make_pattern
          origin
          (Literal (Option.map token ~fn:literal_from_token |> Option.unwrap_or ~default:Unknown))
    | SynAst.Pattern.PolyVariant ->
        make_pattern
          origin
          (PolyVariant {
            tag = poly_variant_tag_from_node origin syntax_pattern;
            payload = child_patterns syntax_pattern
            |> List.head
            |> Option.map ~fn:(build_pattern context)
          })
    | SynAst.Pattern.Tuple ->
        make_pattern
          origin
          (Tuple (child_patterns syntax_pattern |> List.map ~fn:(build_pattern context)))
    | SynAst.Pattern.List ->
        make_pattern
          origin
          (List (child_patterns syntax_pattern |> List.map ~fn:(build_pattern context)))
    | SynAst.Pattern.Record ->
        let record = SynAst.RecordPattern.cast syntax_pattern |> require_some origin "invalid record pattern" in
        make_pattern origin (Record (build_record_pattern_fields context record))
    | SynAst.Pattern.Or { left; right } ->
        make_pattern
          origin
          (Or {
            left = build_pattern context (require_some origin "missing left or-pattern" left);
            right = build_pattern context (require_some origin "missing right or-pattern" right)
          })
    | SynAst.Pattern.Cons { head; tail } ->
        make_pattern
          origin
          (Cons {
            head = build_pattern context (require_some origin "missing cons head" head);
            tail = build_pattern context (require_some origin "missing cons tail" tail)
          })
    | SynAst.Pattern.Parenthesized { inner=Some inner } ->
        make_pattern origin (Parenthesized (build_pattern context inner))
    | SynAst.Pattern.Parenthesized { inner=None } ->
        make_pattern origin (Literal Unit)
    | SynAst.Pattern.Constraint { pattern; annotation } ->
        make_pattern
          origin
          (Constraint {
            pattern = build_pattern
              context
              (require_some origin "missing constrained pattern" pattern);
            annotation = build_core_type
              context
              (require_some origin "missing pattern type annotation" annotation)
          })
    | SynAst.Pattern.Alias { pattern; alias } ->
        make_pattern
          origin
          (Alias {
            pattern = build_pattern context (require_some origin "missing aliased pattern" pattern);
            alias = build_pattern context (require_some origin "missing pattern alias" alias)
          })
    | SynAst.Pattern.Attribute { inner } ->
        make_pattern
          origin
          (Attribute (build_pattern context (require_some origin "missing attributed pattern" inner)))
    | SynAst.Pattern.LabeledParam parameter ->
        make_pattern origin (LabeledParameter (build_parameter context parameter))
    | SynAst.Pattern.OptionalParam parameter ->
        make_pattern origin (OptionalParameter (build_parameter context parameter))
    | SynAst.Pattern.OptionalParamDefault parameter ->
        make_pattern origin (OptionalParameterDefault (build_parameter context parameter))
    | SynAst.Pattern.LocallyAbstractType ->
        build_locally_abstract_type_pattern origin syntax_pattern
    | SynAst.Pattern.FirstClassModule ->
        build_first_class_module_pattern context origin syntax_pattern
    | SynAst.Pattern.Error node ->
        unsupported_node node (node_summary node)
    | SynAst.Pattern.Unknown node ->
        unsupported_node node (node_summary node)
    | SynAst.Pattern.Array
    | SynAst.Pattern.Extension
    | SynAst.Pattern.LocalOpen
    | SynAst.Pattern.Interval _
    | SynAst.Pattern.Lazy _
    | SynAst.Pattern.Exception _ ->
        build_failed origin (Syn.SyntaxKind.to_string origin.kind): pattern)

and build_record_pattern_field = fun context (field: SynAst.RecordPattern.field) ->
  let origin = origin_from_node field.node in
  (
    {
      origin;
      name = path_from_syn_path (require_some origin "missing record pattern field name" field.path);
      pattern = Option.map field.pattern ~fn:(build_pattern context)
    }:
      record_pattern_field
  )

and build_record_pattern_fields = fun context record ->
  let fields = ref [] in
  SynAst.RecordPattern.for_each_field
    record
    ~fn:(fun field -> fields := build_record_pattern_field context field :: !fields);
  List.reverse !fields

and flatten_pattern_application = fun pattern ->
  let rec loop acc (pattern: pattern) =
    match pattern.kind with
    | Apply { callee; argument } -> loop (argument :: acc) callee
    | _ -> pattern :: acc
  in
  loop [] pattern

and is_special_function_parameter = fun (pattern: pattern) ->
  match pattern.kind with
  | LocallyAbstractType _
  | FirstClassModule _ -> true
  | _ -> false

and split_special_function_parameter_group = fun pattern ->
  let parameters = flatten_pattern_application pattern in
  if List.exists is_special_function_parameter parameters then
    Some parameters
  else
    None

and normalize_let_binding_parameters = fun (parameters: pattern list) (
  type_annotation: core_type option
) ->
  let push_parameters acc parameters =
    List.fold_left parameters ~init:acc ~fn:(fun acc parameter -> parameter :: acc)
  in
  let rec loop acc type_annotation parameters =
    match parameters with
    | [] ->
        (List.reverse acc, type_annotation)
    | (({ kind=Constraint { pattern=inner; annotation }; _ }: pattern) as parameter) :: rest -> (
        match split_special_function_parameter_group inner with
        | Some parameters ->
            let type_annotation =
              match type_annotation with
              | Some _ -> type_annotation
              | None -> Some annotation
            in
            loop (push_parameters acc parameters) type_annotation rest
        | None ->
            if List.is_empty rest && Option.is_none type_annotation then
              loop (inner :: acc) (Some annotation) rest
            else
              loop (parameter :: acc) type_annotation rest
      )
    | parameter :: rest -> (
        match split_special_function_parameter_group parameter with
        | Some parameters -> loop (push_parameters acc parameters) type_annotation rest
        | None -> loop (parameter :: acc) type_annotation rest
      )
  in
  loop [] type_annotation parameters

and build_let_binding = fun context binding ->
  let origin = origin_from_node binding in
  let parameters = ref [] in
  SynAst.LetBinding.for_each_parameter
    binding
    ~fn:(fun parameter -> parameters := build_pattern context parameter :: !parameters);
  let type_annotation = Option.map
    (SynAst.LetBinding.type_annotation binding)
    ~fn:(build_core_type context) in
  let parameters, type_annotation = normalize_let_binding_parameters (List.reverse !parameters) type_annotation in
  ({
      origin;
      pattern = build_pattern
        context
        (require_some origin "missing let binding pattern" (SynAst.LetBinding.pattern binding));
      parameters;
      body = build_expression
        context
        (require_some origin "missing let binding body" (SynAst.LetBinding.body binding));
      type_annotation;
    }: let_binding)

and build_match_case = fun context match_case ->
  let origin = origin_from_node match_case in
  let view = SynAst.MatchCase.view match_case in
  (
    {
      origin;
      pattern = build_pattern context (require_some origin "missing match case pattern" view.pattern);
      guard = Option.map view.guard ~fn:(build_expression context);
      body = build_expression context (require_some origin "missing match case body" view.body)
    }:
      match_case
  )

and build_match_cases = fun context syntax_expression ->
  let cases = ref [] in
  SynAst.Expr.for_each_match_case
    syntax_expression
    ~fn:(fun match_case -> cases := build_match_case context match_case :: !cases);
  List.reverse !cases

and build_record_expression_field = fun context (field: SynAst.RecordExpr.field) ->
  let origin = origin_from_node field.node in
  (
    {
      origin;
      name = path_from_syn_path (require_some origin "missing record field name" field.path);
      value = build_expression context (require_some origin "missing record field value" field.value)
    }:
      record_expression_field
  )

and build_record_expression_fields = fun context record ->
  let fields = ref [] in
  SynAst.RecordExpr.for_each_field
    record
    ~fn:(fun field -> fields := build_record_expression_field context field :: !fields);
  List.reverse !fields

and build_expression = fun context syntax_expression ->
  let origin = origin_from_node syntax_expression in
  (match SynAst.Expr.view syntax_expression with
    | SynAst.Expr.Literal { token } ->
        make_expression
          origin
          (Literal (Option.map token ~fn:literal_from_token |> Option.unwrap_or ~default:Unknown))
    | SynAst.Expr.Path { path } ->
        make_expression origin (Path (path_from_syn_path path))
    | SynAst.Expr.Parenthesized { inner=Some inner } ->
        build_expression context inner
    | SynAst.Expr.Parenthesized { inner=None } ->
        make_expression origin (Literal Unit)
    | SynAst.Expr.Attribute { inner=Some inner } ->
        build_expression context inner
    | SynAst.Expr.Attribute { inner=None } ->
        build_failed origin "missing attributed expression"
    | SynAst.Expr.Typed { expr=Some expr; annotation=Some annotation } ->
        let kind =
          if type_expr_is_coercion annotation then
            Coercion
          else
            Annotation
        in
        with_expression_type_hint
          kind
          (build_core_type context annotation)
          (build_expression context expr)
    | SynAst.Expr.Typed { expr=Some expr; annotation=None } ->
        build_expression context expr
    | SynAst.Expr.Typed { expr=None; _ } ->
        build_failed origin "missing typed expression"
    | SynAst.Expr.Tuple ->
        make_expression
          origin
          (Tuple (child_exprs syntax_expression |> List.map ~fn:(build_expression context)))
    | SynAst.Expr.List ->
        make_expression
          origin
          (List (child_exprs syntax_expression |> List.map ~fn:(build_expression context)))
    | SynAst.Expr.PolyVariant { payload } ->
        make_expression
          origin
          (PolyVariant {
            tag = poly_variant_tag_from_node origin syntax_expression;
            payload = Option.map payload ~fn:(build_expression context)
          })
    | SynAst.Expr.Record ->
        let record = SynAst.RecordExpr.cast syntax_expression |> require_some origin "invalid record expression" in
        (
          match SynAst.RecordExpr.base record with
          | Some base -> make_expression
            origin
            (RecordUpdate {
              base = build_expression context base;
              fields = build_record_expression_fields context record
            })
          | None -> make_expression origin (Record (build_record_expression_fields context record))
        )
    | SynAst.Expr.RecordUpdate ->
        let record = SynAst.RecordExpr.cast syntax_expression |> require_some origin "invalid record update" in
        make_expression
          origin
          (RecordUpdate {
            base = build_expression
              context
              (require_some origin "missing record update base" (SynAst.RecordExpr.base record));
            fields = build_record_expression_fields context record
          })
    | SynAst.Expr.FieldAccess { target=Some target; field=Some field } ->
        make_expression
          origin
          (FieldAccess {
            receiver = build_expression context target;
            field = SurfacePath.from_name (token_text field)
          })
    | SynAst.Expr.FieldAccess _ ->
        build_failed origin "incomplete field access"
    | SynAst.Expr.Assign { target=Some target; value=Some value; _ } ->
        make_expression
          origin
          (Assign {
            target = build_expression context target;
            value = build_expression context value
          })
    | SynAst.Expr.Assign _ ->
        build_failed origin "incomplete assignment"
    | SynAst.Expr.Sequence { left=Some left; right=Some right } ->
        make_expression
          origin
          (Sequence { left = build_expression context left; right = build_expression context right })
    | SynAst.Expr.Sequence _ ->
        build_failed origin "incomplete sequence expression"
    | SynAst.Expr.If { condition=Some condition; then_branch=Some then_branch; else_branch } ->
        make_expression
          origin
          (If {
            condition = build_expression context condition;
            then_branch = build_expression context then_branch;
            else_branch = Option.map else_branch ~fn:(build_expression context)
          })
    | SynAst.Expr.If _ ->
        build_failed origin "incomplete if expression"
    | SynAst.Expr.Match { scrutinee=Some scrutinee; first_case=Some _ } ->
        make_expression
          origin
          (Match {
            scrutinee = build_expression context scrutinee;
            cases = build_match_cases context syntax_expression
          })
    | SynAst.Expr.Match _ ->
        build_failed origin "incomplete match expression"
    | SynAst.Expr.Fun { body=Some body } ->
        make_expression
          origin
          (Function {
            parameters = direct_child_patterns syntax_expression
            |> List.map ~fn:(build_pattern context);
            body = Body (build_expression context body)
          })
    | SynAst.Expr.Fun { body=None } ->
        build_failed origin "missing function body"
    | SynAst.Expr.Function { first_case=Some _ } ->
        make_expression
          origin
          (Function { parameters = []; body = Cases (build_match_cases context syntax_expression) })
    | SynAst.Expr.Function { first_case=None } ->
        build_failed origin "missing function cases"
    | SynAst.Expr.Apply { callee=Some callee; argument } ->
        let arguments = [
          build_argument context (require_some origin "missing application argument" argument)
        ] in
        make_expression origin (Apply { callee = build_expression context callee; arguments })
    | SynAst.Expr.Apply { callee=None; _ } ->
        build_failed origin "missing application callee"
    | SynAst.Expr.Infix { left=Some left; operator=Some operator; right=Some right } ->
        make_expression
          origin
          (Infix {
            left = build_expression context left;
            operator = SurfacePath.from_name (token_text operator);
            right = build_expression context right
          })
    | SynAst.Expr.Infix _ ->
        build_failed origin "incomplete infix expression"
    | SynAst.Expr.Prefix { operator=Some operator; operand=Some operand } ->
        let callee = make_expression origin (Path (SurfacePath.from_name (token_text operator))) in
        let argument = build_expression context operand in
        make_expression
          origin
          (Apply { callee; arguments = [ make_argument argument.origin (Positional argument) ] })
    | SynAst.Expr.Prefix _ ->
        build_failed origin "incomplete prefix expression"
    | SynAst.Expr.Let { first_binding=Some first_binding; body=Some body } ->
        make_expression
          origin
          (Let {
            first_binding = build_let_binding context first_binding;
            body = build_expression context body
          })
    | SynAst.Expr.Let _ ->
        build_failed origin "incomplete let expression"
    | SynAst.Expr.LetModule _ ->
        build_let_module_expression context origin syntax_expression
    | SynAst.Expr.LocalOpen _ ->
        build_local_open_expression context origin syntax_expression
    | SynAst.Expr.FirstClassModule ->
        build_first_class_module_expression context origin syntax_expression
    | SynAst.Expr.Assert { argument=Some argument } ->
        make_expression origin (Assert (build_expression context argument))
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
    | SynAst.Expr.LetException _
    | SynAst.Expr.BindingOperator _
    | SynAst.Expr.Unreachable
    | SynAst.Expr.Object
    | SynAst.Expr.New
    | SynAst.Expr.Try _
    | SynAst.Expr.While _
    | SynAst.Expr.For _
    | SynAst.Expr.Lazy _
    | SynAst.Expr.MethodCall _
    | SynAst.Expr.ArrayIndex _
    | SynAst.Expr.StringIndex _ ->
        build_failed origin (Syn.SyntaxKind.to_string origin.kind): expression)

and build_first_class_module_expression = fun context origin syntax_expression ->
  let tokens = direct_child_tokens syntax_expression in
  make_expression
    origin
    (FirstClassModule {
      module_path = first_class_module_path_from_tokens origin tokens;
      package_type = first_class_package_type_from_tokens context origin tokens
    })

and build_argument = fun context syntax_expression ->
  let origin = origin_from_node syntax_expression in
  (match SynAst.Expr.view syntax_expression with
    | SynAst.Expr.LabeledArg { label; value } -> make_argument
      origin
      (Labeled {
        label = token_text (require_some origin "missing labeled argument label" label);
        value = Option.map value ~fn:(build_expression context)
      })
    | SynAst.Expr.OptionalArg { label; value } -> make_argument
      origin
      (Optional {
        label = token_text (require_some origin "missing optional argument label" label);
        value = Option.map value ~fn:(build_expression context)
      })
    | _ -> make_argument origin (Positional (build_expression context syntax_expression)): argument)

and build_let_module_expression = fun context origin syntax_expression ->
  let let_module = SynAst.LetModuleExpr.cast syntax_expression |> require_some origin "invalid let module expression" in
  let body = SynAst.LetModuleExpr.body let_module |> require_some origin "missing let module body" in
  let name = SynAst.LetModuleExpr.name let_module
  |> require_some origin "missing let module name"
  |> token_text in
  let module_body_node = SynAst.LetModuleExpr.module_body_node let_module in
  let alias =
    match module_body_node with
    | Some node when Syn.SyntaxKind.(SynAst.Node.kind node = PATH_MODULE_EXPR) -> Some (path_from_ident_tokens_in_node
      node)
    | _ -> None
  in
  let unpack =
    match module_body_node with
    | Some node -> module_body_unpack context node
    | None -> None
  in
  let items =
    match module_body_node with
    | Some node when Syn.SyntaxKind.(SynAst.Node.kind node = STRUCT_MODULE_EXPR) -> build_structure_items_from_module_expr
      context
      node
    | Some node when Syn.SyntaxKind.(SynAst.Node.kind node = PATH_MODULE_EXPR) -> []
    | Some node when Option.is_some (module_body_unpack context node) -> []
    | _ -> build_failed origin "unsupported let module body"
  in
  make_expression origin
    (
      LetModule {
        name;
        items;
        alias;
        unpack;
        body = build_expression context body;
      }
    )

and build_local_open_expression = fun context origin syntax_expression ->
  let local_open = SynAst.LocalOpenExpr.cast syntax_expression |> require_some origin "invalid local open expression" in
  let module_path, body =
    match SynAst.LocalOpenExpr.view local_open with
    | LetOpen { module_path; body; _ }
    | Delimited { module_path; body; _ } -> (module_path, body)
  in
  make_expression
    origin
    (LocalOpen {
      module_path = path_from_syn_path
        (require_some origin "missing local open module path" module_path);
      body = build_expression context (require_some origin "missing local open body" body)
    })

and build_let_declaration = fun context declaration ->
  let bindings = ref [] in
  SynAst.LetDeclaration.for_each_binding
    declaration
    ~fn:(fun binding -> bindings := build_let_binding context binding :: !bindings);
  (
    {
      origin = origin_from_node declaration;
      recursive = Option.is_some (SynAst.LetDeclaration.rec_token declaration);
      bindings = List.reverse !bindings
    }:
      let_declaration
  )

and name_from_declaration_tokens = fun for_each_token fallback ->
  let tokens = ref [] in
  for_each_token ~fn:(fun token -> tokens := token :: !tokens);
  match List.reverse !tokens with
  | [] -> Option.map fallback ~fn:token_text
  | tokens -> Some (path_from_tokens tokens |> SurfacePath.to_string)

and build_value_declaration = fun context declaration ->
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
      |> build_core_type context
    }:
      value_declaration
  )

and build_external_declaration = fun context declaration ->
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
      |> build_core_type context
    }:
      external_declaration
  )

and build_type_parameter = function
  | SynAst.TypeDeclaration.Named { name; _ } -> Some (token_text name)
  | SynAst.TypeDeclaration.Wildcard _ -> None

and build_type_constructor = fun context constructor ->
  let origin = origin_from_node constructor in
  ({
      origin;
      name = SynAst.VariantConstructor.name constructor
      |> require_some origin "missing variant constructor name"
      |> token_text;
      payload = Option.map
        (SynAst.VariantConstructor.payload_type constructor)
        ~fn:(build_core_type context);
      result = Option.map
        (SynAst.VariantConstructor.result_type constructor)
        ~fn:(build_core_type context);
      inline_record = Option.map
        (SynAst.VariantConstructor.record_payload constructor)
        ~fn:(build_record_field_declarations context);
    }: type_constructor)

and build_record_field_declaration = fun context field ->
  let origin = origin_from_node field in
  (
    {
      origin;
      name = SynAst.RecordField.name field |> require_some origin "missing record field name" |> token_text;
      mutable_ = Option.is_some (SynAst.RecordField.mutable_token field);
      type_annotation = SynAst.RecordField.type_annotation field
      |> require_some origin "missing record field type annotation"
      |> build_core_type context
    }:
      record_field_declaration
  )

and build_record_field_declarations = fun context record ->
  let fields = ref [] in
  SynAst.RecordType.for_each_field
    record
    ~fn:(fun field -> fields := build_record_field_declaration context field :: !fields);
  List.reverse !fields

and build_type_declaration_member = fun context member ->
  let origin = origin_from_node (SynAst.TypeDeclaration.Member.declaration member) in
  let parameters = ref [] in
  SynAst.TypeDeclaration.Member.for_each_parameter
    member
    ~fn:(fun parameter -> parameters := build_type_parameter parameter :: !parameters);
  let definition: type_definition =
    match SynAst.TypeDeclaration.Member.manifest member, SynAst.TypeDeclaration.Member.variant_type member, SynAst.TypeDeclaration.Member.record_type
      member with
    | Some manifest, _, _ ->
        make_type_definition origin (Alias (build_core_type context manifest))
    | None, Some variant, _ ->
        let constructors = ref [] in
        SynAst.VariantType.for_each_constructor
          variant
          ~fn:(fun constructor -> constructors := build_type_constructor context constructor :: !constructors);
        make_type_definition origin (Variant (List.reverse !constructors))
    | None, None, Some record ->
        make_type_definition origin (Record (build_record_field_declarations context record))
    | None, None, None ->
        make_type_definition origin Abstract
  in
  let name = SynAst.TypeDeclaration.Member.name member
  |> require_some origin "missing type declaration name"
  |> token_text in
  (
    match definition.kind with
    | Alias { kind=PolyVariant fields; _ } -> context.poly_variant_type_aliases <- (name, fields)
    :: context.poly_variant_type_aliases
    | _ -> ()
  );
  ({ origin; name; parameters = List.reverse !parameters; definition }: type_declaration)

and build_type_declarations = fun context declaration ->
  let declarations = ref [] in
  SynAst.TypeDeclaration.for_each_member
    declaration
    ~fn:(fun member -> declarations := build_type_declaration_member context member :: !declarations);
  List.reverse !declarations

and build_structure_items_from_module_expr = fun context node ->
  let items = ref [] in
  SynAst.Node.for_each_child_node node
    ~fn:(fun child ->
      match SynAst.Node.kind child with
      | Syn.SyntaxKind.STRUCTURE_ITEM -> items := build_structure_item context child :: !items
      | _ -> ());
  List.reverse !items

and build_module_declaration_member = fun context member ->
  let declaration = SynAst.ModuleDeclaration.Member.declaration member in
  let origin = origin_from_node declaration in
  let module_expr = SynAst.ModuleDeclaration.Member.module_expr member in
  let parameters = functor_parameters_from_member origin member in
  let module_type =
    match SynAst.ModuleDeclaration.Member.module_type member with
    | Some node -> path_from_module_type_node node
    | None -> None
  in
  let alias =
    match module_expr with
    | Some node when Syn.SyntaxKind.(SynAst.Node.kind node = PATH_MODULE_EXPR) -> Some (path_from_ident_tokens_in_node
      node)
    | _ -> None
  in
  let application =
    match module_expr with
    | Some node -> module_application_from_module_expr node
    | None -> None
  in
  let items =
    match module_expr with
    | Some node when Syn.SyntaxKind.(SynAst.Node.kind node = STRUCT_MODULE_EXPR) -> build_structure_items_from_module_expr
      context
      node
    | _ ->
        let items = ref [] in
        SynAst.ModuleDeclaration.for_each_structure_item
          declaration
          ~fn:(fun item -> items := build_structure_item context item :: !items);
        List.reverse !items
  in
  ({
      origin;
      name = SynAst.ModuleDeclaration.Member.name member
      |> require_some origin "missing module declaration name"
      |> token_text;
      parameters;
      items;
      alias;
      module_type;
      application;
    }: module_declaration)

and build_module_declarations = fun context declaration ->
  let declarations = ref [] in
  SynAst.ModuleDeclaration.for_each_member
    declaration
    ~fn:(fun member -> declarations := build_module_declaration_member context member :: !declarations);
  List.reverse !declarations

and build_module_type_declaration = fun context declaration ->
  let origin = origin_from_node declaration in
  let items = ref [] in
  SynAst.ModuleTypeDeclaration.for_each_signature_item
    declaration
    ~fn:(fun item -> items := build_signature_item context item :: !items);
  (
    {
      origin;
      name = SynAst.ModuleTypeDeclaration.name declaration
      |> require_some origin "missing module type declaration name"
      |> token_text;
      items = List.reverse !items
    }:
      module_type_declaration
  )

and build_structure_item = fun context item ->
  let origin = origin_from_node item in
  (match SynAst.StructureItem.view item with
    | Let declaration ->
        make_structure_item origin (Let (build_let_declaration context declaration))
    | Expr expr_item -> (
        match SynAst.ExprItem.expr expr_item with
        | Some expression -> make_structure_item
          origin
          (Expression (build_expression context expression))
        | None -> build_failed origin "missing structure expression"
      )
    | External declaration ->
        make_structure_item origin (External (build_external_declaration context declaration))
    | Type declaration ->
        make_structure_item origin (Type (build_type_declarations context declaration))
    | Module declaration ->
        make_structure_item origin (Module (build_module_declarations context declaration))
    | ModuleType declaration ->
        make_structure_item origin (ModuleType (build_module_type_declaration context declaration))
    | Include declaration ->
        let tokens = ref [] in
        SynAst.IncludeDeclaration.for_each_path_ident
          declaration
          ~fn:(fun token -> tokens := token :: !tokens);
        make_structure_item origin (Include (path_from_ident_tokens (List.reverse !tokens)))
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
    | Open _ ->
        build_failed origin (Syn.SyntaxKind.to_string origin.kind)
    | Error node ->
        unsupported_node node (node_summary node)
    | Unknown node ->
        unsupported_node node (node_summary node): structure_item)

and build_signature_item = fun context item ->
  let origin = origin_from_node item in
  (match SynAst.SignatureItem.view item with
    | Value declaration -> make_signature_item
      origin
      (Value (build_value_declaration context declaration))
    | External declaration -> make_signature_item
      origin
      (External (build_external_declaration context declaration))
    | Type declaration -> make_signature_item
      origin
      (Type (build_type_declarations context declaration))
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
    let context = make_build_context () in
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
            ~fn:(fun item -> items := build_structure_item context item :: !items);
          make_source_file (origin_from_node source_file) (Implementation (List.reverse !items))
      | Interface interface ->
          let items = ref [] in
          SynAst.Interface.for_each_item
            interface
            ~fn:(fun item -> items := build_signature_item context item :: !items);
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

open Std
open Std.Collections

module SurfacePath = Model.Surface_path

type origin = {
  span: Syn.Span.t;
  kind: Syn.SyntaxKind.t;
}

type ident = SurfacePath.t

module TypeVar = struct
  type t = int

  let first = 0

  let next t = t + 1

  let equal = Int.equal

  let compare = Int.compare

  let to_string x = "'" ^ Int.(to_string x)
end

module Type = struct
  module Label = struct
    type t =
      | NoLabel
      | Labelled of string
      | Optional of string

    let equal a b =
      match (a, b) with
      | (NoLabel, NoLabel) -> true
      | (Labelled a, Labelled b) -> String.equal a b
      | (Optional a, Optional b) -> String.equal a b
      | _ -> false

    let to_string = fun __tmp1 ->
      match __tmp1 with
      | NoLabel -> ""
      | Labelled label -> label ^ ":"
      | Optional label -> "?" ^ label ^ ":"
  end

  type variable = {
    id: TypeVar.t;
    mutable link: t option;
  }

  and arrow = {
    label: Label.t;
    parameter: t;
    result: t;
  }

  and application = {
    ident: ident;
    arguments: t list;
  }

  and t =
    | Var of variable
    | Generic of TypeVar.t
    | Tuple of t list
    | Arrow of arrow
    | Apply of application

  let type_var_display_name index =
    match List.get
      [
        "'a";
        "'b";
        "'c";
        "'d";
        "'e";
        "'f";
        "'g";
        "'h";
        "'i";
        "'j";
        "'k";
        "'l";
        "'m";
        "'n";
        "'o";
        "'p";
        "'q";
        "'r";
        "'s";
        "'t";
        "'u";
        "'v";
        "'w";
        "'x";
        "'y";
        "'z";
      ]
      ~at:index with
    | Some name -> name
    | None -> "'a" ^ Int.to_string index

  module Printer = struct
    type printer = {
      names: (TypeVar.t, string) HashMap.t;
      mutable next_name: int;
    }

    let create () = { names = HashMap.with_capacity ~size:4; next_name = 0 }

    let type_var_to_string state id =
      match HashMap.get state.names ~key:id with
      | Some name -> name
      | None ->
          let name = type_var_display_name state.next_name in
          state.next_name <- state.next_name + 1;
          let _ = HashMap.insert state.names ~key:id ~value:name in
          name

    let to_string state type_ =
      let rec loop type_ =
        match type_ with
        | Var { id; link = None } -> type_var_to_string state id
        | Var { link = Some linked; _ } -> loop linked
        | Generic id -> type_var_to_string state id
        | Tuple elements ->
            elements
            |> List.map ~fn:tuple_element_to_string
            |> String.concat " * "
        | Arrow { label; parameter; result } ->
            Label.to_string label ^ arrow_parameter_to_string parameter ^ " -> " ^ loop result
        | Apply { ident; arguments = [] } -> SurfacePath.to_string ident
        | Apply {
            ident;
            arguments = [ argument ];
          } ->
            constructor_argument_to_string argument ^ " " ^ SurfacePath.to_string ident
        | Apply { ident; arguments } ->
            "("
            ^ (
              arguments
              |> List.map ~fn:loop
              |> String.concat ", "
            )
            ^ ") "
            ^ SurfacePath.to_string ident
      and arrow_parameter_to_string type_ =
        match type_ with
        | Arrow _ -> "(" ^ loop type_ ^ ")"
        | _ -> loop type_
      and constructor_argument_to_string type_ =
        match type_ with
        | Arrow _
        | Tuple _ -> "(" ^ loop type_ ^ ")"
        | _ -> loop type_
      and tuple_element_to_string type_ =
        match type_ with
        | Arrow _
        | Tuple _ -> "(" ^ loop type_ ^ ")"
        | _ -> loop type_
      in
      loop type_
  end

  let to_string type_ = Printer.to_string (Printer.create ()) type_

  let same_var (a: variable) (b: variable) = TypeVar.equal a.id b.id

  let rec equal left right =
    match (left, right) with
    | (Var { link = Some left; _ }, right) -> equal left right
    | (left, Var { link = Some right; _ }) -> equal left right
    | (Var left, Var right) -> same_var left right
    | (Generic left, Generic right) -> TypeVar.equal left right
    | (Tuple left, Tuple right) -> equal_many left right
    | (Arrow left, Arrow right) -> equal_arrow left right
    | (Apply left, Apply right) -> equal_application left right
    | _ -> false

  and equal_many left right =
    if not (Int.equal (List.length left) (List.length right)) then
      false
    else
      List.zip left right
      |> List.all ~fn:(fun (left, right) -> equal left right)

  and equal_arrow left right =
    Label.equal left.label right.label
    && equal left.parameter right.parameter
    && equal left.result right.result

  and equal_application left right =
    SurfacePath.equal left.ident right.ident && equal_many left.arguments right.arguments

  let arrow ?(label = Label.NoLabel) parameter result = Arrow { label; parameter; result }
end

type literal =
  | Int
  | Float
  | Char
  | String
  | Bool

type core_type = {
  origin: origin;
  mutable type_: Type.t option;
  kind: core_type_kind;
}

and arrow_label =
  | NoLabel
  | Labelled of string
  | Optional of string

and core_type_kind =
  | Wildcard
  | Var of string option
  | TypeIdent of ident
  | Apply of type_application
  | Arrow of arrow_type
  | Tuple of core_type list
  | ForAll of forall_type
  | PolyVariant of poly_variant_type_field list
  | Package of package_type
  | Parenthesized of core_type

and type_application = {
  constructor: core_type;
  arguments: core_type list;
}

and arrow_type = {
  label: arrow_label;
  parameter: core_type;
  result: core_type;
}

and forall_type = {
  parameters: string list;
  body: core_type;
}

and poly_variant_type_field = {
  origin: origin;
  tag: string;
  payload: core_type option;
}

and package_type = {
  origin: origin;
  binder: ident option;
  module_type: ident;
  constraints: package_type_constraint list;
}

and package_type_constraint = {
  origin: origin;
  type_name: ident;
  manifest: core_type;
}

type type_parameter = string option

type constructor_arguments =
  | Tuple of core_type list
  | Record of record_field_declaration list

and type_constructor = {
  origin: origin;
  name: ident;
  arguments: constructor_arguments;
  result: core_type option;
}

and record_field_declaration = {
  origin: origin;
  name: ident;
  mutable_: bool;
  type_annotation: core_type;
}

type type_definition = {
  origin: origin;
  kind: type_definition_kind;
}

and type_definition_kind =
  | Abstract
  | Extensible
  | Alias of core_type
  | Variant of type_constructor list
  | Record of record_field_declaration list

type type_declaration = {
  origin: origin;
  name: ident;
  parameters: type_parameter list;
  definition: type_definition;
}

type parameter = {
  origin: origin;
  label: parameter_label;
  pattern: pattern;
  annotation: core_type option;
  default: expression option;
}

and parameter_label =
  | Unlabeled
  | Labeled of ident
  | Optional of ident

and pattern = {
  origin: origin;
  mutable type_: Type.t option;
  kind: pattern_kind;
}

and record_pattern_field = {
  origin: origin;
  name: ident;
  pattern: pattern option;
}

and pattern_kind =
  | Wildcard
  | Bind of ident
  | Constructor of constructor_pattern
  | Literal of literal
  | PolyVariant of poly_variant_pattern
  | Tuple of pattern list
  | List of pattern list
  | Record of record_pattern_field list
  | Or of or_pattern
  | Cons of cons_pattern
  | Constraint of constrained_pattern
  | Alias of alias_pattern
  | Attribute of pattern
  | FirstClassModule of first_class_module_pattern

and constructor_pattern = {
  ident: ident;
  payload: pattern option;
}

and or_pattern = {
  left: pattern;
  right: pattern;
}

and cons_pattern = {
  head: pattern;
  tail: pattern;
}

and constrained_pattern = {
  pattern: pattern;
  annotation: core_type;
}

and alias_pattern = {
  pattern: pattern;
  alias: pattern;
}

and first_class_module_pattern = {
  binder: ident option;
  package_type: package_type option;
}

and poly_variant_pattern = {
  tag: string;
  payload: pattern option;
}

and let_binding = {
  origin: origin;
  pattern: pattern;
  type_hint: core_type option;
  expr: expression;
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
  mutable type_: Type.t option;
  type_hint: expression_type_hint option;
  kind: expression_kind;
}

and module_unpack = {
  origin: origin;
  expression: expression;
  package_type: package_type option;
}

and record_expression = {
  update: expression option;
  fields: record_expression_field list;
}

and fun_decl = {
  type_binders: string list;
  parameters: parameter list;
  body: function_body;
}

and expression_kind =
  | Literal of literal
  | Ident of ident
  | Constructor of constructor_expression
  | Tuple of expression list
  | List of expression list
  | Array of expression list
  | PolyVariant of poly_variant_expression
  | Record of record_expression
  | FieldAccess of field_access
  | Assign of assignment
  | Sequence of sequence
  | If of conditional
  | Match of match_expression
  | Try of try_expression
  | While of while_loop
  | For of for_loop
  | Function of fun_decl
  | Apply of application
  | Infix of infix_operation
  | Let of let_expression
  | LetModule of let_module
  | LocalOpen of local_open
  | FirstClassModule of first_class_module
  | Assert of expression

and constructor_expression = {
  ident: ident;
  payload: expression option;
}

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
  name: ident;
  value: expression;
}

and field_access = {
  receiver: expression;
  field: ident;
}

and assignment = {
  target: expression;
  value: expression;
}

and sequence = {
  left: expression;
  right: expression;
}

and conditional = {
  condition: expression;
  then_branch: expression;
  else_branch: expression option;
}

and match_expression = {
  scrutinee: expression;
  cases: match_case list;
}

and try_expression = {
  body: expression;
  cases: match_case list;
}

and while_loop = {
  condition: expression;
  body: expression;
}

and for_loop = {
  pattern: pattern;
  start_: expression;
  stop: expression;
  body: expression;
}

and application = {
  callee: expression;
  arguments: argument list;
}

and infix_operation = {
  left: expression;
  operator: ident;
  right: expression;
}

and let_expression = {
  recursive: bool;
  bindings: let_binding list;
  body: expression;
}

and let_module = {
  name: ident;
  items: structure_item list;
  alias: ident option;
  unpack: module_unpack option;
  body: expression;
}

and local_open = {
  module_: ident;
  body: expression;
}

and first_class_module = {
  module_: ident;
  package_type: package_type option;
}

and argument = {
  origin: origin;
  kind: argument_kind;
}

and argument_kind =
  | Positional of expression
  | Labeled of labeled_argument
  | Optional of labeled_argument

and labeled_argument = {
  label: string;
  value: expression option;
}

and let_declaration = {
  origin: origin;
  recursive: bool;
  bindings: let_binding list;
}

and value_declaration = {
  origin: origin;
  name: ident;
  type_annotation: core_type;
}

and external_declaration = {
  origin: origin;
  name: ident;
  type_annotation: core_type;
  primitives: string list;
}

and type_extension_declaration = {
  origin: origin;
  name: ident;
  constructors: type_constructor list;
}

and exception_declaration = {
  origin: origin;
  name: ident;
  payload: core_type option;
}

and module_declaration = {
  origin: origin;
  name: ident;
  recursive: bool;
  parameters: functor_parameter list;
  items: structure_item list;
  alias: ident option;
  module_type: ident option;
  application: module_application option;
}

and functor_parameter = {
  origin: origin;
  name: ident;
  module_type: ident option;
}

and module_application = {
  callee: ident;
  argument: ident;
}

and module_type_declaration = {
  origin: origin;
  name: ident;
  items: signature_item list;
}

and structure_item = {
  origin: origin;
  kind: structure_item_kind;
}

and structure_item_kind =
  | Let of let_declaration
  | Type of type_declaration list
  | TypeExtension of type_extension_declaration
  | Expression of expression
  | External of external_declaration
  | Exception of exception_declaration
  | Module of module_declaration list
  | ModuleType of module_type_declaration
  | Include of ident

and signature_item = {
  origin: origin;
  kind: signature_item_kind;
}

and signature_item_kind =
  | Value of value_declaration
  | Type of type_declaration list
  | TypeExtension of type_extension_declaration
  | External of external_declaration
  | Exception of exception_declaration

type implementation = {
  origin: origin;
  items: structure_item list;
}

type interface = {
  origin: origin;
  items: signature_item list;
}

type t =
  | Implementation of implementation
  | Interface of interface

let core_type_origin = fun (type_: core_type) -> type_.origin

let core_type_type = fun (type_: core_type) -> type_.type_

let parameter_origin = fun (parameter: parameter) -> parameter.origin

let pattern_origin = fun (pattern: pattern) -> pattern.origin

let pattern_type = fun (pattern: pattern) -> pattern.type_

let expression_origin = fun (expression: expression) -> expression.origin

let expression_type = fun (expression: expression) -> expression.type_

let match_case_origin = fun (match_case: match_case) -> match_case.origin

let structure_item_origin = fun (item: structure_item) -> item.origin

let signature_item_origin = fun (item: signature_item) -> item.origin

let span_from_node = Syn.Ast.Node.span

let origin_from_node = fun node -> { span = span_from_node node; kind = Syn.Ast.Node.kind node }

let origin_from_type_expr = fun type_expr -> {
  span = span_from_node (Syn.Ast.TypeExpr.as_node type_expr);
  kind = Syn.Ast.TypeExpr.kind type_expr;
}

let origin_has_same_span = fun (origin: origin) (span: Syn.Span.t) ->
  Int.equal origin.span.start span.start && Int.equal origin.span.end_ span.end_

let origin_has_same_span_as_node = fun origin node ->
  origin_has_same_span
    origin
    (span_from_node node)

let origin_has_same_span_as_type_expr = fun origin type_expr ->
  origin_has_same_span
    origin
    (span_from_node (Syn.Ast.TypeExpr.as_node type_expr))

let token_text = Syn.Ast.Token.text

let ident_from_syn_ident = SurfacePath.from_syn_ident

let ident_from_token = fun token -> ident_from_syn_ident (Syn.Ast.Ident.Bare token)

let rec syn_ident_from_tokens = fun __tmp1 ->
  match __tmp1 with
  | [] -> None
  | [ token ] -> Some (Syn.Ast.Ident.Bare token)
  | token :: rest ->
      Option.map
        (syn_ident_from_tokens rest)
        ~fn:(fun rest -> Syn.Ast.Ident.Qualified (token, rest))

let ident_from_tokens_as_segments_option = fun tokens ->
  syn_ident_from_tokens tokens
  |> Option.map ~fn:ident_from_syn_ident

let ident_from_tokens_as_segments = fun tokens ->
  ident_from_tokens_as_segments_option tokens
  |> Option.expect ~msg:"expected identifier token list to contain at least one segment"

let ident_tokens = fun tokens ->
  tokens
  |> List.filter ~fn:(fun token -> Syn.SyntaxKind.(Syn.Ast.Token.kind token = IDENT))

let ident_from_tokens_in_tokens = fun tokens ->
  tokens
  |> ident_tokens
  |> ident_from_tokens_as_segments

let ident_from_tokens_in_tokens_option = fun tokens ->
  tokens
  |> ident_tokens
  |> ident_from_tokens_as_segments_option

let ident_from_node = fun node ->
  let tokens =
    Syn.Ast.Node.fold_token
      node
      ~init:[]
      ~fn:(fun token tokens ->
        match Syn.Ast.Token.kind token with
        | Syn.SyntaxKind.IDENT -> Continue (token :: tokens)
        | _ -> Continue tokens)
  in
  ident_from_tokens_as_segments (List.reverse tokens)

let ident_from_node_option = fun node ->
  let tokens =
    Syn.Ast.Node.fold_token
      node
      ~init:[]
      ~fn:(fun token tokens ->
        match Syn.Ast.Token.kind token with
        | Syn.SyntaxKind.IDENT -> Continue (token :: tokens)
        | _ -> Continue tokens)
  in
  ident_from_tokens_as_segments_option (List.reverse tokens)

let rec module_expr_ident = fun module_expr ->
  match Syn.Ast.ModuleExpr.view module_expr with
  | Ident { ident } -> Some (ident_from_syn_ident ident)
  | Constraint { expr = Some expr; _ } -> module_expr_ident expr
  | _ -> None

let module_expr_ident_from_node = fun node ->
  match Syn.Ast.ModuleExpr.cast node with
  | Syn.Ast.Node module_expr -> module_expr_ident module_expr
  | Syn.Ast.Unknown _
  | Syn.Ast.Error _ -> None

let rec module_type_ident = fun module_type ->
  match Syn.Ast.ModuleTypeExpr.view module_type with
  | Ident { ident } -> Some (ident_from_syn_ident ident)
  | With { base = Some base; _ } -> module_type_ident base
  | Typeof { body = Some body } -> module_expr_ident body
  | _ -> None

let module_type_ident_from_node = fun node ->
  match Syn.Ast.ModuleTypeExpr.cast node with
  | Syn.Ast.Node module_type -> module_type_ident module_type
  | Syn.Ast.Unknown _
  | Syn.Ast.Error _ -> None

let direct_child_tokens = fun node ->
  let tokens =
    Syn.Ast.Node.fold_child_token node ~init:[] ~fn:(fun token tokens -> Continue (token :: tokens))
  in
  List.reverse tokens

let token_kind_is = fun token kind -> Syn.SyntaxKind.(Syn.Ast.Token.kind token = kind)

let rec split_at_token_kind = fun kind tokens ->
  match tokens with
  | [] -> ([], None)
  | token :: rest when token_kind_is token kind -> ([], Some rest)
  | token :: rest ->
      let (before, after) = split_at_token_kind kind rest in
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
  | (_, Some after) -> Some after
  | (_, None) -> None

let drop_final_rparen = fun tokens ->
  match List.reverse tokens with
  | token :: rest when token_kind_is token Syn.SyntaxKind.RPAREN -> List.reverse rest
  | _ -> tokens

let type_source_from_tokens = fun tokens ->
  tokens
  |> List.map ~fn:token_text
  |> String.concat " "

let module_application_from_module_expr = fun node ->
  match Syn.Ast.ModuleExpr.cast node with
  | Syn.Ast.Node module_expr -> (
      match Syn.Ast.ModuleExpr.view module_expr with
      | Apply { callee = Some callee; argument = Some argument; _ } -> (
          match (module_expr_ident callee, module_expr_ident argument) with
          | (Some callee, Some argument) -> Some ({ callee; argument }: module_application)
          | _ -> None
        )
      | _ -> None
    )
  | Syn.Ast.Unknown _
  | Syn.Ast.Error _ -> None

let member_ident_tokens_between = fun member ~start ~stop ->
  let rec loop index tokens =
    if index >= stop then
      List.reverse tokens
    else
      match Syn.Ast.ModuleDeclaration.Member.child_token_at member index with
      | Some token when Syn.SyntaxKind.(Syn.Ast.Token.kind token = IDENT) ->
          loop (index + 1) (token :: tokens)
      | _ -> loop (index + 1) tokens
  in
  loop start []

let member_find_token = fun member ~start kind ->
  let rec loop index =
    if index >= Syn.Ast.ModuleDeclaration.Member.child_count member then
      None
    else if Syn.Ast.ModuleDeclaration.Member.child_token_kind_is member index kind then
      Some index
    else
      loop (index + 1)
  in
  loop start

let functor_parameters_from_member = fun origin member ->
  let child_count = Syn.Ast.ModuleDeclaration.Member.child_count member in
  let rec loop index parameters =
    if index >= child_count then
      List.reverse parameters
    else
      match (
        Syn.Ast.ModuleDeclaration.Member.child_token_kind_is member index Syn.SyntaxKind.LPAREN,
        Syn.Ast.ModuleDeclaration.Member.child_token_at member (index + 1),
        Syn.Ast.ModuleDeclaration.Member.child_token_kind_is member (index + 2) Syn.SyntaxKind.COLON,
        member_find_token member ~start:(index + 3) Syn.SyntaxKind.RPAREN
      ) with
      | (true, Some name, true, Some close_index) when Syn.SyntaxKind.(Syn.Ast.Token.kind name
      = IDENT) ->
          let module_type =
            match member_ident_tokens_between member ~start:(index + 3) ~stop:close_index with
            | [] -> None
            | tokens -> ident_from_tokens_as_segments_option tokens
          in
          let parameter = { origin; name = ident_from_token name; module_type } in
          loop (close_index + 1) (parameter :: parameters)
      | _ -> loop (index + 1) parameters
  in
  loop 0 []

exception Build_failed of Diagnostics.Diagnostic.t

let build_failed = fun origin summary ->
  raise
    (Build_failed (Diagnostics.Diagnostic.UnsupportedSyntax {
      span = origin.span;
      kind = origin.kind;
      summary;
    }))

let unit_constructor_ident = fun () ->
  Syn.parse_ident "unit"
  |> Option.map ~fn:ident_from_syn_ident
  |> Option.expect ~msg:"expected builtin unit identifier"

let literal_from_token = fun origin token ->
  match Syn.Ast.Token.kind token with
  | Syn.SyntaxKind.INT -> Int
  | FLOAT -> Float
  | CHAR -> Char
  | STRING -> String
  | TRUE_KW
  | FALSE_KW -> Bool
  | kind -> build_failed origin ("unsupported literal " ^ Syn.SyntaxKind.to_string kind)

let node_summary = fun node -> Syn.SyntaxKind.to_string (Syn.Ast.Node.kind node)

let child_exprs = fun expression ->
  let children =
    Syn.Ast.Expr.fold_child_expr
      expression
      ~init:[]
      ~fn:(fun child children -> Continue (child :: children))
  in
  List.reverse children

let make_core_type = fun origin (kind: core_type_kind) -> ({ origin; type_ = None; kind }: core_type)

let make_type_definition = fun origin (kind: type_definition_kind) ->
  ({ origin; kind }: type_definition)

let make_parameter = fun ?annotation ?default origin label pattern ->
  ({
    origin;
    label;
    pattern;
    annotation;
    default;
  }: parameter)

let make_pattern = fun origin (kind: pattern_kind) -> ({ origin; type_ = None; kind }: pattern)

let make_expression = fun origin (kind: expression_kind) ->
  {
    origin;
    type_ = None;
    type_hint = None;
    kind;
  }

let make_argument = fun origin (kind: argument_kind) -> ({ origin; kind }: argument)

let make_structure_item = fun origin (kind: structure_item_kind) -> ({ origin; kind }:
  structure_item)

let make_signature_item = fun origin (kind: signature_item_kind) -> ({ origin; kind }:
  signature_item)

let make_implementation = fun origin items -> Implementation ({ origin; items }: implementation)

let make_interface = fun origin items -> Interface ({ origin; items }: interface)

let with_expression_type_hint = fun kind type_ (expression: expression) -> {
  expression with
  type_hint = Some { kind; type_ };
}

let require_some = fun origin summary value ->
  match value with
  | Some value -> value
  | None -> build_failed origin summary

let unsupported_node = fun node summary -> build_failed (origin_from_node node) summary

let cast_error_summary = fun label (error: Syn.Ast.cast_error) ->
  let expected =
    error.expected
    |> List.map ~fn:Syn.SyntaxKind.to_string
    |> String.concat ", "
  in
  label ^ ": expected " ^ expected ^ ", got " ^ Syn.SyntaxKind.to_string error.actual

let require_cast = fun label cast_result ->
  match cast_result with
  | Syn.Ast.Node value -> value
  | Syn.Ast.Unknown node -> unsupported_node node (node_summary node)
  | Syn.Ast.Error error ->
      build_failed (origin_from_node error.node) (cast_error_summary label error)

let poly_type_parameters = fun type_expr ->
  let names =
    Syn.Ast.TypeExpr.fold_poly_type_name
      type_expr
      ~init:[]
      ~fn:(fun token names -> Continue (token_text token :: names))
  in
  List.reverse names

let poly_variant_tag_names_from_node = fun node ->
  let (tags, _) =
    Syn.Ast.Node.fold_token
      node
      ~init:([], false)
      ~fn:(fun token (tags, saw_backtick) ->
        match Syn.Ast.Token.kind token with
        | Syn.SyntaxKind.BACKTICK -> Continue (tags, true)
        | IDENT when saw_backtick -> Continue (token_text token :: tags, false)
        | _ -> Continue (tags, false))
  in
  List.reverse tags

let poly_variant_tag_from_node = fun origin node ->
  match poly_variant_tag_names_from_node node with
  | tag :: _ -> tag
  | [] -> build_failed origin "missing polymorphic variant tag"

type build_context = {
  mutable poly_variant_type_aliases: (ident * poly_variant_type_field list) list;
}

let make_build_context = fun () -> { poly_variant_type_aliases = [] }

let poly_variant_type_fields_from_node = fun origin node ->
  poly_variant_tag_names_from_node node
  |> List.map ~fn:(fun tag -> ({ origin; tag; payload = None }: poly_variant_type_field))

let poly_variant_inherited_type_names_from_node = fun node ->
  let (names, _) =
    Syn.Ast.Node.fold_token
      node
      ~init:([], false)
      ~fn:(fun token (names, saw_backtick) ->
        match Syn.Ast.Token.kind token with
        | Syn.SyntaxKind.BACKTICK -> Continue (names, true)
        | IDENT when saw_backtick -> Continue (names, false)
        | IDENT -> Continue (ident_from_token token :: names, false)
        | _ -> Continue (names, false))
  in
  List.reverse names

let find_poly_variant_type_alias = fun context name ->
  List.find
    context.poly_variant_type_aliases
    ~fn:(fun (alias_name, _) -> SurfacePath.equal alias_name name)

let inherited_poly_variant_type_fields_from_node = fun context origin node ->
  let names = poly_variant_inherited_type_names_from_node node in
  if List.is_empty names then
    []
  else
    names
    |> List.flat_map
      ~fn:(fun name ->
        match find_poly_variant_type_alias context name with
        | Some (_, fields) -> fields
        | None ->
            build_failed origin ("unknown polymorphic variant row " ^ SurfacePath.to_string name))

let type_expr_contains_token = fun kind type_expr ->
  Syn.Ast.Node.fold_token
    (Syn.Ast.TypeExpr.as_node type_expr)
    ~init:false
    ~fn:(fun token found ->
      if found || token_kind_is token kind then
        Return true
      else
        Continue false)

let type_expr_is_extensible = fun type_expr ->
  type_expr_contains_token
    Syn.SyntaxKind.DOTDOT
    type_expr

let type_expr_first_token_kind_is = fun kind type_expr ->
  let first =
    Syn.Ast.Node.fold_token
      (Syn.Ast.TypeExpr.as_node type_expr)
      ~init:None
      ~fn:(fun token _ -> Return (Some token))
  in
  match first with
  | Some token -> token_kind_is token kind
  | None -> false

let type_expr_is_coercion = fun type_expr ->
  type_expr_first_token_kind_is
    Syn.SyntaxKind.GT
    type_expr

let type_expr_coercion_target_tokens = fun origin type_expr ->
  let (_, tokens) =
    Syn.Ast.Node.fold_token
      (Syn.Ast.TypeExpr.as_node type_expr)
      ~init:(false, [])
      ~fn:(fun token (seen_coercion, tokens) ->
        if seen_coercion then
          Continue (seen_coercion, token :: tokens)
        else if token_kind_is token Syn.SyntaxKind.GT then
          Continue (true, tokens)
        else
          Continue (seen_coercion, tokens))
  in
  match drop_final_rparen (List.reverse tokens) with
  | [] -> build_failed origin "missing coercion target type"
  | tokens -> tokens

let arrow_label_from_syn_type = fun origin label ->
  match label with
  | None -> NoLabel
  | Some { Syn.Ast.TypeExpr.name = Some label; optional_ = false } -> Labelled (token_text label)
  | Some { name = Some label; optional_ = true } -> Optional (token_text label)
  | Some { name = None; _ } -> build_failed origin "missing labeled type label"

let source_slice = fun source ->
  IO.IoVec.IoSlice.from_string source
  |> Result.expect ~msg:"failed to create typ AST parser source slice"

let rec build_core_type = fun context (type_expr: Syn.Ast.TypeExpr.t) ->
  let origin = origin_from_type_expr type_expr in
  (
    match Syn.Ast.TypeExpr.view type_expr with
    | Syn.Ast.TypeExpr.Wildcard -> make_core_type origin Wildcard
    | Syn.Ast.TypeExpr.Var { name } -> make_core_type origin (Var (Some (token_text name)))
    | Syn.Ast.TypeExpr.Ident { ident } ->
        make_core_type origin (TypeIdent (ident_from_syn_ident ident))
    | Syn.Ast.TypeExpr.Apply { ident; args } ->
        let constructor = make_core_type origin (TypeIdent (ident_from_syn_ident ident)) in
        let arguments =
          args
          |> Vector.iter
          |> Iter.Iterator.map ~fn:(build_core_type context)
          |> Iter.Iterator.to_list
        in
        make_core_type origin (Apply { constructor; arguments })
    | Syn.Ast.TypeExpr.Arrow { label; arg; ret } ->
        make_core_type
          origin
          (Arrow {
            label = arrow_label_from_syn_type origin label;
            parameter = build_core_type context arg;
            result = build_core_type context ret;
          })
    | Syn.Ast.TypeExpr.Tuple { parts } ->
        let parts =
          parts
          |> Vector.iter
          |> Iter.Iterator.map ~fn:(build_core_type context)
          |> Iter.Iterator.to_list
        in
        make_core_type origin (Tuple parts)
    | Syn.Ast.TypeExpr.Forall { body } ->
        make_core_type
          origin
          (ForAll {
            parameters = poly_type_parameters type_expr;
            body = build_core_type context body;
          })
    | Syn.Ast.TypeExpr.Alias { typ; _ } -> build_core_type context typ
    | Syn.Ast.TypeExpr.Unknown node -> (
        match poly_variant_type_fields_from_node origin node with
        | _ :: _ as fields -> make_core_type origin (PolyVariant fields)
        | [] -> (
            match inherited_poly_variant_type_fields_from_node context origin node with
            | _ :: _ as fields -> make_core_type origin (PolyVariant fields)
            | [] -> (
                match Syn.Ast.TypeExpr.inner_without_attribute_suffix type_expr with
                | Some inner -> build_core_type context inner
                | None -> unsupported_node node (node_summary node)
              )
          )
      )
    | Syn.Ast.TypeExpr.Error node -> unsupported_node node (node_summary node)
  )

and build_core_type_from_source = fun context origin source ->
  let parse_result = Syn.parse_interface (source_slice ("val __typ : " ^ source ^ "\n")) in
  let source_file = Syn.Ast.SourceFile.make parse_result.tree in
  match Syn.Ast.SourceFile.view source_file with
  | Interface interface ->
      let annotation =
        Syn.Ast.Interface.fold_item
          interface
          ~init:None
          ~fn:(fun item annotation ->
            match (annotation, Syn.Ast.SignatureItem.view item) with
            | (None, Value declaration) -> (
                match Syn.Ast.ValueDeclaration.view declaration with
                | Value { annotation; _ } -> Return (Some annotation)
                | Unknown _ -> Continue annotation
              )
            | _ -> Continue annotation)
      in
      build_core_type
        context
        (require_some origin ("failed to parse package constraint type: " ^ source) annotation)
  | Implementation _ -> build_failed origin ("failed to parse package constraint type: " ^ source)

and build_core_type_from_tokens = fun context origin tokens ->
  build_core_type_from_source
    context
    origin
    (type_source_from_tokens tokens)

and package_constraint_manifest_tokens = fun tokens ->
  let rec loop depth acc tokens =
    match tokens with
    | [] -> (List.reverse acc, [])
    | token :: rest when Int.equal depth 0 && token_kind_is token Syn.SyntaxKind.WITH_KW -> (
      List.reverse acc,
      tokens
    )
    | token :: rest when token_kind_is token Syn.SyntaxKind.LPAREN ->
        loop (depth + 1) (token :: acc) rest
    | token :: rest when token_kind_is token Syn.SyntaxKind.RPAREN ->
        loop
          (Int.max 0 (depth - 1))
          (token :: acc)
          rest
    | token :: rest -> loop depth (token :: acc) rest
  in
  loop 0 [] tokens

and build_package_constraints = fun context origin tokens ->
  let rec loop constraints tokens =
    match tokens with
    | [] -> List.reverse constraints
    | token :: rest when token_kind_is token Syn.SyntaxKind.WITH_KW -> loop constraints rest
    | token :: rest when token_kind_is token Syn.SyntaxKind.TYPE_KW ->
        let (name_tokens, after_name) = split_at_token_kind Syn.SyntaxKind.EQ rest in
        let (manifest_tokens, rest) =
          match after_name with
          | Some tokens -> package_constraint_manifest_tokens tokens
          | None -> build_failed origin "missing package type constraint manifest"
        in
        let type_name =
          ident_from_tokens_in_tokens_option name_tokens
          |> require_some origin "missing package type constraint name"
        in
        let constraint_: package_type_constraint = {
          origin;
          type_name;
          manifest = build_core_type_from_tokens context origin manifest_tokens;
        }
        in
        loop (constraint_ :: constraints) rest
    | _ :: rest -> loop constraints rest
  in
  loop [] tokens

and build_package_type_from_ascription_tokens = fun context origin ?binder tokens ->
  let (module_type_tokens, constraint_tokens) = split_at_token_kind Syn.SyntaxKind.WITH_KW tokens in
  let module_type =
    ident_from_tokens_in_tokens_option module_type_tokens
    |> require_some origin "missing first-class module type"
  in
  let constraints =
    match constraint_tokens with
    | Some tokens -> build_package_constraints context origin tokens
    | None -> []
  in
  ({
    origin;
    binder;
    module_type;
    constraints;
  }: package_type)

and first_class_module_ident_from_tokens = fun origin tokens ->
  let after_module =
    tokens_after_token_kind Syn.SyntaxKind.MODULE_KW tokens
    |> require_some origin "missing first-class module keyword"
  in
  after_module
  |> tokens_until_any [ Syn.SyntaxKind.COLON; Syn.SyntaxKind.RPAREN ]
  |> ident_from_tokens_in_tokens_option
  |> require_some origin "missing first-class module name"

and first_class_package_type_from_tokens = fun context origin ?binder tokens ->
  match tokens_after_token_kind Syn.SyntaxKind.COLON tokens with
  | None -> None
  | Some tokens ->
      let tokens = drop_final_rparen tokens in
      Some (build_package_type_from_ascription_tokens context origin ?binder tokens)

and first_class_module_unpack_expression_from_tokens = fun origin tokens ->
  let after_val =
    tokens_after_token_kind Syn.SyntaxKind.VAL_KW tokens
    |> require_some origin "missing first-class module unpack keyword"
  in
  let expression_tokens =
    tokens_until_any [ Syn.SyntaxKind.COLON; Syn.SyntaxKind.RPAREN ] after_val
  in
  match ident_tokens expression_tokens with
  | [] -> build_failed origin "missing first-class module unpack expression"
  | identifiers ->
      ident_from_tokens_as_segments_option identifiers
      |> require_some origin "missing first-class module unpack expression"
      |> fun ident -> make_expression origin (Ident ident)

and first_class_module_unpack_from_tokens = fun context origin tokens ->
  ({
    origin;
    expression = first_class_module_unpack_expression_from_tokens origin tokens;
    package_type = first_class_package_type_from_tokens context origin tokens;
  }: module_unpack)

and module_body_unpack = fun context node ->
  let tokens = direct_child_tokens node in
  if List.exists (fun token -> token_kind_is token Syn.SyntaxKind.VAL_KW) tokens then
    Some (first_class_module_unpack_from_tokens context (origin_from_node node) tokens)
  else
    None

let rec build_parameter = fun context parameter ->
  let origin = origin_from_node (Syn.Ast.Parameter.as_node parameter) in
  (
    match Syn.Ast.Parameter.view parameter with
    | Syn.Ast.Parameter.Param { label = NoLabel; pattern } ->
        let pattern =
          require_some origin "missing parameter pattern" pattern
          |> build_pattern context
        in
        let (pattern, annotation) = extract_parameter_annotation pattern in
        make_parameter ?annotation origin Unlabeled pattern
    | Syn.Ast.Parameter.Param { label = Labeled { name }; pattern } ->
        let label_token = require_some origin "missing labeled parameter label" name in
        let label = ident_from_token label_token in
        let pattern = build_parameter_pattern context origin label pattern in
        let (pattern, annotation) = extract_parameter_annotation pattern in
        make_parameter ?annotation origin (Labeled label) pattern
    | Syn.Ast.Parameter.Param { label = Optional { name; default }; pattern } ->
        let label_token = require_some origin "missing optional parameter label" name in
        let label = ident_from_token label_token in
        let pattern = build_parameter_pattern context origin label pattern in
        let (pattern, annotation) = extract_parameter_annotation pattern in
        let default = Option.map default ~fn:(build_expression context) in
        make_parameter ?annotation ?default origin (Optional label) pattern
    | Syn.Ast.Parameter.Unknown node -> ((unsupported_node node (node_summary node)): parameter)
  )

and build_parameter_pattern = fun context origin label pattern ->
  match pattern with
  | Some pattern -> build_pattern context pattern
  | None -> make_pattern origin (Bind label)

and extract_parameter_annotation = fun (pattern: pattern) ->
  match pattern.kind with
  | Constraint { pattern; annotation } -> (pattern, Some annotation)
  | _ -> (pattern, None)

and locally_abstract_type_names = fun origin syntax_pattern ->
  let pattern =
    Syn.Ast.LocallyAbstractTypePattern.cast syntax_pattern
    |> require_cast "invalid locally abstract type pattern"
  in
  match Syn.Ast.LocallyAbstractTypePattern.type_ident pattern with
  | Some ident ->
      [
        ident_from_syn_ident ident
        |> SurfacePath.to_string;
      ]
  | None -> build_failed origin "missing locally abstract type name"

and build_function_parameters = fun context syntax_parameters ->
  let (type_binders, parameters) =
    syntax_parameters
    |> List.fold_left
      ~init:([], [])
      ~fn:(fun (type_binders, parameters) syntax_parameter ->
        let origin = origin_from_node (Syn.Ast.Parameter.as_node syntax_parameter) in
        match Syn.Ast.Parameter.view syntax_parameter with
        | Syn.Ast.Parameter.Param { label = NoLabel; pattern = Some pattern } -> (
            match Syn.Ast.LocallyAbstractTypePattern.cast pattern with
            | Syn.Ast.Node _ -> (
              List.append
                (
                  locally_abstract_type_names origin pattern
                  |> List.reverse
                )
                type_binders,
              parameters
            )
            | Syn.Ast.Unknown _
            | Syn.Ast.Error _ -> (
              type_binders,
              build_parameter context syntax_parameter :: parameters
            )
          )
        | _ -> (type_binders, build_parameter context syntax_parameter :: parameters))
  in
  (List.reverse type_binders, List.reverse parameters)

and strip_built_let_return_annotation_from_parameter = fun
  return_annotation (parameter: parameter) ->
  let pattern =
    match (return_annotation, parameter.pattern.kind) with
    | (Some return_annotation, Constraint { pattern; annotation }) when origin_has_same_span_as_type_expr
      annotation.origin
      return_annotation -> pattern
    | _ -> parameter.pattern
  in
  let annotation =
    match (return_annotation, parameter.annotation) with
    | (Some return_annotation, Some annotation) when origin_has_same_span_as_type_expr
      annotation.origin
      return_annotation -> None
    | _ -> parameter.annotation
  in
  { parameter with pattern; annotation }

and build_first_class_module_pattern = fun context origin (syntax_pattern: Syn.Ast.Pattern.t) ->
  let pattern =
    Syn.Ast.FirstClassModulePattern.cast syntax_pattern
    |> require_cast "invalid first-class module pattern"
  in
  let binder =
    match Syn.Ast.FirstClassModulePattern.binder pattern with
    | Some (Syn.Ast.Ident.Bare token) when token_kind_is token Syn.SyntaxKind.UNDERSCORE -> None
    | Some ident -> Some (ident_from_syn_ident ident)
    | _ -> None
  in
  let tokens = direct_child_tokens (Syn.Ast.Pattern.as_node syntax_pattern) in
  make_pattern
    origin
    (FirstClassModule {
      binder;
      package_type = first_class_package_type_from_tokens context origin ?binder tokens;
    })

and build_pattern = fun context (syntax_pattern: Syn.Ast.Pattern.t) ->
  let origin = origin_from_node (Syn.Ast.Pattern.as_node syntax_pattern) in
  (
    match Syn.Ast.Pattern.view syntax_pattern with
    | Syn.Ast.Pattern.Unit ->
        make_pattern origin (Constructor { ident = unit_constructor_ident (); payload = None })
    | Syn.Ast.Pattern.Wildcard -> make_pattern origin Wildcard
    | Syn.Ast.Pattern.Ident { ident } -> make_pattern origin (Bind (ident_from_syn_ident ident))
    | Syn.Ast.Pattern.Constructor { constructor; payload } ->
        make_pattern
          origin
          (Constructor {
            ident = ident_from_syn_ident constructor;
            payload = Option.map payload ~fn:(build_pattern context);
          })
    | Syn.Ast.Pattern.Literal { token } ->
        make_pattern origin (Literal (literal_from_token origin token))
    | Syn.Ast.Pattern.PolyVariant { tag; payload } ->
        make_pattern
          origin
          (PolyVariant {
            tag = token_text tag;
            payload = Option.map payload ~fn:(build_pattern context);
          })
    | Syn.Ast.Pattern.Tuple { parts } ->
        make_pattern
          origin
          (
            Tuple (
              parts
              |> Vector.iter
              |> Iter.Iterator.map ~fn:(build_pattern context)
              |> Iter.Iterator.to_list
            )
          )
    | Syn.Ast.Pattern.List { items } ->
        make_pattern
          origin
          (
            List (
              items
              |> Vector.iter
              |> Iter.Iterator.map ~fn:(build_pattern context)
              |> Iter.Iterator.to_list
            )
          )
    | Syn.Ast.Pattern.Record { fields; _ } ->
        make_pattern
          origin
          (
            Record (
              fields
              |> Vector.iter
              |> Iter.Iterator.filter_map ~fn:(build_record_pattern_field context)
              |> Iter.Iterator.to_list
            )
          )
    | Syn.Ast.Pattern.Or { left; right } ->
        make_pattern
          origin
          (Or { left = build_pattern context left; right = build_pattern context right })
    | Syn.Ast.Pattern.Cons { head; tail } ->
        make_pattern
          origin
          (Cons { head = build_pattern context head; tail = build_pattern context tail })
    | Syn.Ast.Pattern.Constraint { pattern; annotation } ->
        make_pattern
          origin
          (Constraint {
            pattern = build_pattern context pattern;
            annotation = build_core_type context annotation;
          })
    | Syn.Ast.Pattern.Alias { pattern; alias } ->
        make_pattern
          origin
          (Alias {
            pattern = build_pattern context pattern;
            alias = build_pattern context alias;
          })
    | Syn.Ast.Pattern.FirstClassModule _ ->
        build_first_class_module_pattern context origin syntax_pattern
    | Syn.Ast.Pattern.Error node -> unsupported_node node (node_summary node)
    | Syn.Ast.Pattern.Unknown node -> unsupported_node node (node_summary node)
    | Syn.Ast.Pattern.Array _
    | Syn.Ast.Pattern.Interval _
    | Syn.Ast.Pattern.Lazy _
    | Syn.Ast.Pattern.Exception _ ->
        ((build_failed origin (Syn.SyntaxKind.to_string origin.kind)): pattern)
  )

and build_record_pattern_field = fun context (field: Syn.Ast.RecordPattern.field) ->
  match field with
  | Syn.Ast.RecordPatternField { ident; pattern; node } ->
      let origin = origin_from_node (Syn.Ast.Pattern.as_node node) in
      let field: record_pattern_field = {
        origin;
        name = ident_from_syn_ident ident;
        pattern = Option.map pattern ~fn:(build_pattern context);
      }
      in
      Some field
  | Syn.Ast.UnknownRecordPatternField _ -> None

and build_let_binding = fun context (binding: Syn.Ast.LetBinding.t) ->
  let origin = origin_from_node (Syn.Ast.LetBinding.as_node binding) in
  let (syntax_pattern, syntax_body) =
    match Syn.Ast.LetBinding.view binding with
    | Binding { pattern; body } -> (pattern, body)
    | Unknown node -> unsupported_node node (node_summary node)
  in
  let syntax_type_annotation = Syn.Ast.LetBinding.type_annotation binding in
  let syntax_parameters =
    Syn.Ast.LetBinding.fold_parameter
      binding
      ~init:[]
      ~fn:(fun parameter syntax_parameters -> Continue (parameter :: syntax_parameters))
  in
  let type_annotation = Option.map syntax_type_annotation ~fn:(build_core_type context) in
  let (type_binders, parameters) =
    build_function_parameters context (List.reverse syntax_parameters)
  in
  let parameters =
    parameters
    |> List.map ~fn:(strip_built_let_return_annotation_from_parameter syntax_type_annotation)
  in
  let body = build_expression context syntax_body in
  let (body, type_annotation) =
    match (parameters, type_annotation) with
    | ([], type_annotation) -> (body, type_annotation)
    | (_, Some type_annotation) -> (with_expression_type_hint Annotation type_annotation body, None)
    | (_ :: _, None) -> (body, None)
  in
  let body =
    match parameters with
    | [] -> body
    | _ :: _ -> make_expression origin (Function { type_binders; parameters; body = Body body })
  in
  ({
    origin;
    pattern = build_pattern context syntax_pattern;
    type_hint = type_annotation;
    expr = body;
  }: let_binding)

and build_match_case = fun context (match_case: Syn.Ast.MatchCase.t) ->
  let origin = origin_from_node (Syn.Ast.MatchCase.as_node match_case) in
  match Syn.Ast.MatchCase.view match_case with
  | Syn.Ast.MatchCase.Case { pattern; guard; body } ->
      ({
        origin;
        pattern = build_pattern context pattern;
        guard = Option.map guard ~fn:(build_expression context);
        body = build_expression context body;
      }: match_case)
  | Syn.Ast.MatchCase.Unknown node -> unsupported_node node (node_summary node)

and build_match_case_option = fun context match_case ->
  match Syn.Ast.MatchCase.view match_case with
  | Syn.Ast.MatchCase.Unknown _ -> None
  | Syn.Ast.MatchCase.Case _ -> Some (build_match_case context match_case)

and build_match_cases = fun context syntax_expression ->
  let cases =
    Syn.Ast.Expr.fold_match_case
      syntax_expression
      ~init:[]
      ~fn:(fun match_case cases ->
        match build_match_case_option context match_case with
        | Some match_case -> Continue (match_case :: cases)
        | None -> Continue cases)
  in
  List.reverse cases

and build_record_expression_field = fun context (field: Syn.Ast.RecordExpr.field) ->
  match field with
  | Syn.Ast.RecordExprField { ident; value; node } ->
      let origin = origin_from_node (Syn.Ast.RecordExprField.as_node node) in
      let field: record_expression_field = {
        origin;
        name = ident_from_syn_ident ident;
        value = build_expression context (require_some origin "missing record field value" value);
      }
      in
      Some field
  | Syn.Ast.UnknownRecordExprField _ -> None

and build_expression = fun context (syntax_expression: Syn.Ast.Expr.t) ->
  let origin = origin_from_node (Syn.Ast.Expr.as_node syntax_expression) in
  (
    match Syn.Ast.Expr.view syntax_expression with
    | Syn.Ast.Expr.Unit ->
        make_expression origin (Constructor { ident = unit_constructor_ident (); payload = None })
    | Syn.Ast.Expr.Literal { token } ->
        make_expression origin (Literal (literal_from_token origin token))
    | Syn.Ast.Expr.Ident { ident } -> make_expression origin (Ident (ident_from_syn_ident ident))
    | Syn.Ast.Expr.Constructor { constructor; payload = None } ->
        make_expression
          origin
          (Constructor { ident = ident_from_syn_ident constructor; payload = None })
    | Syn.Ast.Expr.Constructor { constructor; payload = Some payload } ->
        make_expression
          origin
          (Constructor {
            ident = ident_from_syn_ident constructor;
            payload = Some (build_expression context payload);
          })
    | Syn.Ast.Expr.Annotated { expr; annotation } ->
        let (kind, type_) =
          if type_expr_is_coercion annotation then
            (
              Coercion,
              build_core_type_from_tokens
                context
                origin
                (type_expr_coercion_target_tokens origin annotation)
            )
          else
            (Annotation, build_core_type context annotation)
        in
        with_expression_type_hint
          kind
          type_
          (build_expression context expr)
    | Syn.Ast.Expr.Tuple { items } ->
        make_expression
          origin
          (
            Tuple (
              items
              |> Vector.iter
              |> Iter.Iterator.map ~fn:(build_expression context)
              |> Iter.Iterator.to_list
            )
          )
    | Syn.Ast.Expr.List { items } ->
        make_expression
          origin
          (
            List (
              items
              |> Vector.iter
              |> Iter.Iterator.map ~fn:(build_expression context)
              |> Iter.Iterator.to_list
            )
          )
    | Syn.Ast.Expr.Array { items } ->
        make_expression
          origin
          (
            Array (
              items
              |> Vector.iter
              |> Iter.Iterator.map ~fn:(build_expression context)
              |> Iter.Iterator.to_list
            )
          )
    | Syn.Ast.Expr.PolyVariant { tag; payload } ->
        make_expression
          origin
          (PolyVariant {
            tag = token_text tag;
            payload = Option.map payload ~fn:(build_expression context);
          })
    | Syn.Ast.Expr.Record { base; fields } ->
        let fields =
          fields
          |> Vector.iter
          |> Iter.Iterator.filter_map ~fn:(build_record_expression_field context)
          |> Iter.Iterator.to_list
        in
        (
          match base with
          | Some base ->
              make_expression
                origin
                (Record { update = Some (build_expression context base); fields })
          | None -> make_expression origin (Record { update = None; fields })
        )
    | Syn.Ast.Expr.FieldAccess { target; field } ->
        let receiver = build_expression context target in
        let origin = {
          origin with
          span = Syn.Span.union receiver.origin.span (Syn.Ast.Ident.span field);
        }
        in
        make_expression origin (FieldAccess { receiver; field = ident_from_syn_ident field })
    | Syn.Ast.Expr.Assign { target; value; _ } ->
        make_expression
          origin
          (Assign {
            target = build_expression context target;
            value = build_expression context value;
          })
    | Syn.Ast.Expr.Sequence { left; right = Some right } ->
        make_expression
          origin
          (Sequence {
            left = build_expression context left;
            right = build_expression context right;
          })
    | Syn.Ast.Expr.Sequence { right = None; _ } ->
        build_failed origin "incomplete sequence expression"
    | Syn.Ast.Expr.If { condition; then_branch; else_branch } ->
        make_expression
          origin
          (If {
            condition = build_expression context condition;
            then_branch = build_expression context then_branch;
            else_branch = Option.map else_branch ~fn:(build_expression context);
          })
    | Syn.Ast.Expr.Match { scrutinee; first_case = _ } ->
        make_expression
          origin
          (Match {
            scrutinee = build_expression context scrutinee;
            cases = build_match_cases context syntax_expression;
          })
    | Syn.Ast.Expr.Try { body; first_case = _ } ->
        make_expression
          origin
          (Try {
            body = build_expression context body;
            cases = build_match_cases context syntax_expression;
          })
    | Syn.Ast.Expr.While { condition; body } ->
        make_expression
          origin
          (While {
            condition = build_expression context condition;
            body = build_expression context body;
          })
    | Syn.Ast.Expr.For {
        pattern;
        start_;
        stop;
        body;
      } ->
        make_expression
          origin
          (
            For {
              pattern = build_pattern context pattern;
              start_ = build_expression context start_;
              stop = build_expression context stop;
              body = build_expression context body;
            }
          )
    | Syn.Ast.Expr.Fun { body = Body_expr body } ->
        let syntax_parameters =
          Syn.Ast.Expr.fold_parameter
            syntax_expression
            ~init:[]
            ~fn:(fun parameter syntax_parameters -> Continue (parameter :: syntax_parameters))
          |> List.reverse
        in
        let (type_binders, parameters) = build_function_parameters context syntax_parameters in
        make_expression
          origin
          (Function { type_binders; parameters; body = Body (build_expression context body) })
    | Syn.Ast.Expr.Fun { body = Body_cases { first_case = _ } } ->
        make_expression
          origin
          (Function {
            type_binders = [];
            parameters = [];
            body = Cases (build_match_cases context syntax_expression);
          })
    | Syn.Ast.Expr.Apply { callee; argument } ->
        let arguments = [ build_argument context argument ] in
        make_expression origin (Apply { callee = build_expression context callee; arguments })
    | Syn.Ast.Expr.Infix { left; operator; right } ->
        make_expression
          origin
          (Infix {
            left = build_expression context left;
            operator = ident_from_token operator;
            right = build_expression context right;
          })
    | Syn.Ast.Expr.Prefix { operator; operand } ->
        let callee = make_expression origin (Ident (ident_from_token operator)) in
        let argument = build_expression context operand in
        make_expression
          origin
          (Apply { callee; arguments = [ make_argument argument.origin (Positional argument) ] })
    | Syn.Ast.Expr.Let { first_binding; body } ->
        make_expression
          origin
          (Let {
            recursive = false;
            bindings = [ build_let_binding context first_binding ];
            body = build_expression context body;
          })
    | Syn.Ast.Expr.LetModule _ -> build_let_module_expression context origin syntax_expression
    | Syn.Ast.Expr.LocalOpen _ -> build_local_open_expression context origin syntax_expression
    | Syn.Ast.Expr.Error node -> unsupported_node node (node_summary node)
    | Syn.Ast.Expr.Unknown node -> (
        match Syn.Ast.Node.kind node with
        | Syn.SyntaxKind.FIRST_CLASS_MODULE_EXPR ->
            build_first_class_module_expression context origin syntax_expression
        | _ -> unsupported_node node (node_summary node)
      )
    | Syn.Ast.Expr.LetException _ ->
        ((build_failed origin (Syn.SyntaxKind.to_string origin.kind)): expression)
  )

and build_first_class_module_expression = fun context origin (syntax_expression: Syn.Ast.Expr.t) ->
  let tokens = direct_child_tokens (Syn.Ast.Expr.as_node syntax_expression) in
  make_expression
    origin
    (FirstClassModule {
      module_ = first_class_module_ident_from_tokens origin tokens;
      package_type = first_class_package_type_from_tokens context origin tokens;
    })

and build_argument = fun context (syntax_expression: Syn.Ast.Expr.t) ->
  let node = Syn.Ast.Expr.as_node syntax_expression in
  let origin = origin_from_node node in
  let first_ident_token =
    Syn.Ast.Node.fold_token
      node
      ~init:None
      ~fn:(fun token found ->
        match (found, Syn.Ast.Token.kind token) with
        | (None, Syn.SyntaxKind.IDENT) -> Return (Some token)
        | _ -> Continue found)
  in
  match Syn.Ast.Node.kind node with
  | Syn.SyntaxKind.LABELED_ARG ->
      make_argument
        origin
        (
          Labeled {
            label = token_text
              (require_some origin "missing labeled argument label" first_ident_token);
            value =
              child_exprs syntax_expression
              |> List.head
              |> Option.map ~fn:(build_expression context);
          }
        )
  | Syn.SyntaxKind.OPTIONAL_ARG ->
      make_argument
        origin
        (
          Optional {
            label = token_text
              (require_some origin "missing optional argument label" first_ident_token);
            value =
              child_exprs syntax_expression
              |> List.head
              |> Option.map ~fn:(build_expression context);
          }
        )
  | _ ->
      ((make_argument origin (Positional (build_expression context syntax_expression))): argument)

and build_let_module_expression = fun context origin syntax_expression ->
  let let_module =
    Syn.Ast.LetModuleExpr.cast syntax_expression
    |> require_cast "invalid let module expression"
  in
  let body =
    Syn.Ast.LetModuleExpr.body let_module
    |> require_some origin "missing let module body"
  in
  let name =
    Syn.Ast.LetModuleExpr.name let_module
    |> require_some origin "missing let module name"
    |> ident_from_syn_ident
  in
  let module_body_node = Syn.Ast.LetModuleExpr.module_body_node let_module in
  let alias =
    match module_body_node with
    | Some node when Syn.SyntaxKind.(Syn.Ast.Node.kind node = PATH_MODULE_EXPR) ->
        ident_from_node_option node
    | _ -> None
  in
  let unpack =
    match module_body_node with
    | Some node -> module_body_unpack context node
    | None -> None
  in
  let items =
    match module_body_node with
    | Some node when Syn.SyntaxKind.(Syn.Ast.Node.kind node = STRUCT_MODULE_EXPR) ->
        build_structure_items_from_module_expr context node
    | Some node when Syn.SyntaxKind.(Syn.Ast.Node.kind node = PATH_MODULE_EXPR) -> []
    | Some node when Option.is_some (module_body_unpack context node) -> []
    | _ -> build_failed origin "unsupported let module body"
  in
  make_expression
    origin
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
  let local_open =
    Syn.Ast.LocalOpenExpr.cast syntax_expression
    |> require_cast "invalid local open expression"
  in
  let (module_ident, body) =
    match Syn.Ast.LocalOpenExpr.view local_open with
    | LetOpen { module_ident; body; _ }
    | Delimited { module_ident; body; _ } -> (module_ident, body)
    | Unknown node -> unsupported_node node (node_summary node)
  in
  make_expression
    origin
    (LocalOpen {
      module_ = ident_from_syn_ident module_ident;
      body = build_expression context body;
    })

and build_let_declaration = fun context (declaration: Syn.Ast.LetDeclaration.t) ->
  let bindings =
    Syn.Ast.LetDeclaration.fold_binding
      declaration
      ~init:[]
      ~fn:(fun binding bindings -> Continue (build_let_binding context binding :: bindings))
  in
  ({
    origin = origin_from_node (Syn.Ast.LetDeclaration.as_node declaration);
    recursive = Option.is_some (Syn.Ast.LetDeclaration.rec_token declaration);
    bindings = List.reverse bindings;
  }: let_declaration)

and build_value_declaration = fun context (declaration: Syn.Ast.ValueDeclaration.t) ->
  let origin = origin_from_node (Syn.Ast.ValueDeclaration.as_node declaration) in
  match Syn.Ast.ValueDeclaration.view declaration with
  | Value { name; annotation; _ } ->
      ({
        origin;
        name = ident_from_syn_ident name;
        type_annotation = build_core_type context annotation;
      }: value_declaration)
  | Unknown node -> unsupported_node node (node_summary node)

and build_external_declaration = fun context (declaration: Syn.Ast.ExternalDeclaration.t) ->
  let origin = origin_from_node (Syn.Ast.ExternalDeclaration.as_node declaration) in
  match Syn.Ast.ExternalDeclaration.view declaration with
  | External { name; annotation; primitives; _ } ->
      ({
        origin;
        name = ident_from_syn_ident name;
        type_annotation = build_core_type context annotation;
        primitives =
          primitives
          |> Vector.iter
          |> Iter.Iterator.map ~fn:token_text
          |> Iter.Iterator.to_list;
      }: external_declaration)
  | Unknown node -> unsupported_node node (node_summary node)

and build_type_parameter = fun __tmp1 ->
  match __tmp1 with
  | Syn.Ast.TypeDeclaration.Named { name; _ } -> Some (token_text name)
  | Syn.Ast.TypeDeclaration.Wildcard _ -> None

and build_type_constructor = fun context (constructor: Syn.Ast.VariantConstructor.t) ->
  let origin = origin_from_node (Syn.Ast.VariantConstructor.as_node constructor) in
  let arguments: constructor_arguments =
    match (
      Syn.Ast.VariantConstructor.record_payload constructor,
      Syn.Ast.VariantConstructor.payload_type constructor
    ) with
    | (Some record, _) -> Record (build_record_field_declarations context record)
    | (None, Some payload) ->
        let payload = build_core_type context payload in
        (
          match payload.kind with
          | Tuple parts -> Tuple parts
          | _ -> Tuple [ payload ]
        )
    | (None, None) -> Tuple []
  in
  ({
    origin;
    name =
      Syn.Ast.VariantConstructor.name constructor
      |> require_some origin "missing variant constructor name"
      |> ident_from_syn_ident;
    arguments;
    result = Option.map
      (Syn.Ast.VariantConstructor.result_type constructor)
      ~fn:(build_core_type context);
  }: type_constructor)

and build_record_field_declaration = fun context (field: Syn.Ast.RecordField.t) ->
  let origin = origin_from_node (Syn.Ast.RecordField.as_node field) in
  ({
    origin;
    name =
      Syn.Ast.RecordField.name field
      |> require_some origin "missing record field name"
      |> ident_from_syn_ident;
    mutable_ = Option.is_some (Syn.Ast.RecordField.mutable_token field);
    type_annotation =
      Syn.Ast.RecordField.type_annotation field
      |> require_some origin "missing record field type annotation"
      |> build_core_type context;
  }: record_field_declaration)

and build_record_field_declarations = fun context record ->
  let fields =
    Syn.Ast.RecordType.fold_field
      record
      ~init:[]
      ~fn:(fun field fields -> Continue (build_record_field_declaration context field :: fields))
  in
  List.reverse fields

and build_type_declaration_member = fun context member ->
  let declaration = Syn.Ast.TypeDeclaration.Member.declaration member in
  let origin = origin_from_node (Syn.Ast.TypeDeclaration.as_node declaration) in
  let parameters =
    Syn.Ast.TypeDeclaration.Member.fold_parameter
      member
      ~init:[]
      ~fn:(fun parameter parameters -> Continue (build_type_parameter parameter :: parameters))
  in
  let definition: type_definition =
    match (
      Syn.Ast.TypeDeclaration.Member.manifest member,
      Syn.Ast.TypeDeclaration.Member.variant_type member,
      Syn.Ast.TypeDeclaration.Member.record_type member
    ) with
    | (Some manifest, _, _) when type_expr_is_extensible manifest ->
        make_type_definition origin Extensible
    | (Some manifest, _, _) ->
        make_type_definition origin (Alias (build_core_type context manifest))
    | (None, Some variant, _) ->
        let constructors =
          Syn.Ast.VariantType.fold_constructor
            variant
            ~init:[]
            ~fn:(fun constructor constructors ->
              Continue (build_type_constructor context constructor :: constructors))
        in
        make_type_definition origin (Variant (List.reverse constructors))
    | (None, None, Some record) ->
        make_type_definition origin (Record (build_record_field_declarations context record))
    | (None, None, None) -> make_type_definition origin Abstract
  in
  let name =
    Syn.Ast.TypeDeclaration.Member.name member
    |> require_some origin "missing type declaration name"
    |> ident_from_syn_ident
  in
  (
    match definition.kind with
    | Alias { kind = PolyVariant fields; _ } ->
        context.poly_variant_type_aliases <- (name, fields) :: context.poly_variant_type_aliases
    | _ -> ()
  );
  ({
    origin;
    name;
    parameters = List.reverse parameters;
    definition;
  }: type_declaration)

and build_type_declarations = fun context declaration ->
  let declarations =
    Syn.Ast.TypeDeclaration.fold_member
      declaration
      ~init:[]
      ~fn:(fun member declarations ->
        Continue (build_type_declaration_member context member :: declarations))
  in
  List.reverse declarations

and build_type_extension_declaration = fun
  context (declaration: Syn.Ast.TypeExtensionDeclaration.t) ->
  let origin = origin_from_node (Syn.Ast.TypeExtensionDeclaration.as_node declaration) in
  let name =
    Syn.Ast.TypeExtensionDeclaration.name declaration
    |> require_some origin "missing type extension name"
    |> ident_from_syn_ident
  in
  let constructors =
    match Syn.Ast.TypeExtensionDeclaration.variant_type declaration with
    | Some variant ->
        Syn.Ast.VariantType.fold_constructor
          variant
          ~init:[]
          ~fn:(fun constructor constructors ->
            Continue (build_type_constructor context constructor :: constructors))
    | None -> []
  in
  ({ origin; name; constructors = List.reverse constructors }: type_extension_declaration)

and build_exception_declaration = fun context (declaration: Syn.Ast.ExceptionDeclaration.t) ->
  let origin = origin_from_node (Syn.Ast.ExceptionDeclaration.as_node declaration) in
  let payload =
    match Syn.Ast.ExceptionDeclaration.view declaration with
    | Syn.Ast.ExceptionDeclaration.Payload { payload = TypeExpr type_expr; _ } ->
        Some (build_core_type context type_expr)
    | Syn.Ast.ExceptionDeclaration.Payload { payload = Record _; _ } ->
        build_failed origin "exception record payload"
    | Syn.Ast.ExceptionDeclaration.Bare -> None
    | Syn.Ast.ExceptionDeclaration.Alias _ -> build_failed origin "exception alias"
    | Syn.Ast.ExceptionDeclaration.Unknown node -> unsupported_node node (node_summary node)
  in
  ({
    origin;
    name =
      Syn.Ast.ExceptionDeclaration.name declaration
      |> require_some origin "missing exception name"
      |> ident_from_syn_ident;
    payload;
  }: exception_declaration)

and build_structure_items_from_module_expr = fun context node ->
  let module_expr =
    Syn.Ast.ModuleExpr.cast node
    |> require_cast "invalid structure module expression"
  in
  let items =
    Syn.Ast.ModuleExpr.fold_structure_item
      module_expr
      ~init:[]
      ~fn:(fun item items ->
        match build_structure_item_option context item with
        | Some item -> Continue (item :: items)
        | None -> Continue items)
  in
  List.reverse items

and build_module_declaration_member = fun context member ->
  let declaration = Syn.Ast.ModuleDeclaration.Member.declaration member in
  let origin = origin_from_node (Syn.Ast.ModuleDeclaration.as_node declaration) in
  let module_expr = Syn.Ast.ModuleDeclaration.Member.module_expr member in
  let parameters = functor_parameters_from_member origin member in
  let module_type =
    match Syn.Ast.ModuleDeclaration.Member.module_type member with
    | Some node -> module_type_ident_from_node node
    | None -> None
  in
  let alias =
    match module_expr with
    | Some node when Syn.SyntaxKind.(Syn.Ast.Node.kind node = PATH_MODULE_EXPR) ->
        ident_from_node_option node
    | _ -> None
  in
  let application =
    match module_expr with
    | Some node -> module_application_from_module_expr node
    | None -> None
  in
  let items =
    match module_expr with
    | Some node when Syn.SyntaxKind.(Syn.Ast.Node.kind node = STRUCT_MODULE_EXPR) ->
        build_structure_items_from_module_expr context node
    | _ ->
        let items =
          Syn.Ast.ModuleDeclaration.fold_structure_item
            declaration
            ~init:[]
            ~fn:(fun item items ->
              match build_structure_item_option context item with
              | Some item -> Continue (item :: items)
              | None -> Continue items)
        in
        List.reverse items
  in
  ({
    origin;
    name =
      Syn.Ast.ModuleDeclaration.Member.name member
      |> require_some origin "missing module declaration name"
      |> ident_from_syn_ident;
    recursive = Syn.Ast.ModuleDeclaration.is_recursive declaration;
    parameters;
    items;
    alias;
    module_type;
    application;
  }: module_declaration)

and build_module_declarations = fun context declaration ->
  let declarations =
    Syn.Ast.ModuleDeclaration.fold_member
      declaration
      ~init:[]
      ~fn:(fun member declarations ->
        Continue (build_module_declaration_member context member :: declarations))
  in
  List.reverse declarations

and build_module_type_declaration = fun context (declaration: Syn.Ast.ModuleTypeDeclaration.t) ->
  let origin = origin_from_node (Syn.Ast.ModuleTypeDeclaration.as_node declaration) in
  let items =
    Syn.Ast.ModuleTypeDeclaration.fold_signature_item
      declaration
      ~init:[]
      ~fn:(fun item items ->
        match build_signature_item_option context item with
        | Some item -> Continue (item :: items)
        | None -> Continue items)
  in
  ({
    origin;
    name =
      Syn.Ast.ModuleTypeDeclaration.name declaration
      |> require_some origin "missing module type declaration name"
      |> ident_from_syn_ident;
    items = List.reverse items;
  }: module_type_declaration)

and build_structure_item = fun context (item: Syn.Ast.StructureItem.t) ->
  let origin = origin_from_node (Syn.Ast.StructureItem.as_node item) in
  (
    match Syn.Ast.StructureItem.view item with
    | Let declaration ->
        make_structure_item origin (Let (build_let_declaration context declaration))
    | Expr expr_item -> (
        match Syn.Ast.ExprItem.expr expr_item with
        | Some expression ->
            make_structure_item origin (Expression (build_expression context expression))
        | None -> build_failed origin "missing structure expression"
      )
    | External declaration ->
        make_structure_item origin (External (build_external_declaration context declaration))
    | Type (Syn.Ast.TypeDeclarationItem declaration) ->
        make_structure_item origin (Type (build_type_declarations context declaration))
    | Type (Syn.Ast.TypeExtensionItem declaration) ->
        make_structure_item
          origin
          (TypeExtension (build_type_extension_declaration context declaration))
    | Exception declaration ->
        make_structure_item origin (Exception (build_exception_declaration context declaration))
    | Module declaration ->
        make_structure_item origin (Module (build_module_declarations context declaration))
    | ModuleType declaration ->
        make_structure_item origin (ModuleType (build_module_type_declaration context declaration))
    | Include declaration ->
        let ident =
          Syn.Ast.IncludeDeclaration.body_ident declaration
          |> require_some origin "missing include body"
          |> ident_from_syn_ident
        in
        make_structure_item origin (Include ident)
    | Attribute attribute ->
        let node = Syn.Ast.AttributeItem.as_node attribute in
        build_failed (origin_from_node node) (Syn.SyntaxKind.to_string (Syn.Ast.Node.kind node))
    | Extension extension ->
        let node = Syn.Ast.ExtensionItem.as_node extension in
        build_failed (origin_from_node node) (Syn.SyntaxKind.to_string (Syn.Ast.Node.kind node))
    | Open _ -> build_failed origin (Syn.SyntaxKind.to_string origin.kind)
    | Error node -> unsupported_node node (node_summary node)
    | Unknown node -> ((unsupported_node node (node_summary node)): structure_item)
  )

and build_structure_item_option = fun context (item: Syn.Ast.StructureItem.t) ->
  match Syn.Ast.StructureItem.view item with
  | Error _
  | Unknown _ -> None
  | _ -> Some (build_structure_item context item)

and build_signature_item = fun context (item: Syn.Ast.SignatureItem.t) ->
  let origin = origin_from_node (Syn.Ast.SignatureItem.as_node item) in
  (
    match Syn.Ast.SignatureItem.view item with
    | Value declaration ->
        make_signature_item origin (Value (build_value_declaration context declaration))
    | External declaration ->
        make_signature_item origin (External (build_external_declaration context declaration))
    | Type (Syn.Ast.TypeDeclarationItem declaration) ->
        make_signature_item origin (Type (build_type_declarations context declaration))
    | Type (Syn.Ast.TypeExtensionItem declaration) ->
        make_signature_item
          origin
          (TypeExtension (build_type_extension_declaration context declaration))
    | Exception declaration ->
        make_signature_item origin (Exception (build_exception_declaration context declaration))
    | Attribute attribute ->
        let node = Syn.Ast.AttributeItem.as_node attribute in
        build_failed (origin_from_node node) (Syn.SyntaxKind.to_string (Syn.Ast.Node.kind node))
    | Extension extension ->
        let node = Syn.Ast.ExtensionItem.as_node extension in
        build_failed (origin_from_node node) (Syn.SyntaxKind.to_string (Syn.Ast.Node.kind node))
    | Module _
    | ModuleType _
    | Open _
    | Include _ -> build_failed origin (Syn.SyntaxKind.to_string origin.kind)
    | Error node -> unsupported_node node (node_summary node)
    | Unknown node -> ((unsupported_node node (node_summary node)): signature_item)
  )

and build_signature_item_option = fun context (item: Syn.Ast.SignatureItem.t) ->
  match Syn.Ast.SignatureItem.view item with
  | Error _
  | Unknown _ -> None
  | _ -> Some (build_signature_item context item)

let from_parse_result = fun ~source:_ (parse_result: Syn.Parser.parse_result) ->
  try
    let context = make_build_context () in
    let source_file = Syn.Ast.SourceFile.make parse_result.tree in
    let ast =
      match Syn.Ast.SourceFile.view source_file with
      | Implementation implementation ->
          let items =
            Syn.Ast.Implementation.fold_item
              implementation
              ~init:[]
              ~fn:(fun item items ->
                match build_structure_item_option context item with
                | Some item -> Continue (item :: items)
                | None -> Continue items)
          in
          make_implementation
            (origin_from_node (Syn.Ast.SourceFile.as_node source_file))
            (List.reverse items)
      | Interface interface ->
          let items =
            Syn.Ast.Interface.fold_item
              interface
              ~init:[]
              ~fn:(fun item items ->
                match build_signature_item_option context item with
                | Some item -> Continue (item :: items)
                | None -> Continue items)
          in
          make_interface
            (origin_from_node (Syn.Ast.SourceFile.as_node source_file))
            (List.reverse items)
    in
    Ok ast
  with
  | Build_failed diagnostic -> Error [ diagnostic ]

let span_serializer =
  Serde.Ser.record
    (
      Serde.Ser.fields
        [
          Serde.Ser.field "start" Serde.Ser.int (fun (span: Syn.Span.t) -> span.start);
          Serde.Ser.field "end" Serde.Ser.int (fun (span: Syn.Span.t) -> span.end_);
        ]
    )

let origin_serializer =
  Serde.Ser.record
    (
      Serde.Ser.fields
        [
          Serde.Ser.field "span" span_serializer (fun (origin: origin) -> origin.span);
          Serde.Ser.field
            "kind"
            (Serde.Ser.contramap Syn.SyntaxKind.to_string Serde.Ser.string)
            (fun (origin: origin) -> origin.kind);
        ]
    )

let file_kind_serializer =
  Serde.Ser.variant
    [
      Serde.Ser.Variant.unit
        "Implementation"
        (fun __tmp1 ->
          match __tmp1 with
          | `Implementation -> true
          | `Interface -> false);
      Serde.Ser.Variant.unit
        "Interface"
        (fun __tmp1 ->
          match __tmp1 with
          | `Implementation -> false
          | `Interface -> true);
    ]

let file_kind = fun __tmp1 ->
  match __tmp1 with
  | Implementation _ -> `Implementation
  | Interface _ -> `Interface

let file_origin = fun __tmp1 ->
  match __tmp1 with
  | Implementation implementation -> implementation.origin
  | Interface interface -> interface.origin

let view_name = fun __tmp1 ->
  match __tmp1 with
  | Implementation _ -> "Implementation"
  | Interface _ -> "Interface"

let item_count = fun __tmp1 ->
  match __tmp1 with
  | Implementation implementation -> List.length implementation.items
  | Interface interface -> List.length interface.items

let serializer =
  Serde.Ser.record
    (
      Serde.Ser.fields
        [
          Serde.Ser.field "kind" file_kind_serializer (fun (file: t) -> file_kind file);
          Serde.Ser.field "origin" origin_serializer (fun (file: t) -> file_origin file);
          Serde.Ser.field "view" Serde.Ser.string (fun (file: t) -> view_name file);
          Serde.Ser.field "item_count" Serde.Ser.int (fun (file: t) -> item_count file);
        ]
    )

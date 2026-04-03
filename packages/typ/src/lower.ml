open Std
module Typ_diagnostic = Diagnostic
open Syn

type state = {
  mutable next_origin_id: int;
  mutable next_pattern_id: int;
  mutable next_expr_id: int;
  mutable next_binding_id: int;
  mutable next_item_id: int;
  mutable next_synthetic_name: int;
  mutable origins: SemanticTree.origin list;
  mutable patterns: SemanticTree.pattern_node list;
  mutable expressions: SemanticTree.expr_node list;
  mutable diagnostics: Typ_diagnostic.t list;
}

let path_text = fun path ->
  path
  |> Cst.Ident.segments
  |> List.map Cst.Token.text
  |> String.concat "."

let binding_name_of_pattern =
  let rec loop = function
    | Cst.Pattern.Identifier { name_token; _ } -> Some (Cst.Token.text name_token)
    | Cst.Pattern.Parenthesized { inner; _ } -> loop inner
    | Cst.Pattern.Typed { pattern; _ } -> loop pattern
    | _ -> None
  in
  loop

let make_state = fun () ->
  {
    next_origin_id = 0;
    next_pattern_id = 0;
    next_expr_id = 0;
    next_binding_id = 0;
    next_item_id = 0;
    next_synthetic_name = 0;
    origins = [];
    patterns = [];
    expressions = [];
    diagnostics = [];
  }

let add_diagnostic = fun (state: state) diagnostic ->
  state.diagnostics <- diagnostic :: state.diagnostics

let add_origin = fun (state: state) ~kind ~label syntax_node ->
  let origin_id = state.next_origin_id in
  let () = state.next_origin_id <- state.next_origin_id + 1 in
  let origin = {
    SemanticTree.origin_id;
    kind;
    span = Ceibo.Red.SyntaxNode.span syntax_node;
    label;
  } in
  let () = state.origins <- origin :: state.origins in
  origin_id

let add_pattern = fun (state: state) ~syntax_node ~label desc ->
  let origin_id = add_origin state ~kind:SemanticTree.Pattern ~label syntax_node in
  let pat_id = state.next_pattern_id in
  let () = state.next_pattern_id <- state.next_pattern_id + 1 in
  let node = { SemanticTree.pat_id; origin_id; desc } in
  let () = state.patterns <- node :: state.patterns in
  pat_id

let add_expr = fun (state: state) ~syntax_node ~label desc ->
  let origin_id = add_origin state ~kind:SemanticTree.Expr ~label syntax_node in
  let expr_id = state.next_expr_id in
  let () = state.next_expr_id <- state.next_expr_id + 1 in
  let node = { SemanticTree.expr_id; origin_id; desc } in
  let () = state.expressions <- node :: state.expressions in
  expr_id

let add_binding = fun (state: state) ~syntax_node ~name ~pattern_id ~value_id ~recursive ->
  let origin_id = add_origin state ~kind:SemanticTree.Item ~label:"binding" syntax_node in
  let binding_id = state.next_binding_id in
  let () = state.next_binding_id <- state.next_binding_id + 1 in
  { SemanticTree.binding_id; origin_id; name; pattern_id; value_id; recursive }

let add_item = fun (state: state) ~syntax_node item ->
  let origin_id = add_origin state ~kind:SemanticTree.Item ~label:"item" syntax_node in
  let item_id = state.next_item_id in
  let () = state.next_item_id <- state.next_item_id + 1 in
  match item with
  | `Value (bindings, recursive) ->
      SemanticTree.Value { item_id; origin_id; bindings; recursive }
  | `Unsupported summary ->
      SemanticTree.Unsupported { item_id; origin_id; summary }

let fresh_synthetic_name = fun (state: state) prefix ->
  let name = "$" ^ prefix ^ Int.to_string state.next_synthetic_name in
  let () = state.next_synthetic_name <- state.next_synthetic_name + 1 in
  name

let int_text = fun (integer: Cst.integer_constant) ->
  let sign =
    match integer.Cst.sign_token with
    | Some sign -> Cst.Token.text sign
    | None -> ""
  in
  sign ^ Cst.Token.text integer.literal_token

let unsupported_syntax_kind = fun syntax_node ->
  Cst.syntax_kind syntax_node

let supported_literal_subset = [
  Typ_diagnostic.IntLiteral;
  Typ_diagnostic.BoolLiteral;
  Typ_diagnostic.StringLiteral;
  Typ_diagnostic.UnitLiteral;
]

let lower_unsupported_pattern = fun (state: state) ?reason pattern syntax_kind ->
  let syntax_node = Cst.Pattern.syntax_node pattern in
  let () =
    add_diagnostic
      state
      (Typ_diagnostic.UnsupportedSyntax {
        syntax_kind;
        syntax_span = Ceibo.Red.SyntaxNode.span syntax_node;
        context = Typ_diagnostic.Pattern;
        recovery = Typ_diagnostic.RecoveryPattern;
        reason;
      })
  in
  add_pattern
    state
    ~syntax_node
    ~label:"unsupported_pattern"
    (SemanticTree.PUnsupported (SyntaxKind.to_string syntax_kind))

let lower_unsupported_expr = fun (state: state) ?reason expr syntax_kind ->
  let syntax_node = Cst.Expression.syntax_node expr in
  let () =
    add_diagnostic
      state
      (Typ_diagnostic.UnsupportedSyntax {
        syntax_kind;
        syntax_span = Ceibo.Red.SyntaxNode.span syntax_node;
        context = Typ_diagnostic.Expression;
        recovery = Typ_diagnostic.HoleExpression;
        reason;
      })
  in
  add_expr
    state
    ~syntax_node
    ~label:"unsupported_expression"
    (SemanticTree.EHole (SyntaxKind.to_string syntax_kind))

let rec lower_pattern = fun (state: state) pattern ->
  match pattern with
  | Cst.Pattern.Identifier { syntax_node; name_token; _ } ->
      add_pattern state ~syntax_node ~label:"identifier_pattern" (SemanticTree.PVar (Cst.Token.text name_token))
  | Cst.Pattern.Wildcard { syntax_node; _ } ->
      add_pattern state ~syntax_node ~label:"wildcard_pattern" SemanticTree.PWildcard
  | Cst.Pattern.Literal { syntax_node; literal; _ } -> (
      match literal with
      | Cst.PatternLiteral.Int integer ->
          add_pattern state ~syntax_node ~label:"int_pattern" (SemanticTree.PInt (int_text integer))
      | Cst.PatternLiteral.Bool { value; _ } ->
          add_pattern state ~syntax_node ~label:"bool_pattern" (SemanticTree.PBool value)
      | Cst.PatternLiteral.String string_ ->
          add_pattern state ~syntax_node ~label:"string_pattern" (SemanticTree.PString string_.contents)
      | Cst.PatternLiteral.Unit _ ->
          add_pattern state ~syntax_node ~label:"unit_pattern" SemanticTree.PUnit
      | _ ->
          lower_unsupported_pattern
            state
            ~reason:(Typ_diagnostic.LiteralOutsideSupportedSubset { supported_literals = supported_literal_subset })
            pattern
            (unsupported_syntax_kind (Cst.Pattern.syntax_node pattern)))
  | Cst.Pattern.Tuple { syntax_node; elements; _ } ->
      let element_ids =
        elements
        |> List.map (fun (element: Cst.tuple_pattern_element) -> lower_pattern state element.pattern)
      in
      add_pattern state ~syntax_node ~label:"tuple_pattern" (SemanticTree.PTuple element_ids)
  | Cst.Pattern.Parenthesized { inner; _ } ->
      lower_pattern state inner
  | Cst.Pattern.Typed { syntax_node; pattern; _ } ->
      let () =
        add_diagnostic
          state
          (Typ_diagnostic.IgnoredPatternTypeConstraint {
            constraint_span = Ceibo.Red.SyntaxNode.span syntax_node;
          })
      in
      lower_pattern state pattern
  | _ ->
      lower_unsupported_pattern state pattern (unsupported_syntax_kind (Cst.Pattern.syntax_node pattern))

let rec lower_parameter = fun (state: state) parameter ->
  match parameter with
  | Cst.Parameter.Positional { pattern; _ } ->
      lower_pattern state pattern
  | parameter ->
      let syntax_node = Cst.Parameter.syntax_node parameter in
      let () =
        add_diagnostic
          state
          (Typ_diagnostic.ParameterLoweredAsPositional {
            parameter_span = Ceibo.Red.SyntaxNode.span syntax_node;
          })
      in
      match Cst.Parameter.binding_pattern parameter with
      | Some pattern -> lower_pattern state pattern
      | None -> (
          match Cst.Parameter.name parameter with
          | Some name ->
              add_pattern state ~syntax_node ~label:"recovered_parameter" (SemanticTree.PVar name)
          | None ->
              add_pattern state ~syntax_node ~label:"unsupported_parameter" SemanticTree.PWildcard)

let synthetic_var_pattern = fun (state: state) syntax_node ~label ->
  let name = fresh_synthetic_name state label in
  let pat_id =
    add_pattern
      state
      ~syntax_node
      ~label:("synthetic_" ^ label ^ "_pattern")
      (SemanticTree.PVar name)
  in
  (name, pat_id)

let rec lower_match_cases = fun (state: state) cases ->
  List.map
    (fun (case: Cst.match_case) ->
      let pattern_id = lower_pattern state case.pattern in
      let body_id =
        match case.guard with
        | None -> lower_expr state case.body
        | Some _ ->
            let () =
              add_diagnostic
                state
                (Typ_diagnostic.IgnoredMatchGuard {
                  guard_span = Ceibo.Red.SyntaxNode.span case.syntax_node;
                })
            in
            lower_expr state case.body
      in
      { SemanticTree.pattern_id; body_id })
    cases

and lower_function_like = fun (state: state) ~syntax_node ~parameters ~body ->
  let parameter_ids = List.map (lower_parameter state) parameters in
  let body_id =
    match body with
    | `Expr expression ->
        lower_expr state expression
    | `Cases cases ->
        let (synthetic_name, synthetic_pattern_id) =
          synthetic_var_pattern state syntax_node ~label:"function_arg"
        in
        let argument_expr_id =
          add_expr state ~syntax_node ~label:"synthetic_function_argument" (SemanticTree.EVar synthetic_name)
        in
        let match_id =
          add_expr
            state
            ~syntax_node
            ~label:"function_match_body"
            (SemanticTree.EMatch (argument_expr_id, lower_match_cases state cases))
        in
        let parameter_ids = parameter_ids @ [ synthetic_pattern_id ] in
        add_expr state ~syntax_node ~label:"wrapped_fun" (SemanticTree.EFun (parameter_ids, match_id))
  in
  match body with
  | `Expr _ ->
      add_expr state ~syntax_node ~label:"fun_expression" (SemanticTree.EFun (parameter_ids, body_id))
  | `Cases _ ->
      body_id

and lower_binding_source = fun (state: state) ~syntax_node ~binding_pattern ~parameters ~value ~recursive ->
  let pattern_id = lower_pattern state binding_pattern in
  let value_id =
    match parameters with
    | [] -> lower_expr state value
    | _ -> lower_function_like state ~syntax_node ~parameters ~body:(`Expr value)
  in
  let name = binding_name_of_pattern binding_pattern in
  add_binding state ~syntax_node ~name ~pattern_id ~value_id ~recursive

and lower_let_binding_group = fun (state: state) let_binding ->
  let recursive = Cst.LetBinding.is_recursive let_binding in
  let bindings =
    let_binding
    :: Cst.LetBinding.and_bindings let_binding
    |> List.map (fun (binding: Cst.let_binding) ->
      lower_binding_source
        state
        ~syntax_node:(Cst.LetBinding.syntax_node binding)
        ~binding_pattern:(Cst.LetBinding.binding_pattern binding)
        ~parameters:(Cst.LetBinding.parameters binding)
        ~value:(Cst.LetBinding.value binding)
        ~recursive)
  in
  add_item state ~syntax_node:(Cst.LetBinding.syntax_node let_binding) (`Value (bindings, recursive))

and lower_let_expression_bindings = fun (state: state) (let_expression: Cst.let_expression) ->
  let recursive = Option.is_some let_expression.rec_token in
  let head =
    lower_binding_source
      state
      ~syntax_node:let_expression.syntax_node
      ~binding_pattern:let_expression.binding_pattern
      ~parameters:let_expression.parameters
      ~value:let_expression.bound_value
      ~recursive
  in
  let tail =
    match let_expression.and_binding with
    | None -> []
    | Some binding ->
        Cst.LetBinding.and_bindings binding
        |> fun rest -> binding :: rest
        |> List.map (fun (binding: Cst.let_binding) ->
          lower_binding_source
            state
            ~syntax_node:(Cst.LetBinding.syntax_node binding)
            ~binding_pattern:(Cst.LetBinding.binding_pattern binding)
            ~parameters:(Cst.LetBinding.parameters binding)
            ~value:(Cst.LetBinding.value binding)
            ~recursive)
  in
  head :: tail

and lower_apply = fun (state: state) expression ->
  let rec collect arguments = function
    | Cst.Expression.Apply { callee; argument = Positional argument; _ } ->
        let argument_id = lower_expr state argument in
        collect (argument_id :: arguments) callee
    | Cst.Expression.Apply { syntax_node; callee; _ } ->
        let () =
          add_diagnostic
            state
            (Typ_diagnostic.UnsupportedApplicationArgumentLabels {
              application_span = Ceibo.Red.SyntaxNode.span syntax_node;
            })
        in
        let callee_id = lower_expr state callee in
        (callee_id, List.rev arguments)
    | callee ->
        let callee_id = lower_expr state callee in
        (callee_id, List.rev arguments)
  in
  let syntax_node = Cst.Expression.syntax_node expression in
  let (callee_id, arguments) = collect [] expression in
  add_expr state ~syntax_node ~label:"apply_expression" (SemanticTree.EApply (callee_id, arguments))

and lower_infix = fun (state: state) (infix: Cst.infix_expression) ->
  let syntax_node = infix.syntax_node in
  let operator_name = Cst.InfixExpression.operator infix in
  let operator_id = add_expr state ~syntax_node ~label:"infix_operator" (SemanticTree.EVar operator_name) in
  let left_id = lower_expr state infix.left in
  let right_id = lower_expr state infix.right in
  add_expr state ~syntax_node ~label:"infix_expression" (SemanticTree.EApply (operator_id, [ left_id; right_id ]))

and lower_expr = fun (state: state) expression ->
  match expression with
  | Cst.Expression.Path { syntax_node; path; _ } ->
      add_expr state ~syntax_node ~label:"path_expression" (SemanticTree.EVar (path_text path))
  | Cst.Expression.Operator { syntax_node; operator_tokens; _ } ->
      let operator =
        operator_tokens
        |> List.map Cst.Token.text
        |> String.concat ""
      in
      add_expr state ~syntax_node ~label:"operator_expression" (SemanticTree.EVar operator)
  | Cst.Expression.Literal literal -> (
      match literal with
      | Cst.Literal.Int integer ->
          add_expr
            state
            ~syntax_node:integer.syntax_node
            ~label:"int_literal"
            (SemanticTree.EInt (int_text integer))
      | Cst.Literal.Bool { syntax_node; value; _ } ->
          add_expr state ~syntax_node ~label:"bool_literal" (SemanticTree.EBool value)
      | Cst.Literal.String string_ ->
          add_expr state ~syntax_node:string_.syntax_node ~label:"string_literal" (SemanticTree.EString string_.contents)
      | Cst.Literal.Unit { syntax_node; _ } ->
          add_expr state ~syntax_node ~label:"unit_literal" SemanticTree.EUnit
      | _ ->
          lower_unsupported_expr
            state
            ~reason:(Typ_diagnostic.LiteralOutsideSupportedSubset { supported_literals = supported_literal_subset })
            expression
            (unsupported_syntax_kind (Cst.Expression.syntax_node expression)))
  | Cst.Expression.Tuple { syntax_node; elements; _ } ->
      let element_ids = List.map (lower_expr state) elements in
      add_expr state ~syntax_node ~label:"tuple_expression" (SemanticTree.ETuple element_ids)
  | Cst.Expression.Parenthesized { inner; _ } ->
      lower_expr state inner
  | Cst.Expression.TypeAscription { syntax_node; expression; _ } ->
      let () =
        add_diagnostic
          state
          (Typ_diagnostic.IgnoredTypeAscription {
            ascription_span = Ceibo.Red.SyntaxNode.span syntax_node;
          })
      in
      lower_expr state expression
  | Cst.Expression.Polymorphic { syntax_node; expression; _ } ->
      let () =
        add_diagnostic
          state
          (Typ_diagnostic.IgnoredPolymorphicAnnotation {
            annotation_span = Ceibo.Red.SyntaxNode.span syntax_node;
          })
      in
      lower_expr state expression
  | Cst.Expression.Fun { syntax_node; parameters; body; _ } -> (
      match body with
      | Cst.Expression body ->
          lower_function_like state ~syntax_node ~parameters ~body:(`Expr body)
      | Cst.Cases body ->
          lower_function_like state ~syntax_node ~parameters ~body:(`Cases body.cases))
  | Cst.Expression.Function { syntax_node; cases; _ } ->
      lower_function_like state ~syntax_node ~parameters:[] ~body:(`Cases cases)
  | Cst.Expression.Apply _ ->
      lower_apply state expression
  | Cst.Expression.Infix infix ->
      lower_infix state infix
  | Cst.Expression.If { syntax_node; condition; then_branch; else_branch; _ } ->
      let condition_id = lower_expr state condition in
      let then_id = lower_expr state then_branch in
      let else_id =
        match else_branch with
        | Some else_branch -> lower_expr state else_branch
        | None -> add_expr state ~syntax_node ~label:"implicit_else_unit" SemanticTree.EUnit
      in
      add_expr state ~syntax_node ~label:"if_expression" (SemanticTree.EIf (condition_id, then_id, else_id))
  | Cst.Expression.Let let_expression ->
      let bindings = lower_let_expression_bindings state let_expression in
      let body_id = lower_expr state let_expression.body in
      add_expr state ~syntax_node:let_expression.syntax_node ~label:"let_expression" (SemanticTree.ELet (bindings, body_id))
  | Cst.Expression.Match { syntax_node; scrutinee; cases; _ } ->
      let scrutinee_id = lower_expr state scrutinee in
      let cases = lower_match_cases state cases in
      add_expr state ~syntax_node ~label:"match_expression" (SemanticTree.EMatch (scrutinee_id, cases))
  | Cst.Expression.Prefix { syntax_node; operator_token; operand; _ } ->
      let operator_id =
        add_expr state ~syntax_node ~label:"prefix_operator" (SemanticTree.EVar (Cst.Token.text operator_token))
      in
      let operand_id = lower_expr state operand in
      add_expr state ~syntax_node ~label:"prefix_expression" (SemanticTree.EApply (operator_id, [ operand_id ]))
  | _ ->
      lower_unsupported_expr state expression (unsupported_syntax_kind (Cst.Expression.syntax_node expression))

let lower_top_level_expression = fun (state: state) expression ->
  let syntax_node = Cst.Expression.syntax_node expression in
  let pattern_id = add_pattern state ~syntax_node ~label:"top_level_expression_pattern" SemanticTree.PWildcard in
  let value_id = lower_expr state expression in
  let binding = add_binding state ~syntax_node ~name:None ~pattern_id ~value_id ~recursive:false in
  add_item state ~syntax_node (`Value ([ binding ], false))

let lower_structure_item = fun (state: state) item ->
  match item with
  | Cst.StructureItem.LetBinding binding ->
      lower_let_binding_group state binding
  | Cst.StructureItem.Expression expression ->
      lower_top_level_expression state expression
  | item ->
      let syntax_node = Cst.StructureItem.syntax_node item in
      let syntax_kind = Cst.syntax_kind syntax_node in
      let summary = SyntaxKind.to_string syntax_kind in
      let () =
        add_diagnostic
          state
          (Typ_diagnostic.UnsupportedSyntax {
            syntax_kind;
            syntax_span = Ceibo.Red.SyntaxNode.span syntax_node;
            context = Typ_diagnostic.StructureItem;
            recovery = Typ_diagnostic.PlaceholderItem;
            reason = None;
          })
      in
      add_item state ~syntax_node (`Unsupported summary)

let lower_source_file = fun source_file ->
  let state = make_state () in
  let items =
    match source_file with
    | Cst.Implementation implementation ->
        implementation.items
        |> List.map (lower_structure_item state)
    | Cst.Interface interface ->
        let () =
          add_diagnostic
            state
            (Typ_diagnostic.UnsupportedInterfaceFile {
              interface_span = Ceibo.Red.SyntaxNode.span interface.syntax_node;
            })
        in
        []
  in
  {
    SemanticTree.items;
    patterns = List.rev state.patterns;
    expressions = List.rev state.expressions;
    origins = List.rev state.origins;
    diagnostics = List.rev state.diagnostics;
  }

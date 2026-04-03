open Std
open Std.Collections

include Fixme.Traversal

let rec binding_operator_bindings_of_chain = fun (binding: Syn.Cst.binding_operator_binding) ->
  binding :: (
    match binding.and_binding with
    | Some next -> binding_operator_bindings_of_chain next
    | None -> []
  )

type binding_site = {
  syntax_node: Syn.Cst.syntax_node;
  name_token: Syn.Cst.Token.t;
  is_function: bool;
}

let direct_non_trivia_nodes = fun node ->
  let open Syn.Ceibo.Red in
    SyntaxNode.children node |> List.filter_map
      (
        function
        | Node child when not (is_trivia (SyntaxNode.kind child)) -> Some child
        | _ -> None
      )

let is_expression_syntax_kind syntax_kind =
  match syntax_kind with
  | Syn.SyntaxKind.IDENT_EXPR
  | Syn.SyntaxKind.MODULE_PATH
  | Syn.SyntaxKind.OPERATOR_PATTERN
  | Syn.SyntaxKind.ATTRIBUTE_EXPR
  | Syn.SyntaxKind.EXTENSION_EXPR
  | Syn.SyntaxKind.OBJECT_EXPR
  | Syn.SyntaxKind.UNIT_LITERAL
  | Syn.SyntaxKind.METHOD_CALL_EXPR
  | Syn.SyntaxKind.NEW_EXPR
  | Syn.SyntaxKind.FIELD_ACCESS_EXPR
  | Syn.SyntaxKind.ARRAY_INDEX_EXPR
  | Syn.SyntaxKind.STRING_INDEX_EXPR
  | Syn.SyntaxKind.ASSIGN_EXPR
  | Syn.SyntaxKind.STRING_LITERAL
  | Syn.SyntaxKind.INT_LITERAL
  | Syn.SyntaxKind.FLOAT_LITERAL
  | Syn.SyntaxKind.CHAR_LITERAL
  | Syn.SyntaxKind.BOOL_LITERAL
  | Syn.SyntaxKind.ASSERT_EXPR
  | Syn.SyntaxKind.LAZY_EXPR
  | Syn.SyntaxKind.WHILE_EXPR
  | Syn.SyntaxKind.FOR_EXPR
  | Syn.SyntaxKind.APPLY_EXPR
  | Syn.SyntaxKind.POLY_VARIANT_EXPR
  | Syn.SyntaxKind.FIRST_CLASS_MODULE_EXPR
  | Syn.SyntaxKind.LET_MODULE_EXPR
  | Syn.SyntaxKind.LET_EXPR
  | Syn.SyntaxKind.LET_REC_EXPR
  | Syn.SyntaxKind.TYPED_EXPR
  | Syn.SyntaxKind.COERCE_EXPR
  | Syn.SyntaxKind.PREFIX_EXPR
  | Syn.SyntaxKind.INFIX_EXPR
  | Syn.SyntaxKind.SEQUENCE_EXPR
  | Syn.SyntaxKind.TUPLE_EXPR
  | Syn.SyntaxKind.LIST_EXPR
  | Syn.SyntaxKind.ARRAY_EXPR
  | Syn.SyntaxKind.RECORD_EXPR
  | Syn.SyntaxKind.RECORD_UPDATE_EXPR
  | Syn.SyntaxKind.UNREACHABLE_EXPR
  | Syn.SyntaxKind.OBJECT_UPDATE_EXPR
  | Syn.SyntaxKind.LOCAL_OPEN_EXPR
  | Syn.SyntaxKind.FUN_EXPR
  | Syn.SyntaxKind.FUNCTION_EXPR
  | Syn.SyntaxKind.MATCH_EXPR
  | Syn.SyntaxKind.TRY_EXPR
  | Syn.SyntaxKind.IF_EXPR
  | Syn.SyntaxKind.PAREN_EXPR -> true
  | _ -> false

let is_parameter_like_kind syntax_kind =
  match syntax_kind with
  | Syn.SyntaxKind.IDENT_PATTERN
  | Syn.SyntaxKind.WILDCARD_PATTERN
  | Syn.SyntaxKind.LITERAL_PATTERN
  | Syn.SyntaxKind.STRING_LITERAL
  | Syn.SyntaxKind.INT_LITERAL
  | Syn.SyntaxKind.FLOAT_LITERAL
  | Syn.SyntaxKind.CHAR_LITERAL
  | Syn.SyntaxKind.BOOL_LITERAL
  | Syn.SyntaxKind.UNIT_LITERAL
  | Syn.SyntaxKind.CONSTRUCTOR_PATTERN
  | Syn.SyntaxKind.TUPLE_PATTERN
  | Syn.SyntaxKind.LIST_PATTERN
  | Syn.SyntaxKind.ARRAY_PATTERN
  | Syn.SyntaxKind.CONS_PATTERN
  | Syn.SyntaxKind.RECORD_PATTERN
  | Syn.SyntaxKind.OR_PATTERN
  | Syn.SyntaxKind.AS_PATTERN
  | Syn.SyntaxKind.RANGE_PATTERN
  | Syn.SyntaxKind.TYPED_PATTERN
  | Syn.SyntaxKind.LAZY_PATTERN
  | Syn.SyntaxKind.EXCEPTION_PATTERN
  | Syn.SyntaxKind.PAREN_PATTERN
  | Syn.SyntaxKind.POLY_VARIANT_PATTERN
  | Syn.SyntaxKind.POLY_VARIANT_TYPE_PATTERN
  | Syn.SyntaxKind.LOCAL_OPEN_PATTERN
  | Syn.SyntaxKind.OPERATOR_PATTERN
  | Syn.SyntaxKind.FIRST_CLASS_MODULE_PATTERN
  | Syn.SyntaxKind.LABELED_PARAM
  | Syn.SyntaxKind.OPTIONAL_PARAM
  | Syn.SyntaxKind.OPTIONAL_PARAM_DEFAULT
  | Syn.SyntaxKind.LOCALLY_ABSTRACT_TYPE_PARAM -> true
  | _ -> false

let rec binding_name_token_from_pattern pattern =
  match pattern with
  | Syn.Cst.Pattern.Identifier { name_token; _ } -> Some name_token
  | Syn.Cst.Pattern.Alias { name_token; _ } -> Some name_token
  | Syn.Cst.Pattern.Typed { pattern; _ }
  | Syn.Cst.Pattern.Parenthesized { inner=pattern; _ } -> binding_name_token_from_pattern pattern
  | _ -> None

let expression_is_function = fun expr ->
  match Syn.Ceibo.Red.SyntaxNode.kind (Syn.Cst.Expression.syntax_node expr) with
  | Syn.SyntaxKind.FUN_EXPR
  | Syn.SyntaxKind.FUNCTION_EXPR -> true
  | _ -> false

let has_parameter_prefix = fun syntax_node ->
  let rec go nodes =
    match nodes with
    | [] -> false
    | node :: rest ->
        let kind = Syn.Ceibo.Red.SyntaxNode.kind node in
        if is_expression_syntax_kind kind then
          false
        else if is_parameter_like_kind kind then
          true
        else
          go rest
  in
  match direct_non_trivia_nodes syntax_node with
  | _binding_pattern :: rest -> go rest
  | [] -> false

let binding_site_of_let_binding = fun binding ->
  Syn.Cst.LetBinding.binding_name_token binding
  |> Option.map
    (fun name_token ->
      {
        syntax_node = Syn.Cst.LetBinding.syntax_node binding;
        name_token;
        is_function = Syn.Cst.LetBinding.is_function binding
      })

let binding_site_of_expression_let = fun ~syntax_node ~binding_pattern ~bound_value ->
  binding_name_token_from_pattern binding_pattern
  |> Option.map
    (fun name_token ->
      {
        syntax_node;
        name_token;
        is_function = has_parameter_prefix syntax_node || expression_is_function bound_value
      })

let rec binding_sites_of_module_expression module_expression =
  match module_expression with
  | Syn.Cst.ModuleExpression.Path _
  | Syn.Cst.ModuleExpression.Structure _
  | Syn.Cst.ModuleExpression.Extension _ -> []
  | Syn.Cst.ModuleExpression.Functor { body; _ } -> binding_sites_of_module_expression body
  | Syn.Cst.ModuleExpression.Apply { callee; argument; _ } -> binding_sites_of_module_expression callee
  @ binding_sites_of_module_expression argument
  | Syn.Cst.ModuleExpression.ApplyUnit { callee; _ } -> binding_sites_of_module_expression callee
  | Syn.Cst.ModuleExpression.Constraint { module_expression; _ }
  | Syn.Cst.ModuleExpression.Attribute { module_expression; _ } -> binding_sites_of_module_expression
    module_expression
  | Syn.Cst.ModuleExpression.ModuleUnpack { expression; _ } -> binding_sites_of_expression expression
  | Syn.Cst.ModuleExpression.Parenthesized { inner; _ } -> binding_sites_of_module_expression inner

and binding_sites_of_object_member object_member =
  match object_member with
  | Syn.Cst.ObjectMember.Method { body; _ } -> binding_sites_of_expression body
  | Syn.Cst.ObjectMember.Value { value; _ } -> binding_sites_of_expression value
  | Syn.Cst.ObjectMember.Inherit { expression; _ } -> binding_sites_of_expression expression
  | Syn.Cst.ObjectMember.Extension _ -> []
  | Syn.Cst.ObjectMember.Initializer { body; _ } -> binding_sites_of_expression body

and binding_sites_of_function_body function_body =
  match function_body with
  | Syn.Cst.Expression expression -> binding_sites_of_expression expression
  | Syn.Cst.Cases { cases; _ } -> cases |> List.concat_map binding_sites_of_match_case

and binding_sites_of_expression = fun expr ->
  match expr with
  | Syn.Cst.Expression.Path _
  | Syn.Cst.Expression.Operator _
  | Syn.Cst.Expression.Literal _
  | Syn.Cst.Expression.Unreachable _
  | Syn.Cst.Expression.Extension _
  | Syn.Cst.Expression.New _ -> []
  | Syn.Cst.Expression.Constructor { payload; _ } -> Option.to_list payload |> List.concat_map binding_sites_of_expression
  | Syn.Cst.Expression.Object { members; _ } -> members |> List.concat_map binding_sites_of_object_member
  | Syn.Cst.Expression.PolyVariant { payload; _ } -> Option.to_list payload |> List.concat_map binding_sites_of_expression
  | Syn.Cst.Expression.ModulePack { module_expression; _ } -> binding_sites_of_module_expression module_expression
  | Syn.Cst.Expression.LetModule { module_expression; body; _ } -> binding_sites_of_module_expression
    module_expression
  @ binding_sites_of_expression body
  | Syn.Cst.Expression.LetException { body; _ } -> binding_sites_of_expression body
  | Syn.Cst.Expression.Assert { asserted; _ } -> binding_sites_of_expression asserted
  | Syn.Cst.Expression.Lazy { body; _ } -> binding_sites_of_expression body
  | Syn.Cst.Expression.While { condition; body; _ } -> binding_sites_of_expression condition
  @ binding_sites_of_expression body
  | Syn.Cst.Expression.For { start_expr; end_expr; body; _ } -> binding_sites_of_expression start_expr
  @ binding_sites_of_expression end_expr
  @ binding_sites_of_expression body
  | Syn.Cst.Expression.Apply { callee; argument; _ } -> binding_sites_of_expression callee
  @ binding_sites_of_apply_argument argument
  | Syn.Cst.Expression.MethodCall { receiver; _ } -> binding_sites_of_expression receiver
  | Syn.Cst.Expression.Prefix { operand; _ } -> binding_sites_of_expression operand
  | Syn.Cst.Expression.FieldAccess { receiver; _ } -> binding_sites_of_expression receiver
  | Syn.Cst.Expression.Index { collection; index; _ } -> binding_sites_of_expression collection
  @ binding_sites_of_expression index
  | Syn.Cst.Expression.ObjectOverride { fields; _ } -> fields
  |> List.concat_map
    (fun (field: Syn.Cst.object_override_field) -> Option.to_list field.value |> List.concat_map binding_sites_of_expression)
  | Syn.Cst.Expression.InstanceVariableAssign { value; _ } -> binding_sites_of_expression value
  | Syn.Cst.Expression.FieldAssign { target; value; _ } -> binding_sites_of_expression
    (Syn.Cst.Expression.FieldAccess target)
  @ binding_sites_of_expression value
  | Syn.Cst.Expression.Assign { target; value; _ } -> binding_sites_of_expression target
  @ binding_sites_of_expression value
  | Syn.Cst.Expression.Infix { left; right; _ } -> binding_sites_of_expression left
  @ binding_sites_of_expression right
  | Syn.Cst.Expression.TypeAscription { expression; _ }
  | Syn.Cst.Expression.Polymorphic { expression; _ } -> binding_sites_of_expression expression
  | Syn.Cst.Expression.Sequence { expressions; _ } -> expressions |> List.concat_map binding_sites_of_expression
  | Syn.Cst.Expression.Tuple { elements; _ }
  | Syn.Cst.Expression.List { elements; _ }
  | Syn.Cst.Expression.Array { elements; _ } -> elements |> List.concat_map binding_sites_of_expression
  | Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Literal { fields; _ }) -> fields
  |> List.concat_map
    (fun (field: Syn.Cst.record_expression_field) -> binding_sites_of_expression field.value)
  | Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Update { base; fields; _ }) -> binding_sites_of_expression
    base
  @ (fields
  |> List.concat_map
    (fun (field: Syn.Cst.record_expression_field) -> binding_sites_of_expression field.value))
  | Syn.Cst.Expression.LocalOpen (Syn.Cst.LetOpen { body; _ })
  | Syn.Cst.Expression.LocalOpen (Syn.Cst.Delimited { body; _ }) -> binding_sites_of_expression body
  | Syn.Cst.Expression.Fun { body; _ } -> binding_sites_of_function_body body
  | Syn.Cst.Expression.Function { cases; _ } -> cases |> List.concat_map binding_sites_of_match_case
  | Syn.Cst.Expression.LetOperator { binding; body; _ } -> (binding_operator_bindings_of_chain binding
  |> List.concat_map
    (fun ({ bound_value; _ }: Syn.Cst.binding_operator_binding) -> binding_sites_of_expression bound_value))
  @ binding_sites_of_expression body
  | Syn.Cst.Expression.Let {
    syntax_node;
    binding_pattern;
    bound_value;
    and_binding;
    body;
    _
  } -> Option.to_list (binding_site_of_expression_let ~syntax_node ~binding_pattern ~bound_value)
  @ binding_sites_of_expression bound_value
  @ (Option.to_list and_binding |> List.concat_map binding_sites_of_let_binding)
  @ binding_sites_of_expression body
  | Syn.Cst.Expression.Match { scrutinee; cases; _ } -> binding_sites_of_expression scrutinee
  @ (cases |> List.concat_map binding_sites_of_match_case)
  | Syn.Cst.Expression.Try { body; cases; _ } -> binding_sites_of_expression body
  @ (cases |> List.concat_map binding_sites_of_match_case)
  | Syn.Cst.Expression.If { condition; then_branch; else_branch; _ } -> binding_sites_of_expression condition
  @ binding_sites_of_expression then_branch
  @ (Option.to_list else_branch |> List.concat_map binding_sites_of_expression)
  | Syn.Cst.Expression.Parenthesized { inner; _ } -> binding_sites_of_expression inner

and binding_sites_of_apply_argument argument =
  match argument with
  | Syn.Cst.Positional argument -> binding_sites_of_expression argument
  | Syn.Cst.Labeled { value; _ }
  | Syn.Cst.Optional { value; _ } -> Option.to_list value |> List.concat_map binding_sites_of_expression

and binding_sites_of_let_binding = fun binding ->
  Option.to_list (binding_site_of_let_binding binding)
  @ binding_sites_of_expression (Syn.Cst.LetBinding.value binding)

and binding_sites_of_match_case = fun ({ guard; body; _ }: Syn.Cst.match_case) ->
  (Option.to_list guard |> List.concat_map binding_sites_of_expression)
  @ binding_sites_of_expression body

and binding_sites_of_class_field class_field =
  match class_field with
  | Syn.Cst.ClassField.Method { definition; _ } -> (
      match definition with
      | Syn.Cst.ConcreteMethod { body; _ } -> binding_sites_of_expression body
      | Syn.Cst.VirtualMethod _ -> []
    )
  | Syn.Cst.ClassField.Value { definition; _ } -> (
      match definition with
      | Syn.Cst.ConcreteValue { value; _ } -> binding_sites_of_expression value
      | Syn.Cst.VirtualValue _ -> []
    )
  | Syn.Cst.ClassField.Inherit { class_expression; _ } ->
      binding_sites_of_class_expression class_expression
  | Syn.Cst.ClassField.Constraint _ ->
      []
  | Syn.Cst.ClassField.Initializer { body; _ } ->
      binding_sites_of_expression body
  | Syn.Cst.ClassField.Attribute { field; _ } ->
      binding_sites_of_class_field field
  | Syn.Cst.ClassField.Extension _ ->
      []

and binding_sites_of_class_expression class_expression =
  match class_expression with
  | Syn.Cst.ClassExpression.Path _
  | Syn.Cst.ClassExpression.Extension _ -> []
  | Syn.Cst.ClassExpression.Structure { fields; _ } -> fields |> List.concat_map binding_sites_of_class_field
  | Syn.Cst.ClassExpression.Fun { body; _ } -> binding_sites_of_class_expression body
  | Syn.Cst.ClassExpression.Apply { callee; argument; _ } -> binding_sites_of_class_expression callee
  @ binding_sites_of_apply_argument argument
  | Syn.Cst.ClassExpression.Let {
    syntax_node;
    binding_pattern;
    bound_value;
    and_binding;
    body;
    _
  } -> Option.to_list (binding_site_of_expression_let ~syntax_node ~binding_pattern ~bound_value)
  @ binding_sites_of_expression bound_value
  @ (Option.to_list and_binding |> List.concat_map binding_sites_of_let_binding)
  @ binding_sites_of_class_expression body
  | Syn.Cst.ClassExpression.Constraint { class_expression; _ } -> binding_sites_of_class_expression class_expression
  | Syn.Cst.ClassExpression.LocalOpen (Syn.Cst.LetOpen { body; _ })
  | Syn.Cst.ClassExpression.LocalOpen (Syn.Cst.Delimited { body; _ }) -> binding_sites_of_class_expression
    body
  | Syn.Cst.ClassExpression.Parenthesized { inner; _ } -> binding_sites_of_class_expression inner
  | Syn.Cst.ClassExpression.Attribute { class_expression; _ } -> binding_sites_of_class_expression class_expression

let binding_sites_of_structure_item structure_item =
  match structure_item with
  | Syn.Cst.StructureItem.LetBinding binding ->
      binding_sites_of_let_binding binding
  | Syn.Cst.StructureItem.Expression expr ->
      binding_sites_of_expression expr
  | Syn.Cst.StructureItem.ClassDeclaration { class_body; _ } ->
      [ class_body ] |> List.concat_map binding_sites_of_class_expression
  | Syn.Cst.StructureItem.ModuleDeclaration decl ->
      let rec binding_sites_of_module_structure decl =
        let rest =
          match Syn.Cst.ModuleStructure.next_and_declaration decl with
          | Some next -> binding_sites_of_module_structure next
          | None -> []
        in
        binding_sites_of_module_expression (Syn.Cst.ModuleStructure.module_expression decl) @ rest
      in
      binding_sites_of_module_structure decl
  | Syn.Cst.StructureItem.OpenStatement stmt -> (
      match Syn.Cst.OpenStatement.module_expression stmt with
      | Some expr -> binding_sites_of_module_expression expr
      | None -> []
    )
  | Syn.Cst.StructureItem.IncludeStatement { target; _ } -> (
      match target with
      | Syn.Cst.ModuleExpression expr -> binding_sites_of_module_expression expr
      | Syn.Cst.ModuleType _ -> []
    )
  | Syn.Cst.StructureItem.TypeDeclaration _
  | Syn.Cst.StructureItem.TypeExtension _
  | Syn.Cst.StructureItem.Attribute _
  | Syn.Cst.StructureItem.Extension _
  | Syn.Cst.StructureItem.ClassTypeDeclaration _
  | Syn.Cst.StructureItem.ModuleTypeDeclaration _
  | Syn.Cst.StructureItem.ExternalDeclaration _
  | Syn.Cst.StructureItem.ExceptionDeclaration _
  | Syn.Cst.StructureItem.Docstring _
  | Syn.Cst.StructureItem.Comment _ ->
      []

open Std
open Std.Collections

type red_tree = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node

type red_node = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node

type red_token = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_token

type red_element = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_element

let rec binding_operator_bindings_of_chain = fun (binding: Syn.Cst.binding_operator_binding) ->
  binding :: (
    match binding.and_binding with
    | Some next -> binding_operator_bindings_of_chain next
    | None -> []
  )

let is_trivia = fun kind ->
  let open Syn.SyntaxKind in kind = WHITESPACE || kind = COMMENT || kind = DOCSTRING

(* Core traversal that collects elements *)

let traverse = fun ~visit_node ~visit_token tree ->
  let open Syn.Ceibo.Red in
    let rec go elem acc =
      match elem with
      | Node n ->
          let acc = visit_node n acc in
          SyntaxNode.fold_children n acc
            (fun acc child ->
              yield ();
              go child acc)
      | Token t -> visit_token t acc
    in
    go (Node tree) []

(* Find nodes matching predicate *)

let find_nodes = fun predicate tree ->
  traverse
    ~visit_node:(fun node acc ->
      if predicate node then
        node :: acc
      else
        acc)
    ~visit_token:(fun _token acc -> acc)
    tree |> List.rev

(* Find nodes by kind *)

let find_by_kind = fun kind tree ->
  find_nodes (fun node -> let open Syn.Ceibo.Red in SyntaxNode.kind node = kind) tree

(* Find nodes by multiple kinds *)

let find_by_kinds = fun kinds tree ->
  find_nodes (fun node -> let open Syn.Ceibo.Red in List.mem (SyntaxNode.kind node) kinds) tree

(* Find tokens matching predicate *)

let find_tokens = fun predicate tree ->
  traverse ~visit_node:(fun _node acc -> acc)
    ~visit_token:(fun token acc ->
      if predicate token then
        token :: acc
      else
        acc)
    tree |> List.rev

(* First non-trivia child *)

let first_non_trivia_child = fun node ->
  let open Syn.Ceibo.Red in
    SyntaxNode.children node |> List.find_opt
      (
        function
        | Token t when is_trivia (SyntaxToken.kind t) -> false
        | _ -> true
      )

(* First non-trivia token *)

let first_non_trivia_token = fun node ->
  match first_non_trivia_child node with
  | Some (Syn.Ceibo.Red.Token t) -> Some t
  | _ -> None

(* Visitor pattern *)

type 'acc visitor = {
  visit_node: red_node -> 'acc -> 'acc;
  visit_token: red_token -> 'acc -> 'acc;
}

let fold = fun visitor init tree ->
  let open Syn.Ceibo.Red in
    let rec go elem acc =
      match elem with
      | Node n ->
          let acc = visitor.visit_node n acc in
          SyntaxNode.fold_children n acc
            (fun acc child ->
              yield ();
              go child acc)
      | Token t -> visitor.visit_token t acc
    in
    go (Node tree) init

let rec let_bindings_of_module_expression = fun expr ->
  match expr with
  | Syn.Cst.ModuleExpression.Path _
  | Syn.Cst.ModuleExpression.Structure _
  | Syn.Cst.ModuleExpression.Extension _ -> []
  | Syn.Cst.ModuleExpression.Functor { body; _ } -> let_bindings_of_module_expression body
  | Syn.Cst.ModuleExpression.Apply { callee; argument; _ } -> let_bindings_of_module_expression callee
  @ let_bindings_of_module_expression argument
  | Syn.Cst.ModuleExpression.ApplyUnit { callee; _ } -> let_bindings_of_module_expression callee
  | Syn.Cst.ModuleExpression.Constraint { module_expression; _ }
  | Syn.Cst.ModuleExpression.Attribute { module_expression; _ } -> let_bindings_of_module_expression
    module_expression
  | Syn.Cst.ModuleExpression.ModuleUnpack { expression; _ } -> let_bindings_of_expression expression
  | Syn.Cst.ModuleExpression.Parenthesized { inner; _ } -> let_bindings_of_module_expression inner

and let_bindings_of_object_member = function
  | Syn.Cst.ObjectMember.Method { body; _ } -> let_bindings_of_expression body
  | Syn.Cst.ObjectMember.Value { value; _ } -> let_bindings_of_expression value
  | Syn.Cst.ObjectMember.Inherit { expression; _ } -> let_bindings_of_expression expression
  | Syn.Cst.ObjectMember.Extension _ -> []
  | Syn.Cst.ObjectMember.Initializer { body; _ } -> let_bindings_of_expression body

and let_bindings_of_expression = fun expr ->
  match expr with
  | Syn.Cst.Expression.Path _
  | Syn.Cst.Expression.Operator _
  | Syn.Cst.Expression.Literal _
  | Syn.Cst.Expression.Unreachable _
  | Syn.Cst.Expression.Extension _
  | Syn.Cst.Expression.New _ -> []
  | Syn.Cst.Expression.Constructor { payload; _ } -> Option.to_list payload |> List.concat_map let_bindings_of_expression
  | Syn.Cst.Expression.Object { members; _ } -> members |> List.concat_map let_bindings_of_object_member
  | Syn.Cst.Expression.PolyVariant { payload; _ } -> Option.to_list payload |> List.concat_map let_bindings_of_expression
  | Syn.Cst.Expression.ModulePack { module_expression; _ } -> let_bindings_of_module_expression module_expression
  | Syn.Cst.Expression.LetModule { module_expression; body; _ } -> let_bindings_of_module_expression
    module_expression
  @ let_bindings_of_expression body
  | Syn.Cst.Expression.LetException { body; _ } -> let_bindings_of_expression body
  | Syn.Cst.Expression.Assert { asserted; _ } -> let_bindings_of_expression asserted
  | Syn.Cst.Expression.Lazy { body; _ } -> let_bindings_of_expression body
  | Syn.Cst.Expression.While { condition; body; _ } -> let_bindings_of_expression condition
  @ let_bindings_of_expression body
  | Syn.Cst.Expression.For { start_expr; end_expr; body; _ } -> let_bindings_of_expression start_expr
  @ let_bindings_of_expression end_expr
  @ let_bindings_of_expression body
  | Syn.Cst.Expression.Apply { callee; argument; _ } -> let_bindings_of_expression callee
  @ let_bindings_of_apply_argument argument
  | Syn.Cst.Expression.MethodCall { receiver; _ } -> let_bindings_of_expression receiver
  | Syn.Cst.Expression.Prefix { operand; _ } -> let_bindings_of_expression operand
  | Syn.Cst.Expression.FieldAccess { receiver; _ } -> let_bindings_of_expression receiver
  | Syn.Cst.Expression.Index { collection; index; _ } -> let_bindings_of_expression collection
  @ let_bindings_of_expression index
  | Syn.Cst.Expression.ObjectOverride { fields; _ } -> fields
  |> List.concat_map
    (fun (field: Syn.Cst.object_override_field) -> Option.to_list field.value |> List.concat_map let_bindings_of_expression)
  | Syn.Cst.Expression.InstanceVariableAssign { value; _ } -> let_bindings_of_expression value
  | Syn.Cst.Expression.FieldAssign { target; value; _ } -> let_bindings_of_expression
    (Syn.Cst.Expression.FieldAccess target)
  @ let_bindings_of_expression value
  | Syn.Cst.Expression.Assign { target; value; _ } -> let_bindings_of_expression target
  @ let_bindings_of_expression value
  | Syn.Cst.Expression.Infix { left; right; _ } -> let_bindings_of_expression left
  @ let_bindings_of_expression right
  | Syn.Cst.Expression.TypeAscription { expression; _ }
  | Syn.Cst.Expression.Polymorphic { expression; _ } -> let_bindings_of_expression expression
  | Syn.Cst.Expression.Sequence { expressions; _ } -> expressions |> List.concat_map let_bindings_of_expression
  | Syn.Cst.Expression.Tuple { elements; _ }
  | Syn.Cst.Expression.List { elements; _ }
  | Syn.Cst.Expression.Array { elements; _ } -> elements |> List.concat_map let_bindings_of_expression
  | Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Literal { fields; _ }) -> fields
  |> List.concat_map
    (fun (field: Syn.Cst.record_expression_field) -> let_bindings_of_expression field.value)
  | Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Update { base; fields; _ }) -> let_bindings_of_expression
    base
  @ (fields
  |> List.concat_map
    (fun (field: Syn.Cst.record_expression_field) -> let_bindings_of_expression field.value))
  | Syn.Cst.Expression.LocalOpen (Syn.Cst.LetOpen { body; _ })
  | Syn.Cst.Expression.LocalOpen (Syn.Cst.Delimited { body; _ }) -> let_bindings_of_expression body
  | Syn.Cst.Expression.Fun { body; _ } -> let_bindings_of_function_body body
  | Syn.Cst.Expression.Function { cases; _ } -> cases |> List.concat_map let_bindings_of_match_case
  | Syn.Cst.Expression.LetOperator { binding; body; _ } -> (binding_operator_bindings_of_chain binding
  |> List.concat_map
    (fun ({ bound_value; _ }: Syn.Cst.binding_operator_binding) -> let_bindings_of_expression bound_value))
  @ let_bindings_of_expression body
  | Syn.Cst.Expression.Let { bound_value; and_binding; body; _ } -> let_bindings_of_expression bound_value
  @ (Option.to_list and_binding |> List.concat_map let_bindings_of_let_binding)
  @ let_bindings_of_expression body
  | Syn.Cst.Expression.Match { scrutinee; cases; _ } -> let_bindings_of_expression scrutinee
  @ (cases |> List.concat_map let_bindings_of_match_case)
  | Syn.Cst.Expression.Try { body; cases; _ } -> let_bindings_of_expression body
  @ (cases |> List.concat_map let_bindings_of_match_case)
  | Syn.Cst.Expression.If { condition; then_branch; else_branch; _ } -> let_bindings_of_expression condition
  @ let_bindings_of_expression then_branch
  @ (Option.to_list else_branch |> List.concat_map let_bindings_of_expression)
  | Syn.Cst.Expression.Parenthesized { inner; _ } -> let_bindings_of_expression inner

and let_bindings_of_function_body = function
  | Syn.Cst.Expression expression -> let_bindings_of_expression expression
  | Syn.Cst.Cases { cases; _ } -> cases |> List.concat_map let_bindings_of_match_case

and let_bindings_of_apply_argument = function
  | Syn.Cst.Positional argument -> let_bindings_of_expression argument
  | Syn.Cst.Labeled { value; _ }
  | Syn.Cst.Optional { value; _ } -> Option.to_list value |> List.concat_map let_bindings_of_expression

and let_bindings_of_let_binding = fun binding ->
  binding :: let_bindings_of_expression (Syn.Cst.LetBinding.value binding)

and let_bindings_of_match_case = fun ({ guard; body; _ }: Syn.Cst.match_case) ->
  (Option.to_list guard |> List.concat_map let_bindings_of_expression) @ let_bindings_of_expression body

and let_bindings_of_class_field = function
  | Syn.Cst.ClassField.Method { definition=Syn.Cst.ConcreteMethod { body; _ }; _ } -> let_bindings_of_expression
    body
  | Syn.Cst.ClassField.Method { definition=Syn.Cst.VirtualMethod _; _ } -> []
  | Syn.Cst.ClassField.Value { definition=Syn.Cst.ConcreteValue { value; _ }; _ } -> let_bindings_of_expression
    value
  | Syn.Cst.ClassField.Value { definition=Syn.Cst.VirtualValue _; _ } -> []
  | Syn.Cst.ClassField.Inherit { class_expression; _ } -> let_bindings_of_class_expression class_expression
  | Syn.Cst.ClassField.Constraint _ -> []
  | Syn.Cst.ClassField.Initializer { body; _ } -> let_bindings_of_expression body
  | Syn.Cst.ClassField.Attribute { field; _ } -> let_bindings_of_class_field field
  | Syn.Cst.ClassField.Extension _ -> []

and let_bindings_of_class_expression = function
  | Syn.Cst.ClassExpression.Path _
  | Syn.Cst.ClassExpression.Extension _ -> []
  | Syn.Cst.ClassExpression.Structure { fields; _ } -> fields |> List.concat_map let_bindings_of_class_field
  | Syn.Cst.ClassExpression.Fun { body; _ } -> let_bindings_of_class_expression body
  | Syn.Cst.ClassExpression.Apply { callee; argument; _ } -> let_bindings_of_class_expression callee
  @ let_bindings_of_apply_argument argument
  | Syn.Cst.ClassExpression.Let { bound_value; and_binding; body; _ } -> let_bindings_of_expression bound_value
  @ (Option.to_list and_binding |> List.concat_map let_bindings_of_let_binding)
  @ let_bindings_of_class_expression body
  | Syn.Cst.ClassExpression.Constraint { class_expression; _ } -> let_bindings_of_class_expression class_expression
  | Syn.Cst.ClassExpression.LocalOpen (Syn.Cst.LetOpen { body; _ })
  | Syn.Cst.ClassExpression.LocalOpen (Syn.Cst.Delimited { body; _ }) -> let_bindings_of_class_expression
    body
  | Syn.Cst.ClassExpression.Parenthesized { inner; _ } -> let_bindings_of_class_expression inner
  | Syn.Cst.ClassExpression.Attribute { class_expression; _ } -> let_bindings_of_class_expression class_expression

let let_bindings_of_structure_item = fun item ->
  match item with
  | Syn.Cst.StructureItem.LetBinding binding ->
      let_bindings_of_let_binding binding
  | Syn.Cst.StructureItem.Expression expr ->
      let_bindings_of_expression expr
  | Syn.Cst.StructureItem.ClassDeclaration { class_body; _ } ->
      [ class_body ] |> List.concat_map let_bindings_of_class_expression
  | Syn.Cst.StructureItem.ModuleDeclaration decl ->
      let rec let_bindings_of_module_structure decl =
        let rest =
          match Syn.Cst.ModuleStructure.next_and_declaration decl with
          | Some next -> let_bindings_of_module_structure next
          | None -> []
        in
        let_bindings_of_module_expression (Syn.Cst.ModuleStructure.module_expression decl) @ rest
      in
      let_bindings_of_module_structure decl
  | Syn.Cst.StructureItem.OpenStatement stmt -> (
      match Syn.Cst.OpenStatement.module_expression stmt with
      | Some expr -> let_bindings_of_module_expression expr
      | None -> []
    )
  | Syn.Cst.StructureItem.IncludeStatement { target; _ } -> (
      match target with
      | Syn.Cst.ModuleExpression expr -> let_bindings_of_module_expression expr
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

let rec expressions_of_expression = fun expr ->
  let nested =
    match expr with
    | Syn.Cst.Expression.Path _
    | Syn.Cst.Expression.Operator _
    | Syn.Cst.Expression.Literal _
    | Syn.Cst.Expression.Unreachable _
    | Syn.Cst.Expression.Extension _
    | Syn.Cst.Expression.New _ -> []
    | Syn.Cst.Expression.Constructor { payload; _ } -> Option.to_list payload |> List.concat_map expressions_of_expression
    | Syn.Cst.Expression.Object { members; _ } ->
        members |> List.concat_map
          (
            function
            | Syn.Cst.ObjectMember.Method { body; _ } -> expressions_of_expression body
            | Syn.Cst.ObjectMember.Value { value; _ } -> expressions_of_expression value
            | Syn.Cst.ObjectMember.Inherit { expression; _ } -> expressions_of_expression expression
            | Syn.Cst.ObjectMember.Extension _ -> []
            | Syn.Cst.ObjectMember.Initializer { body; _ } -> expressions_of_expression body
          )
    | Syn.Cst.Expression.PolyVariant { payload; _ } -> Option.to_list payload |> List.concat_map expressions_of_expression
    | Syn.Cst.Expression.ModulePack _ -> []
    | Syn.Cst.Expression.LetModule { body; _ } -> expressions_of_expression body
    | Syn.Cst.Expression.LetException { body; _ } -> expressions_of_expression body
    | Syn.Cst.Expression.Assert { asserted; _ } -> expressions_of_expression asserted
    | Syn.Cst.Expression.Lazy { body; _ } -> expressions_of_expression body
    | Syn.Cst.Expression.While { condition; body; _ } -> expressions_of_expression condition
    @ expressions_of_expression body
    | Syn.Cst.Expression.For { start_expr; end_expr; body; _ } -> expressions_of_expression start_expr
    @ expressions_of_expression end_expr
    @ expressions_of_expression body
    | Syn.Cst.Expression.Apply { callee; argument; _ } -> expressions_of_expression callee
    @ expressions_of_apply_argument argument
    | Syn.Cst.Expression.MethodCall { receiver; _ } -> expressions_of_expression receiver
    | Syn.Cst.Expression.Prefix { operand; _ } -> expressions_of_expression operand
    | Syn.Cst.Expression.FieldAccess { receiver; _ } -> expressions_of_expression receiver
    | Syn.Cst.Expression.Index { collection; index; _ } -> expressions_of_expression collection
    @ expressions_of_expression index
    | Syn.Cst.Expression.ObjectOverride { fields; _ } -> fields
    |> List.concat_map
      (fun (field: Syn.Cst.object_override_field) -> Option.to_list field.value |> List.concat_map expressions_of_expression)
    | Syn.Cst.Expression.InstanceVariableAssign { value; _ } -> expressions_of_expression value
    | Syn.Cst.Expression.FieldAssign { target; value; _ } -> expressions_of_expression
      (Syn.Cst.Expression.FieldAccess target)
    @ expressions_of_expression value
    | Syn.Cst.Expression.Assign { target; value; _ } -> expressions_of_expression target
    @ expressions_of_expression value
    | Syn.Cst.Expression.Infix { left; right; _ } -> expressions_of_expression left
    @ expressions_of_expression right
    | Syn.Cst.Expression.TypeAscription { expression; _ }
    | Syn.Cst.Expression.Polymorphic { expression; _ } -> expressions_of_expression expression
    | Syn.Cst.Expression.Sequence { expressions; _ } -> expressions |> List.concat_map expressions_of_expression
    | Syn.Cst.Expression.Tuple { elements; _ }
    | Syn.Cst.Expression.List { elements; _ }
    | Syn.Cst.Expression.Array { elements; _ } -> elements |> List.concat_map expressions_of_expression
    | Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Literal { fields; _ }) -> fields
    |> List.concat_map
      (fun (field: Syn.Cst.record_expression_field) -> expressions_of_expression field.value)
    | Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Update { base; fields; _ }) -> expressions_of_expression
      base
    @ (fields
    |> List.concat_map
      (fun (field: Syn.Cst.record_expression_field) -> expressions_of_expression field.value))
    | Syn.Cst.Expression.LocalOpen (Syn.Cst.LetOpen { body; _ })
    | Syn.Cst.Expression.LocalOpen (Syn.Cst.Delimited { body; _ }) -> expressions_of_expression body
    | Syn.Cst.Expression.Fun { body; _ } -> expressions_of_function_body body
    | Syn.Cst.Expression.Function { cases; _ } -> cases |> List.concat_map expressions_of_match_case
    | Syn.Cst.Expression.LetOperator { binding; body; _ } -> (binding_operator_bindings_of_chain binding
    |> List.concat_map
      (fun ({ bound_value; _ }: Syn.Cst.binding_operator_binding) -> expressions_of_expression bound_value))
    @ expressions_of_expression body
    | Syn.Cst.Expression.Let { bound_value; and_binding; body; _ } -> expressions_of_expression bound_value
    @ (Option.to_list and_binding |> List.concat_map expressions_of_let_binding)
    @ expressions_of_expression body
    | Syn.Cst.Expression.Match { scrutinee; cases; _ } -> expressions_of_expression scrutinee
    @ (cases |> List.concat_map expressions_of_match_case)
    | Syn.Cst.Expression.Try { body; cases; _ } -> expressions_of_expression body
    @ (cases |> List.concat_map expressions_of_match_case)
    | Syn.Cst.Expression.If { condition; then_branch; else_branch; _ } -> expressions_of_expression condition
    @ expressions_of_expression then_branch
    @ (Option.to_list else_branch |> List.concat_map expressions_of_expression)
    | Syn.Cst.Expression.Parenthesized { inner; _ } -> expressions_of_expression inner
  in
  expr :: nested

and expressions_of_function_body = function
  | Syn.Cst.Expression expression -> expressions_of_expression expression
  | Syn.Cst.Cases { cases; _ } -> cases |> List.concat_map expressions_of_match_case

and expressions_of_apply_argument = function
  | Syn.Cst.Positional argument -> expressions_of_expression argument
  | Syn.Cst.Labeled { value; _ }
  | Syn.Cst.Optional { value; _ } -> Option.to_list value |> List.concat_map expressions_of_expression

and expressions_of_let_binding = fun binding ->
  expressions_of_expression (Syn.Cst.LetBinding.value binding)

and expressions_of_match_case = fun ({ guard; body; _ }: Syn.Cst.match_case) ->
  (Option.to_list guard |> List.concat_map expressions_of_expression) @ expressions_of_expression body

and expressions_of_class_field = function
  | Syn.Cst.ClassField.Method { definition=Syn.Cst.ConcreteMethod { body; _ }; _ } -> expressions_of_expression
    body
  | Syn.Cst.ClassField.Method { definition=Syn.Cst.VirtualMethod _; _ } -> []
  | Syn.Cst.ClassField.Value { definition=Syn.Cst.ConcreteValue { value; _ }; _ } -> expressions_of_expression
    value
  | Syn.Cst.ClassField.Value { definition=Syn.Cst.VirtualValue _; _ } -> []
  | Syn.Cst.ClassField.Inherit { class_expression; _ } -> expressions_of_class_expression class_expression
  | Syn.Cst.ClassField.Constraint _ -> []
  | Syn.Cst.ClassField.Initializer { body; _ } -> expressions_of_expression body
  | Syn.Cst.ClassField.Attribute { field; _ } -> expressions_of_class_field field
  | Syn.Cst.ClassField.Extension _ -> []

and expressions_of_class_expression = function
  | Syn.Cst.ClassExpression.Path _
  | Syn.Cst.ClassExpression.Extension _ -> []
  | Syn.Cst.ClassExpression.Structure { fields; _ } -> fields |> List.concat_map expressions_of_class_field
  | Syn.Cst.ClassExpression.Fun { body; _ } -> expressions_of_class_expression body
  | Syn.Cst.ClassExpression.Apply { callee; argument; _ } -> expressions_of_class_expression callee
  @ expressions_of_apply_argument argument
  | Syn.Cst.ClassExpression.Let { bound_value; and_binding; body; _ } -> expressions_of_expression bound_value
  @ (Option.to_list and_binding |> List.concat_map expressions_of_let_binding)
  @ expressions_of_class_expression body
  | Syn.Cst.ClassExpression.Constraint { class_expression; _ } -> expressions_of_class_expression class_expression
  | Syn.Cst.ClassExpression.LocalOpen (Syn.Cst.LetOpen { body; _ })
  | Syn.Cst.ClassExpression.LocalOpen (Syn.Cst.Delimited { body; _ }) -> expressions_of_class_expression
    body
  | Syn.Cst.ClassExpression.Parenthesized { inner; _ } -> expressions_of_class_expression inner
  | Syn.Cst.ClassExpression.Attribute { class_expression; _ } -> expressions_of_class_expression class_expression

let expressions_of_structure_item = fun item ->
  match item with
  | Syn.Cst.StructureItem.TypeDeclaration _
  | Syn.Cst.StructureItem.TypeExtension _
  | Syn.Cst.StructureItem.ModuleDeclaration _
  | Syn.Cst.StructureItem.ModuleTypeDeclaration _
  | Syn.Cst.StructureItem.OpenStatement _
  | Syn.Cst.StructureItem.ExternalDeclaration _
  | Syn.Cst.StructureItem.IncludeStatement _
  | Syn.Cst.StructureItem.ExceptionDeclaration _
  | Syn.Cst.StructureItem.ClassTypeDeclaration _
  | Syn.Cst.StructureItem.Attribute _
  | Syn.Cst.StructureItem.Extension _
  | Syn.Cst.StructureItem.Docstring _
  | Syn.Cst.StructureItem.Comment _ -> []
  | Syn.Cst.StructureItem.LetBinding binding -> expressions_of_let_binding binding
  | Syn.Cst.StructureItem.Expression expr -> expressions_of_expression expr
  | Syn.Cst.StructureItem.ClassDeclaration { class_body; _ } -> [ class_body ] |> List.concat_map expressions_of_class_expression

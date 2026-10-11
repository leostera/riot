open Std

type context = {
  source_file: Ast.t;
  infer_result: Infer.infer_result;
}

module Node = struct
  type kind =
    | SourceFile of Ast.t
    | StructureItem of Ast.structure_item
    | SignatureItem of Ast.signature_item
    | LetDeclaration of Ast.let_declaration
    | LetBinding of Ast.let_binding
    | ValueDeclaration of Ast.value_declaration
    | ExternalDeclaration of Ast.external_declaration
    | TypeDeclaration of Ast.type_declaration
    | TypeDefinition of Ast.type_definition
    | TypeConstructor of Ast.type_constructor
    | TypeExtensionDeclaration of Ast.type_extension_declaration
    | RecordFieldDeclaration of Ast.record_field_declaration
    | RecordExpressionField of Ast.record_expression_field
    | RecordPatternField of Ast.record_pattern_field
    | ExceptionDeclaration of Ast.exception_declaration
    | ModuleDeclaration of Ast.module_declaration
    | ModuleTypeDeclaration of Ast.module_type_declaration
    | FunctorParameter of Ast.functor_parameter
    | ModuleUnpack of Ast.module_unpack
    | PackageType of Ast.package_type
    | PackageTypeConstraint of Ast.package_type_constraint
    | Parameter of Ast.parameter
    | Argument of Ast.argument
    | MatchCase of Ast.match_case
    | Pattern of Ast.pattern
    | Expression of Ast.expression
    | CoreType of Ast.core_type
    | PolyVariantTypeField of Ast.poly_variant_type_field

  type t = {
    kind: kind;
    parent: t option;
  }

  let kind t = t.kind

  let parent t = t.parent

  let span t =
    match t.kind with
    | SourceFile (Ast.Implementation implementation) -> implementation.origin.span
    | SourceFile (Ast.Interface interface) -> interface.origin.span
    | StructureItem item -> item.origin.span
    | SignatureItem item -> item.origin.span
    | LetDeclaration declaration -> declaration.origin.span
    | LetBinding binding -> binding.origin.span
    | ValueDeclaration declaration -> declaration.origin.span
    | ExternalDeclaration declaration -> declaration.origin.span
    | TypeDeclaration declaration -> declaration.origin.span
    | TypeDefinition definition -> definition.origin.span
    | TypeConstructor constructor -> constructor.origin.span
    | TypeExtensionDeclaration declaration -> declaration.origin.span
    | RecordFieldDeclaration field -> field.origin.span
    | RecordExpressionField field -> field.origin.span
    | RecordPatternField field -> field.origin.span
    | ExceptionDeclaration declaration -> declaration.origin.span
    | ModuleDeclaration declaration -> declaration.origin.span
    | ModuleTypeDeclaration declaration -> declaration.origin.span
    | FunctorParameter parameter -> parameter.origin.span
    | ModuleUnpack unpack -> unpack.origin.span
    | PackageType package -> package.origin.span
    | PackageTypeConstraint constraint_ -> constraint_.origin.span
    | Parameter parameter -> parameter.origin.span
    | Argument argument -> argument.origin.span
    | MatchCase case -> case.origin.span
    | Pattern pattern -> pattern.origin.span
    | Expression expression -> expression.origin.span
    | CoreType type_ -> type_.origin.span
    | PolyVariantTypeField field -> field.origin.span
end

let create ~source_file ~infer_result = { source_file; infer_result }

let source_file context = context.source_file

let infer_result context = context.infer_result

let find_first = fun values ~fn ->
  let rec loop values =
    match values with
    | [] -> None
    | value :: rest -> (
        match fn value with
        | Some _ as found -> found
        | None -> loop rest
      )
  in
  loop values

let find_option = fun value ~fn ->
  match value with
  | Some value -> fn value
  | None -> None

let rec find_node = fun query parent kind ~children ->
  let span = Node.span ({ kind; parent }: Node.t) in
  if not (Syn.Span.contains span query) then
    None
  else
    let node = ({ kind; parent }: Node.t) in
    match children node with
    | Some child -> Some child
    | None -> Some node

let rec find_source_file = fun query parent source_file ->
  find_node
    query
    parent
    (Node.SourceFile source_file)
    ~children:(fun node ->
      match source_file with
      | Ast.Implementation implementation ->
          find_first implementation.items ~fn:(find_structure_item query (Some node))
      | Ast.Interface interface ->
          find_first interface.items ~fn:(find_signature_item query (Some node)))

and find_structure_item = fun query parent item ->
  find_node
    query
    parent
    (Node.StructureItem item)
    ~children:(fun node ->
      match item.kind with
      | Ast.Let declaration -> find_let_declaration query (Some node) declaration
      | Ast.Type declarations ->
          find_first declarations ~fn:(find_type_declaration query (Some node))
      | Ast.TypeExtension declaration ->
          find_type_extension_declaration query (Some node) declaration
      | Ast.Expression expression -> find_expression query (Some node) expression
      | Ast.External declaration -> find_external_declaration query (Some node) declaration
      | Ast.Exception declaration -> find_exception_declaration query (Some node) declaration
      | Ast.Module declarations ->
          find_first declarations ~fn:(find_module_declaration query (Some node))
      | Ast.ModuleType declaration -> find_module_type_declaration query (Some node) declaration
      | Ast.Open _
      | Ast.Include _ -> None)

and find_signature_item = fun query parent item ->
  find_node
    query
    parent
    (Node.SignatureItem item)
    ~children:(fun node ->
      match item.kind with
      | Ast.Value declaration -> find_value_declaration query (Some node) declaration
      | Ast.Type declarations ->
          find_first declarations ~fn:(find_type_declaration query (Some node))
      | Ast.TypeExtension declaration ->
          find_type_extension_declaration query (Some node) declaration
      | Ast.External declaration -> find_external_declaration query (Some node) declaration
      | Ast.Exception declaration -> find_exception_declaration query (Some node) declaration
      | Ast.Open _ -> None)

and find_let_declaration = fun query parent declaration ->
  find_node
    query
    parent
    (Node.LetDeclaration declaration)
    ~children:(fun node -> find_first declaration.bindings ~fn:(find_let_binding query (Some node)))

and find_let_binding = fun query parent binding ->
  find_node
    query
    parent
    (Node.LetBinding binding)
    ~children:(fun node ->
      match find_pattern query (Some node) binding.pattern with
      | Some _ as found -> found
      | None -> (
          match find_option binding.type_hint ~fn:(find_core_type query (Some node)) with
          | Some _ as found -> found
          | None -> find_expression query (Some node) binding.expr
        ))

and find_value_declaration = fun query parent declaration ->
  find_node
    query
    parent
    (Node.ValueDeclaration declaration)
    ~children:(fun node ->
      find_core_type query (Some node) declaration.type_annotation)

and find_external_declaration = fun query parent declaration ->
  find_node
    query
    parent
    (Node.ExternalDeclaration declaration)
    ~children:(fun node ->
      find_core_type query (Some node) declaration.type_annotation)

and find_type_declaration = fun query parent declaration ->
  find_node
    query
    parent
    (Node.TypeDeclaration declaration)
    ~children:(fun node ->
      find_type_definition query (Some node) declaration.definition)

and find_type_definition = fun query parent definition ->
  find_node
    query
    parent
    (Node.TypeDefinition definition)
    ~children:(fun node ->
      match definition.kind with
      | Ast.Abstract
      | Ast.Extensible -> None
      | Ast.Alias type_ -> find_core_type query (Some node) type_
      | Ast.Variant constructors ->
          find_first constructors ~fn:(find_type_constructor query (Some node))
      | Ast.Record fields -> find_first fields ~fn:(find_record_field_declaration query (Some node)))

and find_type_constructor = fun query parent constructor ->
  find_node
    query
    parent
    (Node.TypeConstructor constructor)
    ~children:(fun node ->
      match constructor.arguments with
      | Ast.Tuple types -> (
          match find_first types ~fn:(find_core_type query (Some node)) with
          | Some _ as found -> found
          | None -> find_option constructor.result ~fn:(find_core_type query (Some node))
        )
      | Ast.Record fields -> (
          match find_first fields ~fn:(find_record_field_declaration query (Some node)) with
          | Some _ as found -> found
          | None -> find_option constructor.result ~fn:(find_core_type query (Some node))
        ))

and find_record_field_declaration = fun query parent field ->
  find_node
    query
    parent
    (Node.RecordFieldDeclaration field)
    ~children:(fun node ->
      find_core_type query (Some node) field.type_annotation)

and find_type_extension_declaration = fun query parent declaration ->
  find_node
    query
    parent
    (Node.TypeExtensionDeclaration declaration)
    ~children:(fun node ->
      find_first
        declaration.constructors
        ~fn:(find_type_constructor query (Some node)))

and find_exception_declaration = fun query parent declaration ->
  find_node
    query
    parent
    (Node.ExceptionDeclaration declaration)
    ~children:(fun node -> find_option declaration.payload ~fn:(find_core_type query (Some node)))

and find_module_declaration = fun query parent declaration ->
  find_node
    query
    parent
    (Node.ModuleDeclaration declaration)
    ~children:(fun node ->
      match find_first declaration.parameters ~fn:(find_functor_parameter query (Some node)) with
      | Some _ as found -> found
      | None -> find_first declaration.items ~fn:(find_structure_item query (Some node)))

and find_module_type_declaration = fun query parent declaration ->
  find_node
    query
    parent
    (Node.ModuleTypeDeclaration declaration)
    ~children:(fun node -> find_first declaration.items ~fn:(find_signature_item query (Some node)))

and find_functor_parameter = fun query parent parameter ->
  find_node
    query
    parent
    (Node.FunctorParameter parameter)
    ~children:(fun _node -> None)

and find_parameter = fun query parent parameter ->
  find_node
    query
    parent
    (Node.Parameter parameter)
    ~children:(fun node ->
      match find_pattern query (Some node) parameter.pattern with
      | Some _ as found -> found
      | None -> (
          match find_option parameter.annotation ~fn:(find_core_type query (Some node)) with
          | Some _ as found -> found
          | None -> find_option parameter.default ~fn:(find_expression query (Some node))
        ))

and find_argument = fun query parent argument ->
  find_node
    query
    parent
    (Node.Argument argument)
    ~children:(fun node ->
      match argument.kind with
      | Ast.Positional expression -> find_expression query (Some node) expression
      | Ast.Labeled { value; _ }
      | Ast.Optional { value; _ } -> find_option value ~fn:(find_expression query (Some node)))

and find_match_case = fun query parent case ->
  find_node
    query
    parent
    (Node.MatchCase case)
    ~children:(fun node ->
      match find_pattern query (Some node) case.pattern with
      | Some _ as found -> found
      | None -> (
          match find_option case.guard ~fn:(find_expression query (Some node)) with
          | Some _ as found -> found
          | None -> find_expression query (Some node) case.body
        ))

and find_pattern = fun query parent pattern ->
  find_node
    query
    parent
    (Node.Pattern pattern)
    ~children:(fun node ->
      match pattern.kind with
      | Ast.Wildcard
      | Ast.Bind _
      | Ast.Literal _
      | Ast.PolyVariant { payload = None; _ }
      | Ast.FirstClassModule { package_type = None; _ } -> None
      | Ast.Constructor { payload; _ } -> find_option payload ~fn:(find_pattern query (Some node))
      | Ast.PolyVariant { payload = Some payload; _ } -> find_pattern query (Some node) payload
      | Ast.Tuple patterns
      | Ast.List patterns -> find_first patterns ~fn:(find_pattern query (Some node))
      | Ast.Record fields -> find_first fields ~fn:(find_record_pattern_field query (Some node))
      | Ast.Or { left; right }
      | Ast.Cons { head = left; tail = right } -> (
          match find_pattern query (Some node) left with
          | Some _ as found -> found
          | None -> find_pattern query (Some node) right
        )
      | Ast.Constraint { pattern; annotation } -> (
          match find_pattern query (Some node) pattern with
          | Some _ as found -> found
          | None -> find_core_type query (Some node) annotation
        )
      | Ast.Alias { pattern; alias } -> (
          match find_pattern query (Some node) pattern with
          | Some _ as found -> found
          | None -> find_pattern query (Some node) alias
        )
      | Ast.Attribute pattern -> find_pattern query (Some node) pattern
      | Ast.FirstClassModule { package_type = Some package_type; _ } ->
          find_package_type query (Some node) package_type)

and find_record_pattern_field = fun query parent field ->
  find_node
    query
    parent
    (Node.RecordPatternField field)
    ~children:(fun node -> find_option field.pattern ~fn:(find_pattern query (Some node)))

and find_expression = fun query parent expression ->
  find_node
    query
    parent
    (Node.Expression expression)
    ~children:(fun node ->
      match find_option
        expression.type_hint
        ~fn:(fun hint ->
          find_core_type query (Some node) hint.type_) with
      | Some _ as found -> found
      | None -> find_expression_kind query (Some node) expression.kind)

and find_expression_kind = fun query parent kind ->
  match kind with
  | Ast.Literal _
  | Ast.Ident _
  | Ast.PolyVariant { payload = None; _ } -> None
  | Ast.Constructor { payload; _ } -> find_option payload ~fn:(find_expression query parent)
  | Ast.Tuple expressions
  | Ast.List expressions
  | Ast.Array expressions -> find_first expressions ~fn:(find_expression query parent)
  | Ast.PolyVariant { payload = Some payload; _ } -> find_expression query parent payload
  | Ast.Record { update; fields } -> (
      match find_option update ~fn:(find_expression query parent) with
      | Some _ as found -> found
      | None -> find_first fields ~fn:(find_record_expression_field query parent)
    )
  | Ast.FieldAccess { receiver; _ } -> find_expression query parent receiver
  | Ast.Assign { target; value } -> (
      match find_expression query parent target with
      | Some _ as found -> found
      | None -> find_expression query parent value
    )
  | Ast.Sequence { left; right } -> (
      match find_expression query parent left with
      | Some _ as found -> found
      | None -> find_expression query parent right
    )
  | Ast.If { condition; then_branch; else_branch } -> (
      match find_expression query parent condition with
      | Some _ as found -> found
      | None -> (
          match find_expression query parent then_branch with
          | Some _ as found -> found
          | None -> find_option else_branch ~fn:(find_expression query parent)
        )
    )
  | Ast.Match { scrutinee; cases } -> (
      match find_expression query parent scrutinee with
      | Some _ as found -> found
      | None -> find_first cases ~fn:(find_match_case query parent)
    )
  | Ast.Try { body; cases } -> (
      match find_expression query parent body with
      | Some _ as found -> found
      | None -> find_first cases ~fn:(find_match_case query parent)
    )
  | Ast.While { condition; body } -> (
      match find_expression query parent condition with
      | Some _ as found -> found
      | None -> find_expression query parent body
    )
  | Ast.For {
      pattern;
      start_;
      stop;
      body;
    } ->
      (
          match find_pattern query parent pattern with
          | Some _ as found -> found
          | None -> (
              match find_expression query parent start_ with
              | Some _ as found -> found
              | None -> (
                  match find_expression query parent stop with
                  | Some _ as found -> found
                  | None -> find_expression query parent body
                )
            )
        )
  | Ast.Function { parameters; body; _ } -> (
      match find_first parameters ~fn:(find_parameter query parent) with
      | Some _ as found -> found
      | None -> find_function_body query parent body
    )
  | Ast.Apply { callee; arguments } -> (
      match find_expression query parent callee with
      | Some _ as found -> found
      | None -> find_first arguments ~fn:(find_argument query parent)
    )
  | Ast.Infix { left; right; _ } -> (
      match find_expression query parent left with
      | Some _ as found -> found
      | None -> find_expression query parent right
    )
  | Ast.Let { bindings; body; _ } -> (
      match find_first bindings ~fn:(find_let_binding query parent) with
      | Some _ as found -> found
      | None -> find_expression query parent body
    )
  | Ast.LetModule { items; unpack; body; _ } -> (
      match find_first items ~fn:(find_structure_item query parent) with
      | Some _ as found -> found
      | None -> (
          match find_option unpack ~fn:(find_module_unpack query parent) with
          | Some _ as found -> found
          | None -> find_expression query parent body
        )
    )
  | Ast.LocalOpen { body; _ } -> find_expression query parent body
  | Ast.FirstClassModule { package_type; _ } ->
      find_option package_type ~fn:(find_package_type query parent)
  | Ast.Assert expression -> find_expression query parent expression

and find_function_body = fun query parent body ->
  match body with
  | Ast.Body expression -> find_expression query parent expression
  | Ast.Cases cases -> find_first cases ~fn:(find_match_case query parent)

and find_record_expression_field = fun query parent field ->
  find_node
    query
    parent
    (Node.RecordExpressionField field)
    ~children:(fun node ->
      find_expression query (Some node) field.value)

and find_module_unpack = fun query parent unpack ->
  find_node
    query
    parent
    (Node.ModuleUnpack unpack)
    ~children:(fun node ->
      match find_expression query (Some node) unpack.expression with
      | Some _ as found -> found
      | None -> find_option unpack.package_type ~fn:(find_package_type query (Some node)))

and find_package_type = fun query parent package ->
  find_node
    query
    parent
    (Node.PackageType package)
    ~children:(fun node ->
      find_first
        package.constraints
        ~fn:(find_package_type_constraint query (Some node)))

and find_package_type_constraint = fun query parent constraint_ ->
  find_node
    query
    parent
    (Node.PackageTypeConstraint constraint_)
    ~children:(fun node ->
      find_core_type query (Some node) constraint_.manifest)

and find_core_type = fun query parent type_ ->
  find_node
    query
    parent
    (Node.CoreType type_)
    ~children:(fun node ->
      match type_.kind with
      | Ast.Wildcard
      | Ast.Var _
      | Ast.TypeIdent _ -> None
      | Ast.Apply { constructor; arguments } -> (
          match find_core_type query (Some node) constructor with
          | Some _ as found -> found
          | None -> find_first arguments ~fn:(find_core_type query (Some node))
        )
      | Ast.Arrow { parameter; result; _ } -> (
          match find_core_type query (Some node) parameter with
          | Some _ as found -> found
          | None -> find_core_type query (Some node) result
        )
      | Ast.Tuple types -> find_first types ~fn:(find_core_type query (Some node))
      | Ast.ForAll { body; _ }
      | Ast.Parenthesized body -> find_core_type query (Some node) body
      | Ast.PolyVariant fields ->
          find_first fields ~fn:(find_poly_variant_type_field query (Some node))
      | Ast.Package package -> find_package_type query (Some node) package)

and find_poly_variant_type_field = fun query parent field ->
  find_node
    query
    parent
    (Node.PolyVariantTypeField field)
    ~children:(fun node -> find_option field.payload ~fn:(find_core_type query (Some node)))

let node_at context span = find_source_file span None context.source_file

let path_at context span =
  let rec collect node nodes =
    match Node.parent node with
    | Some parent -> collect parent (node :: nodes)
    | None -> node :: nodes
  in
  match node_at context span with
  | Some node -> collect node []
  | None -> []

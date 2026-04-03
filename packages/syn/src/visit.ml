open Std

type 'ctx walker = {
  apply_argument: 'ctx -> Cst.apply_argument -> 'ctx;
  attribute: 'ctx -> Cst.attribute -> 'ctx;
  binding_operator_binding: 'ctx -> Cst.binding_operator_binding -> 'ctx;
  class_declaration: 'ctx -> Cst.ClassDeclaration.t -> 'ctx;
  class_definition: 'ctx -> Cst.ClassDefinition.t -> 'ctx;
  class_expression: 'ctx -> Cst.ClassExpression.t -> 'ctx;
  class_field: 'ctx -> Cst.class_field -> 'ctx;
  class_type: 'ctx -> Cst.ClassType.t -> 'ctx;
  class_type_declaration: 'ctx -> Cst.class_type_declaration -> 'ctx;
  class_type_field: 'ctx -> Cst.ClassTypeField.t -> 'ctx;
  core_type: 'ctx -> Cst.CoreType.t -> 'ctx;
  exception_declaration: 'ctx -> Cst.exception_declaration -> 'ctx;
  expression: 'ctx -> Cst.Expression.t -> 'ctx;
  extension: 'ctx -> Cst.extension -> 'ctx;
  external_declaration: 'ctx -> Cst.external_declaration -> 'ctx;
  functor_parameter: 'ctx -> Cst.FunctorParameter.t -> 'ctx;
  implementation: 'ctx -> Cst.implementation -> 'ctx;
  include_statement: 'ctx -> Cst.include_statement -> 'ctx;
  interface: 'ctx -> Cst.interface -> 'ctx;
  let_binding: 'ctx -> Cst.LetBinding.t -> 'ctx;
  match_case: 'ctx -> Cst.match_case -> 'ctx;
  module_signature: 'ctx -> Cst.ModuleSignature.t -> 'ctx;
  module_structure: 'ctx -> Cst.ModuleStructure.t -> 'ctx;
  module_expression: 'ctx -> Cst.ModuleExpression.t -> 'ctx;
  module_type: 'ctx -> Cst.ModuleType.t -> 'ctx;
  module_type_constraint: 'ctx -> Cst.ModuleTypeConstraint.t -> 'ctx;
  module_type_declaration: 'ctx -> Cst.ModuleTypeDeclaration.t -> 'ctx;
  object_member: 'ctx -> Cst.ObjectMember.t -> 'ctx;
  object_type_field: 'ctx -> Cst.object_type_field -> 'ctx;
  open_statement: 'ctx -> Cst.OpenStatement.t -> 'ctx;
  parameter: 'ctx -> Cst.Parameter.t -> 'ctx;
  pattern: 'ctx -> Cst.Pattern.t -> 'ctx;
  payload: 'ctx -> Cst.Payload.t -> 'ctx;
  record_expression: 'ctx -> Cst.RecordExpression.t -> 'ctx;
  record_type_field: 'ctx -> Cst.record_type_field -> 'ctx;
  row_field: 'ctx -> Cst.RowField.t -> 'ctx;
  signature_item: 'ctx -> Cst.SignatureItem.t -> 'ctx;
  source_file: 'ctx -> Cst.SourceFile.t -> 'ctx;
  structure_item: 'ctx -> Cst.StructureItem.t -> 'ctx;
  type_binder: 'ctx -> Cst.TypeBinder.t -> 'ctx;
  type_constraint: 'ctx -> Cst.TypeConstraint.t -> 'ctx;
  type_declaration: 'ctx -> Cst.TypeDeclaration.t -> 'ctx;
  type_definition: 'ctx -> Cst.TypeDefinition.t -> 'ctx;
  type_extension: 'ctx -> Cst.TypeExtension.t -> 'ctx;
  type_parameter: 'ctx -> Cst.TypeParameter.t -> 'ctx;
  value_declaration: 'ctx -> Cst.value_declaration -> 'ctx;
  variant_constructor: 'ctx -> Cst.VariantConstructor.t -> 'ctx;
  descend_apply_argument: 'ctx -> Cst.apply_argument -> 'ctx;
  descend_attribute: 'ctx -> Cst.attribute -> 'ctx;
  descend_binding_operator_binding: 'ctx -> Cst.binding_operator_binding -> 'ctx;
  descend_class_declaration: 'ctx -> Cst.ClassDeclaration.t -> 'ctx;
  descend_class_definition: 'ctx -> Cst.ClassDefinition.t -> 'ctx;
  descend_class_expression: 'ctx -> Cst.ClassExpression.t -> 'ctx;
  descend_class_field: 'ctx -> Cst.class_field -> 'ctx;
  descend_class_type: 'ctx -> Cst.ClassType.t -> 'ctx;
  descend_class_type_declaration: 'ctx -> Cst.class_type_declaration -> 'ctx;
  descend_class_type_field: 'ctx -> Cst.ClassTypeField.t -> 'ctx;
  descend_core_type: 'ctx -> Cst.CoreType.t -> 'ctx;
  descend_exception_declaration: 'ctx -> Cst.exception_declaration -> 'ctx;
  descend_expression: 'ctx -> Cst.Expression.t -> 'ctx;
  descend_extension: 'ctx -> Cst.extension -> 'ctx;
  descend_external_declaration: 'ctx -> Cst.external_declaration -> 'ctx;
  descend_functor_parameter: 'ctx -> Cst.FunctorParameter.t -> 'ctx;
  descend_implementation: 'ctx -> Cst.implementation -> 'ctx;
  descend_include_statement: 'ctx -> Cst.include_statement -> 'ctx;
  descend_interface: 'ctx -> Cst.interface -> 'ctx;
  descend_let_binding: 'ctx -> Cst.LetBinding.t -> 'ctx;
  descend_match_case: 'ctx -> Cst.match_case -> 'ctx;
  descend_module_signature: 'ctx -> Cst.ModuleSignature.t -> 'ctx;
  descend_module_structure: 'ctx -> Cst.ModuleStructure.t -> 'ctx;
  descend_module_expression: 'ctx -> Cst.ModuleExpression.t -> 'ctx;
  descend_module_type: 'ctx -> Cst.ModuleType.t -> 'ctx;
  descend_module_type_constraint: 'ctx -> Cst.ModuleTypeConstraint.t -> 'ctx;
  descend_module_type_declaration: 'ctx -> Cst.ModuleTypeDeclaration.t -> 'ctx;
  descend_object_member: 'ctx -> Cst.ObjectMember.t -> 'ctx;
  descend_object_type_field: 'ctx -> Cst.object_type_field -> 'ctx;
  descend_open_statement: 'ctx -> Cst.OpenStatement.t -> 'ctx;
  descend_parameter: 'ctx -> Cst.Parameter.t -> 'ctx;
  descend_pattern: 'ctx -> Cst.Pattern.t -> 'ctx;
  descend_payload: 'ctx -> Cst.Payload.t -> 'ctx;
  descend_record_expression: 'ctx -> Cst.RecordExpression.t -> 'ctx;
  descend_record_type_field: 'ctx -> Cst.record_type_field -> 'ctx;
  descend_row_field: 'ctx -> Cst.RowField.t -> 'ctx;
  descend_signature_item: 'ctx -> Cst.SignatureItem.t -> 'ctx;
  descend_source_file: 'ctx -> Cst.SourceFile.t -> 'ctx;
  descend_structure_item: 'ctx -> Cst.StructureItem.t -> 'ctx;
  descend_type_binder: 'ctx -> Cst.TypeBinder.t -> 'ctx;
  descend_type_constraint: 'ctx -> Cst.TypeConstraint.t -> 'ctx;
  descend_type_declaration: 'ctx -> Cst.TypeDeclaration.t -> 'ctx;
  descend_type_definition: 'ctx -> Cst.TypeDefinition.t -> 'ctx;
  descend_type_extension: 'ctx -> Cst.TypeExtension.t -> 'ctx;
  descend_type_parameter: 'ctx -> Cst.TypeParameter.t -> 'ctx;
  descend_value_declaration: 'ctx -> Cst.value_declaration -> 'ctx;
  descend_variant_constructor: 'ctx -> Cst.VariantConstructor.t -> 'ctx;
}

type 'ctx visitor = {
  visit_apply_argument: 'ctx -> 'ctx walker -> Cst.apply_argument -> 'ctx;
  visit_attribute: 'ctx -> 'ctx walker -> Cst.attribute -> 'ctx;
  visit_binding_operator_binding: 'ctx -> 'ctx walker -> Cst.binding_operator_binding -> 'ctx;
  visit_class_declaration: 'ctx -> 'ctx walker -> Cst.ClassDeclaration.t -> 'ctx;
  visit_class_definition: 'ctx -> 'ctx walker -> Cst.ClassDefinition.t -> 'ctx;
  visit_class_expression: 'ctx -> 'ctx walker -> Cst.ClassExpression.t -> 'ctx;
  visit_class_field: 'ctx -> 'ctx walker -> Cst.class_field -> 'ctx;
  visit_class_type: 'ctx -> 'ctx walker -> Cst.ClassType.t -> 'ctx;
  visit_class_type_declaration: 'ctx -> 'ctx walker -> Cst.class_type_declaration -> 'ctx;
  visit_class_type_field: 'ctx -> 'ctx walker -> Cst.ClassTypeField.t -> 'ctx;
  visit_core_type: 'ctx -> 'ctx walker -> Cst.CoreType.t -> 'ctx;
  visit_exception_declaration: 'ctx -> 'ctx walker -> Cst.exception_declaration -> 'ctx;
  visit_expression: 'ctx -> 'ctx walker -> Cst.Expression.t -> 'ctx;
  visit_extension: 'ctx -> 'ctx walker -> Cst.extension -> 'ctx;
  visit_external_declaration: 'ctx -> 'ctx walker -> Cst.external_declaration -> 'ctx;
  visit_functor_parameter: 'ctx -> 'ctx walker -> Cst.FunctorParameter.t -> 'ctx;
  visit_implementation: 'ctx -> 'ctx walker -> Cst.implementation -> 'ctx;
  visit_include_statement: 'ctx -> 'ctx walker -> Cst.include_statement -> 'ctx;
  visit_interface: 'ctx -> 'ctx walker -> Cst.interface -> 'ctx;
  visit_let_binding: 'ctx -> 'ctx walker -> Cst.LetBinding.t -> 'ctx;
  visit_match_case: 'ctx -> 'ctx walker -> Cst.match_case -> 'ctx;
  visit_module_signature: 'ctx -> 'ctx walker -> Cst.ModuleSignature.t -> 'ctx;
  visit_module_structure: 'ctx -> 'ctx walker -> Cst.ModuleStructure.t -> 'ctx;
  visit_module_expression: 'ctx -> 'ctx walker -> Cst.ModuleExpression.t -> 'ctx;
  visit_module_type: 'ctx -> 'ctx walker -> Cst.ModuleType.t -> 'ctx;
  visit_module_type_constraint: 'ctx -> 'ctx walker -> Cst.ModuleTypeConstraint.t -> 'ctx;
  visit_module_type_declaration: 'ctx -> 'ctx walker -> Cst.ModuleTypeDeclaration.t -> 'ctx;
  visit_object_member: 'ctx -> 'ctx walker -> Cst.ObjectMember.t -> 'ctx;
  visit_object_type_field: 'ctx -> 'ctx walker -> Cst.object_type_field -> 'ctx;
  visit_open_statement: 'ctx -> 'ctx walker -> Cst.OpenStatement.t -> 'ctx;
  visit_parameter: 'ctx -> 'ctx walker -> Cst.Parameter.t -> 'ctx;
  visit_pattern: 'ctx -> 'ctx walker -> Cst.Pattern.t -> 'ctx;
  visit_payload: 'ctx -> 'ctx walker -> Cst.Payload.t -> 'ctx;
  visit_record_expression: 'ctx -> 'ctx walker -> Cst.RecordExpression.t -> 'ctx;
  visit_record_type_field: 'ctx -> 'ctx walker -> Cst.record_type_field -> 'ctx;
  visit_row_field: 'ctx -> 'ctx walker -> Cst.RowField.t -> 'ctx;
  visit_signature_item: 'ctx -> 'ctx walker -> Cst.SignatureItem.t -> 'ctx;
  visit_source_file: 'ctx -> 'ctx walker -> Cst.SourceFile.t -> 'ctx;
  visit_structure_item: 'ctx -> 'ctx walker -> Cst.StructureItem.t -> 'ctx;
  visit_type_binder: 'ctx -> 'ctx walker -> Cst.TypeBinder.t -> 'ctx;
  visit_type_constraint: 'ctx -> 'ctx walker -> Cst.TypeConstraint.t -> 'ctx;
  visit_type_declaration: 'ctx -> 'ctx walker -> Cst.TypeDeclaration.t -> 'ctx;
  visit_type_definition: 'ctx -> 'ctx walker -> Cst.TypeDefinition.t -> 'ctx;
  visit_type_extension: 'ctx -> 'ctx walker -> Cst.TypeExtension.t -> 'ctx;
  visit_type_parameter: 'ctx -> 'ctx walker -> Cst.TypeParameter.t -> 'ctx;
  visit_value_declaration: 'ctx -> 'ctx walker -> Cst.value_declaration -> 'ctx;
  visit_variant_constructor: 'ctx -> 'ctx walker -> Cst.VariantConstructor.t -> 'ctx;
}

let rec fold_binding_operator_chain = fun (walk: 'ctx walker) ctx (
  binding: Cst.binding_operator_binding
) ->
  let ctx = walk.binding_operator_binding ctx binding in
  match binding.and_binding with
  | Some next -> fold_binding_operator_chain walk ctx next
  | None -> ctx

let rec descend_attribute = fun walk ctx (attribute: Cst.attribute) ->
  match attribute.payload with
  | Some payload -> walk.payload ctx payload
  | None -> ctx

and descend_extension = fun walk ctx (extension: Cst.extension) ->
  let ctx = List.fold_left walk.attribute ctx extension.attributes in
  match extension.payload with
  | Some payload -> walk.payload ctx payload
  | None -> ctx

and descend_payload = fun _walk ctx (_payload: Cst.Payload.t) -> ctx

and descend_type_binder = fun _walk ctx _type_binder -> ctx

and descend_type_parameter = fun _walk ctx _type_parameter -> ctx

and descend_parameter = fun walk ctx parameter ->
  match parameter with
  | Cst.Parameter.Positional _
  | Cst.Parameter.Labeled _
  | Cst.Parameter.Optional _ -> ctx
  | Cst.Parameter.LocallyAbstract { binders; _ } -> List.fold_left walk.type_binder ctx binders

and descend_pattern = fun walk ctx (pattern: Cst.Pattern.t) ->
  match pattern with
  | Cst.Pattern.Identifier { attributes; _ }
  | Cst.Pattern.Wildcard { attributes; _ }
  | Cst.Pattern.Literal { attributes; _ }
  | Cst.Pattern.Operator { attributes; _ }
  | Cst.Pattern.Range { attributes; _ }
  | Cst.Pattern.PolyVariantInherit { attributes; _ } ->
      List.fold_left walk.attribute ctx attributes
  | Cst.Pattern.Extension { extension; attributes; _ } ->
      let ctx = walk.extension ctx extension in
      List.fold_left walk.attribute ctx attributes
  | Cst.Pattern.Lazy { pattern; attributes; _ }
  | Cst.Pattern.Exception { pattern; attributes; _ }
  | Cst.Pattern.LocalOpen { pattern; attributes; _ }
  | Cst.Pattern.Parenthesized { inner=pattern; attributes; _ } ->
      let ctx = walk.pattern ctx pattern in
      List.fold_left walk.attribute ctx attributes
  | Cst.Pattern.FirstClassModule { package_type; attributes; _ } ->
      let ctx =
        match package_type with
        | Some package_type ->
            let ctx = List.fold_left walk.module_type_constraint ctx package_type.constraints in
            begin
              match package_type.attribute with
              | Some attribute -> walk.attribute ctx attribute
              | None -> ctx
            end
        | None -> ctx
      in
      List.fold_left walk.attribute ctx attributes
  | Cst.Pattern.PolyVariant { payload; attributes; _ } ->
      let ctx =
        match payload with
        | Some payload -> walk.pattern ctx payload
        | None -> ctx
      in
      List.fold_left walk.attribute ctx attributes
  | Cst.Pattern.Constructor { existentials; arguments; attributes; _ } ->
      let ctx =
        match existentials with
        | Some existentials -> List.fold_left walk.type_binder ctx existentials.binders
        | None -> ctx
      in
      let ctx = List.fold_left walk.pattern ctx arguments in
      List.fold_left walk.attribute ctx attributes
  | Cst.Pattern.Tuple { elements; attributes; _ } ->
      let ctx =
        List.fold_left
          (fun ctx (element: Cst.tuple_pattern_element) ->
            walk.pattern ctx element.pattern)
          ctx
          elements
      in
      List.fold_left walk.attribute ctx attributes
  | Cst.Pattern.List { elements; attributes; _ }
  | Cst.Pattern.Array { elements; attributes; _ }
  | Cst.Pattern.Or { alternatives=elements; attributes; _ } ->
      let ctx = List.fold_left walk.pattern ctx elements in
      List.fold_left walk.attribute ctx attributes
  | Cst.Pattern.Record { fields; attributes; _ } ->
      let ctx =
        List.fold_left
          (fun ctx (field: Cst.record_pattern_field) ->
            match field.pattern with
            | Some pattern -> walk.pattern ctx pattern
            | None -> ctx)
          ctx
          fields
      in
      List.fold_left walk.attribute ctx attributes
  | Cst.Pattern.Cons { head; tail; attributes; _ } ->
      let ctx = walk.pattern ctx head in
      let ctx = walk.pattern ctx tail in
      List.fold_left walk.attribute ctx attributes
  | Cst.Pattern.Alias { pattern; attributes; _ } ->
      let ctx = walk.pattern ctx pattern in
      List.fold_left walk.attribute ctx attributes
  | Cst.Pattern.Typed { pattern; type_; attributes; _ } ->
      let ctx = walk.pattern ctx pattern in
      let ctx = walk.core_type ctx type_ in
      List.fold_left walk.attribute ctx attributes
  | Cst.Pattern.Effect { effect_pattern; continuation; attributes; _ } ->
      let ctx = walk.pattern ctx effect_pattern in
      let ctx = walk.pattern ctx continuation in
      List.fold_left walk.attribute ctx attributes

and descend_core_type = fun walk ctx (core_type: Cst.CoreType.t) ->
  match core_type with
  | Cst.CoreType.Wildcard _
  | Cst.CoreType.Var _ ->
      ctx
  | Cst.CoreType.Extension extension ->
      walk.extension ctx extension
  | Cst.CoreType.Constr { arguments; _ }
  | Cst.CoreType.Class { arguments; _ } ->
      List.fold_left walk.core_type ctx arguments
  | Cst.CoreType.Alias { type_; _ }
  | Cst.CoreType.Parenthesized { inner=type_; _ } ->
      walk.core_type ctx type_
  | Cst.CoreType.Attribute { type_; attribute; _ } ->
      let ctx = walk.core_type ctx type_ in
      walk.attribute ctx attribute
  | Cst.CoreType.Poly { body; binders; _ } ->
      let ctx = List.fold_left walk.type_binder ctx binders in
      walk.core_type ctx body
  | Cst.CoreType.Arrow { parameter_type; result_type; _ } ->
      let ctx = walk.core_type ctx parameter_type in
      walk.core_type ctx result_type
  | Cst.CoreType.Tuple { elements; _ } ->
      List.fold_left walk.core_type ctx elements
  | Cst.CoreType.PolyVariant { fields; _ } ->
      List.fold_left walk.row_field ctx fields
  | Cst.CoreType.Record { fields; _ } ->
      List.fold_left walk.record_type_field ctx fields
  | Cst.CoreType.FirstClassModule { package_type; _ } ->
      let ctx = List.fold_left walk.module_type_constraint ctx package_type.constraints in
      begin
        match package_type.attribute with
        | Some attribute -> walk.attribute ctx attribute
        | None -> ctx
      end
  | Cst.CoreType.Object { fields; _ } ->
      List.fold_left walk.object_type_field ctx fields

and descend_exception_declaration = fun _walk ctx _decl -> ctx

and descend_object_type_field = fun walk ctx (field: Cst.object_type_field) ->
  walk.core_type ctx field.field_type

and descend_record_type_field = fun walk ctx (field: Cst.record_type_field) ->
  let ctx = List.fold_left walk.attribute ctx field.attributes in
  walk.core_type ctx field.field_type

and descend_row_field = fun walk ctx row_field ->
  match row_field with
  | Cst.RowField.Tag tag ->
      let ctx = List.fold_left walk.attribute ctx tag.attributes in
      (
        match tag.payload_type with
        | Some payload_type -> walk.core_type ctx payload_type
        | None -> ctx
      )
  | Cst.RowField.Inherit { type_; _ } -> walk.core_type ctx type_

and descend_type_constraint = fun walk ctx (constraint_: Cst.TypeConstraint.t) ->
  let ctx = walk.core_type ctx constraint_.left in
  walk.core_type ctx constraint_.right

and descend_module_type_constraint = fun walk ctx (constraint_: Cst.ModuleTypeConstraint.t) ->
  let ctx = walk.core_type ctx constraint_.constrained_type in
  walk.core_type ctx constraint_.replacement_type

and descend_functor_parameter = fun walk ctx (parameter: Cst.FunctorParameter.t) ->
  walk.module_type ctx parameter.module_type

and descend_module_type = fun walk ctx (module_type: Cst.ModuleType.t) ->
  match module_type with
  | Cst.ModuleType.Path _
  | Cst.ModuleType.TypeOf _
  | Cst.ModuleType.Signature _ ->
      ctx
  | Cst.ModuleType.Functor { parameters; result; _ } ->
      let ctx = List.fold_left walk.functor_parameter ctx parameters in
      walk.module_type ctx result
  | Cst.ModuleType.With { base; constraints; _ } ->
      let ctx = walk.module_type ctx base in
      List.fold_left walk.module_type_constraint ctx constraints
  | Cst.ModuleType.Parenthesized { inner; _ } ->
      walk.module_type ctx inner
  | Cst.ModuleType.Attribute { module_type; attribute; _ } ->
      let ctx = walk.module_type ctx module_type in
      walk.attribute ctx attribute
  | Cst.ModuleType.Extension extension ->
      walk.extension ctx extension

and descend_class_type = fun walk ctx (class_type: Cst.ClassType.t) ->
  match class_type with
  | Cst.ClassType.Path _ ->
      ctx
  | Cst.ClassType.Signature { fields; _ } ->
      List.fold_left walk.class_type_field ctx fields
  | Cst.ClassType.Arrow { parameter_type; result_type; _ } ->
      let ctx = walk.core_type ctx parameter_type in
      walk.class_type ctx result_type
  | Cst.ClassType.Parenthesized { inner; _ } ->
      walk.class_type ctx inner
  | Cst.ClassType.Attribute { class_type; attribute; _ } ->
      let ctx = walk.class_type ctx class_type in
      walk.attribute ctx attribute
  | Cst.ClassType.Extension extension ->
      walk.extension ctx extension

and descend_class_type_field = fun walk ctx (field: Cst.ClassTypeField.t) ->
  match field with
  | Cst.ClassTypeField.Inherit { class_type; _ } ->
      walk.class_type ctx class_type
  | Cst.ClassTypeField.Value { type_; _ }
  | Cst.ClassTypeField.Method { type_; _ } ->
      walk.core_type ctx type_
  | Cst.ClassTypeField.Constraint { left; right; _ } ->
      let ctx = walk.core_type ctx left in
      walk.core_type ctx right
  | Cst.ClassTypeField.Attribute { field; attribute; _ } ->
      let ctx = walk.class_type_field ctx field in
      walk.attribute ctx attribute
  | Cst.ClassTypeField.Extension extension ->
      walk.extension ctx extension

and descend_type_definition = fun walk ctx (type_definition: Cst.TypeDefinition.t) ->
  match type_definition with
  | Cst.TypeDefinition.Abstract
  | Cst.TypeDefinition.Extensible _ ->
      ctx
  | Cst.TypeDefinition.Alias { manifest; _ } ->
      walk.core_type ctx manifest
  | Cst.TypeDefinition.FirstClassModule { package_type; _ } ->
      let ctx = List.fold_left walk.module_type_constraint ctx package_type.constraints in
      begin
        match package_type.attribute with
        | Some attribute -> walk.attribute ctx attribute
        | None -> ctx
      end
  | Cst.TypeDefinition.Object { fields; _ } ->
      List.fold_left walk.object_type_field ctx fields
  | Cst.TypeDefinition.Record { fields; _ } ->
      List.fold_left
        (fun ctx field ->
          let ctx = List.fold_left walk.attribute ctx (Cst.RecordField.attributes field) in
          walk.core_type ctx (Cst.RecordField.field_type field))
        ctx
        fields
  | Cst.TypeDefinition.Variant { constructors; _ } ->
      List.fold_left walk.variant_constructor ctx constructors
  | Cst.TypeDefinition.PolyVariant poly_variant ->
      List.fold_left walk.row_field ctx (Cst.PolyVariant.fields poly_variant)

and descend_variant_constructor = fun walk ctx (constructor: Cst.VariantConstructor.t) ->
  let ctx = List.fold_left walk.attribute ctx (Cst.VariantConstructor.attributes constructor) in
  let ctx =
    match Cst.VariantConstructor.arguments constructor with
    | Some (Cst.ConstructorArguments.Tuple elements) -> List.fold_left walk.core_type ctx elements
    | Some (Cst.ConstructorArguments.Record { fields; _ }) ->
        List.fold_left
          (fun ctx field ->
            let ctx = List.fold_left walk.attribute ctx (Cst.RecordField.attributes field) in
            walk.core_type ctx (Cst.RecordField.field_type field))
          ctx
          fields
    | None -> ctx
  in
  let ctx =
    match Cst.VariantConstructor.payload_type constructor with
    | Some payload_type -> walk.core_type ctx payload_type
    | None -> ctx
  in
  match Cst.VariantConstructor.result_type constructor with
  | Some result_type -> walk.core_type ctx result_type
  | None -> ctx

and descend_type_declaration = fun walk ctx (declaration: Cst.TypeDeclaration.t) ->
  let ctx = List.fold_left walk.type_parameter ctx (Cst.TypeDeclaration.type_params declaration) in
  let ctx =
    match Cst.TypeDeclaration.manifest_alias declaration with
    | Some manifest_alias -> walk.core_type ctx manifest_alias
    | None -> ctx
  in
  let ctx = walk.type_definition ctx (Cst.TypeDeclaration.type_definition declaration) in
  let ctx = List.fold_left walk.type_constraint ctx (Cst.TypeDeclaration.constraints declaration) in
  match Cst.TypeDeclaration.next_and_declaration declaration with
  | Some next -> walk.type_declaration ctx next
  | None -> ctx

and descend_type_extension = fun walk ctx (declaration: Cst.TypeExtension.t) ->
  let ctx = List.fold_left walk.type_parameter ctx (Cst.TypeExtension.type_params declaration) in
  List.fold_left walk.variant_constructor ctx (Cst.TypeExtension.constructors declaration)

and descend_module_expression = fun walk ctx (module_expression: Cst.ModuleExpression.t) ->
  match module_expression with
  | Cst.ModuleExpression.Path _
  | Cst.ModuleExpression.Structure _ ->
      ctx
  | Cst.ModuleExpression.Functor { parameters; body; _ } ->
      let ctx = List.fold_left walk.functor_parameter ctx parameters in
      walk.module_expression ctx body
  | Cst.ModuleExpression.Apply { callee; argument; _ } ->
      let ctx = walk.module_expression ctx callee in
      walk.module_expression ctx argument
  | Cst.ModuleExpression.ApplyUnit { callee; _ } ->
      walk.module_expression ctx callee
  | Cst.ModuleExpression.Constraint { module_expression; module_type; _ } ->
      let ctx = walk.module_expression ctx module_expression in
      walk.module_type ctx module_type
  | Cst.ModuleExpression.ModuleUnpack { expression; package_type; _ } ->
      let ctx = walk.expression ctx expression in
      (
        match package_type with
        | Some package_type ->
            let ctx = List.fold_left walk.module_type_constraint ctx package_type.constraints in
            begin
              match package_type.attribute with
              | Some attribute -> walk.attribute ctx attribute
              | None -> ctx
            end
        | None -> ctx
      )
  | Cst.ModuleExpression.Parenthesized { inner; _ } ->
      walk.module_expression ctx inner
  | Cst.ModuleExpression.Attribute { module_expression; attribute; _ } ->
      let ctx = walk.module_expression ctx module_expression in
      walk.attribute ctx attribute
  | Cst.ModuleExpression.Extension extension ->
      walk.extension ctx extension

and descend_object_member = fun walk ctx (member: Cst.ObjectMember.t) ->
  match member with
  | Cst.ObjectMember.Method method_ ->
      let ctx = List.fold_left walk.attribute ctx method_.attributes in
      let ctx = walk.expression ctx method_.body in
      (
        match method_.type_ with
        | Some type_ -> walk.core_type ctx type_
        | None -> ctx
      )
  | Cst.ObjectMember.Value value ->
      let ctx = List.fold_left walk.attribute ctx value.attributes in
      let ctx = walk.expression ctx value.value in
      (
        match value.type_ with
        | Some type_ -> walk.core_type ctx type_
        | None -> ctx
      )
  | Cst.ObjectMember.Inherit inherit_ ->
      let ctx = List.fold_left walk.attribute ctx inherit_.attributes in
      walk.expression ctx inherit_.expression
  | Cst.ObjectMember.Extension extension ->
      walk.extension ctx extension
  | Cst.ObjectMember.Initializer init_member ->
      walk.expression ctx init_member.body

and descend_apply_argument = fun walk ctx argument ->
  match argument with
  | Cst.Positional expression -> walk.expression ctx expression
  | Cst.Labeled { value; _ }
  | Cst.Optional { value; _ } -> (
      match value with
      | Some value -> walk.expression ctx value
      | None -> ctx
    )

and descend_record_expression = fun walk ctx (record_expression: Cst.RecordExpression.t) ->
  match record_expression with
  | Cst.RecordExpression.Literal { fields; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      List.fold_left
        (fun ctx (field: Cst.record_expression_field) ->
          walk.expression ctx field.value)
        ctx
        fields
  | Cst.RecordExpression.Update { base; fields; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      let ctx = walk.expression ctx base in
      List.fold_left
        (fun ctx (field: Cst.record_expression_field) ->
          walk.expression ctx field.value)
        ctx
        fields

and descend_match_case = fun walk ctx (case: Cst.match_case) ->
  let ctx = walk.pattern ctx case.pattern in
  let ctx =
    match case.guard with
    | Some guard -> walk.expression ctx guard
    | None -> ctx
  in
  walk.expression ctx case.body

and descend_expression = fun walk ctx (expression: Cst.Expression.t) ->
  match expression with
  | Cst.Expression.Path _
  | Cst.Expression.Operator _
  | Cst.Expression.Literal _
  | Cst.Expression.Unreachable _
  | Cst.Expression.New _ ->
      ctx
  | Cst.Expression.Extension extension ->
      walk.extension ctx extension
  | Cst.Expression.Constructor { payload; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      (
        match payload with
        | Some payload -> walk.expression ctx payload
        | None -> ctx
      )
  | Cst.Expression.Object { self_pattern; members; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      let ctx =
        match self_pattern with
        | Some pattern -> walk.pattern ctx pattern
        | None -> ctx
      in
      List.fold_left walk.object_member ctx members
  | Cst.Expression.PolyVariant { payload; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      (
        match payload with
        | Some payload -> walk.expression ctx payload
        | None -> ctx
      )
  | Cst.Expression.ModulePack { module_expression; package_type; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      let ctx = walk.module_expression ctx module_expression in
      (
        match package_type with
        | Some package_type ->
            let ctx = List.fold_left walk.module_type_constraint ctx package_type.constraints in
            begin
              match package_type.attribute with
              | Some attribute -> walk.attribute ctx attribute
              | None -> ctx
            end
        | None -> ctx
      )
  | Cst.Expression.LetModule { module_expression; body; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      let ctx = walk.module_expression ctx module_expression in
      walk.expression ctx body
  | Cst.Expression.LetException { exception_declaration; body; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      let ctx = walk.exception_declaration ctx exception_declaration in
      walk.expression ctx body
  | Cst.Expression.Assert { asserted; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      walk.expression ctx asserted
  | Cst.Expression.Lazy { body; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      walk.expression ctx body
  | Cst.Expression.While { condition; body; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      let ctx = walk.expression ctx condition in
      walk.expression ctx body
  | Cst.Expression.For {
    start_expr;
    end_expr;
    body;
    attributes;
    _
  } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      let ctx = walk.expression ctx start_expr in
      let ctx = walk.expression ctx end_expr in
      walk.expression ctx body
  | Cst.Expression.Apply { callee; argument; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      let ctx = walk.expression ctx callee in
      walk.apply_argument ctx argument
  | Cst.Expression.MethodCall { receiver; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      walk.expression ctx receiver
  | Cst.Expression.Prefix { operand; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      walk.expression ctx operand
  | Cst.Expression.FieldAccess { receiver; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      walk.expression ctx receiver
  | Cst.Expression.Index { collection; index; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      let ctx = walk.expression ctx collection in
      walk.expression ctx index
  | Cst.Expression.ObjectOverride { fields; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      List.fold_left
        (fun ctx (field: Cst.object_override_field) ->
          match field.value with
          | Some value -> walk.expression ctx value
          | None -> ctx)
        ctx
        fields
  | Cst.Expression.InstanceVariableAssign { value; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      walk.expression ctx value
  | Cst.Expression.FieldAssign { target; value; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      let ctx = walk.expression ctx (Cst.Expression.FieldAccess target) in
      walk.expression ctx value
  | Cst.Expression.Assign { target; value; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      let ctx = walk.expression ctx target in
      walk.expression ctx value
  | Cst.Expression.Infix { left; right; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      let ctx = walk.expression ctx left in
      walk.expression ctx right
  | Cst.Expression.Sequence { expressions; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      List.fold_left walk.expression ctx expressions
  | Cst.Expression.TypeAscription { expression; kind; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      let ctx = walk.expression ctx expression in
      (
        match kind with
        | Cst.Type { type_; _ }
        | Cst.Coerce { type_; _ } -> walk.core_type ctx type_
        | Cst.ConstraintCoerce { from_type; to_type; _ } ->
            let ctx = walk.core_type ctx from_type in
            walk.core_type ctx to_type
      )
  | Cst.Expression.Polymorphic { expression; type_; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      let ctx = walk.expression ctx expression in
      walk.core_type ctx type_
  | Cst.Expression.Tuple { elements; attributes; _ }
  | Cst.Expression.List { elements; attributes; _ }
  | Cst.Expression.Array { elements; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      List.fold_left walk.expression ctx elements
  | Cst.Expression.Record record_expression ->
      walk.record_expression ctx record_expression
  | Cst.Expression.LocalOpen (Cst.LetOpen { body; attributes; _ })
  | Cst.Expression.LocalOpen (Cst.Delimited { body; attributes; _ }) ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      walk.expression ctx body
  | Cst.Expression.Fun { parameters; return_type; body; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      let ctx = List.fold_left walk.parameter ctx parameters in
      let ctx =
        match return_type with
        | Some return_type -> walk.core_type ctx return_type
        | None -> ctx
      in
      (
        match body with
        | Cst.Expression expression -> walk.expression ctx expression
        | Cst.Cases { cases; _ } -> List.fold_left walk.match_case ctx cases
      )
  | Cst.Expression.Function { cases; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      List.fold_left walk.match_case ctx cases
  | Cst.Expression.LetOperator { binding; body; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      let ctx = fold_binding_operator_chain walk ctx binding in
      walk.expression ctx body
  | Cst.Expression.Let {
    parameters;
    bound_value;
    and_binding;
    body;
    attributes;
    _
  } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      let ctx = List.fold_left walk.parameter ctx parameters in
      let ctx = walk.expression ctx bound_value in
      let ctx =
        match and_binding with
        | Some binding -> walk.let_binding ctx binding
        | None -> ctx
      in
      walk.expression ctx body
  | Cst.Expression.Match { scrutinee; cases; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      let ctx = walk.expression ctx scrutinee in
      List.fold_left walk.match_case ctx cases
  | Cst.Expression.Try { body; cases; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      let ctx = walk.expression ctx body in
      List.fold_left walk.match_case ctx cases
  | Cst.Expression.If {
    condition;
    then_branch;
    else_branch;
    attributes;
    _
  } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      let ctx = walk.expression ctx condition in
      let ctx = walk.expression ctx then_branch in
      (
        match else_branch with
        | Some else_branch -> walk.expression ctx else_branch
        | None -> ctx
      )
  | Cst.Expression.Parenthesized { inner; attributes; _ } ->
      let ctx = List.fold_left walk.attribute ctx attributes in
      walk.expression ctx inner

and descend_binding_operator_binding = fun walk ctx (binding: Cst.binding_operator_binding) ->
  let ctx = walk.pattern ctx binding.binding_pattern in
  walk.expression ctx binding.bound_value

and descend_let_binding = fun walk ctx binding ->
  let ctx = List.fold_left walk.attribute ctx (Cst.LetBinding.attributes binding) in
  let ctx = walk.pattern ctx (Cst.LetBinding.binding_pattern binding) in
  let ctx = List.fold_left walk.parameter ctx (Cst.LetBinding.parameters binding) in
  let ctx = walk.expression ctx (Cst.LetBinding.value binding) in
  match Cst.LetBinding.and_binding binding with
  | Some next -> walk.let_binding ctx next
  | None -> ctx

and descend_class_field = fun walk ctx (field: Cst.class_field) ->
  match field with
  | Cst.ClassField.Method method_ -> (
      match method_.definition with
      | Cst.ConcreteMethod { body; type_ } ->
          let ctx = walk.expression ctx body in
          (
            match type_ with
            | Some (_, type_) -> walk.core_type ctx type_
            | None -> ctx
          )
      | Cst.VirtualMethod { type_; _ } -> walk.core_type ctx type_
    )
  | Cst.ClassField.Value value -> (
      match value.definition with
      | Cst.ConcreteValue { value; type_ } ->
          let ctx = walk.expression ctx value in
          (
            match type_ with
            | Some (_, type_) -> walk.core_type ctx type_
            | None -> ctx
          )
      | Cst.VirtualValue { type_; _ } -> walk.core_type ctx type_
    )
  | Cst.ClassField.Inherit inherit_ ->
      walk.class_expression ctx inherit_.class_expression
  | Cst.ClassField.Constraint { left; right; _ } ->
      let ctx = walk.core_type ctx left in
      walk.core_type ctx right
  | Cst.ClassField.Initializer init_field ->
      walk.expression ctx init_field.body
  | Cst.ClassField.Attribute { field; attribute; _ } ->
      let ctx = walk.class_field ctx field in
      walk.attribute ctx attribute
  | Cst.ClassField.Extension extension ->
      walk.extension ctx extension

and descend_class_expression = fun walk ctx (class_expression: Cst.ClassExpression.t) ->
  match class_expression with
  | Cst.ClassExpression.Path _ ->
      ctx
  | Cst.ClassExpression.Structure { self_pattern; fields; _ } ->
      let ctx =
        match self_pattern with
        | Some pattern -> walk.pattern ctx pattern
        | None -> ctx
      in
      List.fold_left walk.class_field ctx fields
  | Cst.ClassExpression.Fun { parameters; body; _ } ->
      let ctx = List.fold_left walk.parameter ctx parameters in
      walk.class_expression ctx body
  | Cst.ClassExpression.Apply { callee; argument; _ } ->
      let ctx = walk.class_expression ctx callee in
      walk.apply_argument ctx argument
  | Cst.ClassExpression.Let {
    parameters;
    bound_value;
    and_binding;
    body;
    _
  } ->
      let ctx = List.fold_left walk.parameter ctx parameters in
      let ctx = walk.expression ctx bound_value in
      let ctx =
        match and_binding with
        | Some binding -> walk.let_binding ctx binding
        | None -> ctx
      in
      walk.class_expression ctx body
  | Cst.ClassExpression.Constraint { class_expression; class_type; _ } ->
      let ctx = walk.class_expression ctx class_expression in
      walk.class_type ctx class_type
  | Cst.ClassExpression.LocalOpen (Cst.LetOpen { body; _ })
  | Cst.ClassExpression.LocalOpen (Cst.Delimited { body; _ }) ->
      walk.class_expression ctx body
  | Cst.ClassExpression.Parenthesized { inner; _ } ->
      walk.class_expression ctx inner
  | Cst.ClassExpression.Attribute { class_expression; attribute; _ } ->
      let ctx = walk.class_expression ctx class_expression in
      walk.attribute ctx attribute
  | Cst.ClassExpression.Extension extension ->
      walk.extension ctx extension

and descend_value_declaration = fun walk ctx (declaration: Cst.value_declaration) ->
  let _ = declaration.trailing_comment in
  walk.core_type ctx declaration.type_

and descend_external_declaration = fun walk ctx (declaration: Cst.external_declaration) ->
  let ctx = walk.core_type ctx declaration.type_ in
  List.fold_left walk.attribute ctx declaration.attributes

and descend_class_declaration = fun walk ctx (declaration: Cst.ClassDeclaration.t) ->
  let ctx = List.fold_left walk.type_parameter ctx (Cst.ClassDeclaration.type_params declaration) in
  walk.class_type ctx (Cst.ClassDeclaration.class_type declaration)

and descend_class_definition = fun walk ctx (declaration: Cst.ClassDefinition.t) ->
  let ctx = List.fold_left walk.type_parameter ctx (Cst.ClassDefinition.type_params declaration) in
  let ctx =
    match Cst.ClassDefinition.class_type declaration with
    | Some class_type -> walk.class_type ctx class_type
    | None -> ctx
  in
  walk.class_expression ctx (Cst.ClassDefinition.class_body declaration)

and descend_class_type_declaration = fun walk ctx (declaration: Cst.class_type_declaration) ->
  let ctx = List.fold_left walk.type_parameter ctx declaration.type_params in
  walk.class_type ctx declaration.class_type_body

and descend_module_signature = fun walk ctx (declaration: Cst.ModuleSignature.t) ->
  let ctx = List.fold_left
    walk.functor_parameter
    ctx
    (Cst.ModuleSignature.functor_parameters declaration) in
  let ctx =
    match Cst.ModuleSignature.definition declaration with
    | Cst.ModuleSignature.Signature module_type -> walk.module_type ctx module_type
    | Cst.ModuleSignature.Alias module_expression -> walk.module_expression ctx module_expression
  in
  match Cst.ModuleSignature.next_and_declaration declaration with
  | Some next -> walk.module_signature ctx next
  | None -> ctx

and descend_module_structure = fun walk ctx (declaration: Cst.ModuleStructure.t) ->
  let ctx = List.fold_left
    walk.functor_parameter
    ctx
    (Cst.ModuleStructure.functor_parameters declaration) in
  let ctx =
    match Cst.ModuleStructure.module_type declaration with
    | Some module_type -> walk.module_type ctx module_type
    | None -> ctx
  in
  let ctx = walk.module_expression ctx (Cst.ModuleStructure.module_expression declaration) in
  match Cst.ModuleStructure.next_and_declaration declaration with
  | Some next -> walk.module_structure ctx next
  | None -> ctx

and descend_module_type_declaration = fun walk ctx (declaration: Cst.ModuleTypeDeclaration.t) ->
  match Cst.ModuleTypeDeclaration.module_type declaration with
  | Some module_type -> walk.module_type ctx module_type
  | None -> ctx

and descend_open_statement = fun walk ctx (statement: Cst.OpenStatement.t) ->
  match Cst.OpenStatement.target statement with
  | Cst.OpenStatement.Path _ -> ctx
  | Cst.OpenStatement.ModuleExpression module_expression -> walk.module_expression ctx module_expression

and descend_include_statement = fun walk ctx (statement: Cst.include_statement) ->
  match statement.target with
  | Cst.ModuleExpression module_expression -> walk.module_expression ctx module_expression
  | Cst.ModuleType module_type -> walk.module_type ctx module_type

and descend_structure_item = fun walk ctx (item: Cst.StructureItem.t) ->
  match item with
  | Cst.StructureItem.TypeDeclaration declaration -> walk.type_declaration ctx declaration
  | Cst.StructureItem.TypeExtension declaration -> walk.type_extension ctx declaration
  | Cst.StructureItem.LetBinding binding -> walk.let_binding ctx binding
  | Cst.StructureItem.Expression expression -> walk.expression ctx expression
  | Cst.StructureItem.Attribute attribute -> walk.attribute ctx attribute
  | Cst.StructureItem.Extension extension -> walk.extension ctx extension
  | Cst.StructureItem.ClassDeclaration declaration -> walk.class_definition ctx declaration
  | Cst.StructureItem.ClassTypeDeclaration declaration -> walk.class_type_declaration ctx declaration
  | Cst.StructureItem.ModuleDeclaration declaration -> walk.module_structure ctx declaration
  | Cst.StructureItem.ModuleTypeDeclaration declaration -> walk.module_type_declaration ctx declaration
  | Cst.StructureItem.OpenStatement statement -> walk.open_statement ctx statement
  | Cst.StructureItem.Docstring _ -> ctx
  | Cst.StructureItem.Comment _ -> ctx
  | Cst.StructureItem.ExternalDeclaration declaration -> walk.external_declaration ctx declaration
  | Cst.StructureItem.IncludeStatement statement -> walk.include_statement ctx statement
  | Cst.StructureItem.ExceptionDeclaration declaration -> walk.exception_declaration ctx declaration

and descend_signature_item = fun walk ctx (item: Cst.SignatureItem.t) ->
  match item with
  | Cst.SignatureItem.TypeDeclaration declaration -> walk.type_declaration ctx declaration
  | Cst.SignatureItem.TypeExtension declaration -> walk.type_extension ctx declaration
  | Cst.SignatureItem.Attribute attribute -> walk.attribute ctx attribute
  | Cst.SignatureItem.Extension extension -> walk.extension ctx extension
  | Cst.SignatureItem.ClassDeclaration declaration -> walk.class_declaration ctx declaration
  | Cst.SignatureItem.ClassTypeDeclaration declaration -> walk.class_type_declaration ctx declaration
  | Cst.SignatureItem.ModuleDeclaration declaration -> walk.module_signature ctx declaration
  | Cst.SignatureItem.ModuleTypeDeclaration declaration -> walk.module_type_declaration ctx declaration
  | Cst.SignatureItem.OpenStatement statement -> walk.open_statement ctx statement
  | Cst.SignatureItem.Docstring _ -> ctx
  | Cst.SignatureItem.Comment _ -> ctx
  | Cst.SignatureItem.ValueDeclaration declaration -> walk.value_declaration ctx declaration
  | Cst.SignatureItem.ExternalDeclaration declaration -> walk.external_declaration ctx declaration
  | Cst.SignatureItem.IncludeStatement statement -> walk.include_statement ctx statement
  | Cst.SignatureItem.ExceptionDeclaration declaration -> walk.exception_declaration ctx declaration

and descend_implementation = fun walk ctx (implementation: Cst.implementation) ->
  List.fold_left walk.structure_item ctx implementation.items

and descend_interface = fun walk ctx (interface: Cst.interface) ->
  List.fold_left walk.signature_item ctx interface.items

and descend_source_file = fun walk ctx (source_file: Cst.SourceFile.t) ->
  match source_file with
  | Cst.Implementation implementation -> walk.implementation ctx implementation
  | Cst.Interface interface -> walk.interface ctx interface

let default = {
  visit_apply_argument =
    (fun ctx walk node ->
      walk.descend_apply_argument ctx node);
  visit_attribute =
    (fun ctx walk node ->
      walk.descend_attribute ctx node);
  visit_binding_operator_binding =
    (fun ctx walk node ->
      walk.descend_binding_operator_binding ctx node);
  visit_class_declaration =
    (fun ctx walk node ->
      walk.descend_class_declaration ctx node);
  visit_class_definition =
    (fun ctx walk node ->
      walk.descend_class_definition ctx node);
  visit_class_expression =
    (fun ctx walk node ->
      walk.descend_class_expression ctx node);
  visit_class_field =
    (fun ctx walk node ->
      walk.descend_class_field ctx node);
  visit_class_type =
    (fun ctx walk node ->
      walk.descend_class_type ctx node);
  visit_class_type_declaration =
    (fun ctx walk node ->
      walk.descend_class_type_declaration ctx node);
  visit_class_type_field =
    (fun ctx walk node ->
      walk.descend_class_type_field ctx node);
  visit_core_type =
    (fun ctx walk node ->
      walk.descend_core_type ctx node);
  visit_exception_declaration =
    (fun ctx walk node ->
      walk.descend_exception_declaration ctx node);
  visit_expression =
    (fun ctx walk node ->
      walk.descend_expression ctx node);
  visit_extension =
    (fun ctx walk node ->
      walk.descend_extension ctx node);
  visit_external_declaration =
    (fun ctx walk node ->
      walk.descend_external_declaration ctx node);
  visit_functor_parameter =
    (fun ctx walk node ->
      walk.descend_functor_parameter ctx node);
  visit_implementation =
    (fun ctx walk node ->
      walk.descend_implementation ctx node);
  visit_include_statement =
    (fun ctx walk node ->
      walk.descend_include_statement ctx node);
  visit_interface =
    (fun ctx walk node ->
      walk.descend_interface ctx node);
  visit_let_binding =
    (fun ctx walk node ->
      walk.descend_let_binding ctx node);
  visit_match_case =
    (fun ctx walk node ->
      walk.descend_match_case ctx node);
  visit_module_signature =
    (fun ctx walk node ->
      walk.descend_module_signature ctx node);
  visit_module_structure =
    (fun ctx walk node ->
      walk.descend_module_structure ctx node);
  visit_module_expression =
    (fun ctx walk node ->
      walk.descend_module_expression ctx node);
  visit_module_type =
    (fun ctx walk node ->
      walk.descend_module_type ctx node);
  visit_module_type_constraint =
    (fun ctx walk node ->
      walk.descend_module_type_constraint ctx node);
  visit_module_type_declaration =
    (fun ctx walk node ->
      walk.descend_module_type_declaration ctx node);
  visit_object_member =
    (fun ctx walk node ->
      walk.descend_object_member ctx node);
  visit_object_type_field =
    (fun ctx walk node ->
      walk.descend_object_type_field ctx node);
  visit_open_statement =
    (fun ctx walk node ->
      walk.descend_open_statement ctx node);
  visit_parameter =
    (fun ctx walk node ->
      walk.descend_parameter ctx node);
  visit_pattern =
    (fun ctx walk node ->
      walk.descend_pattern ctx node);
  visit_payload =
    (fun ctx walk node ->
      walk.descend_payload ctx node);
  visit_record_expression =
    (fun ctx walk node ->
      walk.descend_record_expression ctx node);
  visit_record_type_field =
    (fun ctx walk node ->
      walk.descend_record_type_field ctx node);
  visit_row_field =
    (fun ctx walk node ->
      walk.descend_row_field ctx node);
  visit_signature_item =
    (fun ctx walk node ->
      walk.descend_signature_item ctx node);
  visit_source_file =
    (fun ctx walk node ->
      walk.descend_source_file ctx node);
  visit_structure_item =
    (fun ctx walk node ->
      walk.descend_structure_item ctx node);
  visit_type_binder =
    (fun ctx walk node ->
      walk.descend_type_binder ctx node);
  visit_type_constraint =
    (fun ctx walk node ->
      walk.descend_type_constraint ctx node);
  visit_type_declaration =
    (fun ctx walk node ->
      walk.descend_type_declaration ctx node);
  visit_type_definition =
    (fun ctx walk node ->
      walk.descend_type_definition ctx node);
  visit_type_extension =
    (fun ctx walk node ->
      walk.descend_type_extension ctx node);
  visit_type_parameter =
    (fun ctx walk node ->
      walk.descend_type_parameter ctx node);
  visit_value_declaration =
    (fun ctx walk node ->
      walk.descend_value_declaration ctx node);
  visit_variant_constructor =
    (fun ctx walk node ->
      walk.descend_variant_constructor ctx node);
}

let walker = fun visitor ->
  let rec walk = {
    apply_argument =
      (fun ctx node ->
        visitor.visit_apply_argument ctx walk node);
    attribute =
      (fun ctx node ->
        visitor.visit_attribute ctx walk node);
    binding_operator_binding =
      (fun ctx node ->
        visitor.visit_binding_operator_binding ctx walk node);
    class_declaration =
      (fun ctx node ->
        visitor.visit_class_declaration ctx walk node);
    class_definition =
      (fun ctx node ->
        visitor.visit_class_definition ctx walk node);
    class_expression =
      (fun ctx node ->
        visitor.visit_class_expression ctx walk node);
    class_field =
      (fun ctx node ->
        visitor.visit_class_field ctx walk node);
    class_type =
      (fun ctx node ->
        visitor.visit_class_type ctx walk node);
    class_type_declaration =
      (fun ctx node ->
        visitor.visit_class_type_declaration ctx walk node);
    class_type_field =
      (fun ctx node ->
        visitor.visit_class_type_field ctx walk node);
    core_type =
      (fun ctx node ->
        visitor.visit_core_type ctx walk node);
    exception_declaration =
      (fun ctx node ->
        visitor.visit_exception_declaration ctx walk node);
    expression =
      (fun ctx node ->
        visitor.visit_expression ctx walk node);
    extension =
      (fun ctx node ->
        visitor.visit_extension ctx walk node);
    external_declaration =
      (fun ctx node ->
        visitor.visit_external_declaration ctx walk node);
    functor_parameter =
      (fun ctx node ->
        visitor.visit_functor_parameter ctx walk node);
    implementation =
      (fun ctx node ->
        visitor.visit_implementation ctx walk node);
    include_statement =
      (fun ctx node ->
        visitor.visit_include_statement ctx walk node);
    interface =
      (fun ctx node ->
        visitor.visit_interface ctx walk node);
    let_binding =
      (fun ctx node ->
        visitor.visit_let_binding ctx walk node);
    match_case =
      (fun ctx node ->
        visitor.visit_match_case ctx walk node);
    module_signature =
      (fun ctx node ->
        visitor.visit_module_signature ctx walk node);
    module_structure =
      (fun ctx node ->
        visitor.visit_module_structure ctx walk node);
    module_expression =
      (fun ctx node ->
        visitor.visit_module_expression ctx walk node);
    module_type =
      (fun ctx node ->
        visitor.visit_module_type ctx walk node);
    module_type_constraint =
      (fun ctx node ->
        visitor.visit_module_type_constraint ctx walk node);
    module_type_declaration =
      (fun ctx node ->
        visitor.visit_module_type_declaration ctx walk node);
    object_member =
      (fun ctx node ->
        visitor.visit_object_member ctx walk node);
    object_type_field =
      (fun ctx node ->
        visitor.visit_object_type_field ctx walk node);
    open_statement =
      (fun ctx node ->
        visitor.visit_open_statement ctx walk node);
    parameter =
      (fun ctx node ->
        visitor.visit_parameter ctx walk node);
    pattern =
      (fun ctx node ->
        visitor.visit_pattern ctx walk node);
    payload =
      (fun ctx node ->
        visitor.visit_payload ctx walk node);
    record_expression =
      (fun ctx node ->
        visitor.visit_record_expression ctx walk node);
    record_type_field =
      (fun ctx node ->
        visitor.visit_record_type_field ctx walk node);
    row_field =
      (fun ctx node ->
        visitor.visit_row_field ctx walk node);
    signature_item =
      (fun ctx node ->
        visitor.visit_signature_item ctx walk node);
    source_file =
      (fun ctx node ->
        visitor.visit_source_file ctx walk node);
    structure_item =
      (fun ctx node ->
        visitor.visit_structure_item ctx walk node);
    type_binder =
      (fun ctx node ->
        visitor.visit_type_binder ctx walk node);
    type_constraint =
      (fun ctx node ->
        visitor.visit_type_constraint ctx walk node);
    type_declaration =
      (fun ctx node ->
        visitor.visit_type_declaration ctx walk node);
    type_definition =
      (fun ctx node ->
        visitor.visit_type_definition ctx walk node);
    type_extension =
      (fun ctx node ->
        visitor.visit_type_extension ctx walk node);
    type_parameter =
      (fun ctx node ->
        visitor.visit_type_parameter ctx walk node);
    value_declaration =
      (fun ctx node ->
        visitor.visit_value_declaration ctx walk node);
    variant_constructor =
      (fun ctx node ->
        visitor.visit_variant_constructor ctx walk node);
    descend_apply_argument = (fun ctx node -> descend_apply_argument walk ctx node);
    descend_attribute = (fun ctx node -> descend_attribute walk ctx node);
    descend_binding_operator_binding = (fun ctx node -> descend_binding_operator_binding walk ctx node);
    descend_class_declaration = (fun ctx node -> descend_class_declaration walk ctx node);
    descend_class_expression = (fun ctx node -> descend_class_expression walk ctx node);
    descend_class_field = (fun ctx node -> descend_class_field walk ctx node);
    descend_class_definition = (fun ctx node -> descend_class_definition walk ctx node);
    descend_class_type = (fun ctx node -> descend_class_type walk ctx node);
    descend_class_type_declaration = (fun ctx node -> descend_class_type_declaration walk ctx node);
    descend_class_type_field = (fun ctx node -> descend_class_type_field walk ctx node);
    descend_core_type = (fun ctx node -> descend_core_type walk ctx node);
    descend_exception_declaration = (fun ctx node -> descend_exception_declaration walk ctx node);
    descend_expression = (fun ctx node -> descend_expression walk ctx node);
    descend_extension = (fun ctx node -> descend_extension walk ctx node);
    descend_external_declaration = (fun ctx node -> descend_external_declaration walk ctx node);
    descend_functor_parameter = (fun ctx node -> descend_functor_parameter walk ctx node);
    descend_implementation = (fun ctx node -> descend_implementation walk ctx node);
    descend_include_statement = (fun ctx node -> descend_include_statement walk ctx node);
    descend_interface = (fun ctx node -> descend_interface walk ctx node);
    descend_let_binding = (fun ctx node -> descend_let_binding walk ctx node);
    descend_match_case = (fun ctx node -> descend_match_case walk ctx node);
    descend_module_signature = (fun ctx node -> descend_module_signature walk ctx node);
    descend_module_structure = (fun ctx node -> descend_module_structure walk ctx node);
    descend_module_expression = (fun ctx node -> descend_module_expression walk ctx node);
    descend_module_type = (fun ctx node -> descend_module_type walk ctx node);
    descend_module_type_constraint = (fun ctx node -> descend_module_type_constraint walk ctx node);
    descend_module_type_declaration = (fun ctx node -> descend_module_type_declaration walk ctx node);
    descend_object_member = (fun ctx node -> descend_object_member walk ctx node);
    descend_object_type_field = (fun ctx node -> descend_object_type_field walk ctx node);
    descend_open_statement = (fun ctx node -> descend_open_statement walk ctx node);
    descend_parameter = (fun ctx node -> descend_parameter walk ctx node);
    descend_pattern = (fun ctx node -> descend_pattern walk ctx node);
    descend_payload = (fun ctx node -> descend_payload walk ctx node);
    descend_record_expression = (fun ctx node -> descend_record_expression walk ctx node);
    descend_record_type_field = (fun ctx node -> descend_record_type_field walk ctx node);
    descend_row_field = (fun ctx node -> descend_row_field walk ctx node);
    descend_signature_item = (fun ctx node -> descend_signature_item walk ctx node);
    descend_source_file = (fun ctx node -> descend_source_file walk ctx node);
    descend_structure_item = (fun ctx node -> descend_structure_item walk ctx node);
    descend_type_binder = (fun ctx node -> descend_type_binder walk ctx node);
    descend_type_constraint = (fun ctx node -> descend_type_constraint walk ctx node);
    descend_type_declaration = (fun ctx node -> descend_type_declaration walk ctx node);
    descend_type_definition = (fun ctx node -> descend_type_definition walk ctx node);
    descend_type_extension = (fun ctx node -> descend_type_extension walk ctx node);
    descend_type_parameter = (fun ctx node -> descend_type_parameter walk ctx node);
    descend_value_declaration = (fun ctx node -> descend_value_declaration walk ctx node);
    descend_variant_constructor = (fun ctx node -> descend_variant_constructor walk ctx node);
  }
  in
  walk

let apply_argument = fun visitor ctx node -> (walker visitor).apply_argument ctx node

let attribute = fun visitor ctx node -> (walker visitor).attribute ctx node

let binding_operator_binding = fun visitor ctx node ->
  (walker visitor).binding_operator_binding ctx node

let class_declaration = fun visitor ctx node -> (walker visitor).class_declaration ctx node

let class_definition = fun visitor ctx node -> (walker visitor).class_definition ctx node

let class_expression = fun visitor ctx node -> (walker visitor).class_expression ctx node

let class_field = fun visitor ctx node -> (walker visitor).class_field ctx node

let class_type = fun visitor ctx node -> (walker visitor).class_type ctx node

let class_type_declaration = fun visitor ctx node -> (walker visitor).class_type_declaration ctx node

let class_type_field = fun visitor ctx node -> (walker visitor).class_type_field ctx node

let core_type = fun visitor ctx node -> (walker visitor).core_type ctx node

let exception_declaration = fun visitor ctx node -> (walker visitor).exception_declaration ctx node

let expression = fun visitor ctx node -> (walker visitor).expression ctx node

let extension = fun visitor ctx node -> (walker visitor).extension ctx node

let external_declaration = fun visitor ctx node -> (walker visitor).external_declaration ctx node

let functor_parameter = fun visitor ctx node -> (walker visitor).functor_parameter ctx node

let implementation = fun visitor ctx node -> (walker visitor).implementation ctx node

let include_statement = fun visitor ctx node -> (walker visitor).include_statement ctx node

let interface = fun visitor ctx node -> (walker visitor).interface ctx node

let let_binding = fun visitor ctx node -> (walker visitor).let_binding ctx node

let match_case = fun visitor ctx node -> (walker visitor).match_case ctx node

let module_signature = fun visitor ctx node -> (walker visitor).module_signature ctx node

let module_structure = fun visitor ctx node -> (walker visitor).module_structure ctx node

let module_expression = fun visitor ctx node -> (walker visitor).module_expression ctx node

let module_type = fun visitor ctx node -> (walker visitor).module_type ctx node

let module_type_constraint = fun visitor ctx node -> (walker visitor).module_type_constraint ctx node

let module_type_declaration = fun visitor ctx node -> (walker visitor).module_type_declaration ctx node

let object_member = fun visitor ctx node -> (walker visitor).object_member ctx node

let object_type_field = fun visitor ctx node -> (walker visitor).object_type_field ctx node

let open_statement = fun visitor ctx node -> (walker visitor).open_statement ctx node

let parameter = fun visitor ctx node -> (walker visitor).parameter ctx node

let pattern = fun visitor ctx node -> (walker visitor).pattern ctx node

let payload = fun visitor ctx node -> (walker visitor).payload ctx node

let record_expression = fun visitor ctx node -> (walker visitor).record_expression ctx node

let record_type_field = fun visitor ctx node -> (walker visitor).record_type_field ctx node

let row_field = fun visitor ctx node -> (walker visitor).row_field ctx node

let signature_item = fun visitor ctx node -> (walker visitor).signature_item ctx node

let source_file = fun visitor ctx node -> (walker visitor).source_file ctx node

let structure_item = fun visitor ctx node -> (walker visitor).structure_item ctx node

let type_binder = fun visitor ctx node -> (walker visitor).type_binder ctx node

let type_constraint = fun visitor ctx node -> (walker visitor).type_constraint ctx node

let type_declaration = fun visitor ctx node -> (walker visitor).type_declaration ctx node

let type_definition = fun visitor ctx node -> (walker visitor).type_definition ctx node

let type_extension = fun visitor ctx node -> (walker visitor).type_extension ctx node

let type_parameter = fun visitor ctx node -> (walker visitor).type_parameter ctx node

let value_declaration = fun visitor ctx node -> (walker visitor).value_declaration ctx node

let variant_constructor = fun visitor ctx node -> (walker visitor).variant_constructor ctx node

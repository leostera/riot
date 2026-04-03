open Std
open Std.Collections
open Std.Data

module Env = struct
  module Names = struct
    type t = string list

    let empty = []

    let singleton = fun name -> [ name ]

    let union = fun left right ->
      List.sort_uniq String.compare (left @ right)

    let elements = fun names -> names
  end

  type node =
    Node of Names.t * t

  and t = (string * node) list

  let empty = []

  let bound = Node (Names.empty, [])

  let singleton_name = Names.singleton

  let make_leaf name = Node (Names.singleton name, [])

  let make_node map = Node (Names.empty, map)

  let rec remove = fun name ->
    function
    | [] -> []
    | (key, _) :: rest when key = name -> rest
    | entry :: rest -> entry :: remove name rest

  let add = fun name node env -> (name, node) :: remove name env

  let merge = fun left right ->
    List.fold_left (fun env (name, node) -> add name node env) left right

  let top_free = function
    | Node (free, _) -> free

  let children = function
    | Node (_, children) -> children

  let rec collect_free = function
    | Node (free, children) ->
        List.fold_left
          (fun acc (_, child) ->
            Names.union acc (collect_free child))
          free
          children

  let merge_children env node = merge env (children node)

  let rec find = fun name ->
    function
    | [] -> raise Not_found
    | (key, value) :: _ when key = name -> value
    | _ :: rest -> find name rest

  let rec lookup_free segments env =
    match segments with
    | [] -> raise Not_found
    | segment :: rest ->
        let Node (free, children) = find segment env in
        (
          match rest with
          | [] -> free
          | _ -> (
              match lookup_free rest children with
              | free -> free
              | exception Not_found -> free
            )
        )

  let rec lookup_map segments env =
    match segments with
    | [] ->
        raise Not_found
    | [ segment ] ->
        find segment env
    | segment :: rest ->
        let Node (_, children) = find segment env in
        lookup_map rest children
end

module DepSet = struct
  type t = string HashSet.t

  let empty = fun () -> HashSet.create ()

  let add = fun deps name ->
    let _ = HashSet.insert deps name in
    deps

  let add_names = fun deps names ->
    List.iter
      (fun name ->
        let _ = HashSet.insert deps name in
        ())
      names;
    deps

  let elements = fun deps -> HashSet.to_list deps |> List.sort String.compare
end

type t = {
  modules: string list;
  env: Env.t;
}

type parse_error =
  | Parse_diagnostics of Diagnostic.t list
  | Cst_builder_error of Cst_builder.error

let ( let* ) = fun result f ->
  match result with
  | Ok value -> f value
  | Error _ as error -> error

let modules = fun t -> t.modules

let env = fun t -> t.env

let to_json = fun t ->
  Json.Object [ ("modules", Json.Array (List.map (fun name -> Json.String name) t.modules)) ]

let segments_of_ident = fun ident -> Cst.Ident.segments ident |> List.map Cst.Token.text

let drop_last = function
  | [] ->
      []
  | [ _ ] ->
      []
  | items ->
      let rec loop acc = function
        | []
        | [ _ ] -> List.rev acc
        | head :: tail -> loop (head :: acc) tail
      in
      loop [] items

let is_uppercase_ascii = fun ch -> ch >= 'A' && ch <= 'Z'

let is_module_head = fun segment -> String.length segment > 0 && is_uppercase_ascii segment.[0]

let add_names = DepSet.add_names

let add_path = fun env deps segments ->
  match segments with
  | [] -> deps
  | head :: _ ->
      let names =
        match Env.lookup_free segments env with
        | names -> names
        | exception Not_found -> Env.singleton_name head
      in
      add_names deps names

let add_parent = fun env deps ident ->
  let segments = segments_of_ident ident |> drop_last in
  add_path env deps segments

let add_module_path = fun env deps ident -> add_path env deps (segments_of_ident ident)

let rec module_like_segments_of_expression = function
  | Cst.Expression.Path { path; _ } -> (
      match segments_of_ident path with
      | head :: _ as segments when is_module_head head -> Some segments
      | _ -> None
    )
  | Cst.Expression.FieldAccess { receiver; field_name; _ } -> (
      match module_like_segments_of_expression receiver with
      | Some segments -> Some (segments @ [ Cst.Token.text field_name ])
      | None -> None
    )
  | _ ->
      None

let collect_option = fun f env deps value ->
  match value with
  | Some value -> f env deps value
  | None -> Ok deps

let collect_list = fun f env deps values ->
  List.fold_left
    (fun acc value ->
      let* deps = acc in
      f env deps value)
    (Ok deps)
    values

let collect_list_with = fun f acc values ->
  List.fold_left
    (fun acc value ->
      let* acc = acc in
      f acc value)
    (Ok acc)
    values

let module_alias = fun env deps ident ->
  let deps = add_module_path env deps ident in
  let binding =
    match Env.lookup_map (segments_of_ident ident) env with
    | node -> node
    | exception Not_found -> (
        match segments_of_ident ident with
        | [ name ] -> Env.make_leaf name
        | _ -> Env.bound
      )
  in
  (deps, binding)

let open_alias = fun env deps ident ->
  let deps, binding = module_alias env deps ident in
  let deps = add_names deps (Env.top_free binding) in
  (deps, Env.merge_children env binding)

let rec collect_core_type env deps type_ =
  match type_ with
  | Cst.CoreType.Wildcard _ ->
      Ok deps
  | Cst.CoreType.Var _ ->
      Ok deps
  | Cst.CoreType.Constr { constructor_path; arguments; _ } ->
      let deps = add_parent env deps constructor_path in
      collect_list collect_core_type env deps arguments
  | Cst.CoreType.Class { class_path; arguments; _ } ->
      let deps = add_parent env deps class_path in
      collect_list collect_core_type env deps arguments
  | Cst.CoreType.Alias { type_; _ } ->
      collect_core_type env deps type_
  | Cst.CoreType.Attribute { type_; _ } ->
      collect_core_type env deps type_
  | Cst.CoreType.Extension _ ->
      Ok deps
  | Cst.CoreType.Poly { body; _ } ->
      collect_core_type env deps body
  | Cst.CoreType.Arrow { parameter_type; result_type; _ } ->
      let* deps = collect_core_type env deps parameter_type in
      collect_core_type env deps result_type
  | Cst.CoreType.Tuple { elements; _ } ->
      collect_list collect_core_type env deps elements
  | Cst.CoreType.Parenthesized { inner; _ } ->
      collect_core_type env deps inner
  | Cst.CoreType.PolyVariant poly_variant ->
      collect_list collect_row_field env deps poly_variant.fields
  | Cst.CoreType.Record { fields; _ } ->
      collect_list collect_inline_record_type_field env deps fields
  | Cst.CoreType.FirstClassModule { package_type; _ } ->
      collect_package_type env deps package_type
  | Cst.CoreType.Object { fields; _ } ->
      collect_list collect_object_type_field env deps fields

and collect_row_field env deps field =
  match field with
  | Cst.RowField.Tag tag -> collect_option collect_core_type env deps tag.payload_type
  | Cst.RowField.Inherit { type_; _ } -> collect_core_type env deps type_

and collect_module_type_constraint env deps constraint_ =
  let* deps = collect_core_type env deps constraint_.Cst.ModuleTypeConstraint.constrained_type in
  collect_core_type env deps constraint_.replacement_type

and collect_package_type env deps (package_type: Cst.package_type) =
  let deps = add_parent env deps package_type.module_type_path in
  collect_list collect_module_type_constraint env deps package_type.constraints

and collect_type_constraint env deps constraint_ =
  let* deps = collect_core_type env deps constraint_.Cst.TypeConstraint.left in
  collect_core_type env deps constraint_.right

and collect_inline_record_type_field env deps (field: Cst.record_type_field) = collect_core_type
  env
  deps
  field.field_type

and collect_record_type_field env deps (field: Cst.RecordField.t) = collect_core_type
  env
  deps
  field.field_type

and collect_object_type_field env deps (field: Cst.object_type_field) = collect_core_type
  env
  deps
  field.field_type

and collect_variant_constructor_arguments env deps arguments =
  match arguments with
  | Cst.ConstructorArguments.Tuple types -> collect_list collect_core_type env deps types
  | Cst.ConstructorArguments.Record { fields; _ } -> collect_list
    collect_record_type_field
    env
    deps
    fields

and collect_variant_constructor env deps (constructor: Cst.VariantConstructor.t) =
  let* deps =
    match constructor.arguments with
    | Some arguments -> collect_variant_constructor_arguments env deps arguments
    | None -> Ok deps
  in
  let* deps = collect_option collect_core_type env deps constructor.payload_type in
  collect_option collect_core_type env deps constructor.result_type

and collect_type_extension env deps (extension: Cst.TypeExtension.t) =
  let deps = add_parent env deps extension.type_name in
  collect_list collect_variant_constructor env deps extension.constructors

and collect_type_definition env deps definition =
  match definition with
  | Cst.TypeDefinition.Abstract -> Ok deps
  | Cst.TypeDefinition.Alias { manifest; _ } -> collect_core_type env deps manifest
  | Cst.TypeDefinition.Extensible _ -> Ok deps
  | Cst.TypeDefinition.FirstClassModule { package_type; _ } -> collect_package_type env deps package_type
  | Cst.TypeDefinition.Object { fields; _ } -> collect_list collect_object_type_field env deps fields
  | Cst.TypeDefinition.Record { fields; _ } -> collect_list collect_record_type_field env deps fields
  | Cst.TypeDefinition.Variant { constructors; _ } -> collect_list
    collect_variant_constructor
    env
    deps
    constructors
  | Cst.TypeDefinition.PolyVariant poly_variant -> collect_list
    collect_row_field
    env
    deps
    poly_variant.fields

and collect_type_declaration env deps declaration =
  let* deps = collect_option collect_core_type env deps declaration.Cst.TypeDeclaration.manifest_alias in
  let* deps = collect_type_definition env deps declaration.type_definition in
  let* deps = collect_list collect_type_constraint env deps declaration.constraints in
  match Cst.TypeDeclaration.next_and_declaration declaration with
  | Some next -> collect_type_declaration env deps next
  | None -> Ok deps

and collect_functor_parameters env deps parameters =
  List.fold_left
    (fun acc parameter ->
      let* (deps, env) = acc in
      let* deps = collect_module_type env deps parameter.Cst.FunctorParameter.module_type in
      let env = Env.add (Cst.Token.text parameter.name_token) Env.bound env in
      Ok (deps, env))
    (Ok (deps, env))
    parameters

and module_type_binding env deps module_type =
  match module_type with
  | Cst.ModuleType.Path path ->
      Ok (add_parent env deps path, Env.bound)
  | Cst.ModuleType.TypeOf { module_path; _ } ->
      let deps, binding = module_alias env deps module_path in
      Ok (deps, binding)
  | Cst.ModuleType.Signature _ ->
      let* items = Cst_builder.signature_items_of_module_type module_type in
      let* (deps, _, bindings) = collect_signature_binding env deps items in
      Ok (deps, Env.make_node bindings)
  | Cst.ModuleType.Functor { parameters; result; _ } ->
      let* (deps, env) = collect_functor_parameters env deps parameters in
      let* deps = collect_module_type env deps result in
      Ok (deps, Env.bound)
  | Cst.ModuleType.With { base; constraints; _ } ->
      let* deps = collect_module_type env deps base in
      let* deps = collect_list collect_module_type_constraint env deps constraints in
      Ok (deps, Env.bound)
  | Cst.ModuleType.Parenthesized { inner; _ } ->
      module_type_binding env deps inner
  | Cst.ModuleType.Attribute { module_type; _ } ->
      module_type_binding env deps module_type
  | Cst.ModuleType.Extension _ ->
      Ok (deps, Env.bound)

and collect_module_type env deps module_type =
  match module_type with
  | Cst.ModuleType.Path path ->
      Ok (add_parent env deps path)
  | Cst.ModuleType.TypeOf { module_path; _ } ->
      Ok (add_module_path env deps module_path)
  | Cst.ModuleType.Signature _ ->
      let* items = Cst_builder.signature_items_of_module_type module_type in
      let* (deps, _, _) = collect_signature_binding env deps items in
      Ok deps
  | Cst.ModuleType.Functor { parameters; result; _ } ->
      let* (deps, env) = collect_functor_parameters env deps parameters in
      collect_module_type env deps result
  | Cst.ModuleType.With { base; constraints; _ } ->
      let* deps = collect_module_type env deps base in
      collect_list collect_module_type_constraint env deps constraints
  | Cst.ModuleType.Parenthesized { inner; _ } ->
      collect_module_type env deps inner
  | Cst.ModuleType.Attribute { module_type; _ } ->
      collect_module_type env deps module_type
  | Cst.ModuleType.Extension _ ->
      Ok deps

and module_binding env deps module_expression =
  match module_expression with
  | Cst.ModuleExpression.Path path ->
      let deps, binding = module_alias env deps path in
      Ok (deps, binding)
  | Cst.ModuleExpression.Structure _ ->
      let* items = Cst_builder.structure_items_of_module_expression module_expression in
      let* (deps, _, bindings) = collect_structure_binding env deps items in
      Ok (deps, Env.make_node bindings)
  | Cst.ModuleExpression.Functor { parameters; body; _ } ->
      let* (deps, env) = collect_functor_parameters env deps parameters in
      let* deps = collect_module_expression env deps body in
      Ok (deps, Env.bound)
  | Cst.ModuleExpression.Apply { callee; argument; _ } ->
      let* deps = collect_module_expression env deps callee in
      let* deps = collect_module_expression env deps argument in
      Ok (deps, Env.bound)
  | Cst.ModuleExpression.ApplyUnit { callee; _ } ->
      let* deps = collect_module_expression env deps callee in
      Ok (deps, Env.bound)
  | Cst.ModuleExpression.Constraint { module_expression; module_type; _ } ->
      let* deps = collect_module_expression env deps module_expression in
      let* deps = collect_module_type env deps module_type in
      Ok (deps, Env.bound)
  | Cst.ModuleExpression.ModuleUnpack { expression; _ } ->
      let* deps = collect_expression env deps expression in
      Ok (deps, Env.bound)
  | Cst.ModuleExpression.Parenthesized { inner; _ } ->
      module_binding env deps inner
  | Cst.ModuleExpression.Attribute { module_expression; _ } ->
      module_binding env deps module_expression
  | Cst.ModuleExpression.Extension _ ->
      Ok (deps, Env.bound)

and collect_module_expression env deps module_expression =
  match module_expression with
  | Cst.ModuleExpression.Path path ->
      Ok (add_module_path env deps path)
  | Cst.ModuleExpression.Structure _ ->
      let* items = Cst_builder.structure_items_of_module_expression module_expression in
      let* (deps, _) = collect_structure env deps items in
      Ok deps
  | Cst.ModuleExpression.Functor { parameters; body; _ } ->
      let* (deps, env) = collect_functor_parameters env deps parameters in
      collect_module_expression env deps body
  | Cst.ModuleExpression.Apply { callee; argument; _ } ->
      let* deps = collect_module_expression env deps callee in
      collect_module_expression env deps argument
  | Cst.ModuleExpression.ApplyUnit { callee; _ } ->
      collect_module_expression env deps callee
  | Cst.ModuleExpression.Constraint { module_expression; module_type; _ } ->
      let* deps = collect_module_expression env deps module_expression in
      collect_module_type env deps module_type
  | Cst.ModuleExpression.ModuleUnpack { expression; _ } ->
      collect_expression env deps expression
  | Cst.ModuleExpression.Parenthesized { inner; _ } ->
      collect_module_expression env deps inner
  | Cst.ModuleExpression.Attribute { module_expression; _ } ->
      collect_module_expression env deps module_expression
  | Cst.ModuleExpression.Extension _ ->
      Ok deps

and collect_apply_argument env deps argument =
  match argument with
  | Cst.Positional expression -> collect_expression env deps expression
  | Cst.Labeled { value; _ } -> collect_option collect_expression env deps value
  | Cst.Optional { value; _ } -> collect_option collect_expression env deps value

and bind_pattern_modules env pattern =
  match pattern with
  | Cst.Pattern.Identifier _ ->
      env
  | Cst.Pattern.Wildcard _ ->
      env
  | Cst.Pattern.Extension _ ->
      env
  | Cst.Pattern.Literal _ ->
      env
  | Cst.Pattern.Range _ ->
      env
  | Cst.Pattern.Operator _ ->
      env
  | Cst.Pattern.Lazy { pattern; _ } ->
      bind_pattern_modules env pattern
  | Cst.Pattern.Exception { pattern; _ } ->
      bind_pattern_modules env pattern
  | Cst.Pattern.FirstClassModule { binding; _ } -> (
      match binding with
      | Cst.Named { name_token } -> Env.add (Cst.Token.text name_token) Env.bound env
      | Cst.Anonymous _ -> env
    )
  | Cst.Pattern.PolyVariant { payload; _ } -> (
      match payload with
      | Some pattern -> bind_pattern_modules env pattern
      | None -> env
    )
  | Cst.Pattern.PolyVariantInherit _ ->
      env
  | Cst.Pattern.Constructor { arguments; _ } ->
      List.fold_left bind_pattern_modules env arguments
  | Cst.Pattern.Tuple { elements; _ } ->
      List.fold_left
        (fun env (element: Cst.tuple_pattern_element) -> bind_pattern_modules env element.pattern)
        env
        elements
  | Cst.Pattern.List { elements; _ } ->
      List.fold_left bind_pattern_modules env elements
  | Cst.Pattern.Array { elements; _ } ->
      List.fold_left bind_pattern_modules env elements
  | Cst.Pattern.Record { fields; _ } ->
      List.fold_left
        (fun env (field: Cst.record_pattern_field) ->
          match field.pattern with
          | Some pattern -> bind_pattern_modules env pattern
          | None -> env)
        env
        fields
  | Cst.Pattern.Cons { head; tail; _ } ->
      let env = bind_pattern_modules env head in
      bind_pattern_modules env tail
  | Cst.Pattern.Or { alternatives; _ } ->
      List.fold_left bind_pattern_modules env alternatives
  | Cst.Pattern.Alias { pattern; _ } ->
      bind_pattern_modules env pattern
  | Cst.Pattern.Typed { pattern; _ } ->
      bind_pattern_modules env pattern
  | Cst.Pattern.Effect { effect_pattern; continuation; _ } ->
      let env = bind_pattern_modules env effect_pattern in
      bind_pattern_modules env continuation
  | Cst.Pattern.LocalOpen { pattern; _ } ->
      bind_pattern_modules env pattern
  | Cst.Pattern.Parenthesized { inner; _ } ->
      bind_pattern_modules env inner

and bind_parameter_modules env parameter =
  match parameter with
  | Cst.Parameter.Positional positional ->
      bind_pattern_modules env positional.pattern
  | Cst.Parameter.Labeled labeled -> (
      match labeled.binding_pattern with
      | Some pattern -> bind_pattern_modules env pattern
      | None -> env
    )
  | Cst.Parameter.Optional optional -> (
      match optional.binding_pattern with
      | Some pattern -> bind_pattern_modules env pattern
      | None -> env
    )
  | Cst.Parameter.LocallyAbstract _ ->
      env

and collect_parameters env deps parameters =
  List.fold_left
    (fun acc parameter ->
      let* (deps, env) = acc in
      let* deps = collect_parameter env deps parameter in
      let env = bind_parameter_modules env parameter in
      Ok (deps, env))
    (Ok (deps, env))
    parameters

and bind_let_binding_chain_modules env (binding: Cst.let_binding) =
  let env = bind_pattern_modules env binding.binding_pattern in
  match binding.and_binding with
  | Some binding -> bind_let_binding_chain_modules env binding
  | None -> env

and bind_binding_operator_modules env (binding: Cst.binding_operator_binding) =
  let env = bind_pattern_modules env binding.binding_pattern in
  match binding.and_binding with
  | Some binding -> bind_binding_operator_modules env binding
  | None -> env

and collect_pattern env deps pattern =
  match pattern with
  | Cst.Pattern.Identifier _ ->
      Ok deps
  | Cst.Pattern.Wildcard _ ->
      Ok deps
  | Cst.Pattern.Extension _ ->
      Ok deps
  | Cst.Pattern.Literal _ ->
      Ok deps
  | Cst.Pattern.Lazy { pattern; _ } ->
      collect_pattern env deps pattern
  | Cst.Pattern.Exception { pattern; _ } ->
      collect_pattern env deps pattern
  | Cst.Pattern.Range _ ->
      Ok deps
  | Cst.Pattern.Operator _ ->
      Ok deps
  | Cst.Pattern.FirstClassModule { package_type; _ } ->
      collect_option collect_package_type env deps package_type
  | Cst.Pattern.PolyVariant { payload; _ } ->
      collect_option collect_pattern env deps payload
  | Cst.Pattern.PolyVariantInherit { type_path; _ } ->
      Ok (add_parent env deps type_path)
  | Cst.Pattern.Constructor { constructor_path; arguments; _ } ->
      let deps = add_parent env deps constructor_path in
      collect_list collect_pattern env deps arguments
  | Cst.Pattern.Tuple { elements; _ } ->
      collect_list
        (fun env deps (element: Cst.tuple_pattern_element) -> collect_pattern env deps element.pattern)
        env
        deps
        elements
  | Cst.Pattern.List { elements; _ } ->
      collect_list collect_pattern env deps elements
  | Cst.Pattern.Array { elements; _ } ->
      collect_list collect_pattern env deps elements
  | Cst.Pattern.Record { fields; _ } ->
      let deps =
        List.fold_left
          (fun deps (field: Cst.record_pattern_field) -> add_parent env deps field.field_path)
          deps
          fields
      in
      collect_list
        (fun env deps (field: Cst.record_pattern_field) ->
          collect_option collect_pattern env deps field.pattern)
        env
        deps
        fields
  | Cst.Pattern.Cons { head; tail; _ } ->
      let* deps = collect_pattern env deps head in
      collect_pattern env deps tail
  | Cst.Pattern.Or { alternatives; _ } ->
      collect_list collect_pattern env deps alternatives
  | Cst.Pattern.Alias { pattern; _ } ->
      collect_pattern env deps pattern
  | Cst.Pattern.Typed { pattern; type_; _ } ->
      let* deps = collect_pattern env deps pattern in
      collect_core_type env deps type_
  | Cst.Pattern.Effect { effect_pattern; continuation; _ } ->
      let* deps = collect_pattern env deps effect_pattern in
      collect_pattern env deps continuation
  | Cst.Pattern.LocalOpen { module_path; pattern; _ } ->
      let deps, env = open_alias env deps module_path in
      collect_pattern env deps pattern
  | Cst.Pattern.Parenthesized { inner; _ } ->
      collect_pattern env deps inner

and collect_let_binding_chain env deps (binding: Cst.let_binding) =
  let* deps = collect_pattern env deps binding.binding_pattern in
  let* (deps, value_env) = collect_parameters env deps binding.parameters in
  let* deps = collect_expression value_env deps binding.Cst.LetBinding.value in
  collect_option collect_let_binding_chain env deps binding.and_binding

and collect_let_expression env deps (expression: Cst.let_expression) =
  let* deps = collect_pattern env deps expression.binding_pattern in
  let* (deps, value_env) = collect_parameters env deps expression.parameters in
  let* deps = collect_expression value_env deps expression.bound_value in
  let* deps = collect_option collect_let_binding_chain env deps expression.and_binding in
  let env = bind_pattern_modules env expression.binding_pattern in
  let env =
    match expression.and_binding with
    | Some binding -> bind_let_binding_chain_modules env binding
    | None -> env
  in
  collect_expression env deps expression.body

and collect_binding_operator env deps (binding: Cst.binding_operator_binding) =
  let* deps = collect_pattern env deps binding.binding_pattern in
  let* deps = collect_expression env deps binding.bound_value in
  collect_option collect_binding_operator env deps binding.and_binding

and collect_match_case env deps (case: Cst.match_case) =
  let* deps = collect_pattern env deps case.pattern in
  let env = bind_pattern_modules env case.pattern in
  let* deps = collect_option collect_expression env deps case.guard in
  collect_expression env deps case.body

and collect_parameter env deps parameter =
  match parameter with
  | Cst.Parameter.Positional positional ->
      collect_pattern env deps positional.pattern
  | Cst.Parameter.Labeled labeled ->
      collect_option collect_pattern env deps labeled.binding_pattern
  | Cst.Parameter.Optional optional ->
      let* deps = collect_option collect_pattern env deps optional.binding_pattern in
      collect_option collect_expression env deps optional.default_value
  | Cst.Parameter.LocallyAbstract _ ->
      Ok deps

and collect_expression env deps expression =
  match expression with
  | Cst.Expression.Path { path; _ } ->
      Ok (add_parent env deps path)
  | Cst.Expression.Constructor { constructor_path; payload; _ } ->
      let deps = add_parent env deps constructor_path in
      collect_option collect_expression env deps payload
  | Cst.Expression.Operator _ ->
      Ok deps
  | Cst.Expression.Literal _ ->
      Ok deps
  | Cst.Expression.Unreachable _ ->
      Ok deps
  | Cst.Expression.Extension _ ->
      Ok deps
  | Cst.Expression.Object _ ->
      Ok deps
  | Cst.Expression.PolyVariant { payload; _ } ->
      collect_option collect_expression env deps payload
  | Cst.Expression.ModulePack { module_expression; _ } ->
      collect_module_expression env deps module_expression
  | Cst.Expression.LetModule { module_name_token; module_expression; body; _ } ->
      let* (deps, binding) = module_binding env deps module_expression in
      let env = Env.add (Cst.Token.text module_name_token) binding env in
      collect_expression env deps body
  | Cst.Expression.LetException { body; _ } ->
      collect_expression env deps body
  | Cst.Expression.Assert { asserted; _ } ->
      collect_expression env deps asserted
  | Cst.Expression.Lazy { body; _ } ->
      collect_expression env deps body
  | Cst.Expression.While { condition; body; _ } ->
      let* deps = collect_expression env deps condition in
      collect_expression env deps body
  | Cst.Expression.For { start_expr; end_expr; body; _ } ->
      let* deps = collect_expression env deps start_expr in
      let* deps = collect_expression env deps end_expr in
      collect_expression env deps body
  | Cst.Expression.Apply { callee; argument; _ } ->
      let* deps = collect_expression env deps callee in
      collect_apply_argument env deps argument
  | Cst.Expression.MethodCall { receiver; _ } ->
      collect_expression env deps receiver
  | Cst.Expression.New { class_path; _ } ->
      Ok (add_parent env deps class_path)
  | Cst.Expression.Prefix { operand; _ } ->
      collect_expression env deps operand
  | Cst.Expression.FieldAccess ({ receiver; _ } as field_access) -> (
      match module_like_segments_of_expression (Cst.Expression.FieldAccess field_access) with
      | Some segments -> Ok (add_path env deps (drop_last segments))
      | None -> collect_expression env deps receiver
    )
  | Cst.Expression.Index { collection; index; _ } ->
      let* deps = collect_expression env deps collection in
      collect_expression env deps index
  | Cst.Expression.ObjectOverride _ ->
      Ok deps
  | Cst.Expression.InstanceVariableAssign { value; _ } ->
      collect_expression env deps value
  | Cst.Expression.FieldAssign { target; value; _ } ->
      let* deps = collect_expression env deps target.receiver in
      collect_expression env deps value
  | Cst.Expression.Assign { target; value; _ } ->
      let* deps = collect_expression env deps target in
      collect_expression env deps value
  | Cst.Expression.Infix { left; right; _ } ->
      let* deps = collect_expression env deps left in
      collect_expression env deps right
  | Cst.Expression.TypeAscription { expression; kind; _ } ->
      let* deps = collect_expression env deps expression in
      (
        match kind with
        | Cst.Type { type_; _ } ->
            collect_core_type env deps type_
        | Cst.Coerce { type_; _ } ->
            collect_core_type env deps type_
        | Cst.ConstraintCoerce { from_type; to_type; _ } ->
            let* deps = collect_core_type env deps from_type in
            collect_core_type env deps to_type
      )
  | Cst.Expression.Polymorphic { expression; type_; _ } ->
      let* deps = collect_expression env deps expression in
      collect_core_type env deps type_
  | Cst.Expression.Sequence { expressions; _ } ->
      collect_list collect_expression env deps expressions
  | Cst.Expression.Tuple { elements; _ } ->
      collect_list collect_expression env deps elements
  | Cst.Expression.List { elements; _ } ->
      collect_list collect_expression env deps elements
  | Cst.Expression.Array { elements; _ } ->
      collect_list collect_expression env deps elements
  | Cst.Expression.Record (Cst.Literal { fields; _ }) ->
      let deps =
        List.fold_left
          (fun deps (field: Cst.record_expression_field) -> add_parent env deps field.field_path)
          deps
          fields
      in
      collect_list
        (fun env deps (field: Cst.record_expression_field) -> collect_expression env deps field.value)
        env
        deps
        fields
  | Cst.Expression.Record (Cst.Update { base; fields; _ }) ->
      let* deps = collect_expression env deps base in
      let deps =
        List.fold_left
          (fun deps (field: Cst.record_expression_field) -> add_parent env deps field.field_path)
          deps
          fields
      in
      collect_list
        (fun env deps (field: Cst.record_expression_field) -> collect_expression env deps field.value)
        env
        deps
        fields
  | Cst.Expression.LocalOpen (Cst.LetOpen { module_path; body; _ }) ->
      let deps, env = open_alias env deps module_path in
      collect_expression env deps body
  | Cst.Expression.LocalOpen (Cst.Delimited { module_path; body; _ }) ->
      let deps, env = open_alias env deps module_path in
      collect_expression env deps body
  | Cst.Expression.Fun { parameters; return_type; body; _ } ->
      let* (deps, env) = collect_parameters env deps parameters in
      let* deps = collect_option collect_core_type env deps return_type in
      (
        match body with
        | Cst.Expression expression -> collect_expression env deps expression
        | Cst.Cases cases -> collect_list collect_match_case env deps cases.cases
      )
  | Cst.Expression.Function { cases; _ } ->
      collect_list collect_match_case env deps cases
  | Cst.Expression.LetOperator { binding; body; _ } ->
      let* deps = collect_binding_operator env deps binding in
      let env = bind_binding_operator_modules env binding in
      collect_expression env deps body
  | Cst.Expression.Let let_expression ->
      collect_let_expression env deps let_expression
  | Cst.Expression.Match { scrutinee; cases; _ } ->
      let* deps = collect_expression env deps scrutinee in
      collect_list collect_match_case env deps cases
  | Cst.Expression.Try { body; cases; _ } ->
      let* deps = collect_expression env deps body in
      collect_list collect_match_case env deps cases
  | Cst.Expression.If { condition; then_branch; else_branch; _ } ->
      let* deps = collect_expression env deps condition in
      let* deps = collect_expression env deps then_branch in
      collect_option collect_expression env deps else_branch
  | Cst.Expression.Parenthesized { inner; _ } ->
      collect_expression env deps inner

and collect_structure env deps items =
  let* (deps, env, bindings) = collect_structure_binding env deps items in
  let deps = add_names deps (Env.collect_free (Env.make_node bindings)) in
  Ok (deps, env)

and collect_structure_binding env deps items =
  List.fold_left
    (fun acc item ->
      let* (deps, env, bindings) = acc in
      collect_structure_item env deps bindings item)
    (Ok (deps, env, Env.empty))
    items

and module_structure_binding env deps (declaration: Cst.ModuleStructure.t) =
  match declaration.functor_parameters with
  | [] ->
      let* deps = collect_option collect_module_type env deps declaration.module_type in
      module_binding env deps declaration.module_expression
  | parameters ->
      let* (deps, env) = collect_functor_parameters env deps parameters in
      let* deps = collect_option collect_module_type env deps declaration.module_type in
      let* deps = collect_module_expression env deps declaration.module_expression in
      Ok (deps, Env.bound)

and collect_module_structure_rhs env deps (declaration: Cst.ModuleStructure.t) =
  let* (deps, env) = collect_functor_parameters env deps declaration.functor_parameters in
  let* deps = collect_option collect_module_type env deps declaration.module_type in
  collect_module_expression env deps declaration.module_expression

and prebind_module_structure_group env bindings (declaration: Cst.ModuleStructure.t) =
  let name = Cst.ModuleStructure.name declaration in
  let env = Env.add name Env.bound env in
  let bindings = Env.add name Env.bound bindings in
  match Cst.ModuleStructure.next_and_declaration declaration with
  | Some next -> prebind_module_structure_group env bindings next
  | None -> (env, bindings)

and collect_recursive_module_structure_group env deps (declaration: Cst.ModuleStructure.t) =
  let* deps = collect_module_structure_rhs env deps declaration in
  match Cst.ModuleStructure.next_and_declaration declaration with
  | Some next -> collect_recursive_module_structure_group env deps next
  | None -> Ok deps

and collect_sequential_module_structure_group env deps bindings (declaration: Cst.ModuleStructure.t) =
  let* (deps, binding) = module_structure_binding env deps declaration in
  let name = Cst.ModuleStructure.name declaration in
  let env = Env.add name binding env in
  let bindings = Env.add name binding bindings in
  match Cst.ModuleStructure.next_and_declaration declaration with
  | Some next -> collect_sequential_module_structure_group env deps bindings next
  | None -> Ok (deps, env, bindings)

and collect_structure_module_group env deps bindings (declaration: Cst.ModuleStructure.t) =
  if Cst.ModuleStructure.is_recursive declaration then
    let env, bindings = prebind_module_structure_group env bindings declaration in
    let* deps = collect_recursive_module_structure_group env deps declaration in
    Ok (deps, env, bindings)
  else
    collect_sequential_module_structure_group env deps bindings declaration

and collect_open_statement env deps (statement: Cst.OpenStatement.t) =
  match statement.Cst.OpenStatement.target with
  | Cst.OpenStatement.Path path -> Ok (open_alias env deps path)
  | Cst.OpenStatement.ModuleExpression module_expression ->
      let* (deps, binding) = module_binding env deps module_expression in
      let deps = add_names deps (Env.top_free binding) in
      Ok (deps, Env.merge_children env binding)

and collect_structure_include env deps (include_statement: Cst.include_statement) =
  match include_statement.Cst.target with
  | Cst.ModuleExpression module_expression ->
      let* (deps, binding) = module_binding env deps module_expression in
      let deps = add_names deps (Env.collect_free binding) in
      let env = Env.merge_children env binding in
      Ok (deps, env)
  | Cst.ModuleType module_type ->
      let* (deps, binding) = module_type_binding env deps module_type in
      let deps = add_names deps (Env.collect_free binding) in
      let env = Env.merge_children env binding in
      Ok (deps, env)

and collect_structure_item env deps bindings item =
  match item with
  | Cst.StructureItem.TypeDeclaration declaration ->
      let* deps = collect_type_declaration env deps declaration in
      Ok (deps, env, bindings)
  | Cst.StructureItem.TypeExtension extension ->
      let* deps = collect_type_extension env deps extension in
      Ok (deps, env, bindings)
  | Cst.StructureItem.LetBinding binding ->
      let* deps = collect_let_binding_chain env deps binding in
      Ok (deps, env, bindings)
  | Cst.StructureItem.Expression expression ->
      let* deps = collect_expression env deps expression in
      Ok (deps, env, bindings)
  | Cst.StructureItem.Attribute _ ->
      Ok (deps, env, bindings)
  | Cst.StructureItem.Extension _ ->
      Ok (deps, env, bindings)
  | Cst.StructureItem.ClassDeclaration _ ->
      Ok (deps, env, bindings)
  | Cst.StructureItem.ClassTypeDeclaration _ ->
      Ok (deps, env, bindings)
  | Cst.StructureItem.ModuleDeclaration declaration ->
      collect_structure_module_group env deps bindings declaration
  | Cst.StructureItem.ModuleTypeDeclaration declaration ->
      let* deps = collect_option collect_module_type env deps declaration.module_type in
      Ok (deps, env, bindings)
  | Cst.StructureItem.OpenStatement statement ->
      let* (deps, env) = collect_open_statement env deps statement in
      Ok (deps, env, bindings)
  | Cst.StructureItem.Docstring _ ->
      Ok (deps, env, bindings)
  | Cst.StructureItem.Comment _ ->
      Ok (deps, env, bindings)
  | Cst.StructureItem.ExternalDeclaration declaration ->
      let* deps = collect_core_type env deps declaration.type_ in
      Ok (deps, env, bindings)
  | Cst.StructureItem.IncludeStatement include_statement ->
      let* (deps, env) = collect_structure_include env deps include_statement in
      Ok (deps, env, bindings)
  | Cst.StructureItem.ExceptionDeclaration _ ->
      Ok (deps, env, bindings)

and collect_signature_binding env deps items =
  List.fold_left
    (fun acc item ->
      let* (deps, env, bindings) = acc in
      collect_signature_item env deps bindings item)
    (Ok (deps, env, Env.empty))
    items

and module_signature_binding env deps (declaration: Cst.ModuleSignature.t) =
  let* (deps, env) = collect_functor_parameters env deps declaration.Cst.ModuleSignature.functor_parameters in
  match declaration.definition with
  | Cst.ModuleSignature.Signature module_type ->
      module_type_binding env deps module_type
  | Cst.ModuleSignature.Alias (Cst.ModuleExpression.Path path) ->
      let deps, binding = module_alias env deps path in
      Ok (deps, binding)
  | Cst.ModuleSignature.Alias module_expression ->
      let* deps = collect_module_expression env deps module_expression in
      Ok (deps, Env.bound)

and prebind_module_signature_group env bindings (declaration: Cst.ModuleSignature.t) =
  let name = Cst.ModuleSignature.name declaration in
  let env = Env.add name Env.bound env in
  let bindings = Env.add name Env.bound bindings in
  match Cst.ModuleSignature.next_and_declaration declaration with
  | Some next -> prebind_module_signature_group env bindings next
  | None -> (env, bindings)

and collect_recursive_module_signature_group env deps (declaration: Cst.ModuleSignature.t) =
  let* (deps, env_with_params) = collect_functor_parameters env deps declaration.functor_parameters in
  let* deps =
    match declaration.definition with
    | Cst.ModuleSignature.Signature module_type -> collect_module_type env_with_params deps module_type
    | Cst.ModuleSignature.Alias module_expression -> collect_module_expression env_with_params deps module_expression
  in
  match Cst.ModuleSignature.next_and_declaration declaration with
  | Some next -> collect_recursive_module_signature_group env deps next
  | None -> Ok deps

and collect_sequential_module_signature_group env deps bindings (declaration: Cst.ModuleSignature.t) =
  let* (deps, binding) = module_signature_binding env deps declaration in
  let name = Cst.ModuleSignature.name declaration in
  let env = Env.add name binding env in
  let bindings = Env.add name binding bindings in
  match Cst.ModuleSignature.next_and_declaration declaration with
  | Some next -> collect_sequential_module_signature_group env deps bindings next
  | None -> Ok (deps, env, bindings)

and collect_module_signature_group env deps bindings (declaration: Cst.ModuleSignature.t) =
  if Cst.ModuleSignature.is_recursive declaration then
    let env, bindings = prebind_module_signature_group env bindings declaration in
    let* deps = collect_recursive_module_signature_group env deps declaration in
    Ok (deps, env, bindings)
  else
    collect_sequential_module_signature_group env deps bindings declaration

and collect_signature_include env deps (include_statement: Cst.include_statement) =
  match include_statement.Cst.target with
  | Cst.ModuleType module_type ->
      let* (deps, binding) = module_type_binding env deps module_type in
      let deps = add_names deps (Env.top_free binding) in
      let env = Env.merge_children env binding in
      Ok (deps, env)
  | Cst.ModuleExpression module_expression ->
      let* (deps, binding) = module_binding env deps module_expression in
      let deps = add_names deps (Env.top_free binding) in
      let env = Env.merge_children env binding in
      Ok (deps, env)

and collect_signature_item env deps bindings item =
  match item with
  | Cst.SignatureItem.TypeDeclaration declaration ->
      let* deps = collect_type_declaration env deps declaration in
      Ok (deps, env, bindings)
  | Cst.SignatureItem.TypeExtension extension ->
      let* deps = collect_type_extension env deps extension in
      Ok (deps, env, bindings)
  | Cst.SignatureItem.Attribute _ ->
      Ok (deps, env, bindings)
  | Cst.SignatureItem.Extension _ ->
      Ok (deps, env, bindings)
  | Cst.SignatureItem.ClassDeclaration _ ->
      Ok (deps, env, bindings)
  | Cst.SignatureItem.ClassTypeDeclaration _ ->
      Ok (deps, env, bindings)
  | Cst.SignatureItem.ModuleDeclaration declaration ->
      collect_module_signature_group env deps bindings declaration
  | Cst.SignatureItem.ModuleTypeDeclaration declaration ->
      let* deps = collect_option collect_module_type env deps declaration.module_type in
      Ok (deps, env, bindings)
  | Cst.SignatureItem.OpenStatement statement ->
      let* (deps, env) = collect_open_statement env deps statement in
      Ok (deps, env, bindings)
  | Cst.SignatureItem.Docstring _ ->
      Ok (deps, env, bindings)
  | Cst.SignatureItem.Comment _ ->
      Ok (deps, env, bindings)
  | Cst.SignatureItem.ValueDeclaration declaration ->
      let* deps = collect_core_type env deps declaration.type_ in
      Ok (deps, env, bindings)
  | Cst.SignatureItem.ExternalDeclaration declaration ->
      let* deps = collect_core_type env deps declaration.type_ in
      Ok (deps, env, bindings)
  | Cst.SignatureItem.IncludeStatement include_statement ->
      let* (deps, env) = collect_signature_include env deps include_statement in
      Ok (deps, env, bindings)
  | Cst.SignatureItem.ExceptionDeclaration _ ->
      Ok (deps, env, bindings)

let finalize = fun deps env -> { modules = DepSet.elements deps; env }

let of_cst = fun ?(env = Env.empty) source_file ->
  match source_file with
  | Cst.Implementation implementation ->
      let* (deps, env) = collect_structure env (DepSet.empty ()) implementation.items in
      Ok (finalize deps env)
  | Cst.Interface interface ->
      let* (deps, env, _) = collect_signature_binding env (DepSet.empty ()) interface.items in
      Ok (finalize deps env)

let of_parse_result = fun ?(env = Env.empty) result ->
  if result.Parser.diagnostics != [] then
    Error (Parse_diagnostics result.Parser.diagnostics)
  else
    match Cst_builder.create_from_ceibo
      ~kind:result.Parser.kind
      ~source:result.Parser.source
      ~tokens:result.Parser.tokens
      result.tree with
    | Ok cst -> (
        match of_cst ~env cst with
        | Ok deps -> Ok deps
        | Error err -> Error (Cst_builder_error err)
      )
    | Error err -> Error (Cst_builder_error err)

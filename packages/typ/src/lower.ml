open Std
module Typ_diagnostic = Diagnostic
open Syn

type state = {
  source: Source.t;
  mutable scope_path: string list;
  mutable next_origin_id: int;
  mutable next_pattern_id: int;
  mutable next_expr_id: int;
  mutable next_binding_id: int;
  mutable next_item_id: int;
  mutable next_synthetic_name: int;
  mutable origins: OriginMap.origin list;
  mutable patterns: BodyArena.pattern_node list;
  mutable expressions: BodyArena.expr_node list;
  mutable bindings: BodyArena.binding list;
  mutable items: ItemTree.item list;
  mutable diagnostics: Typ_diagnostic.t list;
  mutable declared_type_names: (string * string list) list;
}

let path_text = fun path -> path |> Cst.Ident.segments |> List.map Cst.Token.text |> String.concat "."

let last_path_segment_text = fun path ->
  match List.rev (Cst.Ident.segments path) with
  | segment :: _ -> Cst.Token.text segment
  | [] -> ""

let qualify_scoped_name = fun scope_path name ->
  match scope_path with
  | [] -> name
  | _ -> String.concat "." (scope_path @ [ name ])

let rec scope_prefixes = function
  | [] -> [ [] ]
  | scope_path ->
      let parent_scope_path = List.rev scope_path |> List.tl |> List.rev in
      scope_path :: scope_prefixes parent_scope_path

let resolve_named_type_name = fun (state: state) name ->
  let rec loop = function
    | [] -> name
    | scope_path :: rest ->
        if List.exists
          (fun (candidate_name, candidate_scope_path) ->
            String.equal candidate_name name && List.equal String.equal candidate_scope_path scope_path)
          state.declared_type_names
        then
          qualify_scoped_name scope_path name
        else
          loop rest
  in
  loop (scope_prefixes state.scope_path)

let register_declared_type_name = fun (state: state) name ->
  let binding = (name, state.scope_path) in
  if List.exists
    (fun (candidate_name, candidate_scope_path) ->
      String.equal candidate_name name && List.equal String.equal candidate_scope_path state.scope_path)
    state.declared_type_names
  then
    ()
  else
    state.declared_type_names <- binding :: state.declared_type_names

let is_module_name = fun name ->
  String.length name > 0
  && Char.uppercase_ascii name.[0] = name.[0]
  && Char.lowercase_ascii name.[0] != name.[0]

let rec module_path_segments_of_expr = function
  | Cst.Expression.Path { path; _ } ->
      let segments = Cst.Ident.segments path |> List.map Cst.Token.text in
      if List.is_empty segments || not (List.for_all is_module_name segments) then
        None
      else
        Some segments
  | Cst.Expression.FieldAccess { receiver; field_name; _ } -> (
      match module_path_segments_of_expr receiver with
      | Some segments ->
          let field_name = Cst.Token.text field_name in
          if is_module_name field_name then
            Some (segments @ [ field_name ])
          else
            None
      | None -> None
    )
  | _ ->
      None

let rec module_path_segments_of_module_expression = function
  | Cst.ModuleExpression.Path path -> Some (Cst.Ident.segments path |> List.map Cst.Token.text)
  | Cst.ModuleExpression.Parenthesized { inner; _ }
  | Cst.ModuleExpression.Attribute { module_expression=inner; _ }
  | Cst.ModuleExpression.Constraint { module_expression=inner; _ } -> module_path_segments_of_module_expression
    inner
  | _ -> None

let binding_name_of_pattern =
  let rec loop = function
    | Cst.Pattern.Identifier { name_token; _ } -> Some (Cst.Token.text name_token)
    | Cst.Pattern.Parenthesized { inner; _ } -> loop inner
    | Cst.Pattern.Typed { pattern; _ } -> loop pattern
    | _ -> None
  in
  loop

let type_param_bindings = fun (declaration: Cst.TypeDeclaration.t) ->
  declaration |> Cst.TypeDeclaration.type_params |> List.filter_map
    (fun parameter ->
      match Cst.TypeParameter.type_variable parameter with
      | Some type_variable -> Some (Cst.TypeVariable.name type_variable)
      | None -> None) |> List.mapi (fun index name -> (name, index))

let builtin_type_of_name = fun name arguments ->
  match (name, arguments) with
  | ("int", []) -> Some TypeRepr.Int
  | ("float", []) -> Some TypeRepr.Float
  | ("bool", []) -> Some TypeRepr.Bool
  | ("string", []) -> Some TypeRepr.String
  | ("char", []) -> Some TypeRepr.Char
  | ("unit", []) -> Some TypeRepr.Unit
  | ("option", [ argument ]) -> Some (TypeRepr.Option argument)
  | ("result", [ok_ty;error_ty]) -> Some (TypeRepr.Result (ok_ty, error_ty))
  | ("array", [ argument ]) -> Some (TypeRepr.Array argument)
  | ("list", [ argument ]) -> Some (TypeRepr.List argument)
  | ("Seq.t", [ argument ])
  | ("Std.Seq.t", [ argument ]) -> Some (TypeRepr.Seq argument)
  | _ -> None

let lower_arrow_label = fun (label: Cst.arrow_label option) ->
  match label with
  | None -> TypeRepr.Nolabel
  | Some (Cst.Named { label_token; _ }) -> TypeRepr.Labelled (Cst.Token.text label_token)
  | Some (Cst.OptionalNamed { label_token; _ }) -> TypeRepr.Optional (Cst.Token.text label_token)

let rec lower_core_type = fun (state: state) type_params core_type ->
  match core_type with
  | Cst.CoreType.Parenthesized { inner; _ }
  | Cst.CoreType.Attribute { type_=inner; _ }
  | Cst.CoreType.Alias { type_=inner; _ } ->
      lower_core_type state type_params inner
  | Cst.CoreType.Var { name_token; _ } -> (
      match List.assoc_opt (Cst.Token.text name_token) type_params with
      | Some id -> TypeRepr.Var { id; link = None }
      | None -> TypeRepr.Hole (-100)
    )
  | Cst.CoreType.Constr { constructor_path; arguments; _ } ->
      let name = path_text constructor_path in
      let arguments = List.map (lower_core_type state type_params) arguments in
      begin
        match builtin_type_of_name name arguments with
        | Some builtin -> builtin
        | None ->
            let segments = Cst.Ident.segments constructor_path in
            let name =
              match segments with
              | [ segment ] -> resolve_named_type_name state (Cst.Token.text segment)
              | _ -> name
            in
            TypeRepr.Named { name; arguments }
      end
  | Cst.CoreType.Arrow { label; parameter_type; result_type; _ } ->
      TypeRepr.Arrow {
        label = lower_arrow_label label;
        lhs = lower_core_type state type_params parameter_type;
        rhs = lower_core_type state type_params result_type;
      }
  | Cst.CoreType.Tuple { elements; _ } ->
      TypeRepr.Tuple (List.map (lower_core_type state type_params) elements)
  | _ ->
      TypeRepr.Hole (-101)

let constructor_scheme = fun ~params ~result_type payload_type ->
  let body =
    match payload_type with
    | Some payload_type -> TypeRepr.Arrow {
      label = TypeRepr.Nolabel;
      lhs = payload_type;
      rhs = result_type
    }
    | None -> result_type
  in
  TypeScheme.Forall (List.map snd params, body)

let variant_constructor_payload = fun (state: state) type_params (constructor: Cst.VariantConstructor.t) ->
  match Cst.VariantConstructor.arguments constructor with
  | Some (Cst.ConstructorArguments.Tuple members) ->
      let members = List.map (lower_core_type state type_params) members in
      begin
        match members with
        | [ member ] -> Some member
        | members -> Some (TypeRepr.Tuple members)
      end
  | Some (Cst.ConstructorArguments.Record _) ->
      Some (TypeRepr.Hole (-102))
  | None -> (
      match Cst.VariantConstructor.payload_type constructor with
      | Some payload_type -> Some (lower_core_type state type_params payload_type)
      | None -> None
    )

let make_state = fun source ->
  {
    source;
    scope_path = [];
    next_origin_id = 0;
    next_pattern_id = 0;
    next_expr_id = 0;
    next_binding_id = 0;
    next_item_id = 0;
    next_synthetic_name = 0;
    origins = [];
    patterns = [];
    expressions = [];
    bindings = [];
    items = [];
    diagnostics = [];
    declared_type_names = [];
  }

let add_diagnostic = fun (state: state) diagnostic -> state.diagnostics <- diagnostic :: state.diagnostics

let add_origin = fun (state: state) ~semantic_id ~label syntax_node ->
  let origin_id = OriginId.of_int state.next_origin_id in
  let () =
    state.next_origin_id <- state.next_origin_id + 1
  in
  let origin = {
    OriginMap.origin_id;
    source_id = state.source.source_id;
    source_revision = state.source.revision;
    semantic_id;
    label;
    syntax_kind = Cst.syntax_kind syntax_node;
    span = Ceibo.Red.SyntaxNode.span syntax_node;
  }
  in
  let () =
    state.origins <- origin :: state.origins
  in
  origin_id

let add_pattern = fun (state: state) ~syntax_node ~label desc ->
  let pat_id = PatId.of_int state.next_pattern_id in
  let () =
    state.next_pattern_id <- state.next_pattern_id + 1
  in
  let origin_id = add_origin state ~semantic_id:(OriginMap.Pattern pat_id) ~label syntax_node in
  let node = { BodyArena.pat_id; origin_id; desc } in
  let () =
    state.patterns <- node :: state.patterns
  in
  pat_id

let add_expr = fun (state: state) ~syntax_node ~label desc ->
  let expr_id = ExprId.of_int state.next_expr_id in
  let () =
    state.next_expr_id <- state.next_expr_id + 1
  in
  let origin_id = add_origin state ~semantic_id:(OriginMap.Expr expr_id) ~label syntax_node in
  let node = { BodyArena.expr_id; origin_id; desc } in
  let () =
    state.expressions <- node :: state.expressions
  in
  expr_id

let add_binding = fun (state: state) ~syntax_node ~name ~pattern_id ~value_id ~recursive ->
  let binding_id = BindingId.of_int state.next_binding_id in
  let () =
    state.next_binding_id <- state.next_binding_id + 1
  in
  let origin_id = add_origin state ~semantic_id:(OriginMap.Binding binding_id) ~label:"binding" syntax_node in
  let binding = {
    BodyArena.binding_id;
    origin_id;
    scope_path = state.scope_path;
    name;
    pattern_id;
    value_id;
    recursive;
  }
  in
  let () =
    state.bindings <- binding :: state.bindings
  in
  binding_id

let add_item = fun (state: state) ~syntax_node item ->
  let item_id = ItemId.of_int state.next_item_id in
  let () =
    state.next_item_id <- state.next_item_id + 1
  in
  let origin_id = add_origin state ~semantic_id:(OriginMap.Item item_id) ~label:"item" syntax_node in
  let item =
    match item with
    | `Type declaration -> ItemTree.Type {
      item_id;
      origin_id;
      scope_path = state.scope_path;
      declaration
    }
    | `Exception (exception_name, scheme) ->
        ItemTree.Exception {
          item_id;
          origin_id;
          scope_path = state.scope_path;
          exception_name;
          scheme;
        }
    | `Value (binding_ids, recursive) ->
        ItemTree.Value {
          item_id;
          origin_id;
          scope_path = state.scope_path;
          binding_ids;
          recursive;
        }
    | `DeclaredValue (value_name, scheme) ->
        ItemTree.DeclaredValue {
          item_id;
          origin_id;
          scope_path = state.scope_path;
          value_name;
          scheme;
        }
    | `Open module_path -> ItemTree.Open {
      item_id;
      origin_id;
      scope_path = state.scope_path;
      module_path
    }
    | `Include module_path -> ItemTree.Include {
      item_id;
      origin_id;
      scope_path = state.scope_path;
      module_path
    }
    | `ModuleAlias (alias_name, module_path) ->
        ItemTree.ModuleAlias {
          item_id;
          origin_id;
          scope_path = state.scope_path;
          alias_name;
          module_path;
        }
    | `Unsupported summary -> ItemTree.Unsupported {
      item_id;
      origin_id;
      scope_path = state.scope_path;
      summary
    }
  in
  let () =
    state.items <- item :: state.items
  in
  item

let lower_record_label = fun (state: state) type_params (field: Cst.RecordField.t) ->
  {
    TypeDecl.name = Cst.RecordField.name field;
    field_type = lower_core_type state type_params (Cst.RecordField.field_type field);
    mutable_ = Option.is_some (Cst.RecordField.mutable_token field)
  }

let lower_poly_variant_bound = function
  | Cst.PolyVariantBound.Exact -> TypeDecl.Exact
  | Cst.PolyVariantBound.UpperBound _ -> TypeDecl.UpperBound
  | Cst.PolyVariantBound.LowerBound _ -> TypeDecl.LowerBound

let lower_poly_variant_tag = fun (state: state) type_params (tag: Cst.PolyVariantTag.t) ->
  {
    TypeDecl.name = Cst.PolyVariantTag.name tag;
    payload_type = Cst.PolyVariantTag.payload_type tag
    |> Option.map (lower_core_type state type_params)
  }

let lower_poly_variant_manifest = fun (state: state) type_params (poly_variant: Cst.PolyVariant.t) ->
  let (tags, inherited) =
    Cst.PolyVariant.fields poly_variant
    |> List.fold_left
      (fun (tags, inherited) field ->
        match field with
        | Cst.RowField.Tag tag -> (
          lower_poly_variant_tag state type_params tag :: tags,
          inherited
        )
        | Cst.RowField.Inherit { type_; _ } -> (
          tags,
          lower_core_type state type_params type_ :: inherited
        ))
      ([], [])
  in
  TypeDecl.PolyVariant {
    bound = lower_poly_variant_bound (Cst.PolyVariant.kind poly_variant);
    tags = List.rev tags;
    inherited = List.rev inherited
  }

let add_unsupported_item = fun (state: state) ~context syntax_node ->
  let syntax_kind = Cst.syntax_kind syntax_node in
  let summary = SyntaxKind.to_string syntax_kind in
  let () = add_diagnostic state
    (
      Typ_diagnostic.UnsupportedSyntax {
        syntax_kind;
        syntax_span = Ceibo.Red.SyntaxNode.span syntax_node;
        context;
        recovery = Typ_diagnostic.PlaceholderItem;
        reason = None;
      }
    )
  in
  let _ = add_item state ~syntax_node (`Unsupported summary) in
  ()

let add_unsupported_structure_item = fun (state: state) syntax_node ->
  add_unsupported_item state ~context:Typ_diagnostic.StructureItem syntax_node

let add_unsupported_signature_item = fun (state: state) syntax_node ->
  add_unsupported_item state ~context:Typ_diagnostic.SignatureItem syntax_node

let rec register_type_declaration_names = fun (state: state) (declaration: Cst.TypeDeclaration.t) ->
  let () =
    register_declared_type_name state (Cst.Token.text (Cst.TypeDeclaration.name_token declaration))
  in
  match Cst.TypeDeclaration.next_and_declaration declaration with
  | Some next -> register_type_declaration_names state next
  | None -> ()

let lower_type_declaration = fun (state: state) (declaration: Cst.TypeDeclaration.t) ->
  let syntax_node = Cst.TypeDeclaration.syntax_node declaration in
  let type_name = Cst.TypeDeclaration.name_token declaration |> Cst.Token.text in
  let params = type_param_bindings declaration in
  let result_type = TypeRepr.Named {
    name = qualify_scoped_name state.scope_path type_name;
    arguments = params |> List.map (fun (_, id) -> TypeRepr.Var { id; link = None })
  } in
  let lowered_declaration =
    match Cst.TypeDeclaration.type_definition declaration with
    | Cst.TypeDefinition.Abstract ->
        Some {
          TypeDecl.type_name = type_name;
          param_ids = List.map snd params;
          constructors = [];
          labels = [];
          manifest = None;
        }
    | Cst.TypeDefinition.Alias { manifest; _ } ->
        Some {
          TypeDecl.type_name = type_name;
          param_ids = List.map snd params;
          constructors = [];
          labels = [];
          manifest = Some (TypeDecl.Alias (lower_core_type state params manifest));
        }
    | Cst.TypeDefinition.Variant { constructors; _ } ->
        Some {
          TypeDecl.type_name = type_name;
          param_ids = List.map snd params;
          constructors =
            constructors |> List.map
              (fun (constructor: Cst.VariantConstructor.t) ->
                let payload_type = variant_constructor_payload state params constructor in
                {
                  TypeDecl.name = Cst.VariantConstructor.name constructor;
                  scheme = constructor_scheme ~params ~result_type payload_type
                });
          labels = [];
          manifest = None;
        }
    | Cst.TypeDefinition.Record { fields; _ } ->
        Some {
          TypeDecl.type_name = type_name;
          param_ids = List.map snd params;
          constructors = [];
          labels = List.map (lower_record_label state params) fields;
          manifest = None;
        }
    | Cst.TypeDefinition.PolyVariant poly_variant ->
        Some {
          TypeDecl.type_name = type_name;
          param_ids = List.map snd params;
          constructors = [];
          labels = [];
          manifest = Some (lower_poly_variant_manifest state params poly_variant);
        }
    | _ -> None
  in
  match lowered_declaration with
  | Some lowered_declaration ->
      let _ = add_item state ~syntax_node (`Type lowered_declaration) in
      ()
  | None -> add_unsupported_structure_item state syntax_node

let lower_exception_declaration = fun (state: state) (declaration: Cst.exception_declaration) ->
  let exception_name = Cst.Token.text declaration.name_token in
  let exn_type = TypeRepr.Named { name = "exn"; arguments = [] } in
  let payload_type =
    match declaration.rhs with
    | Some (Cst.Payload { payload_type; _ }) -> Some (lower_core_type state [] payload_type)
    | Some (Cst.Alias _)
    | None -> None
  in
  let scheme =
    match payload_type with
    | Some payload_type -> TypeScheme.Forall (
      [],
      TypeRepr.Arrow { label = TypeRepr.Nolabel; lhs = payload_type; rhs = exn_type }
    )
    | None -> TypeScheme.Forall ([], exn_type)
  in
  let _ = add_item state ~syntax_node:declaration.syntax_node (`Exception (exception_name, scheme)) in
  ()

let declared_value_name = fun name_tokens ->
  let texts = List.map Cst.Token.text name_tokens in
  match texts with
  | "(" :: rest -> (
      match List.rev rest with
      | ")" :: inner_rev -> String.concat "" (List.rev inner_rev)
      | _ -> String.concat "" texts
    )
  | _ -> String.concat "" texts

let rec collect_core_type_var_names = fun core_type ->
  let append_unique acc name =
    if List.exists (String.equal name) acc then
      acc
    else
      acc @ [ name ]
  in
  let collect_many items =
    List.fold_left
      (fun acc item ->
        collect_core_type_var_names item
        |> List.fold_left append_unique acc)
      []
      items
  in
  match core_type with
  | Cst.CoreType.Parenthesized { inner; _ }
  | Cst.CoreType.Attribute { type_=inner; _ }
  | Cst.CoreType.Alias { type_=inner; _ } -> collect_core_type_var_names inner
  | Cst.CoreType.Var { name_token; _ } -> [ Cst.Token.text name_token ]
  | Cst.CoreType.Constr { arguments; _ } -> collect_many arguments
  | Cst.CoreType.Arrow { parameter_type; result_type; _ } ->
      collect_many [ parameter_type; result_type ]
  | Cst.CoreType.Tuple { elements; _ } -> collect_many elements
  | _ -> []

let scheme_of_declared_core_type = fun (state: state) core_type ->
  let params = collect_core_type_var_names core_type |> List.mapi (fun index name -> (name, index)) in
  let ty = lower_core_type state params core_type in
  TypeScheme.Forall (List.map snd params, ty)

let lower_value_declaration = fun (state: state) syntax_node name_tokens type_ ->
  let value_name = declared_value_name name_tokens in
  let scheme = scheme_of_declared_core_type state type_ in
  let _ = add_item state ~syntax_node (`DeclaredValue (value_name, scheme)) in
  ()

let fresh_synthetic_name = fun (state: state) prefix ->
  let name = "$" ^ prefix ^ Int.to_string state.next_synthetic_name in
  let () =
    state.next_synthetic_name <- state.next_synthetic_name + 1
  in
  name

let with_scope = fun (state: state) scope_path f ->
  let previous_scope_path = state.scope_path in
  let () =
    state.scope_path <- scope_path
  in
  let result = f () in
  let () =
    state.scope_path <- previous_scope_path
  in
  result

let int_text = fun (integer: Cst.integer_constant) ->
  let sign =
    match integer.Cst.sign_token with
    | Some sign -> Cst.Token.text sign
    | None -> ""
  in
  sign ^ Cst.Token.text integer.literal_token

let float_text = fun (float_: Cst.float_constant) ->
  let sign =
    match float_.Cst.sign_token with
    | Some sign -> Cst.Token.text sign
    | None -> ""
  in
  sign ^ Cst.Token.text float_.literal_token

let unsupported_syntax_kind = fun syntax_node -> Cst.syntax_kind syntax_node

let supported_literal_subset = [
  Typ_diagnostic.IntLiteral;
  Typ_diagnostic.FloatLiteral;
  Typ_diagnostic.BoolLiteral;
  Typ_diagnostic.StringLiteral;
  Typ_diagnostic.CharLiteral;
  Typ_diagnostic.UnitLiteral;
]

let lower_unsupported_pattern = fun (state: state) ?reason pattern syntax_kind ->
  let syntax_node = Cst.Pattern.syntax_node pattern in
  let () = add_diagnostic state
    (
      Typ_diagnostic.UnsupportedSyntax {
        syntax_kind;
        syntax_span = Ceibo.Red.SyntaxNode.span syntax_node;
        context = Typ_diagnostic.Pattern;
        recovery = Typ_diagnostic.RecoveryPattern;
        reason;
      }
    )
  in
  add_pattern
    state
    ~syntax_node
    ~label:"unsupported_pattern"
    (BodyArena.PUnsupported (SyntaxKind.to_string syntax_kind))

let lower_unsupported_expr = fun (state: state) ?reason expr syntax_kind ->
  let syntax_node = Cst.Expression.syntax_node expr in
  let () = add_diagnostic state
    (
      Typ_diagnostic.UnsupportedSyntax {
        syntax_kind;
        syntax_span = Ceibo.Red.SyntaxNode.span syntax_node;
        context = Typ_diagnostic.Expression;
        recovery = Typ_diagnostic.HoleExpression;
        reason;
      }
    )
  in
  add_expr
    state
    ~syntax_node
    ~label:"unsupported_expression"
    (BodyArena.EHole (SyntaxKind.to_string syntax_kind))

let rec lower_pattern = fun (state: state) pattern ->
  match pattern with
  | Cst.Pattern.Identifier { syntax_node; name_token; _ } ->
      add_pattern
        state
        ~syntax_node
        ~label:"identifier_pattern"
        (BodyArena.PVar (Cst.Token.text name_token))
  | Cst.Pattern.Wildcard { syntax_node; _ } ->
      add_pattern state ~syntax_node ~label:"wildcard_pattern" BodyArena.PWildcard
  | Cst.Pattern.Literal { syntax_node; literal; _ } -> (
      match literal with
      | Cst.PatternLiteral.Int integer -> add_pattern
        state
        ~syntax_node
        ~label:"int_pattern"
        (BodyArena.PInt (int_text integer))
      | Cst.PatternLiteral.Float float_ -> add_pattern
        state
        ~syntax_node
        ~label:"float_pattern"
        (BodyArena.PFloat (float_text float_))
      | Cst.PatternLiteral.Bool { value; _ } -> add_pattern
        state
        ~syntax_node
        ~label:"bool_pattern"
        (BodyArena.PBool value)
      | Cst.PatternLiteral.String string_ -> add_pattern
        state
        ~syntax_node
        ~label:"string_pattern"
        (BodyArena.PString string_.contents)
      | Cst.PatternLiteral.Char char_ -> add_pattern
        state
        ~syntax_node
        ~label:"char_pattern"
        (BodyArena.PChar char_.contents)
      | Cst.PatternLiteral.Unit _ -> add_pattern state ~syntax_node ~label:"unit_pattern" BodyArena.PUnit
    )
  | Cst.Pattern.Tuple { syntax_node; elements; _ } ->
      let element_ids = elements
      |> List.map (fun (element: Cst.tuple_pattern_element) -> lower_pattern state element.pattern) in
      add_pattern state ~syntax_node ~label:"tuple_pattern" (BodyArena.PTuple element_ids)
  | Cst.Pattern.Or { syntax_node; alternatives; _ } ->
      let alternative_ids = List.map (lower_pattern state) alternatives in
      add_pattern state ~syntax_node ~label:"or_pattern" (BodyArena.POr alternative_ids)
  | Cst.Pattern.Constructor { syntax_node; constructor_path; arguments; _ } ->
      let argument_ids = List.map (lower_pattern state) arguments in
      add_pattern
        state
        ~syntax_node
        ~label:"constructor_pattern"
        (BodyArena.PConstructor {
          constructor = path_text constructor_path;
          arguments = argument_ids
        })
  | Cst.Pattern.Record { syntax_node; fields; closedness; _ } ->
      let fields =
        fields
        |> List.map
          (fun (field: Cst.record_pattern_field) ->
            let pattern_id =
              match field.pattern with
              | Some pattern -> lower_pattern state pattern
              | None ->
                  let field_name = last_path_segment_text field.field_path in
                  add_pattern
                    state
                    ~syntax_node:field.syntax_node
                    ~label:"record_punned_field_pattern"
                    (BodyArena.PVar field_name)
            in
            (
              { BodyArena.label = path_text field.field_path; pattern_id }: BodyArena.record_pattern_field
            ))
      in
      add_pattern state ~syntax_node ~label:"record_pattern"
        (
          BodyArena.PRecord {
            fields;
            open_ =
              (
                match closedness with
                | Cst.Open _ -> true
                | Cst.Closed -> false
              );
          }
        )
  | Cst.Pattern.List { syntax_node; elements; _ } ->
      let element_ids = List.map (lower_pattern state) elements in
      add_pattern state ~syntax_node ~label:"list_pattern" (BodyArena.PList element_ids)
  | Cst.Pattern.Alias { syntax_node; pattern; name_token; _ } ->
      let pattern_id = lower_pattern state pattern in
      add_pattern
        state
        ~syntax_node
        ~label:"alias_pattern"
        (BodyArena.PAlias { pattern_id; alias = Cst.Token.text name_token })
  | Cst.Pattern.PolyVariant { syntax_node; tag_token; payload; _ } ->
      let payload = payload |> Option.map (lower_pattern state) in
      add_pattern
        state
        ~syntax_node
        ~label:"poly_variant_pattern"
        (BodyArena.PPolyVariant { tag = Cst.Token.text tag_token; payload })
  | Cst.Pattern.Parenthesized { inner; _ } ->
      lower_pattern state inner
  | Cst.Pattern.Typed { syntax_node; pattern; _ } ->
      let () = add_diagnostic
        state
        (Typ_diagnostic.IgnoredPatternTypeConstraint {
          constraint_span = Ceibo.Red.SyntaxNode.span syntax_node
        }) in
      lower_pattern state pattern
  | _ ->
      lower_unsupported_pattern
        state
        pattern
        (unsupported_syntax_kind (Cst.Pattern.syntax_node pattern))

let recovered_parameter_pattern = fun (state: state) syntax_node ~label parameter ->
  match Cst.Parameter.binding_pattern parameter with
  | Some pattern -> lower_pattern state pattern
  | None -> (
      match Cst.Parameter.name parameter with
      | Some name -> add_pattern state ~syntax_node ~label (BodyArena.PVar name)
      | None -> add_pattern state ~syntax_node ~label:"unsupported_parameter" BodyArena.PWildcard
    )

let positional_function_parameter = fun pattern_id ->
  ({ BodyArena.label = BodyArena.Positional; pattern_id }: BodyArena.function_parameter)

let labeled_function_parameter = fun label pattern_id ->
  ({ BodyArena.label = BodyArena.Labeled label; pattern_id }: BodyArena.function_parameter)

let optional_function_parameter = fun label pattern_id ->
  ({ BodyArena.label = BodyArena.Optional label; pattern_id }: BodyArena.function_parameter)

let rec lower_parameter = fun (state: state) parameter ->
  match parameter with
  | Cst.Parameter.Positional { pattern; _ } ->
      positional_function_parameter (lower_pattern state pattern)
  | Cst.Parameter.Labeled labeled ->
      let pattern_id = recovered_parameter_pattern
        state
        labeled.syntax_node
        ~label:"labeled_parameter_pattern"
        parameter in
      labeled_function_parameter (Cst.Token.text labeled.label_token) pattern_id
  | Cst.Parameter.Optional optional ->
      let pattern_id = recovered_parameter_pattern
        state
        optional.syntax_node
        ~label:"optional_parameter_pattern"
        parameter in
      optional_function_parameter (Cst.Token.text optional.label_token) pattern_id
  | parameter ->
      let syntax_node = Cst.Parameter.syntax_node parameter in
      let () = add_diagnostic
        state
        (Typ_diagnostic.ParameterLoweredAsPositional {
          parameter_span = Ceibo.Red.SyntaxNode.span syntax_node
        }) in
      positional_function_parameter
        (recovered_parameter_pattern state syntax_node ~label:"recovered_parameter" parameter)

let synthetic_var_pattern = fun (state: state) syntax_node ~label ->
  let name = fresh_synthetic_name state label in
  let pat_id = add_pattern
    state
    ~syntax_node
    ~label:(("synthetic_" ^ label ^ "_pattern"))
    (BodyArena.PVar name) in
  (name, pat_id)

let rec lower_match_cases = fun (state: state) cases ->
  List.map
    (fun (case: Cst.match_case) ->
      let pattern_id = lower_pattern state case.pattern in
      let guard_id = case.guard |> Option.map (lower_expr state) in
      let body_id = lower_expr state case.body in
      { BodyArena.pattern_id; guard_id; body_id })
    cases

and lower_function_like = fun (state: state) ~syntax_node ~parameters ~body ->
  let parameter_ids = List.map (lower_parameter state) parameters in
  let body_id =
    match body with
    | `Expr expression -> lower_expr state expression
    | `Cases cases ->
        let (synthetic_name, synthetic_pattern_id) = synthetic_var_pattern state syntax_node ~label:"function_arg" in
        let argument_expr_id = add_expr
          state
          ~syntax_node
          ~label:"synthetic_function_argument"
          (BodyArena.EVar synthetic_name) in
        let match_id = add_expr
          state
          ~syntax_node
          ~label:"function_match_body"
          (BodyArena.EMatch (argument_expr_id, lower_match_cases state cases)) in
        let parameter_ids = parameter_ids @ [ positional_function_parameter synthetic_pattern_id ] in
        add_expr state ~syntax_node ~label:"wrapped_fun" (BodyArena.EFun (parameter_ids, match_id))
  in
  match body with
  | `Expr _ -> add_expr
    state
    ~syntax_node
    ~label:"fun_expression"
    (BodyArena.EFun (parameter_ids, body_id))
  | `Cases _ -> body_id

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
  let binding_ids = let_binding :: Cst.LetBinding.and_bindings let_binding
  |> List.map
    (fun (binding: Cst.let_binding) ->
      lower_binding_source
        state
        ~syntax_node:(Cst.LetBinding.syntax_node binding)
        ~binding_pattern:(Cst.LetBinding.binding_pattern binding)
        ~parameters:(Cst.LetBinding.parameters binding)
        ~value:(Cst.LetBinding.value binding)
        ~recursive) in
  add_item
    state
    ~syntax_node:(Cst.LetBinding.syntax_node let_binding)
    (`Value (binding_ids, recursive))

and lower_let_expression_bindings = fun (state: state) (let_expression: Cst.let_expression) ->
  let recursive = Option.is_some let_expression.rec_token in
  let head = lower_binding_source
    state
    ~syntax_node:let_expression.syntax_node
    ~binding_pattern:let_expression.binding_pattern
    ~parameters:let_expression.parameters
    ~value:let_expression.bound_value
    ~recursive in
  let tail =
    match let_expression.and_binding with
    | None -> []
    | Some binding -> Cst.LetBinding.and_bindings binding
    |> fun rest ->
      binding :: rest
      |> List.map
        (fun (binding: Cst.let_binding) ->
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
  let lower_argument = function
    | Cst.Positional argument ->
        (
          { BodyArena.label = BodyArena.Positional; value_id = lower_expr state argument }:
            BodyArena.apply_argument
        )
    | Cst.Labeled { syntax_node; label_token; value; _ } ->
        let value_id =
          match value with
          | Some value -> lower_expr state value
          | None -> add_expr
            state
            ~syntax_node
            ~label:"implicit_labeled_argument"
            (BodyArena.EVar (Cst.Token.text label_token))
        in
        { BodyArena.label = BodyArena.Labeled (Cst.Token.text label_token); value_id }
    | Cst.Optional { syntax_node; label_token; value; _ } ->
        let value_id =
          match value with
          | Some value -> lower_expr state value
          | None -> add_expr
            state
            ~syntax_node
            ~label:"implicit_optional_argument"
            (BodyArena.EVar (Cst.Token.text label_token))
        in
        { BodyArena.label = BodyArena.Optional (Cst.Token.text label_token); value_id }
  in
  let rec collect = function
    | Cst.Expression.Apply { callee; argument; _ } ->
        let (callee_id, arguments) = collect callee in
        (callee_id, arguments @ [ lower_argument argument ])
    | callee ->
        let callee_id = lower_expr state callee in
        (callee_id, [])
  in
  let syntax_node = Cst.Expression.syntax_node expression in
  let (callee_id, arguments) = collect expression in
  add_expr state ~syntax_node ~label:"apply_expression" (BodyArena.EApply (callee_id, arguments))

and lower_infix = fun (state: state) (infix: Cst.infix_expression) ->
  let syntax_node = infix.syntax_node in
  let operator_name = Cst.InfixExpression.operator infix in
  let operator_id = add_expr
    state
    ~syntax_node
    ~label:"infix_operator"
    (BodyArena.EVar operator_name) in
  let left_id = lower_expr state infix.left in
  let right_id = lower_expr state infix.right in
  add_expr
    state
    ~syntax_node
    ~label:"infix_expression"
    (BodyArena.EApply (
      operator_id,
      [
        { BodyArena.label = BodyArena.Positional; value_id = left_id };
        { BodyArena.label = BodyArena.Positional; value_id = right_id };
      ]
    ))

and lower_list_expression = fun (state: state) (list_expression: Cst.list_expression) ->
  let nil_id = add_expr
    state
    ~syntax_node:list_expression.syntax_node
    ~label:"list_nil_expression"
    (BodyArena.EVar "[]") in
  list_expression.elements |> List.rev |> List.fold_left
    (fun tail_id element ->
      let cons_id = add_expr
        state
        ~syntax_node:list_expression.syntax_node
        ~label:"list_cons_expression"
        (BodyArena.EVar "::") in
      let head_id = lower_expr state element in
      add_expr
        state
        ~syntax_node:list_expression.syntax_node
        ~label:"list_literal_apply"
        (BodyArena.EApply (
          cons_id,
          [
            { BodyArena.label = BodyArena.Positional; value_id = head_id };
            { BodyArena.label = BodyArena.Positional; value_id = tail_id };
          ]
        )))
    nil_id

and lower_expr = fun (state: state) expression ->
  match expression with
  | Cst.Expression.Path { syntax_node; path; _ } ->
      add_expr state ~syntax_node ~label:"path_expression" (BodyArena.EVar (path_text path))
  | Cst.Expression.Constructor { syntax_node; constructor_path; payload; _ } -> (
      let constructor_name = path_text constructor_path in
      match payload with
      | None -> add_expr
        state
        ~syntax_node
        ~label:"constructor_expression"
        (BodyArena.EVar constructor_name)
      | Some payload ->
          let callee_id = add_expr
            state
            ~syntax_node:(Cst.Ident.syntax_node constructor_path)
            ~label:"constructor_path_expression"
            (BodyArena.EVar constructor_name) in
          let payload_id = lower_expr state payload in
          add_expr
            state
            ~syntax_node
            ~label:"constructor_apply_expression"
            (BodyArena.EApply (
              callee_id,
              [ { BodyArena.label = BodyArena.Positional; value_id = payload_id } ]
            ))
    )
  | Cst.Expression.FieldAccess { syntax_node; receiver; field_name; _ } -> (
      match module_path_segments_of_expr receiver with
      | Some module_segments ->
          let qualified_name = String.concat "." (module_segments @ [ Cst.Token.text field_name ]) in
          add_expr
            state
            ~syntax_node
            ~label:"qualified_path_expression"
            (BodyArena.EVar qualified_name)
      | None ->
          let receiver_id = lower_expr state receiver in
          add_expr
            state
            ~syntax_node
            ~label:"field_access_expression"
            (BodyArena.EFieldAccess { receiver_id; label = Cst.Token.text field_name })
    )
  | Cst.Expression.Record (Cst.RecordExpression.Literal { syntax_node; fields; _ }) ->
      let fields = fields
      |> List.map
        (fun (field: Cst.record_expression_field) ->
          (
            { BodyArena.label = path_text field.field_path; value_id = lower_expr state field.value }:
              BodyArena.record_expr_field
          )) in
      add_expr
        state
        ~syntax_node
        ~label:"record_expression"
        (BodyArena.ERecord { base_id = None; fields })
  | Cst.Expression.Record (Cst.RecordExpression.Update { syntax_node; base; fields; _ }) ->
      let base_id = lower_expr state base in
      let fields = fields
      |> List.map
        (fun (field: Cst.record_expression_field) ->
          (
            { BodyArena.label = path_text field.field_path; value_id = lower_expr state field.value }:
              BodyArena.record_expr_field
          )) in
      add_expr
        state
        ~syntax_node
        ~label:"record_update_expression"
        (BodyArena.ERecord { base_id = Some base_id; fields })
  | Cst.Expression.Operator { syntax_node; operator_tokens; _ } ->
      let operator = operator_tokens |> List.map Cst.Token.text |> String.concat "" in
      add_expr state ~syntax_node ~label:"operator_expression" (BodyArena.EVar operator)
  | Cst.Expression.Literal literal -> (
      match literal with
      | Cst.Literal.Int integer -> add_expr
        state
        ~syntax_node:integer.syntax_node
        ~label:"int_literal"
        (BodyArena.EInt (int_text integer))
      | Cst.Literal.Float float_ -> add_expr
        state
        ~syntax_node:float_.syntax_node
        ~label:"float_literal"
        (BodyArena.EFloat (float_text float_))
      | Cst.Literal.Bool { syntax_node; value; _ } -> add_expr
        state
        ~syntax_node
        ~label:"bool_literal"
        (BodyArena.EBool value)
      | Cst.Literal.String string_ -> add_expr
        state
        ~syntax_node:string_.syntax_node
        ~label:"string_literal"
        (BodyArena.EString string_.contents)
      | Cst.Literal.Char char_ -> add_expr
        state
        ~syntax_node:char_.syntax_node
        ~label:"char_literal"
        (BodyArena.EChar char_.contents)
      | Cst.Literal.Unit { syntax_node; _ } -> add_expr
        state
        ~syntax_node
        ~label:"unit_literal"
        BodyArena.EUnit
    )
  | Cst.Expression.Tuple { syntax_node; elements; _ } ->
      let element_ids = List.map (lower_expr state) elements in
      add_expr state ~syntax_node ~label:"tuple_expression" (BodyArena.ETuple element_ids)
  | Cst.Expression.List list_expression ->
      lower_list_expression state list_expression
  | Cst.Expression.Array { syntax_node; elements; _ } ->
      let element_ids = List.map (lower_expr state) elements in
      add_expr state ~syntax_node ~label:"array_expression" (BodyArena.EArray element_ids)
  | Cst.Expression.Sequence { syntax_node; expressions; _ } ->
      let element_ids = List.map (lower_expr state) expressions in
      add_expr state ~syntax_node ~label:"sequence_expression" (BodyArena.ESequence element_ids)
  | Cst.Expression.Parenthesized { inner; _ } ->
      lower_expr state inner
  | Cst.Expression.TypeAscription { expression; _ } ->
      lower_expr state expression
  | Cst.Expression.Polymorphic { syntax_node; expression; _ } ->
      let () = add_diagnostic
        state
        (Typ_diagnostic.IgnoredPolymorphicAnnotation {
          annotation_span = Ceibo.Red.SyntaxNode.span syntax_node
        }) in
      lower_expr state expression
  | Cst.Expression.Fun { syntax_node; parameters; body; _ } -> (
      match body with
      | Cst.Expression body -> lower_function_like state ~syntax_node ~parameters ~body:(`Expr body)
      | Cst.Cases body -> lower_function_like
        state
        ~syntax_node
        ~parameters
        ~body:(`Cases body.cases)
    )
  | Cst.Expression.Function { syntax_node; cases; _ } ->
      lower_function_like state ~syntax_node ~parameters:[] ~body:(`Cases cases)
  | Cst.Expression.Apply _ ->
      lower_apply state expression
  | Cst.Expression.Index { syntax_node; collection; index; _ } ->
      let collection_id = lower_expr state collection in
      let index_id = lower_expr state index in
      add_expr
        state
        ~syntax_node
        ~label:"index_expression"
        (BodyArena.EIndex (collection_id, index_id))
  | Cst.Expression.Infix infix ->
      lower_infix state infix
  | Cst.Expression.If {
    syntax_node;
    condition;
    then_branch;
    else_branch;
    _
  } ->
      let condition_id = lower_expr state condition in
      let then_id = lower_expr state then_branch in
      let else_id =
        match else_branch with
        | Some else_branch -> lower_expr state else_branch
        | None -> add_expr state ~syntax_node ~label:"implicit_else_unit" BodyArena.EUnit
      in
      add_expr
        state
        ~syntax_node
        ~label:"if_expression"
        (BodyArena.EIf (condition_id, then_id, else_id))
  | Cst.Expression.Let let_expression ->
      let binding_ids = lower_let_expression_bindings state let_expression in
      let body_id = lower_expr state let_expression.body in
      add_expr
        state
        ~syntax_node:let_expression.syntax_node
        ~label:"let_expression"
        (BodyArena.ELet (binding_ids, body_id))
  | Cst.Expression.Match { syntax_node; scrutinee; cases; _ } ->
      let scrutinee_id = lower_expr state scrutinee in
      let cases = lower_match_cases state cases in
      add_expr state ~syntax_node ~label:"match_expression" (BodyArena.EMatch (scrutinee_id, cases))
  | Cst.Expression.Try { syntax_node; body; cases; _ } ->
      let body_id = lower_expr state body in
      let cases = lower_match_cases state cases in
      add_expr state ~syntax_node ~label:"try_expression" (BodyArena.ETry (body_id, cases))
  | Cst.Expression.PolyVariant { syntax_node; tag_token; payload; _ } ->
      let payload = payload |> Option.map (lower_expr state) in
      add_expr
        state
        ~syntax_node
        ~label:"poly_variant_expression"
        (BodyArena.EPolyVariant { tag = Cst.Token.text tag_token; payload })
  | Cst.Expression.LocalOpen (LetOpen { syntax_node; module_path; body; _ })
  | Cst.Expression.LocalOpen (Delimited { syntax_node; module_path; body; _ }) ->
      let body_id = lower_expr state body in
      add_expr
        state
        ~syntax_node
        ~label:"local_open_expression"
        (BodyArena.ELocalOpen { module_path = path_text module_path; body_id })
  | Cst.Expression.Prefix { syntax_node; operator_token; operand; _ } -> (
      match (Cst.Token.text operator_token, operand) with
      | ("-", Cst.Expression.Literal (Cst.Literal.Int integer)) ->
          add_expr
            state
            ~syntax_node
            ~label:"negative_int_literal"
            (BodyArena.EInt ("-" ^ int_text integer))
      | ("-", Cst.Expression.Literal (Cst.Literal.Float float_)) ->
          add_expr
            state
            ~syntax_node
            ~label:"negative_float_literal"
            (BodyArena.EFloat ("-" ^ float_text float_))
      | _ ->
          let operator_id = add_expr
            state
            ~syntax_node
            ~label:"prefix_operator"
            (BodyArena.EVar (Cst.Token.text operator_token)) in
          let operand_id = lower_expr state operand in
          add_expr
            state
            ~syntax_node
            ~label:"prefix_expression"
            (BodyArena.EApply (
              operator_id,
              [ { BodyArena.label = BodyArena.Positional; value_id = operand_id } ]
            ))
    )
  | _ ->
      lower_unsupported_expr
        state
        expression
        (unsupported_syntax_kind (Cst.Expression.syntax_node expression))

let lower_top_level_expression = fun (state: state) expression ->
  let syntax_node = Cst.Expression.syntax_node expression in
  let pattern_id = add_pattern state ~syntax_node ~label:"top_level_expression_pattern" BodyArena.PWildcard in
  let value_id = lower_expr state expression in
  let binding_id = add_binding state ~syntax_node ~name:None ~pattern_id ~value_id ~recursive:false in
  add_item state ~syntax_node (`Value ([ binding_id ], false))

let rec lower_structure_item = fun (state: state) item ->
  match item with
  | Cst.StructureItem.Comment _
  | Cst.StructureItem.Docstring _ ->
      ()
  | Cst.StructureItem.LetBinding binding ->
      let _ = lower_let_binding_group state binding in
      ()
  | Cst.StructureItem.Expression expression ->
      let _ = lower_top_level_expression state expression in
      ()
  | Cst.StructureItem.OpenStatement open_statement -> (
      match Cst.OpenStatement.module_path open_statement with
      | Some module_path ->
          let _ = add_item
            state
            ~syntax_node:(Cst.OpenStatement.syntax_node open_statement)
            (`Open (path_text module_path)) in
          ()
      | None -> ()
    )
  | Cst.StructureItem.IncludeStatement include_statement ->
      lower_include_statement state include_statement
  | Cst.StructureItem.TypeDeclaration declaration ->
      let () = register_type_declaration_names state declaration in
      let rec loop declaration =
        let () = lower_type_declaration state declaration in
        match Cst.TypeDeclaration.next_and_declaration declaration with
        | Some declaration -> loop declaration
        | None -> ()
      in
      loop declaration
  | Cst.StructureItem.ExceptionDeclaration declaration ->
      lower_exception_declaration state declaration
  | Cst.StructureItem.ModuleDeclaration declaration ->
      lower_module_declaration state declaration
  | item ->
      add_unsupported_structure_item state (Cst.StructureItem.syntax_node item)

and lower_signature_items_of_module_type = fun (state: state) module_type ~on_unsupported ->
  match module_type with
  | Cst.ModuleType.Signature _ -> (
      match CstBuilder.signature_items_of_module_type module_type with
      | Ok items ->
          let _ = List.map (lower_signature_item state) items in
          ()
      | Error builder_error ->
          add_diagnostic state (Typ_diagnostic.CstBuilderError { builder_error }))
  | Cst.ModuleType.Parenthesized { inner; _ }
  | Cst.ModuleType.Attribute { module_type=inner; _ } ->
      lower_signature_items_of_module_type state inner ~on_unsupported
  | Cst.ModuleType.Path _
  | Cst.ModuleType.TypeOf _
  | Cst.ModuleType.Functor _
  | Cst.ModuleType.With _
  | Cst.ModuleType.Extension _ ->
      on_unsupported ()

and lower_signature_item = fun (state: state) item ->
  match item with
  | Cst.SignatureItem.Comment _
  | Cst.SignatureItem.Docstring _ ->
      ()
  | Cst.SignatureItem.ValueDeclaration declaration ->
      lower_value_declaration
        state
        (Cst.ValueDeclaration.syntax_node declaration)
        (Cst.ValueDeclaration.name_tokens declaration)
        (Cst.ValueDeclaration.type_ declaration)
  | Cst.SignatureItem.ExternalDeclaration declaration ->
      lower_value_declaration
        state
        declaration.syntax_node
        declaration.name_tokens
        declaration.type_
  | Cst.SignatureItem.TypeDeclaration declaration ->
      let () = register_type_declaration_names state declaration in
      let rec loop declaration =
        let () = lower_type_declaration state declaration in
        match Cst.TypeDeclaration.next_and_declaration declaration with
        | Some declaration -> loop declaration
        | None -> ()
      in
      loop declaration
  | Cst.SignatureItem.ExceptionDeclaration declaration ->
      lower_exception_declaration state declaration
  | Cst.SignatureItem.OpenStatement open_statement -> (
      match Cst.OpenStatement.module_path open_statement with
      | Some module_path ->
          let _ = add_item
            state
            ~syntax_node:(Cst.OpenStatement.syntax_node open_statement)
            (`Open (path_text module_path))
          in
          ()
      | None -> ()
    )
  | Cst.SignatureItem.IncludeStatement include_statement ->
      lower_signature_include_statement state include_statement
  | Cst.SignatureItem.ModuleDeclaration declaration ->
      lower_module_signature_declaration state declaration
  | item ->
      add_unsupported_signature_item state (Cst.SignatureItem.syntax_node item)

and lower_include_statement = fun (state: state) (include_statement: Cst.include_statement) ->
  let syntax_node = include_statement.syntax_node in
  match include_statement.target with
  | Cst.ModuleExpression module_expression -> (
      match module_path_segments_of_module_expression module_expression with
      | Some [] ->
          ()
      | Some segments ->
          let _ = add_item state ~syntax_node (`Include (String.concat "." segments)) in
          ()
      | None ->
          add_unsupported_structure_item state syntax_node
    )
  | Cst.ModuleType _ -> ()

and lower_signature_include_statement = fun (state: state) (include_statement: Cst.include_statement) ->
  let syntax_node = include_statement.syntax_node in
  match include_statement.target with
  | Cst.ModuleType module_type -> (
      match module_type with
      | Cst.ModuleType.Path path ->
          let _ = add_item state ~syntax_node (`Include (path_text path)) in
          ()
      | Cst.ModuleType.Parenthesized { inner; _ }
      | Cst.ModuleType.Attribute { module_type=inner; _ } ->
          lower_signature_include_statement state { include_statement with target = Cst.ModuleType inner }
      | _ ->
          lower_signature_items_of_module_type
            state
            module_type
            ~on_unsupported:(fun () -> add_unsupported_signature_item state syntax_node)
    )
  | Cst.ModuleExpression _ ->
      add_unsupported_signature_item state syntax_node

and lower_module_declaration = fun (state: state) (declaration: Cst.ModuleStructure.t) ->
  let syntax_node = Cst.ModuleStructure.syntax_node declaration in
  if
    Cst.ModuleStructure.is_recursive declaration
    || Option.is_some (Cst.ModuleStructure.next_and_declaration declaration)
  then
    add_unsupported_structure_item state syntax_node
  else
    let module_name = Cst.ModuleStructure.name declaration in
    let module_expression = Cst.ModuleStructure.module_expression declaration in
    match module_path_segments_of_module_expression module_expression with
    | Some [] ->
        ()
    | Some segments ->
        let _ = add_item state ~syntax_node (`ModuleAlias (module_name, String.concat "." segments)) in
        ()
    | None ->
        let nested_scope_path = state.scope_path @ [ module_name ] in
        with_scope state nested_scope_path
          (fun () ->
            match CstBuilder.structure_items_of_module_expression module_expression with
            | Ok items ->
                let _ = List.map (lower_structure_item state) items in
                ()
            | Error builder_error -> add_diagnostic
              state
              (Typ_diagnostic.CstBuilderError { builder_error }))

and lower_module_signature_declaration = fun (state: state) (declaration: Cst.ModuleSignature.t) ->
  let syntax_node = Cst.ModuleSignature.syntax_node declaration in
  if
    Cst.ModuleSignature.is_recursive declaration
    || Option.is_some (Cst.ModuleSignature.next_and_declaration declaration)
    || not (List.is_empty (Cst.ModuleSignature.functor_parameters declaration))
  then
    add_unsupported_signature_item state syntax_node
  else
    let module_name = Cst.ModuleSignature.name declaration in
    match Cst.ModuleSignature.definition declaration with
    | Cst.ModuleSignature.Alias module_expression -> (
        match module_path_segments_of_module_expression module_expression with
        | Some [] ->
            ()
        | Some segments ->
            let _ = add_item state ~syntax_node (`ModuleAlias (module_name, String.concat "." segments)) in
            ()
        | None ->
            add_unsupported_signature_item state syntax_node
      )
    | Cst.ModuleSignature.Signature module_type ->
        let nested_scope_path = state.scope_path @ [ module_name ] in
        with_scope state nested_scope_path
          (fun () ->
            lower_signature_items_of_module_type
              state
              module_type
              ~on_unsupported:(fun () -> add_unsupported_signature_item state syntax_node))

let lower_source_file = fun ~source source_file ->
  let state = make_state source in
  let () =
    match source_file with
    | Cst.Implementation implementation ->
        let _items = implementation.items |> List.map (lower_structure_item state) in
        ()
    | Cst.Interface interface ->
        let _items = interface.items |> List.map (lower_signature_item state) in
        ()
  in
  {
    SemanticTree.item_tree = ItemTree.of_list (List.rev state.items);
    body_arena = BodyArena.of_lists
      ~patterns:(List.rev state.patterns)
      ~expressions:(List.rev state.expressions)
      ~bindings:(List.rev state.bindings);
    origin_map = OriginMap.of_list (List.rev state.origins);
    diagnostics = List.rev state.diagnostics
  }

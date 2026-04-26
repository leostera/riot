open Std
module EntityId = Model.Entity_id
module SurfacePath = Model.Surface_path
module TypAst = Ast
module TypingContext = Check.TypingContext

let type_var_name = fun index ->
  match
    List.get
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
      ~at:index
  with
  | Some name -> name
  | None -> "'a" ^ Int.to_string index

let is_ident_start = function
  | 'a' .. 'z'
  | '_' -> true
  | _ -> false

let is_ident_continue = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_'
  | '\'' -> true
  | _ -> false

let is_value_ident = fun name ->
  match String.get name ~at:0 with
  | Some first when is_ident_start first ->
      let rec loop index =
        if index >= String.length name then
          true
        else
          match String.get name ~at:index with
          | Some char when is_ident_continue char -> loop (index + 1)
          | _ -> false
      in
      loop 1
  | _ -> false

let render_value_name = fun name ->
  if is_value_ident name then
    name
  else
    "( " ^ name ^ " )"

let render_type_parameter = function
  | Some name -> "'" ^ name
  | None -> "_"

let render_type_parameters = function
  | [] -> ""
  | [ parameter ] -> render_type_parameter parameter ^ " "
  | parameters -> "(" ^ (parameters |> List.map ~fn:render_type_parameter |> String.concat ", ") ^ ") "

let render_poly_type_parameters = function
  | [] -> ""
  | parameters -> (parameters |> List.map ~fn:(fun name -> "'" ^ name) |> String.concat " ") ^ ". "

let render_constructor_path = fun ~path_prefix path ->
  let segments = SurfacePath.to_segments path in
  let rec strip prefix segments =
    match prefix, segments with
    | [], rest -> Some rest
    | prefix :: prefixes, segment :: segments when String.equal prefix segment -> strip prefixes segments
    | _ -> None
  in
  let segments =
    match path_prefix with
    | [] -> segments
    | prefix -> (
        match strip prefix segments with
        | Some (_ :: _ as rest) -> rest
        | Some []
        | None -> segments
      )
  in
  String.concat "." segments

let normalized_poly_variant_fields = fun fields ->
  fields |> List.sort
    ~compare:(fun left right ->
      String.compare left.TypingContext.tag right.TypingContext.tag) |> List.fold_left ~init:[]
    ~fn:(fun fields field ->
      match fields with
      | previous :: rest when String.equal previous.TypingContext.tag field.TypingContext.tag -> previous
      :: rest
      | _ -> field :: fields) |> List.reverse

let render_poly_variant_fields = fun ~render_payload fields ->
  fields |> normalized_poly_variant_fields |> List.map
    ~fn:(fun field ->
      match field.TypingContext.payload with
      | None -> "`" ^ field.tag
      | Some payload -> "`" ^ field.tag ^ " of " ^ render_payload payload) |> String.concat " | "

let render_poly_variant_type = fun bound ~render_payload fields ->
  let prefix =
    if String.equal bound "" then
      "[ "
    else
      "[" ^ bound
  in
  prefix ^ render_poly_variant_fields ~render_payload fields ^ " ]"

let type_var_names_by_occurrence = fun (type_: TypingContext.type_expr) ->
  let vars = ref [] in
  let remember id =
    if not
        (
          List.exists
            (fun other ->
              Int.equal id other)
            !vars
        ) then
      vars := List.append !vars [ id ]
  in
  let rec collect type_ =
    match type_ with
    | TypingContext.List element
    | TypingContext.Option element ->
        collect element
    | TypingContext.Tuple elements ->
        List.for_each elements ~fn:collect
    | TypingContext.Arrow { parameter; result; _ } ->
        collect parameter;
        collect result
    | TypingContext.TypeConstructor { arguments; _ } ->
        List.for_each arguments ~fn:collect
    | TypingContext.Alias { type_; id } ->
        remember id;
        collect type_
    | TypingContext.Var id ->
        remember id
    | TypingContext.PolyVariant { fields; _ } ->
        fields
        |> List.for_each ~fn:(fun field -> Option.for_each field.TypingContext.payload ~fn:collect)
    | TypingContext.Int
    | TypingContext.Bool
    | TypingContext.Char
    | TypingContext.String
    | TypingContext.Float
    | TypingContext.Unit ->
        ()
    | TypingContext.Package package ->
        package.constraints
        |> List.for_each ~fn:(fun constraint_ -> collect constraint_.TypingContext.manifest)
  in
  collect type_;
  fun id ->
    let rec loop index vars =
      match vars with
      | [] -> type_var_name id
      | var :: rest ->
          if Int.equal var id then
            type_var_name index
          else
            loop (index + 1) rest
    in
    loop 0 !vars

let rec render_type = fun ~path_prefix ~type_var_name type_ ->
  match type_ with
  | TypingContext.Int ->
      "int"
  | TypingContext.Bool ->
      "bool"
  | TypingContext.Char ->
      "char"
  | TypingContext.String ->
      "string"
  | TypingContext.Float ->
      "float"
  | TypingContext.Unit ->
      "unit"
  | TypingContext.List element ->
      render_postfix_argument ~path_prefix ~type_var_name element ^ " list"
  | TypingContext.Option element ->
      render_postfix_argument ~path_prefix ~type_var_name element ^ " option"
  | TypingContext.Tuple elements ->
      elements |> List.map ~fn:(render_tuple_element ~path_prefix ~type_var_name) |> String.concat " * "
  | TypingContext.Arrow { label; parameter; result } ->
      let parameter =
        match parameter with
        | TypingContext.Package package -> render_package_type
          ~path_prefix
          ~type_var_name
          ~include_binder:(package_binder_referenced_in_result package result)
          package
        | _ -> render_arrow_parameter ~path_prefix ~type_var_name parameter
      in
      let parameter = render_arg_label label parameter in
      let result = render_arrow_result ~path_prefix ~type_var_name result in
      parameter ^ " -> " ^ result
  | TypingContext.TypeConstructor { path; arguments=[] } ->
      render_constructor_path ~path_prefix path
  | TypingContext.TypeConstructor { path; arguments=[ argument ] } ->
      render_postfix_argument ~path_prefix ~type_var_name argument
      ^ " "
      ^ render_constructor_path ~path_prefix path
  | TypingContext.TypeConstructor { path; arguments } ->
      "("
      ^ (arguments |> List.map ~fn:(render_type ~path_prefix ~type_var_name) |> String.concat ", ")
      ^ ") "
      ^ render_constructor_path ~path_prefix path
  | TypingContext.Alias { type_; id } ->
      render_type ~path_prefix ~type_var_name type_ ^ " as " ^ type_var_name id
  | TypingContext.Var id ->
      type_var_name id
  | TypingContext.PolyVariant { bound; fields } -> (
      match bound with
      | TypingContext.Exact -> render_poly_variant_type
        ""
        ~render_payload:(render_poly_variant_payload ~path_prefix ~type_var_name)
        fields
      | TypingContext.Upper -> render_poly_variant_type
        "< "
        ~render_payload:(render_poly_variant_payload ~path_prefix ~type_var_name)
        fields
      | TypingContext.Lower -> render_poly_variant_type
        "> "
        ~render_payload:(render_poly_variant_payload ~path_prefix ~type_var_name)
        fields
    )
  | TypingContext.Package package ->
      render_package_type ~path_prefix ~type_var_name ~include_binder:false package

and render_poly_variant_payload = fun ~path_prefix ~type_var_name type_ ->
  match type_ with
  | TypingContext.Arrow _ -> "(" ^ render_type ~path_prefix ~type_var_name type_ ^ ")"
  | _ -> render_type ~path_prefix ~type_var_name type_

and render_postfix_argument = fun ~path_prefix ~type_var_name type_ ->
  match type_ with
  | TypingContext.Arrow _
  | TypingContext.Tuple _
  | TypingContext.Alias _ -> "(" ^ render_type ~path_prefix ~type_var_name type_ ^ ")"
  | _ -> render_type ~path_prefix ~type_var_name type_

and package_binder_referenced_in_result = fun package result ->
  match package.TypingContext.binder with
  | None -> false
  | Some binder -> type_references_prefix [ binder ] result

and type_references_prefix = fun prefix type_ ->
  let path_has_prefix path =
    let rec strip prefix segments =
      match prefix, segments with
      | [], _ -> true
      | expected :: prefix, segment :: segments when String.equal expected segment -> strip prefix segments
      | _ -> false
    in
    strip prefix (SurfacePath.to_segments path)
  in
  match type_ with
  | TypingContext.List element
  | TypingContext.Option element -> type_references_prefix prefix element
  | TypingContext.Tuple elements -> List.exists (type_references_prefix prefix) elements
  | TypingContext.Arrow { parameter; result; _ } -> type_references_prefix prefix parameter
  || type_references_prefix prefix result
  | TypingContext.TypeConstructor { path; arguments } -> path_has_prefix path
  || List.exists (type_references_prefix prefix) arguments
  | TypingContext.Alias { type_; _ } -> type_references_prefix prefix type_
  | TypingContext.Package package -> path_has_prefix package.module_type
  || List.exists
    (fun constraint_ ->
      path_has_prefix constraint_.TypingContext.type_name
      || type_references_prefix prefix constraint_.TypingContext.manifest)
    package.constraints
  | TypingContext.Int
  | TypingContext.Bool
  | TypingContext.Char
  | TypingContext.String
  | TypingContext.Float
  | TypingContext.Unit
  | TypingContext.Var _ -> false
  | TypingContext.PolyVariant { fields; _ } -> List.exists
    (fun field ->
      Option.map field.TypingContext.payload ~fn:(type_references_prefix prefix)
      |> Option.unwrap_or ~default:false)
    fields

and render_package_type = fun ~path_prefix ~type_var_name ~include_binder package ->
  let binder =
    match include_binder, package.TypingContext.binder with
    | true, Some binder -> binder ^ " : "
    | _ -> ""
  in
  let constraints = package.constraints
  |> List.map
    ~fn:(fun constraint_ ->
      " with type "
      ^ render_constructor_path ~path_prefix constraint_.TypingContext.type_name
      ^ " = "
      ^ render_type ~path_prefix ~type_var_name constraint_.TypingContext.manifest)
  |> String.concat "" in
  "(module " ^ binder ^ render_constructor_path ~path_prefix package.module_type ^ constraints ^ ")"

and render_tuple_element = fun ~path_prefix ~type_var_name type_ ->
  match type_ with
  | TypingContext.Arrow _
  | TypingContext.Tuple _
  | TypingContext.Alias _ -> "(" ^ render_type ~path_prefix ~type_var_name type_ ^ ")"
  | _ -> render_type ~path_prefix ~type_var_name type_

and render_arrow_parameter = fun ~path_prefix ~type_var_name type_ ->
  match type_ with
  | TypingContext.Arrow _
  | TypingContext.Alias _ -> "(" ^ render_type ~path_prefix ~type_var_name type_ ^ ")"
  | _ -> render_type ~path_prefix ~type_var_name type_

and render_arrow_result = fun ~path_prefix ~type_var_name type_ ->
  match type_ with
  | TypingContext.Alias _ -> "(" ^ render_type ~path_prefix ~type_var_name type_ ^ ")"
  | _ -> render_type ~path_prefix ~type_var_name type_

and render_arg_label = fun label parameter ->
  match label with
  | TypingContext.Nolabel -> parameter
  | TypingContext.Labelled label -> label ^ ":" ^ parameter
  | TypingContext.Optional label -> "?" ^ label ^ ":" ^ parameter

let render_scheme = fun ~path_prefix (scheme: TypingContext.scheme) ->
  render_type ~path_prefix ~type_var_name:(type_var_names_by_occurrence scheme.body) scheme.body

let is_arrow_type = function
  | TypingContext.Arrow _ -> true
  | _ -> false

let render_multiline_arrow_scheme = fun ~path_prefix (scheme: TypingContext.scheme) ->
  let type_var_name = type_var_names_by_occurrence scheme.body in
  let rec loop type_ =
    match type_ with
    | TypingContext.Arrow { label; parameter; result } ->
        let parameter = render_arrow_parameter ~path_prefix ~type_var_name parameter in
        ("  " ^ render_arg_label label parameter ^ " ->") :: loop result
    | type_ -> [ "  " ^ render_arrow_result ~path_prefix ~type_var_name type_ ]
  in
  loop scheme.body |> String.concat "\n"

let render_named_binding = fun ~path_prefix ~name (binding: TypingContext.value_binding) ->
  let prefix = "val " ^ render_value_name name ^ " : " in
  let rendered = render_scheme ~path_prefix binding.scheme in
  if String.length (prefix ^ rendered) > 80 && is_arrow_type binding.scheme.body then
    "val " ^ render_value_name name ^ " :\n" ^ render_multiline_arrow_scheme ~path_prefix binding.scheme
  else
    prefix ^ rendered

let render_binding = fun (binding: TypingContext.value_binding) ->
  let name = EntityId.surface_path binding.entity_id |> SurfacePath.to_string in
  render_named_binding ~path_prefix:[] ~name binding

type path_substitution = {
  source: SurfacePath.t;
  target: SurfacePath.t;
}

let substitute_path = fun substitutions path ->
  let segments = SurfacePath.to_segments path in
  let rec strip prefix segments =
    match prefix, segments with
    | [], rest -> Some rest
    | prefix :: prefixes, segment :: segments when String.equal prefix segment -> strip prefixes segments
    | _ -> None
  in
  let rec loop substitutions =
    match substitutions with
    | [] -> path
    | substitution :: substitutions -> (
        match strip (SurfacePath.to_segments substitution.source) segments with
        | Some rest -> SurfacePath.from_segments
          (List.append (SurfacePath.to_segments substitution.target) rest)
        | None -> loop substitutions
      )
  in
  loop substitutions

let render_ast_arrow_label = function
  | TypAst.Nolabel -> ""
  | TypAst.Labelled label -> label ^ ":"
  | TypAst.Optional label -> "?" ^ label ^ ":"

let rec render_ast_core_type_with_substitutions = fun substitutions (type_: TypAst.core_type) ->
  match type_.kind with
  | TypAst.Wildcard ->
      "_"
  | TypAst.Var (Some name) ->
      "'" ^ name
  | TypAst.Var None ->
      "_"
  | TypAst.Path path ->
      SurfacePath.to_string (substitute_path substitutions path)
  | TypAst.Apply { constructor; arguments } ->
      render_ast_type_application substitutions constructor arguments
  | TypAst.Arrow { label; parameter; result } ->
      render_ast_arrow_label label
      ^ render_ast_arrow_parameter substitutions parameter
      ^ " -> "
      ^ render_ast_core_type_with_substitutions substitutions result
  | TypAst.Tuple { separator; elements } ->
      let separator =
        match separator with
        | `Comma -> ", "
        | `Star
        | `Unknown -> " * "
      in
      elements |> List.map ~fn:(render_ast_tuple_element substitutions) |> String.concat separator
  | TypAst.ForAll { parameters; body } ->
      render_poly_type_parameters parameters
      ^ render_ast_core_type_with_substitutions substitutions body
  | TypAst.PolyVariant (fields: TypAst.poly_variant_type_field list) ->
      let normalized_fields =
        fields
        |> List.sort
          ~compare:(fun (left: TypAst.poly_variant_type_field) (
            right: TypAst.poly_variant_type_field
          ) ->
            String.compare left.tag right.tag)
        |> List.fold_left ~init:(([]: TypAst.poly_variant_type_field list))
          ~fn:(fun fields (field: TypAst.poly_variant_type_field) ->
            match fields with
            | previous :: rest when String.equal previous.TypAst.tag field.TypAst.tag -> previous
            :: rest
            | _ -> field :: fields)
        |> List.reverse
      in
      let rendered_fields =
        normalized_fields
        |> List.map
          ~fn:(fun (field: TypAst.poly_variant_type_field) ->
            match field.payload with
            | None -> "`" ^ field.tag
            | Some payload -> "`"
            ^ field.tag
            ^ " of "
            ^ render_ast_core_type_with_substitutions substitutions payload)
        |> String.concat " | "
      in
      "[ " ^ rendered_fields ^ " ]"
  | TypAst.Package package ->
      render_ast_package_type substitutions package
  | TypAst.Parenthesized inner ->
      render_ast_core_type_with_substitutions substitutions inner

and render_ast_package_type = fun substitutions (package: TypAst.package_type) ->
  let binder =
    match package.binder with
    | Some binder -> binder ^ " : "
    | None -> ""
  in
  let constraints = package.constraints
  |> List.map
    ~fn:(fun (constraint_: TypAst.package_type_constraint) ->
      " with type "
      ^ SurfacePath.to_string (substitute_path substitutions constraint_.type_name)
      ^ " = "
      ^ render_ast_core_type_with_substitutions substitutions constraint_.manifest)
  |> String.concat "" in
  "(module "
  ^ binder
  ^ SurfacePath.to_string (substitute_path substitutions package.module_type)
  ^ constraints
  ^ ")"

and render_ast_type_application = fun substitutions constructor arguments ->
  let constructor = render_ast_core_type_with_substitutions substitutions constructor in
  match arguments with
  | [] -> constructor
  | [ argument ] -> render_ast_postfix_argument substitutions argument ^ " " ^ constructor
  | arguments -> "("
  ^ (arguments
  |> List.map ~fn:(render_ast_core_type_with_substitutions substitutions)
  |> String.concat ", ")
  ^ ") "
  ^ constructor

and render_ast_postfix_argument = fun substitutions type_ ->
  match type_.kind with
  | TypAst.Arrow _
  | TypAst.Tuple _ -> "(" ^ render_ast_core_type_with_substitutions substitutions type_ ^ ")"
  | _ -> render_ast_core_type_with_substitutions substitutions type_

and render_ast_tuple_element = fun substitutions type_ ->
  match type_.kind with
  | TypAst.Parenthesized inner -> "("
  ^ render_ast_core_type_with_substitutions substitutions inner
  ^ ")"
  | TypAst.Arrow _
  | TypAst.Tuple _ -> "(" ^ render_ast_core_type_with_substitutions substitutions type_ ^ ")"
  | _ -> render_ast_core_type_with_substitutions substitutions type_

and render_ast_arrow_parameter = fun substitutions type_ ->
  match type_.kind with
  | TypAst.Arrow _ -> "(" ^ render_ast_core_type_with_substitutions substitutions type_ ^ ")"
  | _ -> render_ast_core_type_with_substitutions substitutions type_

let render_ast_core_type = render_ast_core_type_with_substitutions []

let render_record_field_declaration_with_substitutions = fun substitutions (
  field: TypAst.record_field_declaration
) ->
  (
    if field.mutable_ then
      "mutable "
    else
      ""
  ) ^ field.name ^ " : " ^ render_ast_core_type_with_substitutions substitutions field.type_annotation ^ ";"

let render_record_field_declaration = render_record_field_declaration_with_substitutions []

let render_type_constructor_with_substitutions = fun substitutions (
  constructor: TypAst.type_constructor
) ->
  match constructor.inline_record, constructor.payload, constructor.result with
  | _, None, Some result -> constructor.name
  ^ " : "
  ^ render_ast_core_type_with_substitutions substitutions result
  | _, Some payload, Some result -> constructor.name
  ^ " : "
  ^ render_ast_arrow_parameter substitutions payload
  ^ " -> "
  ^ render_ast_core_type_with_substitutions substitutions result
  | Some fields, _, None -> constructor.name
  ^ " of { "
  ^ (fields
  |> List.map ~fn:(render_record_field_declaration_with_substitutions substitutions)
  |> String.concat " ")
  ^ " }"
  | None, None, None -> constructor.name
  | None, Some payload, None -> constructor.name
  ^ " of "
  ^ render_ast_core_type_with_substitutions substitutions payload

let render_type_constructor = render_type_constructor_with_substitutions []

let render_type_definition_with_substitutions = fun substitutions (
  definition: TypAst.type_definition
) ->
  match definition.kind with
  | TypAst.Abstract -> ""
  | TypAst.Extensible -> " = .."
  | TypAst.Alias type_ -> " = " ^ render_ast_core_type_with_substitutions substitutions type_
  | TypAst.Variant constructors -> " = "
  ^ (constructors
  |> List.map ~fn:(render_type_constructor_with_substitutions substitutions)
  |> String.concat " | ")
  | TypAst.Record fields -> " = { "
  ^ (fields
  |> List.map ~fn:(render_record_field_declaration_with_substitutions substitutions)
  |> String.concat " ")
  ^ " }"

let render_type_definition = render_type_definition_with_substitutions []

let render_type_declaration_with_keyword_and_substitutions = fun substitutions keyword (
  declaration: TypAst.type_declaration
) ->
  let prefix = keyword ^ " " ^ render_type_parameters declaration.parameters ^ declaration.name in
  match declaration.definition.kind with
  | TypAst.Variant constructors ->
      let constructors = constructors
      |> List.map ~fn:(render_type_constructor_with_substitutions substitutions) in
      let inline = prefix ^ " = " ^ String.concat " | " constructors in
      if String.length inline <= 77 then
        inline
      else
        (
          match constructors with
          | [] -> prefix ^ " ="
          | constructor :: constructors -> prefix
          ^ " =\n    "
          ^ constructor
          ^ (constructors
          |> List.map ~fn:(fun constructor -> "\n  | " ^ constructor)
          |> String.concat "")
        )
  | _ -> prefix ^ render_type_definition_with_substitutions substitutions declaration.definition

let render_type_declaration_with_keyword = render_type_declaration_with_keyword_and_substitutions []

let render_type_declaration = render_type_declaration_with_keyword "type"

let render_type_declaration_group_with_substitutions = fun substitutions ->
  function
  | [] -> ""
  | declaration :: declarations ->
      let lines = render_type_declaration_with_keyword_and_substitutions substitutions "type" declaration
      :: List.map
        declarations
        ~fn:(render_type_declaration_with_keyword_and_substitutions substitutions "and") in
      String.concat "\n" lines

let render_type_declaration_group = render_type_declaration_group_with_substitutions []

let render_type_extension_declaration_with_substitutions = fun substitutions (
  declaration: TypAst.type_extension_declaration
) ->
  "type "
  ^ SurfacePath.to_string (substitute_path substitutions declaration.name)
  ^ " += "
  ^ (declaration.constructors
  |> List.map ~fn:(render_type_constructor_with_substitutions substitutions)
  |> String.concat " | ")

let render_type_extension_declaration = render_type_extension_declaration_with_substitutions []

let render_exception_declaration_with_substitutions = fun substitutions (
  declaration: TypAst.exception_declaration
) ->
  match declaration.payload with
  | Some payload -> "exception "
  ^ declaration.name
  ^ " of "
  ^ render_ast_core_type_with_substitutions substitutions payload
  | None -> "exception " ^ declaration.name

let render_exception_declaration = render_exception_declaration_with_substitutions []

let render_value_declaration = fun (declaration: TypAst.value_declaration) ->
  "val " ^ render_value_name declaration.name ^ " : " ^ render_ast_core_type declaration.type_annotation

let render_external_declaration_with_substitutions = fun substitutions (
  declaration: TypAst.external_declaration
) ->
  "external "
  ^ render_value_name declaration.name
  ^ " : "
  ^ render_ast_core_type_with_substitutions substitutions declaration.type_annotation
  ^ " = "
  ^ String.concat " " declaration.primitives

let render_external_declaration = render_external_declaration_with_substitutions []

let rec render_signature_item = fun (item: TypAst.signature_item) ->
  match item.kind with
  | TypAst.Value declaration -> Some (render_value_declaration declaration)
  | TypAst.Type declarations -> Some (render_type_declaration_group declarations)
  | TypAst.TypeExtension declaration -> Some (render_type_extension_declaration declaration)
  | TypAst.External declaration -> Some (render_external_declaration declaration)
  | TypAst.Exception declaration -> Some (render_exception_declaration declaration)

let render_module_type_declaration = fun (declaration: TypAst.module_type_declaration) ->
  let signature_items = declaration.items
  |> List.filter_map ~fn:render_signature_item
  |> String.concat " " in
  "module type " ^ declaration.name ^ " = sig " ^ signature_items ^ " end"

let path_from_prefix = fun path_prefix name ->
  match path_prefix with
  | [] -> SurfacePath.from_name name
  | prefix -> SurfacePath.from_segments (List.append prefix [ name ])

let find_value_binding = fun (typing_context: TypingContext.t) path ->
  List.find typing_context.values
    ~fn:(fun binding ->
      SurfacePath.equal (EntityId.surface_path binding.entity_id) path)

let rec pattern_bound_name = fun (pattern: TypAst.pattern) ->
  match pattern.kind with
  | TypAst.Path path -> (
      match List.reverse (SurfacePath.to_segments path) with
      | name :: _ -> Some name
      | [] -> None
    )
  | TypAst.Constraint { pattern; _ }
  | TypAst.Attribute pattern
  | TypAst.Parenthesized pattern ->
      pattern_bound_name pattern
  | TypAst.Alias { alias; pattern } -> (
      match pattern_bound_name alias with
      | Some name -> Some name
      | None -> pattern_bound_name pattern
    )
  | _ ->
      None

let let_declaration_names = fun (declaration: TypAst.let_declaration) ->
  declaration.bindings
  |> List.filter_map ~fn:(fun (binding: TypAst.let_binding) -> pattern_bound_name binding.pattern)

let render_let_declaration = fun ~typing_context ~path_prefix declaration ->
  declaration |> let_declaration_names |> List.filter_map
    ~fn:(fun name ->
      let path = path_from_prefix path_prefix name in
      Option.map
        (find_value_binding typing_context path)
        ~fn:(render_named_binding ~path_prefix ~name)) |> String.concat " "

let rec find_module_declaration = fun (items: TypAst.structure_item list) path ->
  find_module_declaration_segments items (SurfacePath.to_segments path)

and find_module_declaration_segments = fun (items: TypAst.structure_item list) segments ->
  match segments with
  | [] -> None
  | name :: rest ->
      let rec find_in_items (items: TypAst.structure_item list) =
        match items with
        | [] -> None
        | (item: TypAst.structure_item) :: items -> (
            match item.TypAst.kind with
            | TypAst.Module declarations -> (
                match find_in_declarations declarations name rest with
                | Some declaration -> Some declaration
                | None -> find_in_items items
              )
            | _ -> find_in_items items
          )
      in
      find_in_items items

and find_in_declarations = fun (declarations: TypAst.module_declaration list) name rest ->
  match declarations with
  | [] -> None
  | declaration :: declarations ->
      if String.equal declaration.TypAst.name name then
        if List.is_empty rest then
          Some declaration
        else
          find_module_declaration_segments declaration.items rest
      else
        find_in_declarations declarations name rest

let rec resolve_module_declaration_alias = fun ~root_items (declaration: TypAst.module_declaration) ->
  match declaration.alias with
  | Some alias -> (
      match find_module_declaration root_items alias with
      | Some target when not (String.equal target.name declaration.name) -> resolve_module_declaration_alias
        ~root_items
        target
      | _ -> declaration
    )
  | None -> declaration

let render_functor_parameter = fun (parameter: TypAst.functor_parameter) ->
  let module_type =
    match parameter.module_type with
    | Some path -> SurfacePath.to_string path
    | None -> "_"
  in
  "(" ^ parameter.name ^ " : " ^ module_type ^ ") -> "

let render_functor_parameters = fun parameters ->
  parameters |> List.map ~fn:render_functor_parameter |> String.concat ""

let application_items_and_substitutions = fun ~root_items ~path_prefix ~module_prefix application ->
  match find_module_declaration root_items application.TypAst.callee with
  | Some callee -> (
      match callee.parameters with
      | parameter :: _ -> (
        callee.items,
        [
          { source = SurfacePath.from_name parameter.name; target = application.argument };
          { source = application.callee; target = SurfacePath.from_segments module_prefix };
        ]
      )
      | [] -> (
        callee.items,
        [ { source = application.callee; target = SurfacePath.from_segments module_prefix }; ]
      )
    )
  | None -> ([], [])

let rec render_module_declaration_with_keyword = fun ~root_items ~typing_context ~substitutions ~path_prefix ~keyword (
  declaration: TypAst.module_declaration
) ->
  let module_head = keyword ^ " " ^ declaration.name in
  let module_prefix = List.append path_prefix [ declaration.name ] in
  match declaration.alias with
  | Some alias -> module_head ^ " = " ^ SurfacePath.to_string alias
  | None ->
      let items, local_substitutions =
        match declaration.application with
        | Some application -> application_items_and_substitutions
          ~root_items
          ~path_prefix
          ~module_prefix
          application
        | None -> (declaration.items, [])
      in
      let substitutions = List.append local_substitutions substitutions in
      let functor_parameters = render_functor_parameters declaration.parameters in
      let signature_item_lines = render_module_signature_item_lines
        ~root_items
        ~typing_context
        ~substitutions
        ~path_prefix:module_prefix
        items in
      let signature_items = String.concat " " signature_item_lines in
      let inline = module_head ^ " : " ^ functor_parameters ^ "sig " ^ signature_items ^ " end" in
      if String.length inline > 77 then
        if String.equal functor_parameters "" then
          let signature_line = "sig " ^ signature_items ^ " end" in
          if String.length ("  " ^ signature_line) <= 77 then
            module_head ^ " :\n  " ^ signature_line
          else
            module_head ^ " :\n  sig\n    " ^ String.concat "\n    " signature_item_lines ^ "\n  end"
        else
          module_head ^ " :\n  " ^ functor_parameters ^ "sig " ^ signature_items ^ " end"
      else
        inline

and render_module_declaration = fun ~root_items ~typing_context ~substitutions ~path_prefix declaration ->
  render_module_declaration_with_keyword
    ~root_items
    ~typing_context
    ~substitutions
    ~path_prefix
    ~keyword:"module"
    declaration

and render_module_declaration_group = fun ~root_items ~typing_context ~substitutions ~path_prefix declarations ->
  match declarations with
  | [] ->
      ""
  | first :: rest when first.TypAst.recursive ->
      let first = render_module_declaration_with_keyword
        ~root_items
        ~typing_context
        ~substitutions
        ~path_prefix
        ~keyword:"module rec"
        first in
      let rest = rest
      |> List.map
        ~fn:(render_module_declaration_with_keyword
          ~root_items
          ~typing_context
          ~substitutions
          ~path_prefix
          ~keyword:"and") in
      String.concat "\n" (first :: rest)
  | _ ->
      declarations
      |> List.map
        ~fn:(render_module_declaration ~root_items ~typing_context ~substitutions ~path_prefix)
      |> String.concat " "

and render_module_signature_item_lines = fun ~root_items ~typing_context ~substitutions ~path_prefix items ->
  items
  |> List.filter_map
    ~fn:(render_structure_signature_item ~root_items ~typing_context ~substitutions ~path_prefix)

and render_module_signature_items = fun ~root_items ~typing_context ~substitutions ~path_prefix items ->
  render_module_signature_item_lines ~root_items ~typing_context ~substitutions ~path_prefix items
  |> String.concat " "

and render_structure_signature_item = fun ~root_items ~typing_context ~substitutions ~path_prefix (
  item: TypAst.structure_item
) ->
  match item.kind with
  | TypAst.Type declarations ->
      Some (render_type_declaration_group_with_substitutions substitutions declarations)
  | TypAst.TypeExtension declaration ->
      Some (render_type_extension_declaration_with_substitutions substitutions declaration)
  | TypAst.Module declarations ->
      Some (render_module_declaration_group ~root_items ~typing_context ~substitutions ~path_prefix declarations)
  | TypAst.ModuleType declaration ->
      if List.is_empty path_prefix then
        Some (render_module_type_declaration declaration)
      else
        None
  | TypAst.Let declaration ->
      if List.is_empty path_prefix then
        None
      else
        Some (render_let_declaration ~typing_context ~path_prefix declaration)
  | TypAst.Include path -> (
      match find_module_declaration root_items path with
      | Some declaration ->
          let declaration = resolve_module_declaration_alias ~root_items declaration in
          Some (render_module_signature_items
            ~root_items
            ~typing_context
            ~substitutions
            ~path_prefix
            declaration.items)
      | None -> None
    )
  | TypAst.External declaration ->
      Some (render_external_declaration_with_substitutions substitutions declaration)
  | TypAst.Exception declaration ->
      Some (render_exception_declaration_with_substitutions substitutions declaration)
  | TypAst.Expression _ ->
      None

let ast_signature_declarations = fun typing_context (ast: TypAst.t) ->
  match ast.kind with
  | TypAst.Implementation items -> items
  |> List.filter_map
    ~fn:(render_structure_signature_item
      ~root_items:items
      ~typing_context
      ~substitutions:[]
      ~path_prefix:[])
  | TypAst.Interface items -> items
  |> List.filter_map ~fn:(fun (item: TypAst.signature_item) -> render_signature_item item)
  | TypAst.Empty _ -> []

let from_typings = fun (typings: Check.Typings.t) ->
  let declaration_lines =
    match ast_signature_declarations typings.typing_context typings.ast with
    | [] -> List.map typings.type_declarations ~fn:(fun declaration -> [ declaration ])
    |> List.map ~fn:render_type_declaration_group
    | declarations -> declarations
  in
  let lines = List.append declaration_lines (List.map typings.bindings ~fn:render_binding) in
  match lines with
  | [] -> ""
  | lines -> lines |> String.concat "\n" |> fun source -> source ^ "\n"

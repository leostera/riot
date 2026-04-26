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

let normalized_poly_variant_tags = fun tags ->
  tags |> List.sort ~compare:String.compare |> List.unique ~compare:String.compare

let render_poly_variant_tags = fun tags ->
  tags |> normalized_poly_variant_tags |> List.map ~fn:(fun tag -> "`" ^ tag) |> String.concat " | "

let render_poly_variant_type = fun bound tags ->
  let prefix =
    if String.equal bound "" then
      "[ "
    else
      "[" ^ bound
  in
  prefix ^ render_poly_variant_tags tags ^ " ]"

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
        collect type_;
        remember id
    | TypingContext.Var id ->
        remember id
    | TypingContext.Int
    | TypingContext.Bool
    | TypingContext.Char
    | TypingContext.String
    | TypingContext.Float
    | TypingContext.Unit
    | TypingContext.PolyVariant _ ->
        ()
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
      let parameter = render_arrow_parameter ~path_prefix ~type_var_name parameter in
      let parameter = render_arg_label label parameter in
      let result = render_type ~path_prefix ~type_var_name result in
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
  | TypingContext.PolyVariant { bound; tags } -> (
      match bound with
      | TypingContext.Exact -> render_poly_variant_type "" tags
      | TypingContext.Upper -> render_poly_variant_type "< " tags
      | TypingContext.Lower -> render_poly_variant_type "> " tags
    )

and render_postfix_argument = fun ~path_prefix ~type_var_name type_ ->
  match type_ with
  | TypingContext.Arrow _
  | TypingContext.Tuple _
  | TypingContext.Alias _ -> "(" ^ render_type ~path_prefix ~type_var_name type_ ^ ")"
  | _ -> render_type ~path_prefix ~type_var_name type_

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

and render_arg_label = fun label parameter ->
  match label with
  | TypingContext.Nolabel -> parameter
  | TypingContext.Labelled label -> label ^ ":" ^ parameter
  | TypingContext.Optional label -> "?" ^ label ^ ":" ^ parameter

let render_scheme = fun ~path_prefix (scheme: TypingContext.scheme) ->
  render_type ~path_prefix ~type_var_name:(type_var_names_by_occurrence scheme.body) scheme.body

let render_named_binding = fun ~path_prefix ~name (binding: TypingContext.value_binding) ->
  "val " ^ render_value_name name ^ " : " ^ render_scheme ~path_prefix binding.scheme

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

let rec render_ast_core_type_with_substitutions = fun substitutions (type_: TypAst.core_type) ->
  match type_.kind with
  | TypAst.Wildcard -> "_"
  | TypAst.Var (Some name) -> "'" ^ name
  | TypAst.Var None -> "_"
  | TypAst.Path path -> SurfacePath.to_string (substitute_path substitutions path)
  | TypAst.Apply _ -> render_ast_type_application substitutions type_
  | TypAst.Arrow { left; right } -> render_ast_arrow_parameter substitutions left
  ^ " -> "
  ^ render_ast_core_type_with_substitutions substitutions right
  | TypAst.Tuple elements -> elements
  |> List.map ~fn:(render_ast_tuple_element substitutions)
  |> String.concat " * "
  | TypAst.Labeled annotation -> render_ast_core_type_with_substitutions substitutions annotation
  | TypAst.Poly { parameters; body } -> render_poly_type_parameters parameters
  ^ render_ast_core_type_with_substitutions substitutions body
  | TypAst.PolyVariant tags -> render_poly_variant_type "" tags
  | TypAst.Parenthesized inner -> render_ast_core_type_with_substitutions substitutions inner

and render_ast_type_application = fun substitutions type_ ->
  let rec collect arguments (current: TypAst.core_type) =
    match current.kind with
    | TypAst.Apply { argument; constructor } -> collect (argument :: arguments) constructor
    | _ -> (current, arguments)
  in
  let constructor, arguments = collect [] type_ in
  match arguments with
  | [] -> render_ast_core_type_with_substitutions substitutions constructor
  | [ argument ] -> render_ast_postfix_argument substitutions argument
  ^ " "
  ^ render_ast_core_type_with_substitutions substitutions constructor
  | arguments -> "("
  ^ (arguments
  |> List.map ~fn:(render_ast_core_type_with_substitutions substitutions)
  |> String.concat ", ")
  ^ ") "
  ^ render_ast_core_type_with_substitutions substitutions constructor

and render_ast_postfix_argument = fun substitutions type_ ->
  match type_.kind with
  | TypAst.Arrow _
  | TypAst.Tuple _ -> "(" ^ render_ast_core_type_with_substitutions substitutions type_ ^ ")"
  | _ -> render_ast_core_type_with_substitutions substitutions type_

and render_ast_tuple_element = fun substitutions type_ ->
  match type_.kind with
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
  match constructor.inline_record, constructor.payload with
  | Some fields, _ -> constructor.name
  ^ " of { "
  ^ (fields
  |> List.map ~fn:(render_record_field_declaration_with_substitutions substitutions)
  |> String.concat " ")
  ^ " }"
  | None, None -> constructor.name
  | None, Some payload -> constructor.name
  ^ " of "
  ^ render_ast_core_type_with_substitutions substitutions payload

let render_type_constructor = render_type_constructor_with_substitutions []

let render_type_definition_with_substitutions = fun substitutions (
  definition: TypAst.type_definition
) ->
  match definition.kind with
  | TypAst.Abstract -> ""
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
  keyword
  ^ " "
  ^ render_type_parameters declaration.parameters
  ^ declaration.name
  ^ render_type_definition_with_substitutions substitutions declaration.definition

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

let render_value_declaration = fun (declaration: TypAst.value_declaration) ->
  "val " ^ render_value_name declaration.name ^ " : " ^ render_ast_core_type declaration.type_annotation

let render_external_declaration = fun (declaration: TypAst.external_declaration) ->
  "val " ^ render_value_name declaration.name ^ " : " ^ render_ast_core_type declaration.type_annotation

let rec render_signature_item = fun (item: TypAst.signature_item) ->
  match item.kind with
  | TypAst.Value declaration -> Some (render_value_declaration declaration)
  | TypAst.Type declarations -> Some (render_type_declaration_group declarations)
  | TypAst.External declaration -> Some (render_external_declaration declaration)

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

let rec render_module_declaration = fun ~root_items ~typing_context ~substitutions ~path_prefix (
  declaration: TypAst.module_declaration
) ->
  let module_prefix = List.append path_prefix [ declaration.name ] in
  match declaration.alias with
  | Some alias -> "module " ^ declaration.name ^ " = " ^ SurfacePath.to_string alias
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
      let inline = "module "
      ^ declaration.name
      ^ " : "
      ^ functor_parameters
      ^ "sig "
      ^ signature_items
      ^ " end" in
      if String.length inline > 77 then
        if String.equal functor_parameters "" then
          match signature_item_lines with
          | [ line ] -> "module " ^ declaration.name ^ " :\n  sig " ^ line ^ " end"
          | _ -> "module "
          ^ declaration.name
          ^ " :\n  sig\n    "
          ^ String.concat "\n    " signature_item_lines
          ^ "\n  end"
        else
          "module " ^ declaration.name ^ " :\n  " ^ functor_parameters ^ "sig " ^ signature_items ^ " end"
      else
        inline

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
  | TypAst.Module declarations ->
      Some (declarations
      |> List.map
        ~fn:(render_module_declaration ~root_items ~typing_context ~substitutions ~path_prefix)
      |> String.concat " ")
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
      | Some declaration -> Some (render_module_signature_items
        ~root_items
        ~typing_context
        ~substitutions
        ~path_prefix
        declaration.items)
      | None -> None
    )
  | TypAst.Expression _
  | TypAst.External _ ->
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

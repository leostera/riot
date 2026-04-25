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

let render_constructor_path = fun path -> SurfacePath.to_string path

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

let rec render_type = fun ~type_var_name type_ ->
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
      render_postfix_argument ~type_var_name element ^ " list"
  | TypingContext.Option element ->
      render_postfix_argument ~type_var_name element ^ " option"
  | TypingContext.Tuple elements ->
      elements |> List.map ~fn:(render_tuple_element ~type_var_name) |> String.concat " * "
  | TypingContext.Arrow { label; parameter; result } ->
      let parameter = render_arrow_parameter ~type_var_name parameter in
      let parameter = render_arg_label label parameter in
      let result = render_type ~type_var_name result in
      parameter ^ " -> " ^ result
  | TypingContext.TypeConstructor { path; arguments=[] } ->
      render_constructor_path path
  | TypingContext.TypeConstructor { path; arguments=[ argument ] } ->
      render_postfix_argument ~type_var_name argument ^ " " ^ render_constructor_path path
  | TypingContext.TypeConstructor { path; arguments } ->
      "("
      ^ (arguments |> List.map ~fn:(render_type ~type_var_name) |> String.concat ", ")
      ^ ") "
      ^ render_constructor_path path
  | TypingContext.Var id ->
      type_var_name id
  | TypingContext.PolyVariant { bound; tags } -> (
      match bound with
      | TypingContext.Exact -> render_poly_variant_type "" tags
      | TypingContext.Upper -> render_poly_variant_type "< " tags
      | TypingContext.Lower -> render_poly_variant_type "> " tags
    )

and render_postfix_argument = fun ~type_var_name type_ ->
  match type_ with
  | TypingContext.Arrow _
  | TypingContext.Tuple _ -> "(" ^ render_type ~type_var_name type_ ^ ")"
  | _ -> render_type ~type_var_name type_

and render_tuple_element = fun ~type_var_name type_ ->
  match type_ with
  | TypingContext.Arrow _
  | TypingContext.Tuple _ -> "(" ^ render_type ~type_var_name type_ ^ ")"
  | _ -> render_type ~type_var_name type_

and render_arrow_parameter = fun ~type_var_name type_ ->
  match type_ with
  | TypingContext.Arrow _ -> "(" ^ render_type ~type_var_name type_ ^ ")"
  | _ -> render_type ~type_var_name type_

and render_arg_label = fun label parameter ->
  match label with
  | TypingContext.Nolabel -> parameter
  | TypingContext.Labelled label -> label ^ ":" ^ parameter
  | TypingContext.Optional label -> "?" ^ label ^ ":" ^ parameter

let render_scheme = fun (scheme: TypingContext.scheme) ->
  render_type ~type_var_name:(type_var_names_by_occurrence scheme.body) scheme.body

let render_binding = fun (binding: TypingContext.value_binding) ->
  let name = EntityId.surface_path binding.entity_id |> SurfacePath.to_string in
  "val " ^ render_value_name name ^ " : " ^ render_scheme binding.scheme

let rec render_ast_core_type = fun (type_: TypAst.core_type) ->
  match type_.kind with
  | TypAst.Wildcard -> "_"
  | TypAst.Var (Some name) -> "'" ^ name
  | TypAst.Var None -> "_"
  | TypAst.Path path -> SurfacePath.to_string path
  | TypAst.Apply _ -> render_ast_type_application type_
  | TypAst.Arrow { left; right } -> render_ast_arrow_parameter left ^ " -> " ^ render_ast_core_type right
  | TypAst.Tuple elements -> elements |> List.map ~fn:render_ast_tuple_element |> String.concat " * "
  | TypAst.Labeled annotation -> render_ast_core_type annotation
  | TypAst.Poly { parameters; body } -> render_poly_type_parameters parameters
  ^ render_ast_core_type body
  | TypAst.PolyVariant tags -> render_poly_variant_type "" tags
  | TypAst.Parenthesized inner -> render_ast_core_type inner

and render_ast_type_application = fun type_ ->
  let rec collect arguments (current: TypAst.core_type) =
    match current.kind with
    | TypAst.Apply { argument; constructor } -> collect (argument :: arguments) constructor
    | _ -> (current, arguments)
  in
  let constructor, arguments = collect [] type_ in
  match arguments with
  | [] -> render_ast_core_type constructor
  | [ argument ] -> render_ast_postfix_argument argument ^ " " ^ render_ast_core_type constructor
  | arguments -> "("
  ^ (arguments |> List.map ~fn:render_ast_core_type |> String.concat ", ")
  ^ ") "
  ^ render_ast_core_type constructor

and render_ast_postfix_argument = fun type_ ->
  match type_.kind with
  | TypAst.Arrow _
  | TypAst.Tuple _ -> "(" ^ render_ast_core_type type_ ^ ")"
  | _ -> render_ast_core_type type_

and render_ast_tuple_element = fun type_ ->
  match type_.kind with
  | TypAst.Arrow _
  | TypAst.Tuple _ -> "(" ^ render_ast_core_type type_ ^ ")"
  | _ -> render_ast_core_type type_

and render_ast_arrow_parameter = fun type_ ->
  match type_.kind with
  | TypAst.Arrow _ -> "(" ^ render_ast_core_type type_ ^ ")"
  | _ -> render_ast_core_type type_

let render_type_constructor = fun (constructor: TypAst.type_constructor) ->
  match constructor.payload with
  | None -> constructor.name
  | Some payload -> constructor.name ^ " of " ^ render_ast_core_type payload

let render_record_field_declaration = fun (field: TypAst.record_field_declaration) ->
  (
    if field.mutable_ then
      "mutable "
    else
      ""
  ) ^ field.name ^ " : " ^ render_ast_core_type field.type_annotation ^ ";"

let render_type_definition = fun (definition: TypAst.type_definition) ->
  match definition.kind with
  | TypAst.Abstract -> ""
  | TypAst.Alias type_ -> " = " ^ render_ast_core_type type_
  | TypAst.Variant constructors -> " = "
  ^ (constructors |> List.map ~fn:render_type_constructor |> String.concat " | ")
  | TypAst.Record fields -> " = { "
  ^ (fields |> List.map ~fn:render_record_field_declaration |> String.concat " ")
  ^ " }"

let render_type_declaration_with_keyword = fun keyword (declaration: TypAst.type_declaration) ->
  keyword
  ^ " "
  ^ render_type_parameters declaration.parameters
  ^ declaration.name
  ^ render_type_definition declaration.definition

let render_type_declaration = render_type_declaration_with_keyword "type"

let render_type_declaration_group = function
  | [] -> ""
  | declaration :: declarations ->
      let lines = render_type_declaration declaration
      :: List.map declarations ~fn:(render_type_declaration_with_keyword "and") in
      String.concat "\n" lines

let ast_type_declaration_groups = fun (ast: TypAst.t) ->
  match ast.kind with
  | TypAst.Implementation items ->
      items |> List.filter_map
        ~fn:(fun (item: TypAst.structure_item) ->
          match item.kind with
          | TypAst.Type declarations -> Some declarations
          | _ -> None)
  | TypAst.Interface items ->
      items |> List.filter_map
        ~fn:(fun (item: TypAst.signature_item) ->
          match item.kind with
          | TypAst.Type declarations -> Some declarations
          | _ -> None)
  | TypAst.Empty _ -> []

let from_typings = fun (typings: Check.Typings.t) ->
  let type_declaration_groups =
    match ast_type_declaration_groups typings.ast with
    | [] -> List.map typings.type_declarations ~fn:(fun declaration -> [ declaration ])
    | groups -> groups
  in
  let lines = List.append
    (List.map type_declaration_groups ~fn:render_type_declaration_group)
    (List.map typings.bindings ~fn:render_binding) in
  match lines with
  | [] -> ""
  | lines -> lines |> String.concat "\n" |> fun source -> source ^ "\n"

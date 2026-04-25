open Std
module EntityId = Model.Entity_id
module SurfacePath = Model.Surface_path
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

let scheme_type_var_name = fun (scheme: TypingContext.scheme) id ->
  let rec loop index vars =
    match vars with
    | [] -> type_var_name id
    | var :: rest ->
        if Int.equal var id then
          type_var_name index
        else
          loop (index + 1) rest
  in
  loop 0 scheme.forall

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
  | TypingContext.Arrow { parameter; result } ->
      let parameter = render_arrow_parameter ~type_var_name parameter in
      let result = render_type ~type_var_name result in
      parameter ^ " -> " ^ result
  | TypingContext.Var id ->
      type_var_name id

and render_postfix_argument = fun ~type_var_name type_ ->
  match type_ with
  | TypingContext.Arrow _
  | TypingContext.Tuple _ -> "(" ^ render_type ~type_var_name type_ ^ ")"
  | _ -> render_type ~type_var_name type_

and render_tuple_element = fun ~type_var_name type_ ->
  match type_ with
  | TypingContext.Arrow _ -> "(" ^ render_type ~type_var_name type_ ^ ")"
  | _ -> render_type ~type_var_name type_

and render_arrow_parameter = fun ~type_var_name type_ ->
  match type_ with
  | TypingContext.Arrow _ -> "(" ^ render_type ~type_var_name type_ ^ ")"
  | _ -> render_type ~type_var_name type_

let render_scheme = fun (scheme: TypingContext.scheme) ->
  render_type ~type_var_name:(scheme_type_var_name scheme) scheme.body

let render_binding = fun (binding: TypingContext.value_binding) ->
  let name = EntityId.surface_path binding.entity_id |> SurfacePath.to_string in
  "val " ^ render_value_name name ^ " : " ^ render_scheme binding.scheme

let from_typings = fun (typings: Check.Typings.t) ->
  match typings.bindings with
  | [] -> ""
  | bindings -> bindings
  |> List.map ~fn:render_binding
  |> String.concat "\n"
  |> fun source -> source ^ "\n"

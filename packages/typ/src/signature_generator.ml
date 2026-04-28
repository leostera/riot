open Std

module SurfacePath = Model.Surface_path
module TypAst = Ast

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

let render_value = fun (name, type_) ->
  "val " ^ render_value_name (SurfacePath.to_string name) ^ " : " ^ TypAst.Type.to_string type_

let from_values = fun values ->
  match values
  |> Iter.Iterator.to_list
  |> List.map ~fn:render_value with
  | [] -> ""
  | lines -> String.concat "\n" lines ^ "\n"

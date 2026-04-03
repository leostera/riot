open Std

type render_state = {
  mutable next_name: int;
  mutable names: (int * string) list;
}

let make_render_state = fun () ->
  { next_name = 0; names = [] }

let pretty_name = fun index ->
  let suffix =
    if index < 26 then
      ""
    else
      string_of_int (index / 26)
  in
  let letter =
    Char.chr (Char.code 'a' + (index mod 26))
  in
  "'" ^ String.make 1 letter ^ suffix

let name_for_var = fun state id ->
  match List.assoc_opt id state.names with
  | Some name -> name
  | None ->
      let name = pretty_name state.next_name in
      let () = state.next_name <- state.next_name + 1 in
      let () = state.names <- (id, name) :: state.names in
      name

let rec render_type = fun state ~nested ty ->
  match TypeRepr.prune ty with
  | TypeRepr.Int -> "int"
  | TypeRepr.Bool -> "bool"
  | TypeRepr.String -> "string"
  | TypeRepr.Unit -> "unit"
  | TypeRepr.Hole hole_id -> "_?" ^ Int.to_string hole_id
  | TypeRepr.Var var -> name_for_var state var.id
  | TypeRepr.Tuple members ->
      members
      |> List.map (render_type state ~nested:true)
      |> String.concat " * "
      |> fun text -> if nested then "(" ^ text ^ ")" else text
  | TypeRepr.Arrow (lhs, rhs) ->
      let text =
        render_type state ~nested:true lhs ^ " -> " ^ render_type state ~nested:false rhs
      in
      if nested then
        "(" ^ text ^ ")"
      else
        text

let type_to_string = fun ty ->
  let state = make_render_state () in
  render_type state ~nested:false ty

let scheme_to_string = fun (TypeScheme.Forall (quantified, body)) ->
  let state = make_render_state () in
  let quantified_names =
    quantified
    |> List.rev
    |> List.map (name_for_var state)
  in
  let body = render_type state ~nested:false body in
  match quantified_names with
  | [] -> body
  | _ -> String.concat " " quantified_names ^ ". " ^ body

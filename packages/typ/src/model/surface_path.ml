open Std
open Std.Collections

type t =
  | Bare of string
  | Qualified of string * t

let empty = Bare ""

let is_empty = fun value ->
  match value with
  | Bare "" -> true
  | _ -> false

let of_name = fun name -> Bare name

let of_segments = fun segments ->
  let rec loop segments =
    match segments with
    | [] -> empty
    | [ name ] -> Bare name
    | module_name :: rest -> Qualified (module_name, loop rest)
  in
  loop segments

let to_segments =
  let rec loop acc value =
    match value with
    | Bare "" -> List.reverse acc
    | Bare name -> List.reverse (name :: acc)
    | Qualified (name, tail) -> loop (name :: acc) tail
  in
  loop []

let to_string = fun value -> value |> to_segments |> String.concat "."

let rec equal = fun left right ->
  match left, right with
  | Bare left_name, Bare right_name -> String.equal left_name right_name
  | Qualified (left_name, left_tail), Qualified (right_name, right_tail) -> String.equal left_name right_name
  && equal left_tail right_tail
  | _ -> false

let rec compare = fun left right ->
  match left, right with
  | Bare left_name, Bare right_name ->
      String.compare left_name right_name
  | Bare _, Qualified _ ->
      Order.LT
  | Qualified _, Bare _ ->
      Order.GT
  | Qualified (left_name, left_tail), Qualified (right_name, right_tail) -> (
      match String.compare left_name right_name with
      | Order.EQ -> compare left_tail right_tail
      | order -> order
    )

let serializer = Serde.Ser.contramap
  to_segments
  (Serde.Ser.contramap Array.from_list (Serde.Ser.array Serde.Ser.string))

open Std

type var = {
  id: int;
  mutable link: t option;
}

and t =
  | Int
  | Bool
  | String
  | Unit
  | Tuple of t list
  | Arrow of t * t
  | Var of var
  | Hole of int

let rec prune = function
  | Var ({ link = Some linked; _ } as var) ->
      let linked = prune linked in
      let () = var.link <- Some linked in
      linked
  | ty -> ty

let add_unique = fun xs x ->
  if List.mem x xs then
    xs
  else
    x :: xs

let union = fun left right ->
  List.fold_left add_unique left right

let diff = fun left right ->
  List.filter (fun value -> not (List.mem value right)) left

let rec free_vars = function
  | Int
  | Bool
  | String
  | Unit
  | Hole _ -> []
  | Tuple members ->
      List.fold_left
        (fun acc member -> union acc (free_vars (prune member)))
        []
        members
  | Arrow (lhs, rhs) ->
      union (free_vars (prune lhs)) (free_vars (prune rhs))
  | Var var -> (
      match var.link with
      | Some linked -> free_vars linked
      | None -> [ var.id ])

let rec occurs = fun needle ty ->
  match prune ty with
  | Int
  | Bool
  | String
  | Unit
  | Hole _ -> false
  | Tuple members ->
      List.exists (occurs needle) members
  | Arrow (lhs, rhs) ->
      occurs needle lhs || occurs needle rhs
  | Var var -> (
      match var.link with
      | Some linked -> occurs needle linked
      | None -> var.id = needle)

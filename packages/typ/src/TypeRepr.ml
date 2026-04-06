open Std

type label =
  | Nolabel
  | Labelled of string
  | Optional of string

type var = {
  id: int;
  mutable link: t option;
}

and t =
  | Int
  | Float
  | Bool
  | String
  | Char
  | Unit
  | Option of t
  | Result of t * t
  | Array of t
  | List of t
  | Seq of t
  | Named of { name: string; arguments: t list }
  | Tuple of t list
  | Arrow of { label: label; lhs: t; rhs: t }
  | Var of var
  | Hole of int

let rec prune = function
  | Var ({ link=Some linked; _ } as var) ->
      let linked = prune linked in
      let () =
        var.link <- Some linked
      in
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

type variance =
  | Covariant
  | Contravariant
  | Invariant

let flip_variance = function
  | Covariant -> Contravariant
  | Contravariant -> Covariant
  | Invariant -> Invariant

let join_variance = fun left right ->
  match (left, right) with
  | (Invariant, _)
  | (_, Invariant) -> Invariant
  | Covariant, Covariant -> Covariant
  | Contravariant, Contravariant -> Contravariant
  | (Covariant, Contravariant)
  | (Contravariant, Covariant) -> Invariant

let rec free_vars = function
  | Int
  | Float
  | Bool
  | String
  | Char
  | Unit
  | Hole _ ->
      []
  | Option element ->
      free_vars (prune element)
  | Result (ok_ty, error_ty) ->
      union (free_vars (prune ok_ty)) (free_vars (prune error_ty))
  | Array element ->
      free_vars (prune element)
  | List element ->
      free_vars (prune element)
  | Seq element ->
      free_vars (prune element)
  | Named { arguments; _ } ->
      List.fold_left (fun acc argument -> union acc (free_vars (prune argument))) [] arguments
  | Tuple members ->
      List.fold_left (fun acc member -> union acc (free_vars (prune member))) [] members
  | Arrow { lhs; rhs; _ } ->
      union (free_vars (prune lhs)) (free_vars (prune rhs))
  | Var var -> (
      match var.link with
      | Some linked -> free_vars linked
      | None -> [ var.id ]
    )

let add_variance = fun acc var_id variance ->
  match List.assoc_opt var_id acc with
  | Some existing -> (var_id, join_variance existing variance) :: List.remove_assoc var_id acc
  | None -> (var_id, variance) :: acc

let merge_variances = fun left right ->
  List.fold_left (fun acc (var_id, variance) -> add_variance acc var_id variance) left right

let rec collect_variances = fun variance ty ->
  match prune ty with
  | Int
  | Float
  | Bool
  | String
  | Char
  | Unit
  | Hole _ ->
      []
  | Option element ->
      collect_variances variance element
  | Result (ok_ty, error_ty) ->
      merge_variances (collect_variances variance ok_ty) (collect_variances variance error_ty)
  | Array element ->
      collect_variances Invariant element
  | List element ->
      collect_variances variance element
  | Seq element ->
      collect_variances variance element
  | Named { arguments; _ } ->
      List.fold_left
        (fun acc argument -> merge_variances acc (collect_variances Invariant argument))
        []
        arguments
  | Tuple members ->
      List.fold_left (fun acc member -> merge_variances acc (collect_variances variance member)) [] members
  | Arrow { lhs; rhs; _ } ->
      merge_variances
        (collect_variances (flip_variance variance) lhs)
        (collect_variances variance rhs)
  | Var var -> (
      match var.link with
      | Some linked -> collect_variances variance linked
      | None -> [ (var.id, variance) ]
    )

let covariant_vars = fun ty ->
  collect_variances Covariant ty |> List.filter_map
    (fun (var_id, variance) ->
      match variance with
      | Covariant -> Some var_id
      | Contravariant
      | Invariant -> None)

let rec occurs = fun needle ty ->
  match prune ty with
  | Int
  | Float
  | Bool
  | String
  | Char
  | Unit
  | Hole _ ->
      false
  | Option element ->
      occurs needle element
  | Result (ok_ty, error_ty) ->
      occurs needle ok_ty || occurs needle error_ty
  | Array element ->
      occurs needle element
  | List element ->
      occurs needle element
  | Seq element ->
      occurs needle element
  | Named { arguments; _ } ->
      List.exists (occurs needle) arguments
  | Tuple members ->
      List.exists (occurs needle) members
  | Arrow { lhs; rhs; _ } ->
      occurs needle lhs || occurs needle rhs
  | Var var -> (
      match var.link with
      | Some linked -> occurs needle linked
      | None -> var.id = needle
    )

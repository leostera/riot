open Std
open Std.Collections
open Ast

module IdentMap = Map.Make (Model.Surface_path)

type binding = {
  scheme: TypeScheme.t;
  ordinal: int;
}

type scope = binding IdentMap.t

type scopes =
  | Root of scope
  | Scope of { root: scope; current: scope; parent: scopes }

type t = { scopes: scopes; next_ordinal: int }

let create () = { scopes = Root IdentMap.empty; next_ordinal = 0 }

let root_scope = function
  | Root scope -> scope
  | Scope { root; _ } -> root

let map_current scopes ~fn =
  match scopes with
  | Root current -> Root (fn current)
  | Scope ({ current; _ } as scope) -> Scope { scope with current = fn current }

let push_scope t =
  let root = root_scope t.scopes in
  let parent = t.scopes in
  let current = IdentMap.empty in
  let scopes = Scope { root; current; parent } in
  { t with scopes }

let pop_scope t =
  match t.scopes with
  | Root _ -> t
  | Scope { parent; _ } -> { t with scopes = parent }

let add_value t ~name ~scheme =
  let binding = { scheme; ordinal = t.next_ordinal } in
  let scopes =
    map_current t.scopes ~fn:(fun scope -> IdentMap.insert scope ~key:name ~value:binding)
  in
  { scopes; next_ordinal = t.next_ordinal + 1 }

let rec get_value_in_scope scopes ~name =
  match scopes with
  | Root scope -> Option.map (IdentMap.get scope ~key:name) ~fn:(fun binding -> binding.scheme)
  | Scope { current; parent; _ } -> (
      match IdentMap.get current ~key:name with
      | Some binding -> Some binding.scheme
      | None -> get_value_in_scope parent ~name
    )

let get_value t ~name = get_value_in_scope t.scopes ~name

let has_value t ~name = Option.is_some (get_value t ~name)

module ExportIter = struct
  type state = (ident * TypeScheme.t) list

  type item = ident * TypeScheme.t

  let next = function
    | [] -> (None, [])
    | item :: rest -> (Some item, rest)

  let size = List.length
end

let exports t =
  root_scope t.scopes
  |> IdentMap.to_list
  |> List.sort ~compare:(fun (_, left) (_, right) -> Int.compare left.ordinal right.ordinal)
  |> List.map ~fn:(fun (name, binding) -> (name, binding.scheme))
  |> Iter.Iterator.make (module ExportIter)

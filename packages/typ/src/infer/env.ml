open Std
open Std.Collections
open Ast

module IdentMap = Map.Make (Model.Surface_path)

type scope = TypeScheme.t IdentMap.t

type scopes =
  | Root of scope
  | Scope of { root: scope; current: scope; parent: scopes }

type t = { scopes: scopes }

let create () = { scopes = Root IdentMap.empty }

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
  { scopes }

let pop_scope t =
  match t.scopes with
  | Root _ -> t
  | Scope { parent; _ } -> { scopes = parent }

let add_value t ~name ~scheme =
  let scopes = map_current t.scopes ~fn:(fun scope -> IdentMap.insert scope ~key:name ~value:scheme) in
  { scopes }

let rec get_value_in_scope scopes ~name =
  match scopes with
  | Root scope -> IdentMap.get scope ~key:name
  | Scope { current; parent; _ } -> (
      match IdentMap.get current ~key:name with
      | Some _ as scheme -> scheme
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
  |> Iter.Iterator.make (module ExportIter)

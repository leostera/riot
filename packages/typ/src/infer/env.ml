open Std
open Std.Collections
open Std.Iter
open Ast

module IdentMap = Map.Make (Model.Surface_path)

(**
   Lexical value scopes inside one module.

   Values are the only bindings here that follow regular expression-level
   lexical scoping. A function body, match branch, or local let can push a new
   scope. Lookups walk from the current scope to the root scope, while exports
   only read the root scope of the module.
*)
module ValueScopes = struct
  type binding = {
    scheme: TypeScheme.t;
    ordinal: int;
  }

  type scope = {
    values: binding IdentMap.t;
  }

  type t =
    | Root of scope
    | Scope of {
        root: scope;
        current: scope;
        parent: t;
      }

  let empty_scope = { values = IdentMap.empty }

  let create () = Root empty_scope

  let root = fun __tmp1 ->
    match __tmp1 with
    | Root scope -> scope
    | Scope { root; _ } -> root

  let map_current t ~fn =
    match t with
    | Root current -> Root (fn current)
    | Scope ({ current; _ } as scope) -> Scope { scope with current = fn current }

  let push t = Scope { root = root t; current = empty_scope; parent = t }

  let pop = fun __tmp1 ->
    match __tmp1 with
    | Root _ as t -> t
    | Scope { parent; _ } -> parent

  let add t ~name ~scheme ~ordinal =
    let binding = { scheme; ordinal } in
    map_current
      t
      ~fn:(fun scope -> { values = IdentMap.insert scope.values ~key:name ~value:binding })

  let rec get_binding t ~name =
    match t with
    | Root scope -> IdentMap.get scope.values ~key:name
    | Scope { current; parent; _ } -> (
        match IdentMap.get current.values ~key:name with
        | Some _ as binding -> binding
        | None -> get_binding parent ~name
      )

  let get t ~name =
    Option.map (get_binding t ~name) ~fn:(fun binding -> binding.scheme)

  let root_bindings t = (root t).values

  let binding_scheme binding = binding.scheme
end

(**
   Type declarations stored in a single module frame.

   Types are not expression-level lexical bindings in this first checker. A type
   declaration is added to the current module table and remains visible for the
   rest of that module. Nested modules get their own table, and unqualified
   lookup through parent modules is handled by `ModuleScopes`.
*)
module TypeScopes = struct
  type binding = {
    declaration: type_declaration;
    ordinal: int;
  }

  type t = binding IdentMap.t

  let empty = IdentMap.empty

  let add t ~name ~declaration ~ordinal =
    IdentMap.insert t ~key:name ~value:{ declaration; ordinal }

  let get_binding t ~name = IdentMap.get t ~key:name

  let get t ~name = Option.map (get_binding t ~name) ~fn:(fun binding -> binding.declaration)

  let has t ~name = Option.is_some (get t ~name)
end

type module_summary = {
  values: ValueScopes.binding IdentMap.t;
  constructors: constructor_binding IdentMap.t;
  record_fields: record_field_binding IdentMap.t;
  types: TypeScopes.binding IdentMap.t;
  modules: module_binding IdentMap.t;
}

and module_binding = {
  summary: module_summary;
  ordinal: int;
}

and opened_module = {
  summary: module_summary;
  ordinal: int;
}

and record_field_info = {
  owner: type_declaration;
  field: record_field_declaration;
}

and record_field_binding = {
  info: record_field_info;
  ordinal: int;
}

and inline_record_field = {
  declaration: record_field_declaration;
  type_: Type.t;
}

and inline_record = {
  owner: type_declaration;
  constructor: type_constructor;
  payload_type: Type.t;
  fields: inline_record_field list;
}

and constructor_arguments =
  | Tuple of Type.t list
  | InlineRecord of inline_record

and constructor_description = {
  name: ident;
  scheme: TypeScheme.t;
  result: Type.t;
  arguments: constructor_arguments;
}

and constructor_binding = {
  description: constructor_description;
  ordinal: int;
}

(**
   Module chain for module-level namespaces.

   Each module frame has:

   - lexical value scopes for values introduced inside that module;
   - a flat constructor table for value constructors;
   - a flat type table for type declarations;
   - a flat nested-module table.

   Entering a module pushes a fresh frame whose lookups fall back to parent
   frames. Leaving a module stores only its direct exports in the parent module,
   so summaries do not copy parent names.
*)
module ModuleScopes = struct
  type frame = {
    name: ident option;
    values: ValueScopes.t;
    record_fields: record_field_binding IdentMap.t;
    constructors: constructor_binding IdentMap.t;
    types: TypeScopes.t;
    modules: module_binding IdentMap.t;
    opens: opened_module list;
  }

  type t =
    | Root of frame
    | Scope of {
        root: frame;
        current: frame;
        parent: t;
      }

  let empty_frame ?name () = {
    name;
    values = ValueScopes.create ();
    record_fields = IdentMap.empty;
    constructors = IdentMap.empty;
    types = TypeScopes.empty;
    modules = IdentMap.empty;
    opens = [];
  }

  let create () = Root (empty_frame ())

  let root = fun __tmp1 ->
    match __tmp1 with
    | Root frame -> frame
    | Scope { root; _ } -> root

  let current = fun __tmp1 ->
    match __tmp1 with
    | Root frame -> frame
    | Scope { current; _ } -> current

  let map_current t ~fn =
    match t with
    | Root current -> Root (fn current)
    | Scope ({ current; _ } as scope) -> Scope { scope with current = fn current }

  let push t ~name = Scope { root = root t; current = empty_frame ~name (); parent = t }

  let summary_of_frame frame = {
    values = ValueScopes.root_bindings frame.values;
    constructors = frame.constructors;
    record_fields = frame.record_fields;
    types = frame.types;
    modules = frame.modules;
  }

  let open_module t ~summary ~ordinal =
    let opened: opened_module = { summary; ordinal } in
    map_current
      t
      ~fn:(fun current -> { current with opens = opened :: current.opens })

  let add_module_to_current t ~name ~summary ~ordinal =
    let binding = { summary; ordinal } in
    map_current
      t
      ~fn:(fun current -> {
        current with
        modules = IdentMap.insert current.modules ~key:name ~value:binding;
      })

  let pop t ~ordinal =
    match t with
    | Root _ -> (t, false)
    | Scope { current; parent; _ } -> (
        match current.name with
        | None -> (parent, false)
        | Some name ->
            let summary = summary_of_frame current in
            (add_module_to_current parent ~name ~summary ~ordinal, true)
      )

  let push_value_scope t =
    map_current t ~fn:(fun current -> { current with values = ValueScopes.push current.values })

  let pop_value_scope t =
    map_current t ~fn:(fun current -> { current with values = ValueScopes.pop current.values })

  let prefer_open direct opened =
    match (direct, opened) with
    | (Some (direct_ordinal, direct), Some (open_ordinal, opened)) ->
        if direct_ordinal > open_ordinal then Some direct else Some opened
    | (Some (_, direct), None) -> Some direct
    | (None, Some (_, opened)) -> Some opened
    | (None, None) -> None

  let rec find_open (opens: opened_module list) ~lookup =
    match opens with
    | [] -> None
    | (opened: opened_module) :: rest -> (
        match lookup opened.summary with
        | Some value -> Some (opened.ordinal, value)
        | None -> find_open rest ~lookup
      )

  let get_open_value t ~name =
    find_open (current t).opens ~lookup:(fun summary ->
      Option.map (IdentMap.get summary.values ~key:name) ~fn:ValueScopes.binding_scheme)

  let get_open_constructor t ~name =
    find_open (current t).opens ~lookup:(fun summary ->
      Option.map (IdentMap.get summary.constructors ~key:name) ~fn:(fun binding -> binding.description))

  let get_open_record_field t ~name =
    find_open (current t).opens ~lookup:(fun summary ->
      Option.map (IdentMap.get summary.record_fields ~key:name) ~fn:(fun binding -> binding.info))

  let get_open_type t ~name =
    find_open (current t).opens ~lookup:(fun summary -> TypeScopes.get summary.types ~name)

  let get_open_module t ~name =
    find_open (current t).opens ~lookup:(fun summary ->
      Option.map (IdentMap.get summary.modules ~key:name) ~fn:(fun binding -> binding.summary))

  let add_value t ~name ~scheme ~ordinal =
    map_current
      t
      ~fn:(fun current -> {
        current with
        values = ValueScopes.add current.values ~name ~scheme ~ordinal;
      })

  let rec get_value t ~name =
    let direct =
      ValueScopes.get_binding (current t).values ~name
      |> Option.map ~fn:(fun (binding: ValueScopes.binding) -> (binding.ordinal, binding.scheme))
    in
    match prefer_open direct (get_open_value t ~name) with
    | Some scheme -> Some scheme
    | None -> (
        match t with
        | Root _ -> None
        | Scope { parent; _ } -> get_value parent ~name
      )

  let add_constructor t ~name ~description ~ordinal =
    let binding = { description; ordinal } in
    map_current
      t
      ~fn:(fun current -> {
        current with
        constructors = IdentMap.insert current.constructors ~key:name ~value:binding;
      })

  let rec get_constructor t ~name =
    let direct =
      IdentMap.get (current t).constructors ~key:name
      |> Option.map ~fn:(fun (binding: constructor_binding) -> (binding.ordinal, binding.description))
    in
    match prefer_open direct (get_open_constructor t ~name) with
    | Some description -> Some description
    | None -> (
        match t with
        | Root _ -> None
        | Scope { parent; _ } -> get_constructor parent ~name
      )

  let add_record_field t ~name ~info ~ordinal =
    let binding = { info; ordinal } in
    map_current
      t
      ~fn:(fun current -> {
        current with
        record_fields = IdentMap.insert current.record_fields ~key:name ~value:binding;
      })

  let rec get_record_field t ~name =
    let direct =
      IdentMap.get (current t).record_fields ~key:name
      |> Option.map ~fn:(fun (binding: record_field_binding) -> (binding.ordinal, binding.info))
    in
    match prefer_open direct (get_open_record_field t ~name) with
    | Some info -> Some info
    | None -> (
        match t with
        | Root _ -> None
        | Scope { parent; _ } -> get_record_field parent ~name
      )

  let add_type t ~name ~declaration ~ordinal =
    map_current
      t
      ~fn:(fun current -> {
        current with
        types = TypeScopes.add current.types ~name ~declaration ~ordinal;
      })

  let rec get_type t ~name =
    let direct =
      TypeScopes.get_binding (current t).types ~name
      |> Option.map ~fn:(fun (binding: TypeScopes.binding) -> (binding.ordinal, binding.declaration))
    in
    match prefer_open direct (get_open_type t ~name) with
    | Some declaration -> Some declaration
    | None -> (
        match t with
        | Root _ -> None
        | Scope { parent; _ } -> get_type parent ~name
      )

  let rec get_module t ~name =
    let direct =
      IdentMap.get (current t).modules ~key:name
      |> Option.map ~fn:(fun (binding: module_binding) -> (binding.ordinal, binding.summary))
    in
    match prefer_open direct (get_open_module t ~name) with
    | Some summary -> Some summary
    | None -> (
        match t with
        | Root _ -> None
        | Scope { parent; _ } -> get_module parent ~name
      )

  let root_value_bindings t = ValueScopes.root_bindings (root t).values

  let root_type_bindings t = (root t).types

  let root_module_bindings t = (root t).modules
end

type t = {
  modules: ModuleScopes.t;
  next_ordinal: int;
}

let create () = { modules = ModuleScopes.create (); next_ordinal = 0 }

let push_scope t = { t with modules = ModuleScopes.push_value_scope t.modules }

let pop_scope t = { t with modules = ModuleScopes.pop_value_scope t.modules }

let push_module t ~name = { t with modules = ModuleScopes.push t.modules ~name }

let pop_module t =
  let (modules, registered) = ModuleScopes.pop t.modules ~ordinal:t.next_ordinal in
  {
    modules;
    next_ordinal =
      if registered then
        t.next_ordinal + 1
      else
        t.next_ordinal;
  }

let add_value t ~name ~scheme = {
  modules = ModuleScopes.add_value t.modules ~name ~scheme ~ordinal:t.next_ordinal;
  next_ordinal = t.next_ordinal + 1;
}

let get_value t ~name = ModuleScopes.get_value t.modules ~name

let has_value t ~name = Option.is_some (get_value t ~name)

let add_constructor t ~name ~description = {
  modules = ModuleScopes.add_constructor t.modules ~name ~description ~ordinal:t.next_ordinal;
  next_ordinal = t.next_ordinal + 1;
}

let get_constructor t ~name = ModuleScopes.get_constructor t.modules ~name

let has_constructor t ~name = Option.is_some (get_constructor t ~name)

let add_record_field t ~name ~info = {
  modules = ModuleScopes.add_record_field t.modules ~name ~info ~ordinal:t.next_ordinal;
  next_ordinal = t.next_ordinal + 1;
}

let get_record_field t ~name = ModuleScopes.get_record_field t.modules ~name

let has_record_field t ~name = Option.is_some (get_record_field t ~name)

let add_type t ~name ~declaration = {
  modules = ModuleScopes.add_type t.modules ~name ~declaration ~ordinal:t.next_ordinal;
  next_ordinal = t.next_ordinal + 1;
}

let get_type t ~name = ModuleScopes.get_type t.modules ~name

let has_type t ~name = Option.is_some (get_type t ~name)

let get_module t ~name = ModuleScopes.get_module t.modules ~name

let has_module t ~name = Option.is_some (get_module t ~name)

let module_get_value (summary: module_summary) ~name =
  Option.map (IdentMap.get summary.values ~key:name) ~fn:ValueScopes.binding_scheme

let module_has_value summary ~name = Option.is_some (module_get_value summary ~name)

let module_get_constructor (summary: module_summary) ~name =
  Option.map (IdentMap.get summary.constructors ~key:name) ~fn:(fun binding -> binding.description)

let module_has_constructor summary ~name = Option.is_some (module_get_constructor summary ~name)

let module_get_record_field (summary: module_summary) ~name =
  Option.map (IdentMap.get summary.record_fields ~key:name) ~fn:(fun binding -> binding.info)

let module_has_record_field summary ~name = Option.is_some (module_get_record_field summary ~name)

let module_get_type (summary: module_summary) ~name = TypeScopes.get summary.types ~name

let module_has_type summary ~name = Option.is_some (module_get_type summary ~name)

let module_get_module (summary: module_summary) ~name =
  Option.map (IdentMap.get summary.modules ~key:name) ~fn:(fun binding -> binding.summary)

let module_has_module summary ~name = Option.is_some (module_get_module summary ~name)

module ExportIter = struct
  type state = (ident * TypeScheme.t) list

  type item = ident * TypeScheme.t

  let next = fun __tmp1 ->
    match __tmp1 with
    | [] -> (None, [])
    | item :: rest -> (Some item, rest)

  let size = List.length
end

let value_bindings bindings =
  bindings
  |> IdentMap.to_list
  |> List.sort
    ~compare:(fun (_, (left: ValueScopes.binding)) (_, (right: ValueScopes.binding)) ->
      Int.compare
        left.ordinal
        right.ordinal)
  |> List.map ~fn:(fun (name, binding) -> (name, ValueScopes.binding_scheme binding))
  |> Iterator.make (module ExportIter)

let exports t = value_bindings (ModuleScopes.root_value_bindings t.modules)

module TypeExportIter = struct
  type state = (ident * type_declaration) list

  type item = ident * type_declaration

  let next = fun __tmp1 ->
    match __tmp1 with
    | [] -> (None, [])
    | item :: rest -> (Some item, rest)

  let size = List.length
end

let type_bindings bindings =
  bindings
  |> IdentMap.to_list
  |> List.sort
    ~compare:(fun (_, (left: TypeScopes.binding)) (_, (right: TypeScopes.binding)) ->
      Int.compare
        left.ordinal
        right.ordinal)
  |> List.map ~fn:(fun (name, (binding: TypeScopes.binding)) -> (name, binding.declaration))
  |> Iterator.make (module TypeExportIter)

let exported_types t = type_bindings (ModuleScopes.root_type_bindings t.modules)

module ModuleExportIter = struct
  type state = (ident * module_summary) list

  type item = ident * module_summary

  let next = fun __tmp1 ->
    match __tmp1 with
    | [] -> (None, [])
    | item :: rest -> (Some item, rest)

  let size = List.length
end

let module_bindings bindings =
  bindings
  |> IdentMap.to_list
  |> List.sort
    ~compare:(fun (_, (left: module_binding)) (_, (right: module_binding)) ->
      Int.compare
        left.ordinal
        right.ordinal)
  |> List.map ~fn:(fun (name, (binding: module_binding)) -> (name, binding.summary))
  |> Iterator.make (module ModuleExportIter)

let exported_modules t = module_bindings (ModuleScopes.root_module_bindings t.modules)

let module_values (summary: module_summary) = value_bindings summary.values

let module_types (summary: module_summary) = type_bindings summary.types

let module_modules (summary: module_summary) = module_bindings summary.modules

let module_path_segment name =
  Model.Surface_path.from_parts [ name ]
  |> Result.expect ~msg:"module path segment must be a non-empty identifier"

let get_module_path t ~path =
  let rec descend summary path =
    match path with
    | [] -> Some summary
    | name :: rest -> (
        match module_get_module summary ~name with
        | Some nested -> descend nested rest
        | None -> None
      )
  in
  match path with
  | [] -> None
  | root :: rest -> (
      match ModuleScopes.get_module t.modules ~name:root with
      | Some summary -> descend summary rest
      | None -> None
    )

let open_module t ~path =
  let path =
    Model.Surface_path.to_segments path
    |> List.map ~fn:module_path_segment
  in
  match get_module_path t ~path with
  | None -> None
  | Some summary ->
      Some {
        modules = ModuleScopes.open_module t.modules ~summary ~ordinal:t.next_ordinal;
        next_ordinal = t.next_ordinal + 1;
      }

let add_module_alias t ~name ~target =
  let path =
    Model.Surface_path.to_segments target
    |> List.map ~fn:module_path_segment
  in
  match get_module_path t ~path with
  | None -> None
  | Some summary ->
      Some {
        modules = ModuleScopes.add_module_to_current
          t.modules
          ~name
          ~summary
          ~ordinal:t.next_ordinal;
        next_ordinal = t.next_ordinal + 1;
      }

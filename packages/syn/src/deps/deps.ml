open Std
open Std.Collections
open Std.Data

module Iterator = Iter.Iterator

module Env = struct
  module Names = struct
    type t = string list

    let empty = []

    let singleton = fun name -> [ name ]

    let union = fun left right ->
      List.unique
        (List.sort (left @ right) ~compare:String.compare)
        ~compare:String.compare

    let elements = fun names -> names
  end

  type node =
    | Node of Names.t * t

  and t = (string * node) list

  let empty = []

  let open_fallback_key = "\000open_fallback"

  let bound = Node (Names.empty, [])

  let singleton_name = Names.singleton

  let make_leaf name = Node (Names.singleton name, [])

  let make_leaf_from_names names = Node (names, [])

  let make_node map = Node (Names.empty, map)

  let rec remove = fun name ->
    fun __tmp1 ->
      match __tmp1 with
      | [] -> []
      | (key, _) :: rest when key = name -> rest
      | entry :: rest -> entry :: remove name rest

  let add = fun name node env -> (name, node) :: remove name env

  let merge = fun left right ->
    List.fold_left
      right
      ~init:left
      ~fn:(fun env (name, node) ->
        add name node env)

  let rec rebind = fun free_names ->
    fun (Node (_, children)) ->
      Node (
        free_names,
        List.map children ~fn:(fun (name, child) -> (name, rebind free_names child))
      )

  let rebind_exports = fun free_names exports ->
    List.map
      exports
      ~fn:(fun (name, node) -> (name, rebind free_names node))

  let rec add_path = fun env ~path ~free_names ->
    match path with
    | [] -> env
    | segment :: rest ->
        let existing =
          match List.find env ~fn:(fun (name, _) -> String.equal name segment) with
          | Some (_, node) -> node
          | None -> Node (Names.empty, [])
        in
        let Node (free, children) = existing in
        let updated_children =
          match rest with
          | [] -> children
          | _ -> add_path children ~path:rest ~free_names
        in
        add segment (Node (Names.union free free_names, updated_children)) env

  let rec add_binding = fun env ~path ~free_names ~exports ->
    match path with
    | [] -> env
    | [ segment ] ->
        let existing =
          match List.find env ~fn:(fun (name, _) -> String.equal name segment) with
          | Some (_, node) -> node
          | None -> Node (Names.empty, [])
        in
        let Node (free, children) = existing in
        let merged_children = merge children (rebind_exports free_names exports) in
        add segment (Node (Names.union free free_names, merged_children)) env
    | segment :: rest ->
        let existing =
          match List.find env ~fn:(fun (name, _) -> String.equal name segment) with
          | Some (_, node) -> node
          | None -> Node (Names.empty, [])
        in
        let Node (free, children) = existing in
        let updated_children = add_binding children ~path:rest ~free_names ~exports in
        add segment (Node (free, updated_children)) env

  let rec add_scoped_binding = fun env ~path ~free_names ~exports ->
    match path with
    | [] -> env
    | [ segment ] ->
        let existing =
          match List.find env ~fn:(fun (name, _) -> String.equal name segment) with
          | Some (_, node) -> node
          | None -> Node (Names.empty, [])
        in
        let Node (free, children) = existing in
        let merged_children = merge children exports in
        add segment (Node (Names.union free free_names, merged_children)) env
    | segment :: rest ->
        let existing =
          match List.find env ~fn:(fun (name, _) -> String.equal name segment) with
          | Some (_, node) -> node
          | None -> Node (Names.empty, [])
        in
        let Node (free, children) = existing in
        let updated_children = add_scoped_binding children ~path:rest ~free_names ~exports in
        add segment (Node (free, updated_children)) env

  let top_free = fun (Node (free, _)) -> free

  let children = fun (Node (_, children)) -> children

  let rec collect_free = fun (Node (free, children)) ->
    List.fold_left
      children
      ~init:free
      ~fn:(fun acc (_, child) -> Names.union acc (collect_free child))

  let merge_children env node = merge env (children node)

  let find = fun name env ->
    List.find env ~fn:(fun (key, _) -> String.equal key name)
    |> Option.map ~fn:(fun (_, value) -> value)

  let open_fallback_free = fun env ->
    match find open_fallback_key env with
    | Some (Node (free, _)) -> Some free
    | None -> None

  let add_open_fallback = fun env ~free_names ->
    let existing =
      match open_fallback_free env with
      | Some names -> names
      | None -> Names.empty
    in
    add open_fallback_key (Node (Names.union existing free_names, [])) env

  let has_children = fun (Node (_, children)) -> not (List.is_empty children)

  let rec lookup_free = fun ~use_open_fallback segments env ->
    match segments with
    | [] -> None
    | segment :: rest ->
        match find segment env with
        | None ->
            if use_open_fallback then
              open_fallback_free env
            else
              None
        | Some (Node (free, children)) -> (
            match rest with
            | [] -> Some free
            | _ -> (
                match lookup_free ~use_open_fallback rest children with
                | Some child_free -> Some (Names.union free child_free)
                | None -> Some free
              )
          )

  let rec lookup_map segments env =
    match segments with
    | [] -> None
    | [ segment ] -> find segment env
    | segment :: rest ->
        match find segment env with
        | None -> None
        | Some (Node (_, children)) -> lookup_map rest children

  let open_path = fun env ~path ->
    match lookup_map path env with
    | Some node -> merge_children env node
    | None -> env
end

module DepSet = struct
  type t = string HashSet.t

  let empty = fun () -> HashSet.create ()

  let add = fun deps name ->
    let _ = HashSet.insert deps ~value:name in
    deps

  let add_names = fun deps names ->
    List.for_each
      names
      ~fn:(fun name ->
        let _ = HashSet.insert deps ~value:name in
        ());
    deps

  let elements = fun deps ->
    HashSet.to_list deps
    |> List.sort ~compare:String.compare
end

type t = {
  modules: string list;
  env: Env.t;
  exports: Env.t;
}

type parse_error =
  | Parse_diagnostics of Diagnostic.t list

let ( let* ) = fun result f ->
  match result with
  | Ok value -> f value
  | Error _ as error -> error

let modules = fun t -> t.modules

let env = fun t -> t.env

let exports = fun t -> t.exports

let to_json = fun t ->
  Json.Object [ ("modules", Json.Array (List.map t.modules ~fn:(fun name -> Json.String name))); ]

let drop_last = fun __tmp1 ->
  match __tmp1 with
  | [] -> []
  | [ _ ] -> []
  | items ->
      let rec loop acc = fun __tmp1 ->
        match __tmp1 with
        | []
        | [ _ ] -> List.reverse acc
        | head :: tail -> loop (head :: acc) tail
      in
      loop [] items

let is_uppercase_ascii = fun ch -> ch >= 'A' && ch <= 'Z'

let is_module_head = fun segment ->
  String.length segment > 0 && is_uppercase_ascii (String.get_unchecked segment ~at:0)

let add_names = DepSet.add_names

let add_path = fun env deps segments ->
  match segments with
  | [] -> deps
  | head :: _ when is_module_head head ->
      let names =
        match Env.lookup_free ~use_open_fallback:true segments env with
        | Some names -> names
        | None -> Env.singleton_name head
      in
      add_names deps names
  | _ -> deps

module Ast_deps = struct
  module A = Ast

  let node_kind = A.Node.kind

  let token_kind = A.Token.kind

  let child_node_at = fun (node: A.Node.t) index ->
    match A.Node.child_at node index with
    | Some (Syntax_tree.Node id) -> Some ({ tree = node.tree; id }: A.Node.t)
    | Some (Syntax_tree.Token _)
    | Some (Syntax_tree.Missing _)
    | None -> None

  let child_token_at = fun (node: A.Node.t) index ->
    match A.Node.child_at node index with
    | Some (Syntax_tree.Token id) -> Some ({ tree = node.tree; id }: A.Token.t)
    | Some (Syntax_tree.Node _)
    | Some (Syntax_tree.Missing _)
    | None -> None

  let child_token_kind_is = fun node index kind ->
    match child_token_at node index with
    | Some token -> Syntax_kind.(token_kind token = kind)
    | None -> false

  let node_kind_is = fun node kind -> Syntax_kind.(node_kind node = kind)

  let is_module_expr_kind = fun __tmp1 ->
    match __tmp1 with
    | Syntax_kind.MODULE_EXPR
    | Syntax_kind.PATH_MODULE_EXPR
    | Syntax_kind.STRUCT_MODULE_EXPR
    | Syntax_kind.FUNCTOR_MODULE_EXPR
    | Syntax_kind.APPLY_MODULE_EXPR
    | Syntax_kind.CONSTRAINT_MODULE_EXPR
    | Syntax_kind.PAREN_MODULE_EXPR
    | Syntax_kind.OPAQUE_MODULE_EXPR -> true
    | _ -> false

  let is_module_type_kind = fun __tmp1 ->
    match __tmp1 with
    | Syntax_kind.MODULE_TYPE_EXPR
    | Syntax_kind.PATH_MODULE_TYPE
    | Syntax_kind.SIGNATURE_MODULE_TYPE
    | Syntax_kind.TYPEOF_MODULE_TYPE
    | Syntax_kind.FUNCTOR_MODULE_TYPE
    | Syntax_kind.WITH_MODULE_TYPE
    | Syntax_kind.PAREN_MODULE_TYPE
    | Syntax_kind.OPAQUE_MODULE_TYPE -> true
    | _ -> false

  let vector_to_list = fun vector ->
    Vector.to_array vector
    |> Array.to_list

  let path_segments = fun node ->
    let segments = Vector.with_capacity ~size:(A.Node.child_count node) in
    A.Node.fold_token
      node
      ~init:()
      ~fn:(fun token () ->
        if Syntax_kind.(token_kind token = IDENT) then
          Vector.push segments ~value:(A.Token.text token);
        A.Continue ());
    vector_to_list segments

  let ast_ident_segments = fun ident ->
    let segments = Vector.with_capacity ~size:4 in
    A.Ident.fold_segment
      ident
      ~init:()
      ~fn:(fun token () ->
        Vector.push segments ~value:(A.Token.text token);
        A.Continue ());
    vector_to_list segments

  let add_parent_segments = fun env deps segments ->
    match drop_last segments with
    | head :: _ as parent when is_module_head head -> add_path env deps parent
    | _ -> deps

  let add_module_segments = fun env deps segments ->
    match segments with
    | head :: _ when is_module_head head -> add_path env deps segments
    | _ -> deps

  let module_alias = fun env deps segments ->
    let deps = add_module_segments env deps segments in
    let resolved_free_names = Env.lookup_free ~use_open_fallback:true segments env in
    let binding =
      match Env.lookup_map segments env with
      | Some node -> (
          match Env.top_free node with
          | [] -> node
          | free_names -> Env.rebind free_names node
        )
      | None -> (
          match (segments, resolved_free_names) with
          | (_, Some names) -> Env.make_leaf_from_names names
          | ([ name ], None) -> Env.make_leaf name
          | _ -> Env.bound
        )
    in
    (deps, binding)

  let open_alias = fun ?(fallback = false) env deps segments ->
    let binding_known = Option.is_some (Env.lookup_map segments env) in
    let binding_resolves = Option.is_some (Env.lookup_free ~use_open_fallback:true segments env) in
    let (deps, binding) = module_alias env deps segments in
    let deps = add_names deps (Env.top_free binding) in
    let env = Env.merge_children env binding in
    let env =
      if fallback && not (Env.has_children binding) && (binding_known || binding_resolves) then
        match segments with
        | head :: _ when is_module_head head ->
            let free_names =
              match Env.top_free binding with
              | [] -> [ head ]
              | names -> names
            in
            Env.add_open_fallback env ~free_names
        | _ -> env
      else
        env
    in
    (deps, env)

  let direct_path_between = fun node start stop ->
    let rec loop index expect_ident acc =
      if index >= stop then
        if not (List.is_empty acc) && not expect_ident then
          Some (List.reverse acc)
        else
          None
      else
        match child_token_at node index with
        | Some token when expect_ident && Syntax_kind.(token_kind token = IDENT) ->
            loop (index + 1) false (A.Token.text token :: acc)
        | Some token when (not expect_ident) && Syntax_kind.(token_kind token = DOT) ->
            loop (index + 1) true acc
        | _ -> None
    in
    loop start true []

  let collect_direct_module_refs_between = fun env deps node start stop ->
    let rec collect_path index acc =
      if index >= stop then
        (index, List.reverse acc)
      else
        match child_token_at node index with
        | Some token when Syntax_kind.(token_kind token = IDENT) ->
            collect_path (index + 1) (A.Token.text token :: acc)
        | Some token when Syntax_kind.(token_kind token = DOT) -> collect_path (index + 1) acc
        | _ -> (index, List.reverse acc)
    in
    let rec scan index deps =
      if index >= stop then
        deps
      else
        match child_token_at node index with
        | Some token when Syntax_kind.(token_kind token = IDENT) ->
            let (next, segments) = collect_path index [] in
            scan next (add_module_segments env deps segments)
        | _ -> scan (index + 1) deps
    in
    scan start deps

  let collect_direct_module_accesses_between = fun env deps node start stop ->
    let rec collect_path index acc saw_dot =
      if index >= stop then
        (index, List.reverse acc, saw_dot)
      else
        match child_token_at node index with
        | Some token when Syntax_kind.(token_kind token = IDENT) ->
            collect_path (index + 1) (A.Token.text token :: acc) saw_dot
        | Some token when Syntax_kind.(token_kind token = DOT) -> collect_path (index + 1) acc true
        | _ -> (index, List.reverse acc, saw_dot)
    in
    let rec scan index deps =
      if index >= stop then
        deps
      else
        match child_token_at node index with
        | Some token when Syntax_kind.(token_kind token = IDENT) ->
            let (next, segments, saw_dot) = collect_path index [] false in
            let deps =
              match segments with
              | head :: _ when saw_dot && is_module_head head ->
                  add_parent_segments env deps segments
              | _ -> deps
            in
            scan next deps
        | _ -> scan (index + 1) deps
    in
    scan start deps

  let rec direct_module_binding_between env deps node start stop =
    match direct_path_between node start stop with
    | Some segments -> Ok (module_alias env deps segments)
    | None -> (
        match child_token_at node start with
        | Some token when Syntax_kind.(token_kind token = STRUCT_KW) ->
            direct_struct_binding_between env deps node (start + 1) stop
        | _ ->
            let deps = collect_direct_module_refs_between env deps node start stop in
            Ok (deps, Env.bound)
      )

  and direct_struct_binding_between env deps node start stop =
    let deps = collect_direct_module_accesses_between env deps node start stop in
    let rec find_token index limit kind =
      if index >= limit then
        None
      else if child_token_kind_is node index kind then
        Some index
      else
        find_token (index + 1) limit kind
    in
    let rec member_stop index =
      if
        index >= stop
        || child_token_kind_is node index Syntax_kind.MODULE_KW
        || child_token_kind_is node index Syntax_kind.END_KW
      then
        index
      else
        member_stop (index + 1)
    in
    let rec scan index deps bindings =
      if index >= stop then
        Ok (deps, Env.make_node bindings)
      else if child_token_kind_is node index Syntax_kind.MODULE_KW then
        let name =
          match child_token_at node (index + 1) with
          | Some token when Syntax_kind.(token_kind token = IDENT) -> Some (A.Token.text token)
          | _ -> None
        in
        let member_stop =
          match find_token (index + 1) stop Syntax_kind.EQ with
          | Some eq_index -> member_stop (eq_index + 1)
          | None -> member_stop (index + 1)
        in
        let* (deps, binding) =
          match find_token (index + 1) member_stop Syntax_kind.EQ with
          | Some eq_index -> direct_module_binding_between env deps node (eq_index + 1) member_stop
          | None -> Ok (deps, Env.bound)
        in
        let bindings =
          match name with
          | Some name -> Env.add name binding bindings
          | None -> bindings
        in
        scan member_stop deps bindings
      else
        scan (index + 1) deps bindings
    in
    scan start deps Env.empty

  let first_child_node_matching = fun node ~matches ->
    A.Node.fold_child_node
      node
      ~init:None
      ~fn:(fun child _ ->
        if matches (node_kind child) then
          A.Return (Some child)
        else
          A.Continue None)

  let rec unwrap_module_expr = fun node ->
    match node_kind node with
    | Syntax_kind.MODULE_EXPR -> (
        match first_child_node_matching node ~matches:is_module_expr_kind with
        | Some child -> unwrap_module_expr child
        | None -> node
      )
    | _ -> node

  let rec unwrap_module_type = fun node ->
    match node_kind node with
    | Syntax_kind.MODULE_TYPE_EXPR -> (
        match first_child_node_matching node ~matches:is_module_type_kind with
        | Some child -> unwrap_module_type child
        | None -> node
      )
    | _ -> node

  let first_direct_child_node = fun node kind ->
    first_child_node_matching
      node
      ~matches:(fun child_kind -> Syntax_kind.(child_kind = kind))

  let fold_child_nodes = fun node init fn ->
    A.Node.fold_child_node
      node
      ~init
      ~fn:(fun child acc -> A.Continue (fn acc child))

  let collect_child_nodes = fun collect env deps node ->
    fold_child_nodes
      node
      (Ok deps)
      (fun acc child ->
        let* deps = acc in
        collect env deps child)

  let collect_direct_type_payload_paths = fun env deps node ->
    let count = A.Node.child_count node in
    let rec collect_path index acc =
      if index >= count then
        (index, List.reverse acc)
      else
        match child_token_at node index with
        | Some token when Syntax_kind.(token_kind token = IDENT) ->
            collect_path (index + 1) (A.Token.text token :: acc)
        | Some token when Syntax_kind.(token_kind token = DOT) -> collect_path (index + 1) acc
        | _ -> (index, List.reverse acc)
    in
    let rec scan active index deps =
      if index >= count then
        deps
      else
        match child_token_at node index with
        | Some token when Syntax_kind.(token_kind token = OF_KW || token_kind token = COLON) ->
            scan true (index + 1) deps
        | Some token when Syntax_kind.(token_kind token = PIPE || token_kind token = AND_KW) ->
            scan false (index + 1) deps
        | Some token when active && Syntax_kind.(token_kind token = IDENT) ->
            let (next, segments) = collect_path index [] in
            scan
              active
              next
              (add_parent_segments env deps segments)
        | _ -> scan active (index + 1) deps
    in
    scan false 0 deps

  let collect_direct_type_extension_path = fun env deps node ->
    let count = A.Node.child_count node in
    let rec find_plus index =
      if index >= count then
        None
      else if child_token_kind_is node index Syntax_kind.PLUS then
        Some index
      else
        find_plus (index + 1)
    in
    let rec collect_before_plus index plus_index acc =
      if index >= plus_index then
        add_parent_segments env deps (List.reverse acc)
      else
        match child_token_at node index with
        | Some token when Syntax_kind.(token_kind token = IDENT) ->
            collect_before_plus (index + 1) plus_index (A.Token.text token :: acc)
        | Some token when Syntax_kind.(token_kind token = DOT) ->
            collect_before_plus (index + 1) plus_index acc
        | _ -> collect_before_plus (index + 1) plus_index []
    in
    match find_plus 0 with
    | Some plus_index -> collect_before_plus 0 plus_index []
    | None -> deps

  let rec bind_pattern_modules env node =
    match node_kind node with
    | Syntax_kind.FIRST_CLASS_MODULE_PATTERN ->
        let count = A.Node.child_count node in
        let rec find_binding index seen_module =
          if index >= count then
            None
          else
            match child_token_at node index with
            | Some token when Syntax_kind.(token_kind token = MODULE_KW) ->
                find_binding (index + 1) true
            | Some token when seen_module && Syntax_kind.(token_kind token = IDENT) ->
                Some (A.Token.text token)
            | Some token when seen_module && Syntax_kind.(token_kind token = UNDERSCORE) -> None
            | _ -> find_binding (index + 1) seen_module
        in
        (
          match find_binding 0 false with
          | Some name -> Env.add name Env.bound env
          | None -> env
        )
    | _ -> fold_child_nodes node env (fun env child -> bind_pattern_modules env child)

  let rec collect_node env deps node =
    match node_kind node with
    | Syntax_kind.PATH_EXPR
    | Syntax_kind.PATH_PATTERN
    | Syntax_kind.PATH_TYPE -> Ok (add_parent_segments env deps (path_segments node))
    | Syntax_kind.PATH_MODULE_EXPR -> Ok (add_module_segments env deps (path_segments node))
    | Syntax_kind.PATH_MODULE_TYPE -> Ok (add_parent_segments env deps (path_segments node))
    | Syntax_kind.FIELD_ACCESS_EXPR ->
        let segments = path_segments node in
        let deps =
          match segments with
          | head :: _ when is_module_head head -> add_parent_segments env deps segments
          | _ -> deps
        in
        Ok deps
    | Syntax_kind.LOCAL_OPEN_EXPR
    | Syntax_kind.LOCAL_OPEN_PATTERN -> collect_local_open env deps node
    | Syntax_kind.LET_BINDING -> collect_let_binding env deps node
    | Syntax_kind.LET_MODULE_EXPR -> collect_let_module_expr env deps node
    | Syntax_kind.FUN_EXPR -> collect_fun_expr env deps node
    | Syntax_kind.MATCH_CASE -> collect_match_case env deps node
    | Syntax_kind.TYPE_DECL ->
        let deps = collect_direct_type_extension_path env deps node in
        let deps = collect_direct_type_payload_paths env deps node in
        collect_child_nodes collect_node env deps node
    | Syntax_kind.OPAQUE_TYPE ->
        Ok (collect_direct_module_accesses_between env deps node 0 (A.Node.child_count node))
    | Syntax_kind.STRUCTURE_ITEM ->
        let* (deps, _, _) = collect_structure_item env deps Env.empty node in
        Ok deps
    | Syntax_kind.SIGNATURE_ITEM ->
        let* (deps, _, _) = collect_signature_item env deps Env.empty node in
        Ok deps
    | Syntax_kind.MODULE_DECL ->
        let* (deps, _, _) = collect_module_decl env deps Env.empty node in
        Ok deps
    | Syntax_kind.MODULE_TYPE_DECL ->
        let* deps = collect_module_type_decl env deps node in
        Ok deps
    | _ -> collect_child_nodes collect_node env deps node

  and collect_local_open env deps node =
    match A.cast_result_to_option (A.Expr.cast node) with
    | Some expr -> (
        match A.cast_result_to_option (A.LocalOpenExpr.cast expr) with
        | Some local_open -> (
            match A.LocalOpenExpr.view local_open with
            | A.LocalOpenExpr.LetOpen { module_ident; body; _ }
            | A.LocalOpenExpr.Delimited { module_ident; body; _ } ->
                let (deps, env) =
                  match ast_ident_segments module_ident with
                  | head :: _ as segments when is_module_head head ->
                      open_alias ~fallback:true env deps segments
                  | _ -> (deps, env)
                in
                collect_node env deps (A.Expr.as_node body)
            | A.LocalOpenExpr.Unknown _ -> collect_local_open_fallback env deps node
          )
        | None -> collect_local_open_fallback env deps node
      )
    | None -> collect_local_open_fallback env deps node

  and collect_local_open_fallback env deps node =
    let segments = path_segments node in
    let (deps, env) =
      match segments with
      | head :: _ when is_module_head head -> open_alias ~fallback:true env deps segments
      | _ -> (deps, env)
    in
    collect_child_nodes collect_node env deps node

  and collect_let_binding env deps node =
    match A.cast_result_to_option (A.LetBinding.cast node) with
    | None -> collect_child_nodes collect_node env deps node
    | Some binding ->
        let parameters = Vector.with_capacity ~size:4 in
        A.LetBinding.fold_parameter
          binding
          ~init:()
          ~fn:(fun parameter () ->
            Vector.push parameters ~value:parameter;
            A.Continue ());
        let* deps =
          match A.LetBinding.pattern binding with
          | Some pattern -> collect_node env deps (A.Pattern.as_node pattern)
          | None -> Ok deps
        in
        let* deps =
          Vector.iter parameters
          |> Iterator.fold
            ~init:(Ok deps)
            ~fn:(fun (parameter: A.Parameter.t) acc ->
              let* deps = acc in
              collect_node env deps (A.Parameter.as_node parameter))
        in
        let* deps =
          match A.LetBinding.type_annotation binding with
          | Some annotation -> collect_node env deps (A.TypeExpr.as_node annotation)
          | None -> Ok deps
        in
        let env =
          match A.LetBinding.pattern binding with
          | Some pattern -> bind_pattern_modules env (A.Pattern.as_node pattern)
          | None -> env
        in
        let env =
          Vector.iter parameters
          |> Iterator.fold
            ~init:env
            ~fn:(fun (parameter: A.Parameter.t) env ->
              bind_pattern_modules
                env
                (A.Parameter.as_node parameter))
        in
        (
          match A.LetBinding.body binding with
          | Some body -> collect_node env deps (A.Expr.as_node body)
          | None -> Ok deps
        )

  and collect_match_case env deps node =
    match A.cast_result_to_option (A.MatchCase.cast node) with
    | None -> collect_child_nodes collect_node env deps node
    | Some match_case -> (
        match A.MatchCase.view match_case with
        | A.MatchCase.Case { pattern; guard; body } ->
            let* deps = collect_node env deps (A.Pattern.as_node pattern) in
            let env = bind_pattern_modules env (A.Pattern.as_node pattern) in
            let* deps =
              match guard with
              | Some guard -> collect_node env deps (A.Expr.as_node guard)
              | None -> Ok deps
            in
            collect_node env deps (A.Expr.as_node body)
        | A.MatchCase.Unknown node -> collect_child_nodes collect_node env deps node
      )

  and collect_let_module_expr env deps node =
    let count = A.Node.child_count node in
    let rec find_name index =
      if index >= count then
        None
      else
        match child_token_at node index with
        | Some token when Syntax_kind.(token_kind token = IDENT) -> Some (A.Token.text token)
        | _ -> find_name (index + 1)
    in
    let rec find_node_after index ~matches =
      if index >= count then
        None
      else
        match child_node_at node index with
        | Some child when matches (node_kind child) -> Some child
        | _ -> find_node_after (index + 1) ~matches
    in
    let rec find_token index kind =
      if index >= count then
        None
      else if child_token_kind_is node index kind then
        Some index
      else
        find_token (index + 1) kind
    in
    let eq_index = find_token 0 Syntax_kind.EQ in
    let in_index = find_token 0 Syntax_kind.IN_KW in
    let body_expr =
      match in_index with
      | Some in_index ->
          find_node_after (in_index + 1) ~matches:(fun kind -> not Syntax_kind.(kind = MODULE_EXPR))
      | None -> None
    in
    let module_expr =
      match (eq_index, in_index) with
      | (Some eq_index, Some in_index) ->
          find_node_between node (eq_index + 1) in_index ~matches:is_module_expr_kind
      | _ -> find_node_after 0 ~matches:is_module_expr_kind
    in
    let* (deps, binding) =
      match module_expr with
      | Some module_expr -> module_binding env deps module_expr
      | None -> (
          match (eq_index, in_index) with
          | (Some eq_index, Some in_index) ->
              direct_module_binding_between env deps node (eq_index + 1) in_index
          | _ -> Ok (deps, Env.bound)
        )
    in
    let env =
      match find_name 0 with
      | Some name -> Env.add name binding env
      | None -> env
    in
    match body_expr with
    | Some body -> collect_node env deps body
    | None -> Ok deps

  and collect_fun_expr env deps node =
    let count = A.Node.child_count node in
    let rec find_arrow index =
      if index >= count then
        None
      else if child_token_kind_is node index Syntax_kind.ARROW then
        Some index
      else
        find_arrow (index + 1)
    in
    let rec collect_patterns index stop deps =
      if index >= stop then
        Ok deps
      else
        match child_node_at node index with
        | Some child ->
            let* deps = collect_node env deps child in
            collect_patterns (index + 1) stop deps
        | None -> collect_patterns (index + 1) stop deps
    in
    let rec bind_patterns index stop env =
      if index >= stop then
        env
      else
        match child_node_at node index with
        | Some child -> bind_patterns
          (index + 1)
          stop
          (bind_pattern_modules env child)
        | None -> bind_patterns (index + 1) stop env
    in
    match find_arrow 0 with
    | Some arrow_index ->
        let* deps = collect_patterns 0 arrow_index deps in
        let env = bind_patterns 0 arrow_index env in
        (
          match find_node_between node (arrow_index + 1) count ~matches:(fun _ -> true) with
          | Some body -> collect_node env deps body
          | None -> Ok deps
        )
    | None -> collect_child_nodes collect_node env deps node

  and collect_structure_items_in env deps node = collect_structure_binding env deps node

  and collect_signature_items_in env deps node = collect_signature_binding env deps node

  and module_binding env deps node =
    let node = unwrap_module_expr node in
    match node_kind node with
    | Syntax_kind.PATH_MODULE_EXPR -> Ok (module_alias env deps (path_segments node))
    | Syntax_kind.STRUCT_MODULE_EXPR ->
        let* (deps, _, bindings) = collect_structure_items_in env deps node in
        Ok (deps, Env.make_node bindings)
    | Syntax_kind.CONSTRAINT_MODULE_EXPR
    | Syntax_kind.PAREN_MODULE_EXPR -> (
        match first_child_node_matching node ~matches:is_module_expr_kind with
        | Some inner -> module_binding env deps inner
        | None ->
            let* deps = collect_node env deps node in
            Ok (deps, Env.bound)
      )
    | Syntax_kind.FUNCTOR_MODULE_EXPR
    | Syntax_kind.APPLY_MODULE_EXPR
    | Syntax_kind.OPAQUE_MODULE_EXPR ->
        let* deps = collect_node env deps node in
        Ok (deps, Env.bound)
    | _ ->
        let* deps = collect_node env deps node in
        Ok (deps, Env.bound)

  and collect_module_expression env deps node =
    let* (deps, _) = module_binding env deps node in
    Ok deps

  and module_type_binding env deps node =
    let node = unwrap_module_type node in
    match node_kind node with
    | Syntax_kind.PATH_MODULE_TYPE ->
        Ok (add_parent_segments env deps (path_segments node), Env.bound)
    | Syntax_kind.SIGNATURE_MODULE_TYPE ->
        let* (deps, _, bindings) = collect_signature_items_in env deps node in
        Ok (deps, Env.make_node bindings)
    | Syntax_kind.PAREN_MODULE_TYPE -> (
        match first_child_node_matching node ~matches:is_module_type_kind with
        | Some inner -> module_type_binding env deps inner
        | None -> Ok (deps, Env.bound)
      )
    | Syntax_kind.TYPEOF_MODULE_TYPE -> (
        match first_child_node_matching
          node
          ~matches:(fun kind -> is_module_expr_kind kind || Syntax_kind.(kind = MODULE_EXPR)) with
        | Some module_expr -> module_binding env deps module_expr
        | None -> Ok (deps, Env.bound)
      )
    | Syntax_kind.FUNCTOR_MODULE_TYPE
    | Syntax_kind.WITH_MODULE_TYPE
    | Syntax_kind.OPAQUE_MODULE_TYPE ->
        let* deps = collect_node env deps node in
        Ok (deps, Env.bound)
    | _ ->
        let* deps = collect_node env deps node in
        Ok (deps, Env.bound)

  and collect_module_type env deps node =
    let* (deps, _) = module_type_binding env deps node in
    Ok deps

  and collect_functor_head env deps member =
    let module Member = A.ModuleDeclaration.Member in
    let stop = Member.child_count member in
    let rec path_after_colon index acc =
      if index >= stop || Member.child_token_kind_is member index Syntax_kind.RPAREN then
        add_parent_segments env deps (List.reverse acc)
      else
        match Member.child_token_at member index with
        | Some token when Syntax_kind.(token_kind token = IDENT) ->
            path_after_colon (index + 1) (A.Token.text token :: acc)
        | _ -> path_after_colon (index + 1) acc
    in
    let rec scan index env deps =
      if index >= stop then
        (deps, env)
      else if Member.child_token_kind_is member index Syntax_kind.LPAREN then
        let name =
          match Member.child_token_at member (index + 1) with
          | Some token when Syntax_kind.(token_kind token = IDENT) -> Some (A.Token.text token)
          | _ -> None
        in
        let deps =
          let rec find_colon i =
            if i >= stop || Member.child_token_kind_is member i Syntax_kind.RPAREN then
              deps
            else if Member.child_token_kind_is member i Syntax_kind.COLON then
              path_after_colon (i + 1) []
            else
              find_colon (i + 1)
          in
          find_colon (index + 1)
        in
        let env =
          match name with
          | Some name -> Env.add name Env.bound env
          | None -> env
        in
        scan (index + 1) env deps
      else
        scan (index + 1) env deps
    in
    scan 0 env deps

  and module_member_name member =
    match A.ModuleDeclaration.Member.name member with
    | Some ident -> Some (A.Ident.text ident)
    | None -> None

  and find_token_between node start stop kind =
    let rec loop index =
      if index >= stop then
        None
      else if child_token_kind_is node index kind then
        Some index
      else
        loop (index + 1)
    in
    loop start

  and find_node_between node start stop ~matches =
    let rec loop index =
      if index >= stop then
        None
      else
        match child_node_at node index with
        | Some child when matches (node_kind child) -> Some child
        | _ -> loop (index + 1)
    in
    loop start

  and prebind_module_decl_group env bindings node =
    A.ModuleDeclaration.fold_members
      node
      (env, bindings)
      (fun (env, bindings) member ->
        match module_member_name member with
        | Some name -> (Env.add name Env.bound env, Env.add name Env.bound bindings)
        | None -> (env, bindings))

  and collect_module_member_rhs env deps member =
    let (deps, env) = collect_functor_head env deps member in
    let* deps =
      match A.ModuleDeclaration.Member.module_type member with
      | Some module_type -> collect_module_type env deps module_type
      | None -> Ok deps
    in
    match A.ModuleDeclaration.Member.module_expr member with
    | Some module_expr -> collect_module_expression env deps module_expr
    | None -> Ok deps

  and module_member_binding env deps member =
    let (deps, env) = collect_functor_head env deps member in
    let* deps =
      match A.ModuleDeclaration.Member.module_type member with
      | Some module_type -> collect_module_type env deps module_type
      | None -> Ok deps
    in
    match A.ModuleDeclaration.Member.module_expr member with
    | Some module_expr -> module_binding env deps module_expr
    | None -> Ok (deps, Env.bound)

  and collect_module_decl env deps bindings node =
    match A.cast_result_to_option (A.ModuleDeclaration.cast node) with
    | None ->
        let* deps = collect_child_nodes collect_node env deps node in
        Ok (deps, env, bindings)
    | Some decl ->
        if A.ModuleDeclaration.is_recursive decl then
          let (env, bindings) = prebind_module_decl_group env bindings decl in
          let* deps =
            A.ModuleDeclaration.fold_members
              decl
              (Ok deps)
              (fun acc member ->
                let* deps = acc in
                collect_module_member_rhs env deps member)
          in
          Ok (deps, env, bindings)
        else
          A.ModuleDeclaration.fold_members
            decl
            (Ok (deps, env, bindings))
            (fun acc member ->
              let* (deps, env, bindings) = acc in
              let* (deps, binding) = module_member_binding env deps member in
              match module_member_name member with
              | Some name ->
                  let env = Env.add name binding env in
                  let bindings = Env.add name binding bindings in
                  Ok (deps, env, bindings)
              | None -> Ok (deps, env, bindings))

  and collect_module_type_decl env deps node =
    match first_child_node_matching
      node
      ~matches:(fun kind -> Syntax_kind.(kind = MODULE_TYPE_DECL_BODY)) with
    | Some body -> (
        match first_child_node_matching
          body
          ~matches:(fun kind -> is_module_type_kind kind || Syntax_kind.(kind = MODULE_TYPE_EXPR)) with
        | Some module_type -> collect_module_type env deps module_type
        | None -> Ok deps
      )
    | None -> Ok deps

  and include_structure_binding env deps node =
    match first_child_node_matching node ~matches:is_module_expr_kind with
    | Some module_expr -> module_binding env deps module_expr
    | None -> (
        match first_child_node_matching node ~matches:is_module_type_kind with
        | Some module_type -> module_type_binding env deps module_type
        | None -> direct_module_binding_between env deps node 1 (A.Node.child_count node)
      )

  and include_signature_binding env deps node =
    match first_child_node_matching node ~matches:is_module_type_kind with
    | Some module_type -> module_type_binding env deps module_type
    | None -> (
        match first_child_node_matching node ~matches:is_module_expr_kind with
        | Some module_expr -> module_binding env deps module_expr
        | None -> (
            let count = A.Node.child_count node in
            match direct_path_between node 1 count with
            | Some segments -> Ok (add_parent_segments env deps segments, Env.bound)
            | None ->
                let deps = collect_direct_module_refs_between env deps node 1 count in
                Ok (deps, Env.bound)
          )
      )

  and collect_open_decl env deps node =
    match first_child_node_matching node ~matches:is_module_expr_kind with
    | Some module_expr -> (
        let module_expr = unwrap_module_expr module_expr in
        match path_segments module_expr with
        | head :: _ as segments when node_kind_is module_expr Syntax_kind.PATH_MODULE_EXPR
        && is_module_head head ->
            Ok (open_alias ~fallback:true env deps segments)
        | _ ->
            let* (deps, binding) = module_binding env deps module_expr in
            let deps = add_names deps (Env.collect_free binding) in
            Ok (deps, Env.merge_children env binding)
      )
    | None ->
        let count = A.Node.child_count node in
        match direct_path_between node 1 count with
        | Some (head :: _ as segments) when is_module_head head ->
            Ok (open_alias ~fallback:true env deps segments)
        | Some _ -> Ok (deps, env)
        | None ->
            let deps = collect_direct_module_refs_between env deps node 1 count in
            Ok (deps, env)

  and collect_include_structure env deps node =
    let* (deps, binding) = include_structure_binding env deps node in
    let deps = add_names deps (Env.collect_free binding) in
    Ok (deps, Env.merge_children env binding)

  and collect_include_signature env deps node =
    let* (deps, binding) = include_signature_binding env deps node in
    let deps = add_names deps (Env.top_free binding) in
    Ok (deps, Env.merge_children env binding)

  and collect_structure_item env deps bindings item =
    match first_child_node_matching item ~matches:(fun _ -> true) with
    | Some decl when node_kind_is decl Syntax_kind.MODULE_DECL ->
        collect_module_decl env deps bindings decl
    | Some decl when node_kind_is decl Syntax_kind.MODULE_TYPE_DECL ->
        let* deps = collect_module_type_decl env deps decl in
        Ok (deps, env, bindings)
    | Some decl when node_kind_is decl Syntax_kind.OPEN_DECL ->
        let* (deps, env) = collect_open_decl env deps decl in
        Ok (deps, env, bindings)
    | Some decl when node_kind_is decl Syntax_kind.INCLUDE_DECL ->
        let* (deps, env) = collect_include_structure env deps decl in
        Ok (deps, env, bindings)
    | Some decl ->
        let* deps = collect_node env deps decl in
        Ok (deps, env, bindings)
    | None -> Ok (deps, env, bindings)

  and collect_signature_item env deps bindings item =
    match first_child_node_matching item ~matches:(fun _ -> true) with
    | Some decl when node_kind_is decl Syntax_kind.MODULE_DECL ->
        collect_module_decl env deps bindings decl
    | Some decl when node_kind_is decl Syntax_kind.MODULE_TYPE_DECL ->
        let* deps = collect_module_type_decl env deps decl in
        Ok (deps, env, bindings)
    | Some decl when node_kind_is decl Syntax_kind.OPEN_DECL ->
        let* (deps, env) = collect_open_decl env deps decl in
        Ok (deps, env, bindings)
    | Some decl when node_kind_is decl Syntax_kind.INCLUDE_DECL ->
        let* (deps, env) = collect_include_signature env deps decl in
        Ok (deps, env, bindings)
    | Some decl ->
        let* deps = collect_node env deps decl in
        Ok (deps, env, bindings)
    | None -> Ok (deps, env, bindings)

  and collect_structure_binding env deps node =
    fold_child_nodes
      node
      (Ok (deps, env, Env.empty))
      (fun acc child ->
        let* (deps, env, bindings) = acc in
        if node_kind_is child Syntax_kind.STRUCTURE_ITEM then
          collect_structure_item env deps bindings child
        else
          Ok (deps, env, bindings))

  and collect_signature_binding env deps node =
    fold_child_nodes
      node
      (Ok (deps, env, Env.empty))
      (fun acc child ->
        let* (deps, env, bindings) = acc in
        if node_kind_is child Syntax_kind.SIGNATURE_ITEM then
          collect_signature_item env deps bindings child
        else
          Ok (deps, env, bindings))

  let finalize_impl = fun env impl ->
    let* (deps, env, exports) = collect_structure_binding env (DepSet.empty ()) impl in
    let deps = add_names deps (Env.collect_free (Env.make_node exports)) in
    Ok (deps, env, exports)

  let finalize_intf = fun env intf ->
    let* (deps, env, exports) = collect_signature_binding env (DepSet.empty ()) intf in
    Ok (deps, env, exports)

  let from_parse_result = fun ~env result ->
    match A.SourceFile.view (A.SourceFile.make result.Parser.tree) with
    | A.SourceFile.Implementation impl -> finalize_impl env (A.Implementation.as_node impl)
    | A.SourceFile.Interface intf -> finalize_intf env (A.Interface.as_node intf)
end

let finalize = fun deps env exports -> { modules = DepSet.elements deps; env; exports }

let from_parse_result = fun ?(env = Env.empty) result ->
  if Int.(Vector.length result.Parser.diagnostics != 0) then
    Error (
      Parse_diagnostics (
        Vector.iter result.Parser.diagnostics
        |> Iterator.to_list
      )
    )
  else
    match Ast_deps.from_parse_result ~env result with
    | Ok (deps, env, exports) -> Ok (finalize deps env exports)
    | Error err -> Error err

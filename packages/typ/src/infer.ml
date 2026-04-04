open Std
module Typ_diagnostic = Diagnostic

type t = {
  exports: Check_result.env;
  item_traces: Check_result.item_trace list;
  expr_traces: Check_result.expr_trace list;
  diagnostics: Typ_diagnostic.t list;
}

type state = {
  file: SemanticTree.file;
  config: TypConfig.t;
  mutable next_type_var_id: int;
  mutable next_hole_id: int;
  mutable diagnostics: Typ_diagnostic.t list;
  mutable expr_traces: Check_result.expr_trace list;
  mutable item_traces: Check_result.item_trace list;
}

let empty_span = Syn.Ceibo.Span.make ~start:0 ~end_:0

let make_state = fun ~config file ->
  {
    file;
    config;
    next_type_var_id = 0;
    next_hole_id = 0;
    diagnostics = [];
    expr_traces = [];
    item_traces = [];
  }

let unique_env = fun env ->
  let rec loop seen acc = function
    | [] -> List.rev acc
    | (name, scheme) :: rest ->
        if List.mem name seen then
          loop seen acc rest
        else
          loop (name :: seen) ((name, scheme) :: acc) rest
  in
  loop [] [] env

let render_env = fun env ->
  env
  |> unique_env
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)

let env_lookup = fun env name ->
  List.find_opt (fun (candidate, _) -> String.equal candidate name) env

let env_names = fun env ->
  render_env env
  |> List.map fst

let introduced_names = fun before after ->
  let before_names = env_names before in
  env_names after
  |> List.filter (fun name -> not (List.mem name before_names))

let env_free_vars = fun env ->
  env
  |> unique_env
  |> List.fold_left (fun acc (_, scheme) -> TypeRepr.union acc (TypeScheme.free_vars scheme)) []

let fresh_var = fun (state: state) ->
  let id = state.next_type_var_id in
  let () = state.next_type_var_id <- state.next_type_var_id + 1 in
  TypeRepr.Var { id; link = None }

let fresh_hole = fun (state: state) ->
  let hole_id = state.next_hole_id in
  let () = state.next_hole_id <- state.next_hole_id + 1 in
  TypeRepr.Hole hole_id

let instantiate = fun (state: state) (TypeScheme.Forall (quantified, body)) ->
  let mapping =
    quantified
    |> List.map (fun quantified_id -> (quantified_id, fresh_var state))
  in
  let rec loop ty =
    match TypeRepr.prune ty with
    | TypeRepr.Int -> TypeRepr.Int
    | TypeRepr.Bool -> TypeRepr.Bool
    | TypeRepr.String -> TypeRepr.String
    | TypeRepr.Unit -> TypeRepr.Unit
    | TypeRepr.Hole hole_id -> TypeRepr.Hole hole_id
    | TypeRepr.Tuple members -> TypeRepr.Tuple (List.map loop members)
    | TypeRepr.Arrow (lhs, rhs) -> TypeRepr.Arrow (loop lhs, loop rhs)
    | TypeRepr.Var var -> (
        match List.assoc_opt var.id mapping with
        | Some replacement -> replacement
        | None -> TypeRepr.Var var)
  in
  loop body

let generalize = fun env ty ->
  let env_free = env_free_vars env in
  let ty_free = TypeRepr.free_vars ty in
  TypeScheme.Forall (TypeRepr.diff ty_free env_free |> List.rev, ty)

let origin_of_expr = fun (state: state) expr_id ->
  match SemanticTree.find_expr state.file expr_id with
  | Some node -> SemanticTree.find_origin state.file node.origin_id
  | None -> None

let origin_of_pattern = fun (state: state) pat_id ->
  match SemanticTree.find_pattern state.file pat_id with
  | Some node -> SemanticTree.find_origin state.file node.origin_id
  | None -> None

let origin_of_binding = fun (state: state) (binding: BodyArena.binding) ->
  SemanticTree.find_origin state.file binding.origin_id

let diagnostic_span = fun origin ->
  match origin with
  | Some (origin: OriginMap.origin) -> origin.span
  | None -> empty_span

let add_diagnostic = fun (state: state) diagnostic ->
  state.diagnostics <- diagnostic :: state.diagnostics

exception Unify_error of Typ_diagnostic.mismatch

let rec unify = fun (state: state) ~origin left right ->
  let left = TypeRepr.prune left in
  let right = TypeRepr.prune right in
  match (left, right) with
  | TypeRepr.Int, TypeRepr.Int
  | TypeRepr.Bool, TypeRepr.Bool
  | TypeRepr.String, TypeRepr.String
  | TypeRepr.Unit, TypeRepr.Unit -> ()
  | TypeRepr.Hole _, _
  | _, TypeRepr.Hole _ -> ()
  | TypeRepr.Tuple left_members, TypeRepr.Tuple right_members ->
      if List.length left_members != List.length right_members then
        raise (Unify_error (Typ_diagnostic.TupleArityMismatch {
          left = TypePrinter.type_to_string left;
          right = TypePrinter.type_to_string right;
          left_arity = List.length left_members;
          right_arity = List.length right_members;
        }));
      List.iter2 (unify state ~origin) left_members right_members
  | TypeRepr.Arrow (left_arg, left_res), TypeRepr.Arrow (right_arg, right_res) ->
      let () = unify state ~origin left_arg right_arg in
      unify state ~origin left_res right_res
  | TypeRepr.Var left_var, TypeRepr.Var right_var when left_var.id = right_var.id -> ()
  | TypeRepr.Var var, ty
  | ty, TypeRepr.Var var ->
      if TypeRepr.occurs var.id ty then
        raise (Unify_error (Typ_diagnostic.OccursCheckFailed {
          variable_id = var.id;
          in_type = TypePrinter.type_to_string ty;
        }))
      else
        var.link <- Some ty
  | _ ->
      raise (Unify_error (Typ_diagnostic.ExpectedActual {
        expected = TypePrinter.type_to_string left;
        actual = TypePrinter.type_to_string right;
      }))

let try_unify = fun (state: state) ~origin left right ->
  try
    unify state ~origin left right;
    ()
  with
  | Unify_error mismatch ->
      add_diagnostic
        state
        (Typ_diagnostic.TypeMismatch {
          mismatch_span = diagnostic_span origin;
          mismatch;
        })

let bind_env = fun env bindings ->
  bindings @ env

let rec bind_pattern = fun (state: state) pat_id expected_ty ->
  match SemanticTree.find_pattern state.file pat_id with
  | None -> []
  | Some pattern -> (
      match pattern.desc with
      | BodyArena.PVar name -> [ (name, TypeScheme.Forall ([], expected_ty)) ]
      | BodyArena.PWildcard -> []
      | BodyArena.PInt _ ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.Int in
          []
      | BodyArena.PBool _ ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.Bool in
          []
      | BodyArena.PString _ ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.String in
          []
      | BodyArena.PUnit ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.Unit in
          []
      | BodyArena.PTuple elements ->
          let element_types = List.map (fun _ -> fresh_var state) elements in
          let () =
            try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty (TypeRepr.Tuple element_types)
          in
          List.map2 (bind_pattern state) elements element_types
          |> List.flatten
      | BodyArena.PUnsupported _ -> [])

let record_expr_trace = fun (state: state) expr_id origin_id env_before inferred_type ->
  state.expr_traces <- ({
    Check_result.expr_id;
    origin_id;
    env_before = render_env env_before;
    inferred_type;
  }: Check_result.expr_trace) :: state.expr_traces

let rec infer_expr = fun (state: state) env expr_id ->
  match SemanticTree.find_expr state.file expr_id with
  | None ->
      fresh_hole state
  | Some expr ->
      let inferred_type =
        match expr.desc with
        | BodyArena.EVar name -> (
            match env_lookup env name with
            | Some (_, scheme) -> instantiate state scheme
            | None ->
                let hole = fresh_hole state in
                let () =
                  add_diagnostic
                    state
                    (Typ_diagnostic.UnboundName {
                      reference_span = diagnostic_span (origin_of_expr state expr_id);
                      name;
                    })
                in
                hole)
        | BodyArena.EInt _ -> TypeRepr.Int
        | BodyArena.EBool _ -> TypeRepr.Bool
        | BodyArena.EString _ -> TypeRepr.String
        | BodyArena.EUnit -> TypeRepr.Unit
        | BodyArena.ETuple elements ->
            TypeRepr.Tuple (List.map (infer_expr state env) elements)
        | BodyArena.EFun (parameters, body_id) ->
            let rec lower_parameters env arg_types = function
              | [] ->
                  let body_ty = infer_expr state env body_id in
                  List.fold_right (fun arg_ty acc -> TypeRepr.Arrow (arg_ty, acc)) (List.rev arg_types) body_ty
              | parameter_id :: rest ->
                  let arg_ty = fresh_var state in
                  let bindings = bind_pattern state parameter_id arg_ty in
                  lower_parameters (bind_env env bindings) (arg_ty :: arg_types) rest
            in
            lower_parameters env [] parameters
        | BodyArena.EApply (callee_id, arguments) ->
            let callee_ty = infer_expr state env callee_id in
            let rec apply current_ty = function
              | [] -> current_ty
              | argument_id :: rest ->
                  let argument_ty = infer_expr state env argument_id in
                  let result_ty = fresh_var state in
                  let () =
                    try_unify
                      state
                      ~origin:(origin_of_expr state expr_id)
                      current_ty
                      (TypeRepr.Arrow (argument_ty, result_ty))
                  in
                  apply result_ty rest
            in
            apply callee_ty arguments
        | BodyArena.ELet (binding_ids, body_id) ->
            let env = infer_binding_group state env binding_ids in
            infer_expr state env body_id
        | BodyArena.EIf (condition_id, then_id, else_id) ->
            let condition_ty = infer_expr state env condition_id in
            let () = try_unify state ~origin:(origin_of_expr state condition_id) condition_ty TypeRepr.Bool in
            let then_ty = infer_expr state env then_id in
            let else_ty = infer_expr state env else_id in
            let () = try_unify state ~origin:(origin_of_expr state expr_id) then_ty else_ty in
            then_ty
        | BodyArena.EMatch (scrutinee_id, cases) ->
            let scrutinee_ty = infer_expr state env scrutinee_id in
            let result_ty = fresh_var state in
            let () =
              List.iter
                (fun (case: BodyArena.match_case) ->
                  let bindings = bind_pattern state case.pattern_id scrutinee_ty in
                  let case_ty = infer_expr state (bind_env env bindings) case.body_id in
                  try_unify state ~origin:(origin_of_expr state case.body_id) result_ty case_ty)
                cases
            in
            result_ty
        | BodyArena.EUnsupported summary ->
            let hole = fresh_hole state in
            let () =
              add_diagnostic
                state
                (Typ_diagnostic.UnsupportedSemanticExpression {
                  expression_span = diagnostic_span (origin_of_expr state expr_id);
                  summary;
                })
            in
            hole
        | BodyArena.EHole _ ->
            fresh_hole state
      in
      let () = record_expr_trace state expr_id expr.origin_id env inferred_type in
      inferred_type

and infer_binding_group = fun (state: state) env binding_ids ->
  let bindings =
    binding_ids
    |> List.filter_map (SemanticTree.find_binding state.file)
  in
  let recursive =
    match bindings with
    | [] -> false
    | (binding: BodyArena.binding) :: _ -> binding.recursive
  in
  if recursive then
    infer_recursive_group state env bindings
  else
    infer_nonrecursive_group state env bindings

and infer_nonrecursive_group = fun (state: state) env bindings ->
  let inferred_bindings =
    List.map
      (fun (binding: BodyArena.binding) ->
        let value_ty = infer_expr state env binding.value_id in
        let bound_entries = bind_pattern state binding.pattern_id value_ty in
        let generalized =
          bound_entries
          |> List.map (fun (name, TypeScheme.Forall (_, ty)) -> (name, generalize env ty))
        in
        (binding, generalized))
      bindings
  in
  List.fold_left
    (fun env (_, entries) -> bind_env env entries)
    env
    inferred_bindings

and infer_recursive_group = fun (state: state) env bindings ->
  let names =
    bindings
    |> List.map (fun (binding: BodyArena.binding) -> (binding, binding.name))
  in
  if List.exists (fun (_, name) -> Option.is_none name) names then (
    let () =
      List.iter
        (fun ((binding: BodyArena.binding), _) ->
          add_diagnostic
            state
            (Typ_diagnostic.RecursiveGroupRequiresSimpleVariableBinders {
              binding_span = diagnostic_span (origin_of_binding state binding);
            }))
        names
    in
    infer_nonrecursive_group state env bindings
  ) else
    let placeholders =
      names
      |> List.filter_map (fun ((binding: BodyArena.binding), name) ->
        match name with
        | Some name -> Some (binding, name, fresh_var state)
        | None -> None)
    in
    let provisional_env =
      placeholders
      |> List.map (fun (_, name, ty) -> (name, TypeScheme.Forall ([], ty)))
      |> bind_env env
    in
    let () =
      List.iter
        (fun ((binding: BodyArena.binding), _, placeholder_ty) ->
          let value_ty = infer_expr state provisional_env binding.value_id in
          try_unify state ~origin:(origin_of_binding state binding) placeholder_ty value_ty)
        placeholders
    in
    let generalized =
      placeholders
      |> List.map (fun (_, name, ty) -> (name, generalize env ty))
    in
    bind_env env generalized

let prelude_names = fun (config: TypConfig.t) ->
  config.prelude
  |> List.map fst

let export_env = fun config env ->
  let hidden_names = prelude_names config in
  render_env env
  |> List.filter (fun (name, _) -> not (List.mem name hidden_names))

let infer_file = fun ~config file ->
  let state = make_state ~config file in
  let exports =
    List.fold_left
      (fun env item ->
        match item with
        | ItemTree.Value value_item ->
            let env_before = export_env config env in
            let env = infer_binding_group state env value_item.binding_ids in
            let exports_after = export_env config env in
            let binding_names = introduced_names env_before exports_after in
            let () =
              state.item_traces <- ({
                Check_result.item_id = value_item.item_id;
                binding_names;
                exports_after;
              }: Check_result.item_trace) :: state.item_traces
            in
            env
        | ItemTree.Unsupported unsupported_item ->
            let exports_after = export_env config env in
            let () =
              state.item_traces <- ({
                Check_result.item_id = unsupported_item.item_id;
                binding_names = [];
                exports_after;
              }: Check_result.item_trace) :: state.item_traces
            in
            env)
      config.prelude
      (ItemTree.items file.item_tree)
  in
  {
    exports = export_env config exports;
    item_traces = List.rev state.item_traces;
    expr_traces = List.rev state.expr_traces;
    diagnostics = List.rev state.diagnostics;
  }

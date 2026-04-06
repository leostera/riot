open Std
module Typ_diagnostic = Diagnostic

type t = {
  exports: Check_result.env;
  type_decls: FileSummary.type_decl list;
  item_traces: Check_result.item_trace list;
  expr_traces: Check_result.expr_trace list;
  diagnostics: Typ_diagnostic.t list;
}

type record_type_decl = {
  owner_name: string;
  param_ids: int list;
  labels: TypeDecl.label list;
}

type state = {
  file: SemanticTree.file;
  config: TypConfig.t;
  mutable next_type_var_id: int;
  mutable next_hole_id: int;
  mutable diagnostics: Typ_diagnostic.t list;
  mutable expr_traces: Check_result.expr_trace list;
  mutable item_traces: Check_result.item_trace list;
  mutable record_types: record_type_decl list;
  mutable forced_export_names: string list;
}

let empty_span = Syn.Ceibo.Span.make ~start:0 ~end_:0

let qualify_name = fun scope_path name ->
  match scope_path with
  | [] -> name
  | _ -> String.concat "." scope_path ^ "." ^ name

let record_type_of_summary_decl = fun (type_decl: FileSummary.type_decl) ->
  match type_decl.declaration.labels with
  | [] -> None
  | labels ->
      Some {
        owner_name = qualify_name type_decl.scope_path type_decl.declaration.type_name;
        param_ids = type_decl.declaration.param_ids;
        labels;
      }

let unique_record_types = fun record_types ->
  let rec loop seen acc = function
    | [] -> List.rev acc
    | (record_decl: record_type_decl) :: rest ->
        if List.mem record_decl.owner_name seen then
          loop seen acc rest
        else
          loop (record_decl.owner_name :: seen) (record_decl :: acc) rest
  in
  loop [] [] record_types

let type_decl_key = fun (type_decl: FileSummary.type_decl) ->
  qualify_name type_decl.scope_path type_decl.declaration.type_name

let bind_type_decls = fun type_decls introduced ->
  List.fold_left
    (fun acc (type_decl: FileSummary.type_decl) ->
      let key = type_decl_key type_decl in
      let acc = List.filter (fun candidate -> not (String.equal (type_decl_key candidate) key)) acc in
      acc @ [ type_decl ])
    type_decls
    introduced

let split_module_path = fun module_path ->
  if String.equal module_path "" then
    []
  else
    String.split_on_char '.' module_path

let rec strip_scope_prefix = fun prefix scope_path ->
  match (prefix, scope_path) with
  | [], rest -> Some rest
  | prefix_segment :: prefix_rest, scope_segment :: scope_rest
    when String.equal prefix_segment scope_segment -> strip_scope_prefix prefix_rest scope_rest
  | _ -> None

let aliases_for_type_decls = fun type_decls module_path ->
  let prefix = split_module_path module_path in
  type_decls |> List.filter_map
    (fun (type_decl: FileSummary.type_decl) ->
      match strip_scope_prefix prefix type_decl.scope_path with
      | Some scope_path -> Some { type_decl with scope_path }
      | None -> None)

let prefix_type_decls = fun prefix type_decls ->
  List.map
    (fun (type_decl: FileSummary.type_decl) ->
      { type_decl with scope_path = prefix @ type_decl.scope_path })
    type_decls

let type_decls_for_include = fun type_decls module_path ->
  aliases_for_type_decls type_decls module_path

let type_decls_for_module_alias = fun type_decls ~alias_name ~module_path ->
  if String.equal alias_name module_path then
    []
  else
    aliases_for_type_decls type_decls module_path |> prefix_type_decls [ alias_name ]

let make_state = fun ~config file ->
  {
    file;
    config;
    next_type_var_id = 0;
    next_hole_id = 0;
    diagnostics = [];
    expr_traces = [];
    item_traces = [];
    record_types =
      config.ambient_type_decls |> List.filter_map record_type_of_summary_decl |> unique_record_types;
    forced_export_names = [];
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
  env |> unique_env |> List.sort
    (fun (left, _) (right, _) ->
      String.compare left right)

let env_lookup = fun env name ->
  List.find_opt
    (fun (candidate, _) ->
      String.equal candidate name)
    env

let env_lookup_all = fun env name ->
  env |> List.filter_map
    (fun (candidate, scheme) ->
      if String.equal candidate name then
        Some scheme
      else
        None)

let env_names = fun env -> render_env env |> List.map fst

let introduced_names = fun before after ->
  let before_names = env_names before in
  env_names after |> List.filter (fun name -> not (List.mem name before_names))

let env_free_vars = fun env ->
  env |> unique_env |> List.fold_left
    (fun acc (_, scheme) ->
      TypeRepr.union acc (TypeScheme.free_vars scheme))
    []

let fresh_var = fun (state: state) ->
  let id = state.next_type_var_id in
  let () =
    state.next_type_var_id <- state.next_type_var_id + 1
  in
  TypeRepr.Var { id; link = None }

let fresh_hole = fun (state: state) ->
  let hole_id = state.next_hole_id in
  let () =
    state.next_hole_id <- state.next_hole_id + 1
  in
  TypeRepr.Hole hole_id

let instantiate_type = fun ty mapping ->
  let rec loop ty =
    match TypeRepr.prune ty with
    | TypeRepr.Int ->
        TypeRepr.Int
    | TypeRepr.Float ->
        TypeRepr.Float
    | TypeRepr.Bool ->
        TypeRepr.Bool
    | TypeRepr.String ->
        TypeRepr.String
    | TypeRepr.Char ->
        TypeRepr.Char
    | TypeRepr.Unit ->
        TypeRepr.Unit
    | TypeRepr.Option element ->
        TypeRepr.Option (loop element)
    | TypeRepr.Result (ok_ty, error_ty) ->
        TypeRepr.Result (loop ok_ty, loop error_ty)
    | TypeRepr.Array element ->
        TypeRepr.Array (loop element)
    | TypeRepr.List element ->
        TypeRepr.List (loop element)
    | TypeRepr.Seq element ->
        TypeRepr.Seq (loop element)
    | TypeRepr.Named { name; arguments } ->
        TypeRepr.Named { name; arguments = List.map loop arguments }
    | TypeRepr.Hole hole_id ->
        TypeRepr.Hole hole_id
    | TypeRepr.Tuple members ->
        TypeRepr.Tuple (List.map loop members)
    | TypeRepr.Arrow { label; lhs; rhs } ->
        TypeRepr.Arrow { label; lhs = loop lhs; rhs = loop rhs }
    | TypeRepr.Var var -> (
        match List.assoc_opt var.id mapping with
        | Some replacement -> replacement
        | None -> TypeRepr.Var var
      )
  in
  loop ty

let instantiate = fun (state: state) (TypeScheme.Forall (quantified, body)) ->
  let mapping = quantified |> List.map (fun quantified_id -> (quantified_id, fresh_var state)) in
  instantiate_type body mapping

let labels_match = fun left right ->
  match (left, right) with
  | (TypeRepr.Nolabel, TypeRepr.Nolabel) -> true
  | (TypeRepr.Labelled left, TypeRepr.Labelled right) -> String.equal left right
  | (TypeRepr.Optional left, TypeRepr.Optional right) -> String.equal left right
  | _ -> false

let type_label_of_body_label = function
  | BodyArena.Positional -> TypeRepr.Nolabel
  | BodyArena.Labeled label -> TypeRepr.Labelled label
  | BodyArena.Optional label -> TypeRepr.Optional label

let diagnostic_label_of_type_label = function
  | TypeRepr.Nolabel -> Typ_diagnostic.PositionalArgument
  | TypeRepr.Labelled label -> Typ_diagnostic.LabeledArgument label
  | TypeRepr.Optional label -> Typ_diagnostic.OptionalArgument label

let diagnostic_label_of_body_label = function
  | BodyArena.Positional -> Typ_diagnostic.PositionalArgument
  | BodyArena.Labeled label -> Typ_diagnostic.LabeledArgument label
  | BodyArena.Optional label -> Typ_diagnostic.OptionalArgument label

let argument_matches_parameter_label = fun parameter_label argument_label ->
  match (parameter_label, argument_label) with
  | (TypeRepr.Nolabel, BodyArena.Positional) -> true
  | (TypeRepr.Labelled expected, BodyArena.Labeled actual) -> String.equal expected actual
  | (TypeRepr.Optional expected, BodyArena.Optional actual)
  | (TypeRepr.Optional expected, BodyArena.Labeled actual) -> String.equal expected actual
  | _ -> false

let take_matching_argument = fun parameter_label arguments ->
  let rec loop prefix = function
    | [] -> None
    | ((argument: BodyArena.apply_argument) as candidate) :: rest ->
        if argument_matches_parameter_label parameter_label argument.label then
          Some (candidate, List.rev_append prefix rest)
        else
          loop (candidate :: prefix) rest
  in
  loop [] arguments

let last_segment_text = fun text ->
  match List.rev (String.split_on_char '.' text) with
  | segment :: _ -> segment
  | [] -> text

let record_label_matches = fun requested candidate ->
  String.equal requested candidate || String.equal (last_segment_text requested) candidate

let owner_name_of_type = fun ty ->
  match TypeRepr.prune ty with
  | TypeRepr.Named { name; _ } -> Some name
  | _ -> None

let rec result_type_of_type = fun ty ->
  match TypeRepr.prune ty with
  | TypeRepr.Arrow { rhs; _ } -> result_type_of_type rhs
  | ty -> ty

let owner_name_of_scheme = fun (TypeScheme.Forall (_, body)) ->
  owner_name_of_type (result_type_of_type body)

let resolve_constructor_scheme = fun env constructor ~expected_ty ->
  let candidates = env_lookup_all env constructor in
  match candidates with
  | [] ->
      None
  | [ scheme ] ->
      Some scheme
  | candidates -> (
      match owner_name_of_type expected_ty with
      | Some expected_owner -> (
          match List.filter (fun scheme -> owner_name_of_scheme scheme = Some expected_owner) candidates with
          | [] -> Some (List.hd candidates)
          | scheme :: _ -> Some scheme
        )
      | None -> Some (List.hd candidates)
    )

let record_decl_field = fun (record_decl: record_type_decl) label_name ->
  List.find_opt
    (fun (field: TypeDecl.label) -> record_label_matches label_name field.name)
    record_decl.labels

let record_decl_fields = fun (record_decl: record_type_decl) ->
  List.map (fun (field: TypeDecl.label) -> field.name) record_decl.labels

let instantiate_record_decl = fun (state: state) (record_decl: record_type_decl) ->
  let mapping = record_decl.param_ids
  |> List.map (fun quantified_id -> (quantified_id, fresh_var state)) in
  let owner_ty = TypeRepr.Named {
    name = record_decl.owner_name;
    arguments =
      record_decl.param_ids |> List.map
        (fun quantified_id ->
          match List.assoc_opt quantified_id mapping with
          | Some ty -> ty
          | None -> TypeRepr.Hole (-200));
  }
  in
  let field_types = record_decl.labels
  |> List.map
    (fun (field: TypeDecl.label) -> (field.name, instantiate_type field.field_type mapping)) in
  (owner_ty, field_types)

let record_field_type = fun field_types label_name ->
  List.find_map
    (fun (field_name, field_ty) ->
      if record_label_matches label_name field_name then
        Some field_ty
      else
        None)
    field_types

let add_diagnostic = fun (state: state) diagnostic -> state.diagnostics <- diagnostic :: state.diagnostics

let add_record_resolution_error = fun (state: state) ~span ~context reason ->
  add_diagnostic
    state
    (Typ_diagnostic.RecordResolutionError { operation_span = span; context; reason })

let add_or_pattern_bindings_mismatch = fun (state: state) ~span ~expected_names ~actual_names ->
  add_diagnostic
    state
    (Typ_diagnostic.OrPatternBindingsMismatch { pattern_span = span; expected_names; actual_names })

let resolve_record_decl = fun (state: state) ~field_names ~owner_hint ~span ~context ->
  let candidates =
    state.record_types
    |> List.filter
      (fun (record_decl: record_type_decl) ->
        List.for_all (fun field_name -> Option.is_some (record_decl_field record_decl field_name)) field_names)
  in
  let known_labels = state.record_types
  |> List.concat_map record_decl_fields
  |> List.sort_uniq String.compare in
  let candidates =
    match owner_hint with
    | Some owner_name ->
        candidates |> List.filter
          (fun (record_decl: record_type_decl) ->
            String.equal record_decl.owner_name owner_name)
    | None -> candidates
  in
  match candidates with
  | [ record_decl ] ->
      Some record_decl
  | [] ->
      let unknown_labels = field_names
      |> List.filter
        (fun field_name -> not (List.exists (record_label_matches field_name) known_labels)) in
      let reason =
        if not (List.is_empty unknown_labels) then
          Typ_diagnostic.UnknownRecordLabels unknown_labels
        else
          Typ_diagnostic.IncompatibleRecordLabels field_names
      in
      let () = add_record_resolution_error state ~span ~context reason in
      None
  | _ ->
      let () = add_record_resolution_error
        state
        ~span
        ~context
        (Typ_diagnostic.AmbiguousRecordLabels field_names) in
      None

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

exception Unify_error of Typ_diagnostic.mismatch

let rec unify = fun (state: state) ~origin left right ->
  let left = TypeRepr.prune left in
  let right = TypeRepr.prune right in
  match (left, right) with
  | (TypeRepr.Int, TypeRepr.Int)
  | (TypeRepr.Float, TypeRepr.Float)
  | (TypeRepr.Bool, TypeRepr.Bool)
  | (TypeRepr.String, TypeRepr.String)
  | (TypeRepr.Char, TypeRepr.Char)
  | (TypeRepr.Unit, TypeRepr.Unit) ->
      ()
  | TypeRepr.Option left_element, TypeRepr.Option right_element ->
      unify state ~origin left_element right_element
  | TypeRepr.Result (left_ok, left_error), TypeRepr.Result (right_ok, right_error) ->
      let () = unify state ~origin left_ok right_ok in
      unify state ~origin left_error right_error
  | (TypeRepr.Hole _, _)
  | (_, TypeRepr.Hole _) ->
      ()
  | TypeRepr.Array left_element, TypeRepr.Array right_element ->
      unify state ~origin left_element right_element
  | TypeRepr.List left_element, TypeRepr.List right_element ->
      unify state ~origin left_element right_element
  | TypeRepr.Seq left_element, TypeRepr.Seq right_element ->
      unify state ~origin left_element right_element
  | TypeRepr.Named { name=left_name; arguments=left_arguments }, TypeRepr.Named {
    name=right_name;
    arguments=right_arguments
  } ->
      if not (String.equal left_name right_name) then
        raise
          (Unify_error (Typ_diagnostic.ExpectedActual {
            expected = TypePrinter.type_to_string left;
            actual = TypePrinter.type_to_string right
          }))
      else if List.length left_arguments != List.length right_arguments then
        raise
          (Unify_error (Typ_diagnostic.ExpectedActual {
            expected = TypePrinter.type_to_string left;
            actual = TypePrinter.type_to_string right
          }))
      else
        List.iter2 (unify state ~origin) left_arguments right_arguments
  | TypeRepr.Tuple left_members, TypeRepr.Tuple right_members ->
      if List.length left_members != List.length right_members then
        raise
          (Unify_error (Typ_diagnostic.TupleArityMismatch {
            left = TypePrinter.type_to_string left;
            right = TypePrinter.type_to_string right;
            left_arity = List.length left_members;
            right_arity = List.length right_members
          }));
      List.iter2 (unify state ~origin) left_members right_members
  | TypeRepr.Arrow { label=left_label; lhs=left_arg; rhs=left_res }, TypeRepr.Arrow {
    label=right_label;
    lhs=right_arg;
    rhs=right_res
  } ->
      if not (labels_match left_label right_label) then
        raise
          (Unify_error (Typ_diagnostic.ExpectedActual {
            expected = TypePrinter.type_to_string left;
            actual = TypePrinter.type_to_string right
          }));
      let () = unify state ~origin left_arg right_arg in
      unify state ~origin left_res right_res
  | TypeRepr.Var left_var, TypeRepr.Var right_var when left_var.id = right_var.id ->
      ()
  | (TypeRepr.Var var, ty)
  | (ty, TypeRepr.Var var) ->
      if TypeRepr.occurs var.id ty then
        raise
          (Unify_error (Typ_diagnostic.OccursCheckFailed {
            variable_id = var.id;
            in_type = TypePrinter.type_to_string ty
          }))
      else
        var.link <- Some ty
  | _ ->
      raise
        (Unify_error (Typ_diagnostic.ExpectedActual {
          expected = TypePrinter.type_to_string left;
          actual = TypePrinter.type_to_string right
        }))

let try_unify = fun (state: state) ~origin left right ->
  try
    unify state ~origin left right;
    ()
  with
  | Unify_error mismatch -> add_diagnostic
    state
    (Typ_diagnostic.TypeMismatch { mismatch_span = diagnostic_span origin; mismatch })

let bind_env = fun env bindings -> bindings @ env

let has_prefix = fun ~prefix text ->
  let prefix_length = String.length prefix in
  if String.length text < prefix_length then
    false
  else
    String.sub text 0 prefix_length = prefix

let aliases_for_local_open = fun env module_path ->
  let prefix = module_path ^ "." in
  env |> List.filter_map
    (fun (name, scheme) ->
      if has_prefix ~prefix name then
        let suffix = String.sub
          name
          (String.length prefix)
          (String.length name - String.length prefix) in
        Some (suffix, scheme)
      else
        None)

let env_with_local_open = fun env module_path ->
  let aliases = aliases_for_local_open env module_path in
  bind_env env aliases

let entries_for_include = fun env module_path ->
  aliases_for_local_open env module_path |> unique_env |> render_env

let prefix_entries = fun prefix entries ->
  entries |> List.map (fun (name, scheme) -> (prefix ^ "." ^ name, scheme))

let export_names_for_module_alias = fun env ~alias_name ~module_path ->
  aliases_for_local_open env module_path
  |> unique_env
  |> render_env
  |> prefix_entries alias_name
  |> List.map fst

let entries_for_module_alias = fun env ~alias_name ~module_path ->
  if String.equal alias_name module_path then
    []
  else
    aliases_for_local_open env module_path |> unique_env |> render_env |> prefix_entries alias_name

let is_recursive_binding_supported = fun (state: state) (binding: BodyArena.binding) ->
  match binding.name with
  | None -> false
  | Some _ -> (
      match SemanticTree.find_expr state.file binding.value_id with
      | Some value -> (
          match value.desc with
          | BodyArena.EFun _ -> true
          | _ -> false
        )
      | None -> false
    )

let rec constructor_pattern_argument_types = fun (state: state) constructor_ty arguments origin ->
  match arguments with
  | [] -> ([], constructor_ty)
  | _ :: rest ->
      let argument_ty = fresh_var state in
      let result_ty = fresh_var state in
      let () = try_unify
        state
        ~origin
        constructor_ty
        (TypeRepr.Arrow { label = TypeRepr.Nolabel; lhs = argument_ty; rhs = result_ty }) in
      let (rest_argument_types, final_result_ty) = constructor_pattern_argument_types
        state
        result_ty
        rest
        origin in
      (argument_ty :: rest_argument_types, final_result_ty)

let rec bind_pattern = fun (state: state) env pat_id expected_ty ->
  let normalize_bindings bindings = bindings |> unique_env |> render_env in
  let binding_names bindings = bindings |> List.map fst in
  let scheme_type (TypeScheme.Forall (_, ty)) = ty in
  let unify_or_pattern_bindings origin bindings alternatives =
    let expected_bindings = normalize_bindings bindings in
    let expected_names = binding_names expected_bindings in
    let rec loop current_bindings remaining =
      match remaining with
      | [] -> Some current_bindings
      | alternative_bindings :: rest ->
          let alternative_bindings = normalize_bindings alternative_bindings in
          let actual_names = binding_names alternative_bindings in
          if not (List.equal String.equal expected_names actual_names) then
            let () = add_or_pattern_bindings_mismatch
              state
              ~span:(diagnostic_span origin)
              ~expected_names
              ~actual_names in
            None
          else
            let () =
              List.iter2
                (fun (_, expected_scheme) (_, actual_scheme) ->
                  try_unify state ~origin (scheme_type expected_scheme) (scheme_type actual_scheme))
                current_bindings
                alternative_bindings
            in
            loop current_bindings rest
    in
    loop expected_bindings alternatives
  in
  match SemanticTree.find_pattern state.file pat_id with
  | None -> []
  | Some pattern -> (
      match pattern.desc with
      | BodyArena.PVar name ->
          [ (name, TypeScheme.Forall ([], expected_ty)) ]
      | BodyArena.PWildcard ->
          []
      | BodyArena.PInt _ ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.Int in
          []
      | BodyArena.PFloat _ ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.Float in
          []
      | BodyArena.PBool _ ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.Bool in
          []
      | BodyArena.PString _ ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.String in
          []
      | BodyArena.PChar _ ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.Char in
          []
      | BodyArena.PUnit ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.Unit in
          []
      | BodyArena.PTuple elements ->
          let element_types =
            List.map (fun _ -> fresh_var state) elements
          in
          let () = try_unify
            state
            ~origin:(origin_of_pattern state pat_id)
            expected_ty
            (TypeRepr.Tuple element_types) in
          List.map2 (bind_pattern state env) elements element_types |> List.flatten
      | BodyArena.POr alternatives -> (
          match alternatives with
          | [] -> []
          | alternative :: rest ->
              let origin = origin_of_pattern state pat_id in
              let bindings = bind_pattern state env alternative expected_ty in
              let alternative_bindings = rest
              |> List.map (fun alternative_id -> bind_pattern state env alternative_id expected_ty) in
              match unify_or_pattern_bindings origin bindings alternative_bindings with
              | Some bindings -> bindings
              | None -> []
        )
      | BodyArena.PConstructor { constructor; arguments } -> (
          match resolve_constructor_scheme env constructor ~expected_ty with
          | Some scheme ->
              let origin = origin_of_pattern state pat_id in
              let constructor_ty = instantiate state scheme in
              let (argument_types, result_ty) = constructor_pattern_argument_types
                state
                constructor_ty
                arguments
                origin in
              let () = try_unify state ~origin expected_ty result_ty in
              List.map2 (bind_pattern state env) arguments argument_types |> List.flatten
          | None ->
              let argument_types =
                List.map (fun _ -> fresh_var state) arguments
              in
              List.map2 (bind_pattern state env) arguments argument_types |> List.flatten
        )
      | BodyArena.PRecord { fields; open_ } -> (
          let origin = origin_of_pattern state pat_id in
          let field_names =
            List.map (fun (field: BodyArena.record_pattern_field) -> field.label) fields
          in
          match resolve_record_decl
            state
            ~field_names
            ~owner_hint:(owner_name_of_type expected_ty)
            ~span:(diagnostic_span origin)
            ~context:Typ_diagnostic.RecordPattern with
          | Some record_decl ->
              let (owner_ty, field_types) = instantiate_record_decl state record_decl in
              let () = try_unify state ~origin expected_ty owner_ty in
              let missing_fields =
                if open_ then
                  []
                else
                  record_decl_fields record_decl
                  |> List.filter
                    (fun label_name ->
                      not (List.exists (record_label_matches label_name) field_names))
              in
              let () =
                if not (List.is_empty missing_fields) then
                  add_record_resolution_error
                    state
                    ~span:(diagnostic_span origin)
                    ~context:Typ_diagnostic.RecordPattern (Typ_diagnostic.MissingRecordFields missing_fields)
              in
              fields |> List.map
                (fun (field: BodyArena.record_pattern_field) ->
                  let field_ty =
                    match record_field_type field_types field.label with
                    | Some field_ty -> field_ty
                    | None -> fresh_hole state
                  in
                  bind_pattern state env field.pattern_id field_ty) |> List.flatten
          | None -> fields
          |> List.map
            (fun (field: BodyArena.record_pattern_field) ->
              bind_pattern state env field.pattern_id (fresh_hole state))
          |> List.flatten
        )
      | BodyArena.PList elements ->
          let element_ty = fresh_var state in
          let () = try_unify
            state
            ~origin:(origin_of_pattern state pat_id)
            expected_ty
            (TypeRepr.List element_ty) in
          elements
          |> List.map (fun element_id -> bind_pattern state env element_id element_ty)
          |> List.flatten
      | BodyArena.PAlias { pattern_id; alias } ->
          let bindings = bind_pattern state env pattern_id expected_ty in
          (alias, TypeScheme.Forall ([], expected_ty)) :: bindings
      | BodyArena.PPolyVariant { payload; _ } -> (
          match payload with
          | Some payload_id -> bind_pattern state env payload_id (fresh_hole state)
          | None -> []
        )
      | BodyArena.PUnsupported _ ->
          []
    )

let record_expr_trace = fun (state: state) expr_id origin_id env_before inferred_type ->
  state.expr_traces <- (
    { Check_result.expr_id; origin_id; env_before = render_env env_before; inferred_type }:
      Check_result.expr_trace
  )
  :: state.expr_traces

let add_application_label_mismatch = fun (state: state) ~expr_id ~expected_label arguments ->
  add_diagnostic
    state
    (Typ_diagnostic.ApplicationLabelMismatch {
      application_span = diagnostic_span (origin_of_expr state expr_id);
      expected_label = diagnostic_label_of_type_label expected_label;
      actual_labels = List.map
        (fun (argument: BodyArena.apply_argument) -> diagnostic_label_of_body_label argument.label)
        arguments
    })

let add_application_non_function = fun (state: state) ~expr_id current_ty ->
  add_diagnostic
    state
    (Typ_diagnostic.TypeMismatch {
      mismatch_span = diagnostic_span (origin_of_expr state expr_id);
      mismatch = Typ_diagnostic.ExpectedActual {
        expected = "function";
        actual = TypePrinter.type_to_string current_ty
      }
    })

let rec infer_match_case = fun (state: state) env scrutinee_ty result_ty (case: BodyArena.match_case) ->
  let bindings = bind_pattern state env case.pattern_id scrutinee_ty in
  let case_env = bind_env env bindings in
  let () =
    match case.guard_id with
    | Some guard_id ->
        let guard_ty = infer_expr state case_env guard_id in
        try_unify state ~origin:(origin_of_expr state guard_id) guard_ty TypeRepr.Bool
    | None -> ()
  in
  let case_ty = infer_expr state case_env case.body_id in
  try_unify state ~origin:(origin_of_expr state case.body_id) result_ty case_ty

and infer_record_expr = fun (state: state) env expr_id base_id fields ->
  let operation_span = diagnostic_span (origin_of_expr state expr_id) in
  let base_ty =
    match base_id with
    | Some base_id -> Some (infer_expr state env base_id)
    | None -> None
  in
  let field_names =
    List.map (fun (field: BodyArena.record_expr_field) -> field.label) fields
  in
  let context =
    match base_id with
    | Some _ -> Typ_diagnostic.RecordUpdate
    | None -> Typ_diagnostic.RecordConstruction
  in
  match
    resolve_record_decl state ~field_names
      ~owner_hint:((
        match base_ty with
        | Some base_ty -> owner_name_of_type base_ty
        | None -> None
      ))
      ~span:operation_span
      ~context
  with
  | Some record_decl ->
      let (owner_ty, field_types) = instantiate_record_decl state record_decl in
      let () =
        match base_id, base_ty with
        | Some base_id, Some base_ty -> try_unify
          state
          ~origin:(origin_of_expr state base_id)
          base_ty
          owner_ty
        | _ -> ()
      in
      let missing_fields =
        match base_id with
        | Some _ -> []
        | None -> record_decl_fields record_decl
        |> List.filter
          (fun label_name -> not (List.exists (record_label_matches label_name) field_names))
      in
      let () =
        if not (List.is_empty missing_fields) then
          add_record_resolution_error
            state
            ~span:operation_span
            ~context
            (Typ_diagnostic.MissingRecordFields missing_fields)
      in
      let () =
        List.iter
          (fun (field: BodyArena.record_expr_field) ->
            let field_ty =
              match record_field_type field_types field.label with
              | Some field_ty -> field_ty
              | None -> fresh_hole state
            in
            let inferred_field_ty = infer_expr state env field.value_id in
            try_unify state ~origin:(origin_of_expr state field.value_id) field_ty inferred_field_ty)
          fields
      in
      owner_ty
  | None ->
      let () =
        List.iter
          (fun (field: BodyArena.record_expr_field) ->
            let _ = infer_expr state env field.value_id in
            ())
          fields
      in
      fresh_hole state

and infer_expr = fun (state: state) env expr_id ->
  match SemanticTree.find_expr state.file expr_id with
  | None -> fresh_hole state
  | Some expr ->
      let inferred_type =
        match expr.desc with
        | BodyArena.EVar name -> (
            match env_lookup env name with
            | Some (_, scheme) -> instantiate state scheme
            | None ->
                let hole = fresh_hole state in
                let () = add_diagnostic
                  state
                  (Typ_diagnostic.UnboundName {
                    reference_span = diagnostic_span (origin_of_expr state expr_id);
                    name
                  }) in
                hole
          )
        | BodyArena.EInt _ ->
            TypeRepr.Int
        | BodyArena.EFloat _ ->
            TypeRepr.Float
        | BodyArena.EBool _ ->
            TypeRepr.Bool
        | BodyArena.EString _ ->
            TypeRepr.String
        | BodyArena.EChar _ ->
            TypeRepr.Char
        | BodyArena.EUnit ->
            TypeRepr.Unit
        | BodyArena.ETuple elements ->
            TypeRepr.Tuple (List.map (infer_expr state env) elements)
        | BodyArena.EArray elements ->
            let element_ty = fresh_var state in
            let () =
              List.iter
                (fun element_id ->
                  let inferred_element = infer_expr state env element_id in
                  try_unify state ~origin:(origin_of_expr state element_id) element_ty inferred_element)
                elements
            in
            TypeRepr.Array element_ty
        | BodyArena.ESequence elements -> (
            match List.rev elements with
            | [] -> TypeRepr.Unit
            | last_id :: reversed_prefix ->
                let prefix = List.rev reversed_prefix in
                let () =
                  List.iter
                    (fun element_id ->
                      let element_ty = infer_expr state env element_id in
                      try_unify state ~origin:(origin_of_expr state element_id) element_ty TypeRepr.Unit)
                    prefix
                in
                infer_expr state env last_id
          )
        | BodyArena.EFun (parameters, body_id) ->
            let rec lower_parameters env = function
              | [] -> infer_expr state env body_id
              | (parameter: BodyArena.function_parameter) :: rest ->
                  let arg_ty = fresh_var state in
                  let bindings = bind_pattern state env parameter.pattern_id arg_ty in
                  let body_ty = lower_parameters (bind_env env bindings) rest in
                  TypeRepr.Arrow {
                    label = type_label_of_body_label parameter.label;
                    lhs = arg_ty;
                    rhs = body_ty
                  }
            in
            lower_parameters env parameters
        | BodyArena.EApply (callee_id, arguments) ->
            let callee_ty = infer_expr state env callee_id in
            let rec apply_with_known_type current_ty arguments =
              match arguments with
              | [] -> current_ty
              | _ -> (
                  match TypeRepr.prune current_ty with
                  | TypeRepr.Arrow { label; lhs; rhs } -> (
                      match take_matching_argument label arguments with
                      | Some ((argument: BodyArena.apply_argument), rest_arguments) ->
                          let argument_ty = infer_expr_against state env argument.value_id lhs in
                          let () = try_unify
                            state
                            ~origin:(origin_of_expr state argument.value_id)
                            lhs
                            argument_ty in
                          apply_with_known_type rhs rest_arguments
                      | None -> (
                          match label with
                          | TypeRepr.Optional _ -> apply_with_known_type rhs arguments
                          | _ ->
                              let () = add_application_label_mismatch
                                state
                                ~expr_id
                                ~expected_label:label
                                arguments in
                              fresh_hole state
                        )
                    )
                  | TypeRepr.Var _
                  | TypeRepr.Hole _ ->
                      apply_from_unknown_type current_ty arguments
                  | ty ->
                      let () = add_application_non_function state ~expr_id ty in
                      fresh_hole state
                )
            and apply_from_unknown_type current_ty arguments =
              match arguments with
              | [] -> current_ty
              | (argument: BodyArena.apply_argument) :: rest ->
                  let argument_ty = infer_expr state env argument.value_id in
                  let result_ty = fresh_var state in
                  let () = try_unify
                    state
                    ~origin:(origin_of_expr state expr_id)
                    current_ty
                    (TypeRepr.Arrow {
                      label = type_label_of_body_label argument.label;
                      lhs = argument_ty;
                      rhs = result_ty
                    }) in
                  apply_with_known_type result_ty rest
            in
            apply_with_known_type callee_ty arguments
        | BodyArena.ERecord { base_id; fields } ->
            infer_record_expr state env expr_id base_id fields
        | BodyArena.EFieldAccess { receiver_id; label } ->
            let receiver_ty = infer_expr state env receiver_id in
            let field_names = [ label ] in
            begin
              match resolve_record_decl
                state
                ~field_names
                ~owner_hint:(owner_name_of_type receiver_ty)
                ~span:(diagnostic_span (origin_of_expr state expr_id))
                ~context:Typ_diagnostic.RecordFieldAccess with
              | Some record_decl ->
                  let (owner_ty, field_types) = instantiate_record_decl state record_decl in
                  let () = try_unify state ~origin:(origin_of_expr state receiver_id) receiver_ty owner_ty in
                  begin
                    match record_field_type field_types label with
                    | Some field_ty -> field_ty
                    | None -> fresh_hole state
                  end
              | None -> fresh_hole state
            end
        | BodyArena.EIndex (collection_id, index_id) ->
            let collection_ty = infer_expr state env collection_id in
            let index_ty = infer_expr state env index_id in
            let element_ty = fresh_var state in
            let () = try_unify state ~origin:(origin_of_expr state index_id) index_ty TypeRepr.Int in
            begin
              match TypeRepr.prune collection_ty with
              | TypeRepr.String -> TypeRepr.Char
              | _ ->
                  let () = try_unify
                    state
                    ~origin:(origin_of_expr state collection_id)
                    collection_ty
                    (TypeRepr.Array element_ty) in
                  element_ty
            end
        | BodyArena.ELet (binding_ids, body_id) ->
            let env = infer_binding_group state env binding_ids in
            infer_expr state env body_id
        | BodyArena.EIf (condition_id, then_id, else_id) ->
            let condition_ty = infer_expr state env condition_id in
            let () = try_unify
              state
              ~origin:(origin_of_expr state condition_id)
              condition_ty
              TypeRepr.Bool in
            let then_ty = infer_expr state env then_id in
            let else_ty = infer_expr state env else_id in
            let () = try_unify state ~origin:(origin_of_expr state expr_id) then_ty else_ty in
            then_ty
        | BodyArena.EMatch (scrutinee_id, cases) ->
            let scrutinee_ty = infer_expr state env scrutinee_id in
            let result_ty = fresh_var state in
            let () = List.iter (infer_match_case state env scrutinee_ty result_ty) cases in
            result_ty
        | BodyArena.ETry (body_id, cases) ->
            let body_ty = infer_expr state env body_id in
            let exn_ty = TypeRepr.Named { name = "exn"; arguments = [] } in
            let result_ty = fresh_var state in
            let () = try_unify state ~origin:(origin_of_expr state body_id) result_ty body_ty in
            let () = List.iter (infer_match_case state env exn_ty result_ty) cases in
            result_ty
        | BodyArena.EPolyVariant { payload; _ } ->
            let () =
              match payload with
              | Some payload_id ->
                  let _ = infer_expr state env payload_id in
                  ()
              | None -> ()
            in
            fresh_hole state
        | BodyArena.ELocalOpen { module_path; body_id } ->
            infer_expr state (env_with_local_open env module_path) body_id
        | BodyArena.EUnsupported summary ->
            let hole = fresh_hole state in
            let () = add_diagnostic
              state
              (Typ_diagnostic.UnsupportedSemanticExpression {
                expression_span = diagnostic_span (origin_of_expr state expr_id);
                summary
              }) in
            hole
        | BodyArena.EHole _ ->
            fresh_hole state
      in
      let () = record_expr_trace state expr_id expr.origin_id env inferred_type in
      inferred_type

and infer_expr_against = fun (state: state) env expr_id expected_ty ->
  match SemanticTree.find_expr state.file expr_id with
  | Some expr -> (
      match expr.desc with
      | BodyArena.EVar name -> (
          match origin_of_expr state expr_id with
          | Some origin when String.equal origin.label "constructor_expression"
          || String.equal origin.label "constructor_path_expression" -> (
              match resolve_constructor_scheme env name ~expected_ty with
              | Some scheme ->
                  let inferred_type = instantiate state scheme in
                  let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
                  inferred_type
              | None ->
                  let inferred_type = infer_expr state env expr_id in
                  let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
                  inferred_type
            )
          | _ ->
              let inferred_type = infer_expr state env expr_id in
              let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
              inferred_type
        )
      | BodyArena.EApply (callee_id, arguments) -> (
          match origin_of_expr state expr_id with
          | Some origin when String.equal origin.label "constructor_apply_expression" -> (
              match SemanticTree.find_expr state.file callee_id with
              | Some { desc=BodyArena.EVar constructor; _ } -> (
                  match resolve_constructor_scheme env constructor ~expected_ty with
                  | Some scheme ->
                      let callee_ty = instantiate state scheme in
                      let rec apply_with_known_type current_ty arguments =
                        match arguments with
                        | [] ->
                            let () = try_unify
                              state
                              ~origin:(origin_of_expr state expr_id)
                              expected_ty
                              current_ty in
                            current_ty
                        | (argument: BodyArena.apply_argument) :: rest -> (
                            match TypeRepr.prune current_ty with
                            | TypeRepr.Arrow { label; lhs; rhs } ->
                                if argument_matches_parameter_label label argument.label then
                                  let argument_ty = infer_expr_against state env argument.value_id lhs in
                                  let () = try_unify
                                    state
                                    ~origin:(origin_of_expr state argument.value_id)
                                    lhs
                                    argument_ty in
                                  apply_with_known_type rhs rest
                                else
                                  let inferred_type = infer_expr state env expr_id in
                                  let () = try_unify
                                    state
                                    ~origin:(origin_of_expr state expr_id)
                                    expected_ty
                                    inferred_type in
                                  inferred_type
                            | _ ->
                                let inferred_type = infer_expr state env expr_id in
                                let () = try_unify
                                  state
                                  ~origin:(origin_of_expr state expr_id)
                                  expected_ty
                                  inferred_type in
                                inferred_type
                          )
                      in
                      apply_with_known_type callee_ty arguments
                  | None ->
                      let inferred_type = infer_expr state env expr_id in
                      let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
                      inferred_type
                )
              | _ ->
                  let inferred_type = infer_expr state env expr_id in
                  let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
                  inferred_type
            )
          | _ ->
              let inferred_type = infer_expr state env expr_id in
              let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
              inferred_type
        )
      | _ ->
          let inferred_type = infer_expr state env expr_id in
          let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
          inferred_type
    )
  | None -> fresh_hole state

and infer_binding_group = fun (state: state) env binding_ids ->
  let bindings = binding_ids |> List.filter_map (SemanticTree.find_binding state.file) in
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
        let bound_entries = bind_pattern state env binding.pattern_id value_ty in
        let generalized = bound_entries
        |> List.map (fun (name, TypeScheme.Forall (_, ty)) -> (name, generalize env ty)) in
        (binding, generalized))
      bindings
  in
  List.fold_left (fun env (_, entries) -> bind_env env entries) env inferred_bindings

and infer_recursive_group = fun (state: state) env bindings ->
  let unsupported_bindings =
    List.filter
      (fun (binding: BodyArena.binding) -> not (is_recursive_binding_supported state binding))
      bindings
  in
  if List.is_empty unsupported_bindings then
    let placeholders =
      bindings
      |> List.map (fun (binding: BodyArena.binding) -> (binding, fresh_var state))
      |> List.filter_map
        (fun ((binding: BodyArena.binding), ty) ->
          match binding.name with
          | Some name -> Some (name, ty)
          | None -> None)
    in
    let provisional_env = placeholders
    |> List.map (fun (name, ty) -> (name, TypeScheme.Forall ([], ty)))
    |> bind_env env in
    let () =
      List.iter2
        (fun (binding: BodyArena.binding) placeholder_ty ->
          let value_ty = infer_expr state provisional_env binding.value_id in
          try_unify state ~origin:(origin_of_binding state binding) placeholder_ty value_ty)
        bindings
        (placeholders |> List.map snd)
    in
    let generalized = placeholders |> List.map (fun (name, ty) -> (name, generalize env ty)) in
    bind_env env generalized
  else
    let () =
      List.iter
        (fun (binding: BodyArena.binding) ->
          add_diagnostic
            state
            (Typ_diagnostic.RecursiveGroupRequiresSimpleVariableBinders {
              binding_span = diagnostic_span (origin_of_binding state binding)
            }))
        unsupported_bindings
    in
    List.fold_left
      (fun env (binding: BodyArena.binding) ->
        let placeholder = fresh_hole state in
        let bound_entries = bind_pattern state env binding.pattern_id placeholder in
        bind_env env bound_entries)
      env
      bindings

let prelude_names = fun (config: TypConfig.t) -> config.prelude |> List.map fst

let ambient_names = fun (config: TypConfig.t) -> config.ambient |> List.map fst

let export_env = fun config env ->
  let hidden_names = prelude_names config @ ambient_names config in
  render_env env |> List.filter (fun (name, _) -> not (List.mem name hidden_names))

let export_env_with_forced_names = fun (state: state) env ->
  let hidden_names = prelude_names state.config @ ambient_names state.config in
  render_env env
  |> List.filter
    (fun (name, _) -> not (List.mem name hidden_names) || List.mem name state.forced_export_names)

let introduced_entries = fun before after ->
  let introduced = introduced_names before after in
  render_env after |> List.filter
    (fun (name, _) ->
      List.mem name introduced)

let qualify_entries = fun scope_path entries ->
  List.map (fun (name, scheme) -> (qualify_name scope_path name, scheme)) entries

let scope_key = fun scope_path ->
  String.concat "." scope_path

let scope_prefix_keys = fun scope_path ->
  let rec loop acc current = function
    | [] -> List.rev acc
    | segment :: rest ->
        let current = current @ [ segment ] in
        loop (scope_key current :: acc) current rest
  in
  loop [ scope_key [] ] [] scope_path

let scope_locals_for = fun scope_entries scope_path ->
  scope_prefix_keys scope_path |> List.fold_left
    (fun acc key ->
      match List.assoc_opt key scope_entries with
      | Some entries -> bind_env acc entries
      | None -> acc)
    []

let update_scope_entries = fun scope_entries scope_path entries ->
  let key = scope_key scope_path in
  let existing =
    match List.assoc_opt key scope_entries with
    | Some entries -> entries
    | None -> []
  in
  let updated = bind_env existing entries in
  (key, updated) :: List.remove_assoc key scope_entries

let scope_opens_for = fun scope_opens scope_path ->
  scope_prefix_keys scope_path |> List.fold_left
    (fun acc key ->
      match List.assoc_opt key scope_opens with
      | Some modules -> acc @ modules
      | None -> acc)
    []

let update_scope_opens = fun scope_opens scope_path module_path ->
  let key = scope_key scope_path in
  let existing =
    match List.assoc_opt key scope_opens with
    | Some modules -> modules
    | None -> []
  in
  let updated = existing @ [ module_path ] in
  (key, updated) :: List.remove_assoc key scope_opens

let env_for_item_scope = fun export_env scope_entries scope_opens scope_path ->
  let locals = scope_locals_for scope_entries scope_path in
  let base_env = bind_env export_env locals in
  scope_opens_for scope_opens scope_path |> List.fold_left env_with_local_open base_env

let infer_file = fun ~config file ->
  let state = make_state ~config file in
  let initial_env = bind_env config.prelude config.ambient in
  let rec loop export_state type_decls scope_entries scope_opens = function
    | [] -> (export_state, type_decls)
    | item :: rest -> (
        match item with
        | ItemTree.Type type_item ->
            let visible_exports_before = export_env config export_state in
            let introduced = TypeDecl.constructor_entries type_item.declaration in
            let introduced_type_decls = [ {
              FileSummary.scope_path = type_item.scope_path;
              declaration = type_item.declaration;
            } ] in
            let () =
              match type_item.declaration.labels with
              | [] -> ()
              | labels -> state.record_types <- {
                owner_name = qualify_name type_item.scope_path type_item.declaration.type_name;
                param_ids = type_item.declaration.param_ids;
                labels
              }
              :: state.record_types
            in
            let (export_state, scope_entries) =
              match type_item.scope_path with
              | [] -> (bind_env export_state introduced, scope_entries)
              | scope_path -> (
                bind_env export_state (qualify_entries scope_path introduced),
                update_scope_entries scope_entries scope_path introduced
              )
            in
            let exports_after = export_env config export_state in
            let binding_names = introduced_names visible_exports_before exports_after in
            let () =
              state.item_traces <- (
                { Check_result.item_id = type_item.item_id; binding_names; exports_after }:
                  Check_result.item_trace
              )
              :: state.item_traces
            in
            loop
              export_state
              (bind_type_decls type_decls introduced_type_decls)
              scope_entries
              scope_opens
              rest
        | ItemTree.Exception exception_item ->
            let visible_exports_before = export_env config export_state in
            let introduced = [ (exception_item.exception_name, exception_item.scheme) ] in
            let (export_state, scope_entries) =
              match exception_item.scope_path with
              | [] -> (bind_env export_state introduced, scope_entries)
              | scope_path -> (
                bind_env export_state (qualify_entries scope_path introduced),
                update_scope_entries scope_entries scope_path introduced
              )
            in
            let exports_after = export_env config export_state in
            let binding_names = introduced_names visible_exports_before exports_after in
            let () =
              state.item_traces <- (
                { Check_result.item_id = exception_item.item_id; binding_names; exports_after }:
                  Check_result.item_trace
              )
              :: state.item_traces
            in
            loop export_state type_decls scope_entries scope_opens rest
        | ItemTree.Value value_item ->
            let visible_exports_before = export_env config export_state in
            let item_env = env_for_item_scope export_state scope_entries scope_opens value_item.scope_path in
            let env_after_item = infer_binding_group state item_env value_item.binding_ids in
            let introduced = introduced_entries item_env env_after_item in
            let (export_state, scope_entries) =
              match value_item.scope_path with
              | [] -> (env_after_item, scope_entries)
              | scope_path -> (
                bind_env export_state (qualify_entries scope_path introduced),
                update_scope_entries scope_entries scope_path introduced
              )
            in
            let exports_after = export_env config export_state in
            let binding_names = introduced_names visible_exports_before exports_after in
            let () =
              state.item_traces <- (
                { Check_result.item_id = value_item.item_id; binding_names; exports_after }:
                  Check_result.item_trace
              )
              :: state.item_traces
            in
            loop export_state type_decls scope_entries scope_opens rest
        | ItemTree.Open open_item ->
            let scope_opens = update_scope_opens scope_opens open_item.scope_path open_item.module_path in
            let exports_after = export_env config export_state in
            let () =
              state.item_traces <- (
                { Check_result.item_id = open_item.item_id; binding_names = []; exports_after }:
                  Check_result.item_trace
              )
              :: state.item_traces
            in
            loop export_state type_decls scope_entries scope_opens rest
        | ItemTree.Include include_item ->
            let visible_exports_before = export_env config export_state in
            let item_env = env_for_item_scope export_state scope_entries scope_opens include_item.scope_path in
            let introduced = entries_for_include item_env include_item.module_path in
            let visible_type_decls = bind_type_decls config.ambient_type_decls type_decls in
            let introduced_type_decls = type_decls_for_include visible_type_decls include_item.module_path
            |> prefix_type_decls include_item.scope_path in
            let (export_state, scope_entries) =
              match include_item.scope_path with
              | [] -> (bind_env export_state introduced, scope_entries)
              | scope_path -> (
                bind_env export_state (qualify_entries scope_path introduced),
                update_scope_entries scope_entries scope_path introduced
              )
            in
            let exports_after = export_env config export_state in
            let binding_names = introduced_names visible_exports_before exports_after in
            let () =
              state.item_traces <- (
                { Check_result.item_id = include_item.item_id; binding_names; exports_after }:
                  Check_result.item_trace
              )
              :: state.item_traces
            in
            loop
              export_state
              (bind_type_decls type_decls introduced_type_decls)
              scope_entries
              scope_opens
              rest
        | ItemTree.ModuleAlias module_alias_item ->
            let visible_exports_before = export_env_with_forced_names state export_state in
            let item_env = env_for_item_scope
              export_state
              scope_entries
              scope_opens
              module_alias_item.scope_path in
            let alias_export_names = export_names_for_module_alias
              item_env
              ~alias_name:module_alias_item.alias_name
              ~module_path:module_alias_item.module_path in
            let introduced = entries_for_module_alias
              item_env
              ~alias_name:module_alias_item.alias_name
              ~module_path:module_alias_item.module_path in
            let visible_type_decls = bind_type_decls config.ambient_type_decls type_decls in
            let introduced_type_decls = type_decls_for_module_alias
              visible_type_decls
              ~alias_name:module_alias_item.alias_name
              ~module_path:module_alias_item.module_path
            |> prefix_type_decls module_alias_item.scope_path in
            let (export_state, scope_entries) =
              match module_alias_item.scope_path with
              | [] -> (bind_env export_state introduced, scope_entries)
              | scope_path -> (
                bind_env export_state (qualify_entries scope_path introduced),
                update_scope_entries scope_entries scope_path introduced
              )
            in
            let forced_export_names =
              match module_alias_item.scope_path with
              | [] -> alias_export_names
              | scope_path -> List.map (qualify_name scope_path) alias_export_names
            in
            let () =
              state.forced_export_names <- forced_export_names @ state.forced_export_names
            in
            let exports_after = export_env_with_forced_names state export_state in
            let binding_names = introduced_names visible_exports_before exports_after in
            let () =
              state.item_traces <- (
                { Check_result.item_id = module_alias_item.item_id; binding_names; exports_after }:
                  Check_result.item_trace
              )
              :: state.item_traces
            in
            loop
              export_state
              (bind_type_decls type_decls introduced_type_decls)
              scope_entries
              scope_opens
              rest
        | ItemTree.Unsupported unsupported_item ->
            let exports_after = export_env config export_state in
            let () =
              state.item_traces <- (
                {
                  Check_result.item_id = unsupported_item.item_id;
                  binding_names = [];
                  exports_after
                }:
                  Check_result.item_trace
              )
              :: state.item_traces
            in
            loop export_state type_decls scope_entries scope_opens rest
      )
  in
  let (exports, type_decls) = loop initial_env [] [] [] (ItemTree.items file.item_tree) in
  {
    exports = export_env_with_forced_names state exports;
    type_decls;
    item_traces = List.rev state.item_traces;
    expr_traces = List.rev state.expr_traces;
    diagnostics = List.rev state.diagnostics
  }

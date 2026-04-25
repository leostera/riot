open Std
open Std.Collections
module TypAst = Ast
module SurfacePath = Model.Surface_path
module BindingId = Model.Binding_id
module EntityId = Model.Entity_id

type ty =
  | TInt
  | TBool
  | TChar
  | TString
  | TFloat
  | TUnit
  | TList of ty
  | TOption of ty
  | TTuple of ty list
  | TArrow of ty * ty
  | TVar of tyvar_cell

and tyvar_cell = {
  mutable var: tvar;
}

and tvar =
  | Unbound of int * int
  | Link of ty
  | Generic of int

type binding = {
  binding_id: BindingId.t;
  entity_id: EntityId.t;
  ty: ty;
}

type env = binding list

type state = {
  mutable next_tyvar: int;
  mutable next_binding_stamp: int;
  mutable diagnostics: Diagnostics.Diagnostic.t list;
}

let unsupported_syntax = fun origin summary ->
  Diagnostics.Diagnostic.UnsupportedSyntax { span = origin.TypAst.span; kind = origin.kind; summary }

let unsupported_type = fun origin summary ->
  Diagnostics.Diagnostic.UnsupportedType { span = origin.TypAst.span; summary }

let add_diagnostic = fun state diagnostic -> state.diagnostics <- diagnostic :: state.diagnostics

let make_state = fun ~next_binding_stamp -> { next_tyvar = 0; next_binding_stamp; diagnostics = [] }

let fresh_tyvar = fun state ~level ->
  let id = state.next_tyvar in
  state.next_tyvar <- state.next_tyvar + 1;
  TVar { var = Unbound (id, level) }

let fresh_binding_id = fun state ~name ->
  let stamp = state.next_binding_stamp in
  state.next_binding_stamp <- stamp + 1;
  BindingId.local ~stamp ~name

let make_binding = fun state ~name ~ty ->
  let binding_id = fresh_binding_id state ~name in
  let entity_id = EntityId.resolved ~binding_id ~surface_path:name in
  { binding_id; entity_id; ty }

let rec prune = function
  | TVar ({ var=Link linked_ty } as cell) ->
      let linked_ty = prune linked_ty in
      cell.var <- Link linked_ty;
      linked_ty
  | ty -> ty

let rec string_of_ty = fun ty ->
  match prune ty with
  | TInt -> "int"
  | TBool -> "bool"
  | TChar -> "char"
  | TString -> "string"
  | TFloat -> "float"
  | TUnit -> "unit"
  | TList element -> string_of_ty element ^ " list"
  | TOption element -> string_of_ty element ^ " option"
  | TTuple elements -> elements |> List.map ~fn:string_of_ty |> String.concat " * "
  | TArrow (parameter, result) -> string_of_ty parameter ^ " -> " ^ string_of_ty result
  | TVar { var=Unbound (id, _) } -> "'_" ^ Int.to_string id
  | TVar { var=Generic id } -> "'a" ^ Int.to_string id
  | TVar { var=Link linked_ty } -> string_of_ty linked_ty

exception Occurs

let rec occurs_adjust_levels = fun id level ty ->
  match prune ty with
  | TVar ({ var=Unbound (other_id, other_level) } as cell) ->
      if Int.equal id other_id then
        raise Occurs;
      if other_level > level then
        cell.var <- Unbound (other_id, level)
  | TVar { var=Generic _ } ->
      ()
  | TList element ->
      occurs_adjust_levels id level element
  | TOption element ->
      occurs_adjust_levels id level element
  | TTuple elements ->
      List.for_each elements ~fn:(occurs_adjust_levels id level)
  | TArrow (parameter, result) ->
      occurs_adjust_levels id level parameter;
      occurs_adjust_levels id level result
  | TInt
  | TBool
  | TChar
  | TString
  | TFloat
  | TUnit ->
      ()
  | TVar { var=Link linked_ty } ->
      occurs_adjust_levels id level linked_ty

let rec unify = fun state ~at left right ->
  match prune left, prune right with
  | TVar left_cell, TVar right_cell when Ptr.equal left_cell right_cell ->
      ()
  | (TInt, TInt)
  | (TBool, TBool)
  | (TChar, TChar)
  | (TString, TString)
  | (TFloat, TFloat)
  | (TUnit, TUnit) ->
      ()
  | TList left, TList right ->
      unify state ~at left right
  | TOption left, TOption right ->
      unify state ~at left right
  | TTuple left, TTuple right ->
      if Int.equal (List.length left) (List.length right) then
        List.zip left right |> List.for_each ~fn:(fun (left, right) -> unify state ~at left right)
      else
        add_diagnostic
          state
          (unsupported_type
            at
            ("tuple arity mismatch: expected "
            ^ Int.to_string (List.length left)
            ^ " but got "
            ^ Int.to_string (List.length right)))
  | TArrow (left_parameter, left_result), TArrow (right_parameter, right_result) ->
      unify state ~at left_parameter right_parameter;
      unify state ~at left_result right_result
  | (TVar ({ var=Unbound (id, level) } as cell), ty)
  | (ty, TVar ({ var=Unbound (id, level) } as cell)) -> (
      try
        occurs_adjust_levels id level ty;
        cell.var <- Link ty
      with
      | Occurs -> add_diagnostic state (unsupported_type at "occurs check failed")
    )
  | (TVar { var=Generic _ }, _)
  | (_, TVar { var=Generic _ }) ->
      add_diagnostic state (unsupported_type at "unexpected generic type variable")
  | left, right ->
      add_diagnostic
        state
        (unsupported_type at ("type mismatch: " ^ string_of_ty left ^ " vs " ^ string_of_ty right))

let rec generalize = fun level ty ->
  match prune ty with
  | TVar ({ var=Unbound (id, other_level) } as cell) when other_level > level ->
      cell.var <- Generic id;
      TVar cell
  | TList element ->
      TList (generalize level element)
  | TOption element ->
      TOption (generalize level element)
  | TTuple elements ->
      TTuple (List.map elements ~fn:(generalize level))
  | TArrow (parameter, result) ->
      TArrow (generalize level parameter, generalize level result)
  | ty ->
      ty

let instantiate = fun state ~level ty ->
  let subst = ref [] in
  let rec loop ty =
    match prune ty with
    | TVar { var=Generic id } -> (
        match
          List.find !subst
            ~fn:(fun (other_id, _) ->
              Int.equal id other_id)
        with
        | Some (_, replacement) -> replacement
        | None ->
            let replacement = fresh_tyvar state ~level in
            subst := (id, replacement) :: !subst;
            replacement
      )
    | TList element ->
        TList (loop element)
    | TOption element ->
        TOption (loop element)
    | TTuple elements ->
        TTuple (List.map elements ~fn:loop)
    | TArrow (parameter, result) ->
        TArrow (loop parameter, loop result)
    | ty ->
        ty
  in
  loop ty

let path_int = SurfacePath.of_name "int"

let path_bool = SurfacePath.of_name "bool"

let path_char = SurfacePath.of_name "char"

let path_string = SurfacePath.of_name "string"

let path_float = SurfacePath.of_name "float"

let path_unit = SurfacePath.of_name "unit"

let path_list = SurfacePath.of_name "list"

let path_option = SurfacePath.of_name "option"

let path_none = SurfacePath.of_name "None"

let path_some = SurfacePath.of_name "Some"

let path_not = SurfacePath.of_name "not"

let path_plus = SurfacePath.of_name "+"

let path_minus = SurfacePath.of_name "-"

let path_star = SurfacePath.of_name "*"

let path_slash = SurfacePath.of_name "/"

let path_plus_dot = SurfacePath.of_name "+."

let path_minus_dot = SurfacePath.of_name "-."

let path_star_dot = SurfacePath.of_name "*."

let path_slash_dot = SurfacePath.of_name "/."

type builtin = {
  path: SurfacePath.t;
  ty: ty;
}

let generic_var = fun id -> TVar { var = Generic id }

let builtin_bindings = [
  { path = path_none; ty = TOption (generic_var 0) };
  { path = path_some; ty = TArrow (generic_var 0, TOption (generic_var 0)) };
  { path = path_not; ty = TArrow (TBool, TBool) };
  { path = path_plus; ty = TArrow (TInt, TArrow (TInt, TInt)) };
  { path = path_minus; ty = TArrow (TInt, TArrow (TInt, TInt)) };
  { path = path_star; ty = TArrow (TInt, TArrow (TInt, TInt)) };
  { path = path_slash; ty = TArrow (TInt, TArrow (TInt, TInt)) };
  { path = path_plus_dot; ty = TArrow (TFloat, TArrow (TFloat, TFloat)) };
  { path = path_minus_dot; ty = TArrow (TFloat, TArrow (TFloat, TFloat)) };
  { path = path_star_dot; ty = TArrow (TFloat, TArrow (TFloat, TFloat)) };
  { path = path_slash_dot; ty = TArrow (TFloat, TArrow (TFloat, TFloat)) };
]

let rec lookup_builtin = fun path builtins ->
  match builtins with
  | [] -> None
  | builtin :: rest ->
      if SurfacePath.equal builtin.path path then
        Some builtin.ty
      else
        lookup_builtin path rest

let rec public_type_of_ty = fun vars ty ->
  match prune ty with
  | TInt -> Typing_context.Int
  | TBool -> Typing_context.Bool
  | TChar -> Typing_context.Char
  | TString -> Typing_context.String
  | TFloat -> Typing_context.Float
  | TUnit -> Typing_context.Unit
  | TList element -> Typing_context.List (public_type_of_ty vars element)
  | TOption element -> Typing_context.Option (public_type_of_ty vars element)
  | TTuple elements -> Typing_context.Tuple (List.map elements ~fn:(public_type_of_ty vars))
  | TArrow (parameter, result) -> Typing_context.Arrow {
    parameter = public_type_of_ty vars parameter;
    result = public_type_of_ty vars result
  }
  | TVar { var=Generic id } -> Typing_context.Var (public_tyvar_id vars id)
  | TVar { var=Unbound (id, _) } -> Typing_context.Var (public_tyvar_id vars id)
  | TVar { var=Link linked_ty } -> public_type_of_ty vars linked_ty

and public_tyvar_id = fun vars id ->
  match
    List.find !vars
      ~fn:(fun (other_id, _) ->
        Int.equal id other_id)
  with
  | Some (_, public_id) -> public_id
  | None ->
      let public_id = List.length !vars in
      vars := (id, public_id) :: !vars;
      public_id

let public_scheme_of_ty = fun ty ->
  let vars = ref [] in
  let body = public_type_of_ty vars ty in
  let forall = !vars |> List.map ~fn:(fun (_, public_id) -> public_id) |> List.reverse in
  { Typing_context.forall; body }

let public_binding_of_binding = fun binding ->
  {
    Typing_context.binding_id = binding.binding_id;
    entity_id = binding.entity_id;
    scheme = public_scheme_of_ty binding.ty
  }

let import_scheme = fun scheme ->
  let rec loop type_expr =
    match type_expr with
    | Typing_context.Int -> TInt
    | Typing_context.Bool -> TBool
    | Typing_context.Char -> TChar
    | Typing_context.String -> TString
    | Typing_context.Float -> TFloat
    | Typing_context.Unit -> TUnit
    | Typing_context.List element -> TList (loop element)
    | Typing_context.Option element -> TOption (loop element)
    | Typing_context.Tuple elements -> TTuple (List.map elements ~fn:loop)
    | Typing_context.Arrow { parameter; result } -> TArrow (loop parameter, loop result)
    | Typing_context.Var id -> TVar { var = Generic id }
  in
  let _ = scheme.Typing_context.forall in
  loop scheme.body

let env_of_typing_context = fun typing_context ->
  List.fold_left
    typing_context.Typing_context.values
    ~init:[]
    ~fn:(fun env (value_binding: Typing_context.value_binding) ->
      {
        binding_id = value_binding.binding_id;
        entity_id = value_binding.entity_id;
        ty = import_scheme value_binding.scheme
      }
      :: env)

let rec lookup_env_binding = fun env surface_path ->
  match env with
  | [] -> None
  | binding :: rest ->
      if SurfacePath.equal (EntityId.surface_path binding.entity_id) surface_path then
        Some binding
      else
        lookup_env_binding rest surface_path

let lookup_surface_path = fun state env ~level ~at surface_path ->
  match lookup_env_binding env surface_path with
  | Some binding -> instantiate state ~level binding.ty
  | None -> (
      match lookup_builtin surface_path builtin_bindings with
      | Some ty -> instantiate state ~level ty
      | None ->
          add_diagnostic
            state
            (unsupported_type at ("unbound value " ^ SurfacePath.to_string surface_path));
          fresh_tyvar state ~level
    )

let literal_type = function
  | TypAst.Int -> TInt
  | TypAst.Float -> TFloat
  | TypAst.Char -> TChar
  | TypAst.String -> TString
  | TypAst.Bool -> TBool
  | TypAst.Unit
  | TypAst.Unknown -> TUnit

let rec lookup_type_var = fun vars name ->
  match !vars with
  | [] -> None
  | (other_name, ty) :: rest ->
      if SurfacePath.equal name other_name then
        Some ty
      else (
        vars := rest;
        let result = lookup_type_var vars name in
        vars := (other_name, ty) :: rest;
        result
      )

let bind_type_var = fun vars name ty -> vars := (name, ty) :: !vars

let type_of_constructor = fun state ~level ~at path arguments ->
  match arguments with
  | [] when SurfacePath.equal path path_int ->
      TInt
  | [] when SurfacePath.equal path path_bool ->
      TBool
  | [] when SurfacePath.equal path path_char ->
      TChar
  | [] when SurfacePath.equal path path_string ->
      TString
  | [] when SurfacePath.equal path path_float ->
      TFloat
  | [] when SurfacePath.equal path path_unit ->
      TUnit
  | [ element ] when SurfacePath.equal path path_list ->
      TList element
  | [ element ] when SurfacePath.equal path path_option ->
      TOption element
  | _ ->
      add_diagnostic
        state
        (unsupported_type at ("unsupported type constructor " ^ SurfacePath.to_string path));
      fresh_tyvar state ~level

let rec lower_apply_type = fun state ~level vars (type_expr: TypAst.core_type) ->
  let rec loop arguments (current: TypAst.core_type) =
    match current.TypAst.view with
    | TypAst.TypeApply { argument=Some argument; constructor=Some constructor } ->
        loop (lower_core_type state ~level vars argument :: arguments) constructor
    | TypAst.TypeApply { argument=None; constructor=Some constructor } ->
        loop (fresh_tyvar state ~level :: arguments) constructor
    | TypAst.TypeApply { constructor=None; _ } ->
        add_diagnostic state (unsupported_type type_expr.origin "type application");
        fresh_tyvar state ~level
    | TypAst.TypePath path ->
        type_of_constructor state ~level ~at:current.origin path (List.reverse arguments)
    | _ ->
        add_diagnostic state (unsupported_type current.origin "type application constructor");
        fresh_tyvar state ~level
  in
  loop [] type_expr

and lower_labeled_type = fun state ~level vars (type_expr: TypAst.core_type) ->
  match type_expr.TypAst.view with
  | TypAst.TypeLabeled { annotation=Some annotation } ->
      lower_core_type state ~level vars annotation
  | TypAst.TypeLabeled { annotation=None } ->
      add_diagnostic state (unsupported_type type_expr.origin "missing labeled type annotation");
      fresh_tyvar state ~level
  | _ ->
      lower_core_type state ~level vars type_expr

and lower_core_type = fun state ~level vars (type_expr: TypAst.core_type) ->
  match type_expr.TypAst.view with
  | TypAst.TypeWildcard ->
      fresh_tyvar state ~level
  | TypAst.TypeVar (Some name) ->
      let name = SurfacePath.of_name name in
      (
        match lookup_type_var vars name with
        | Some ty -> ty
        | None ->
            let ty = fresh_tyvar state ~level in
            bind_type_var vars name ty;
            ty
      )
  | TypAst.TypeVar None ->
      add_diagnostic state (unsupported_type type_expr.origin "missing type variable");
      fresh_tyvar state ~level
  | TypAst.TypePath path ->
      type_of_constructor state ~level ~at:type_expr.origin path []
  | TypAst.TypeApply _ ->
      lower_apply_type state ~level vars type_expr
  | TypAst.TypeArrow { left=Some left; right=Some right } ->
      TArrow (lower_labeled_type state ~level vars left, lower_core_type state ~level vars right)
  | TypAst.TypeArrow _ ->
      add_diagnostic state (unsupported_type type_expr.origin "arrow type");
      fresh_tyvar state ~level
  | TypAst.TypeTuple elements ->
      TTuple (List.map elements ~fn:(lower_core_type state ~level vars))
  | TypAst.TypeLabeled _ ->
      lower_labeled_type state ~level vars type_expr
  | TypAst.TypePoly _ ->
      add_diagnostic state (unsupported_type type_expr.origin "polymorphic annotation");
      fresh_tyvar state ~level
  | TypAst.TypeUnsupported summary ->
      add_diagnostic state (unsupported_type type_expr.origin summary);
      fresh_tyvar state ~level
  | TypAst.TypeError summary ->
      add_diagnostic state (unsupported_type type_expr.origin summary);
      fresh_tyvar state ~level

let extend_mono = fun (env: env) (bindings: binding list) ->
  List.fold_left bindings ~init:env ~fn:(fun extended_env binding -> binding :: extended_env)

let extend_generalized = fun (env: env) ~level (bindings: binding list) ->
  List.fold_left
    bindings
    ~init:env
    ~fn:(fun extended_env binding -> { binding with ty = generalize level binding.ty } :: extended_env)

let is_uppercase_name = fun name ->
  match String.get name ~at:0 with
  | Some char -> char >= 'A' && char <= 'Z'
  | None -> false

let simple_path_name = fun path ->
  match List.reverse (SurfacePath.to_segments path) with
  | name :: _ -> Some name
  | [] -> None

let rec infer_pattern = fun state env ~level (pattern: TypAst.pattern) ->
  match pattern.TypAst.view with
  | TypAst.PatternPath path ->
      if SurfacePath.equal path path_none then
        (TOption (fresh_tyvar state ~level), [])
      else
        (
          match simple_path_name path with
          | Some name when not (is_uppercase_name name) ->
              let ty = fresh_tyvar state ~level in
              let binding = make_binding state ~name:(SurfacePath.of_name name) ~ty in
              (ty, [ binding ])
          | Some name ->
              add_diagnostic
                state
                (unsupported_syntax pattern.origin ("unsupported constructor pattern " ^ name));
              (fresh_tyvar state ~level, [])
          | None ->
              add_diagnostic state (unsupported_syntax pattern.origin "path pattern");
              (fresh_tyvar state ~level, [])
        )
  | TypAst.PatternWildcard ->
      (fresh_tyvar state ~level, [])
  | TypAst.PatternLiteral literal ->
      (literal_type literal, [])
  | TypAst.PatternTuple elements ->
      let element_types, binding_groups = elements
      |> List.map ~fn:(fun child -> infer_pattern state env ~level child)
      |> List.unzip in
      (TTuple element_types, List.concat binding_groups)
  | TypAst.PatternList elements ->
      let element_ty = fresh_tyvar state ~level in
      let bindings =
        elements
        |> List.flat_map
          ~fn:(fun child ->
            let inferred_ty, bindings = infer_pattern state env ~level child in
            unify state ~at:child.origin element_ty inferred_ty;
            bindings)
      in
      (TList element_ty, bindings)
  | TypAst.PatternCons { head=Some head; tail=Some tail } ->
      let head_ty, head_bindings = infer_pattern state env ~level head in
      let tail_ty, tail_bindings = infer_pattern state env ~level tail in
      unify state ~at:tail.origin tail_ty (TList head_ty);
      (TList head_ty, List.append head_bindings tail_bindings)
  | TypAst.PatternApply { callee=Some callee; argument=Some argument } -> (
      match callee.view with
      | TypAst.PatternPath path when SurfacePath.equal path path_some ->
          let argument_ty, bindings = infer_pattern state env ~level argument in
          (TOption argument_ty, bindings)
      | TypAst.PatternPath path ->
          add_diagnostic
            state
            (unsupported_syntax
              pattern.origin
              ("unsupported constructor pattern " ^ SurfacePath.to_string path));
          (fresh_tyvar state ~level, [])
      | _ ->
          add_diagnostic state (unsupported_syntax pattern.origin "constructor pattern");
          (fresh_tyvar state ~level, [])
    )
  | TypAst.PatternConstraint { pattern=Some inner; annotation=Some annotation } ->
      let pattern_ty, bindings = infer_pattern state env ~level inner in
      let annotated = lower_core_type state ~level (ref []) annotation in
      unify state ~at:pattern.origin pattern_ty annotated;
      (pattern_ty, bindings)
  | TypAst.PatternAlias { pattern=Some inner; alias=Some alias } ->
      let pattern_ty, bindings = infer_pattern state env ~level inner in
      (
        match alias.view with
        | TypAst.PatternPath path -> (
            match simple_path_name path with
            | Some alias_name ->
                let alias_binding = make_binding state ~name:(SurfacePath.of_name alias_name) ~ty:pattern_ty in
                (pattern_ty, alias_binding :: bindings)
            | None -> (pattern_ty, bindings)
          )
        | _ -> (pattern_ty, bindings)
      )
  | TypAst.PatternAttribute { inner=Some inner } ->
      infer_pattern state env ~level inner
  | TypAst.PatternAttribute { inner=None } ->
      add_diagnostic state (unsupported_syntax pattern.origin "attribute pattern");
      (fresh_tyvar state ~level, [])
  | TypAst.PatternLabeledParam parameter
  | TypAst.PatternOptionalParam parameter
  | TypAst.PatternOptionalParamDefault parameter ->
      infer_parameter state env ~level parameter
  | TypAst.PatternApply _
  | TypAst.PatternCons _
  | TypAst.PatternConstraint _
  | TypAst.PatternAlias _ ->
      add_diagnostic state (unsupported_syntax pattern.origin "incomplete pattern");
      (fresh_tyvar state ~level, [])
  | TypAst.PatternUnsupported summary
  | TypAst.PatternError summary ->
      add_diagnostic state (unsupported_syntax pattern.origin summary);
      (fresh_tyvar state ~level, [])

and infer_parameter = fun state env ~level (parameter: TypAst.parameter) ->
  match parameter.TypAst.view with
  | TypAst.Labeled { label; pattern } ->
      add_diagnostic state (unsupported_syntax parameter.origin "labeled parameter");
      infer_labeled_parameter state env ~level label pattern
  | TypAst.Optional { label; pattern } ->
      add_diagnostic state (unsupported_syntax parameter.origin "optional parameter");
      infer_labeled_parameter state env ~level label pattern
  | TypAst.OptionalDefault { label; pattern; default } ->
      add_diagnostic state (unsupported_syntax parameter.origin "optional parameter");
      let ty, bindings = infer_labeled_parameter state env ~level label pattern in
      (
        match default with
        | Some default ->
            let default_ty = infer_expression state env ~level default in
            unify state ~at:parameter.origin ty default_ty
        | None -> ()
      );
      (ty, bindings)
  | TypAst.UnknownParameter summary ->
      add_diagnostic state (unsupported_syntax parameter.origin summary);
      (fresh_tyvar state ~level, [])

and infer_labeled_parameter = fun state env ~level label pattern ->
  match pattern with
  | Some pattern -> infer_pattern state env ~level pattern
  | None -> (
      match label with
      | Some label ->
          let ty = fresh_tyvar state ~level in
          let binding = make_binding state ~name:(SurfacePath.of_name label) ~ty in
          (ty, [ binding ])
      | None -> (fresh_tyvar state ~level, [])
    )

and infer_expression = fun state env ~level (expression: TypAst.expr) ->
  match expression.TypAst.view with
  | TypAst.ExprLiteral literal ->
      literal_type literal
  | TypAst.ExprPath path ->
      lookup_surface_path state env ~level ~at:expression.origin path
  | TypAst.ExprParenthesized { inner=Some inner } ->
      infer_expression state env ~level inner
  | TypAst.ExprParenthesized { inner=None } ->
      TUnit
  | TypAst.ExprAttribute { inner=Some inner } ->
      infer_expression state env ~level inner
  | TypAst.ExprAttribute { inner=None } ->
      add_diagnostic state (unsupported_syntax expression.origin "attribute expression");
      fresh_tyvar state ~level
  | TypAst.ExprTyped { expr=Some expr; annotation=Some annotation } ->
      let inferred = infer_expression state env ~level expr in
      let annotated = lower_core_type state ~level (ref []) annotation in
      unify state ~at:expression.origin inferred annotated;
      inferred
  | TypAst.ExprTyped { expr=Some expr; annotation=None } ->
      infer_expression state env ~level expr
  | TypAst.ExprTuple elements ->
      TTuple (List.map elements ~fn:(infer_expression state env ~level))
  | TypAst.ExprList elements ->
      let element_ty = fresh_tyvar state ~level in
      elements |> List.for_each
        ~fn:(fun child ->
          let child_ty = infer_expression state env ~level child in
          unify state ~at:child.origin element_ty child_ty);
      TList element_ty
  | TypAst.ExprSequence { left=Some left; right=Some right } ->
      let _ = infer_expression state env ~level left in
      infer_expression state env ~level right
  | TypAst.ExprSequence { left=Some left; right=None } ->
      infer_expression state env ~level left
  | TypAst.ExprIf { condition=Some condition; then_branch=Some then_branch; else_branch } ->
      let condition_ty = infer_expression state env ~level condition in
      unify state ~at:condition.origin condition_ty TBool;
      let then_ty = infer_expression state env ~level then_branch in
      (
        match else_branch with
        | Some else_branch ->
            let else_ty = infer_expression state env ~level else_branch in
            unify state ~at:expression.origin then_ty else_ty
        | None -> unify state ~at:expression.origin then_ty TUnit
      );
      then_ty
  | TypAst.ExprApply _ ->
      infer_apply state env ~level expression
  | TypAst.ExprInfix { left=Some left; operator=Some operator; right=Some right } ->
      let callee_ty = lookup_surface_path state env ~level ~at:expression.origin operator in
      let left_ty = infer_expression state env ~level left in
      let right_ty = infer_expression state env ~level right in
      let result_ty = fresh_tyvar state ~level in
      unify state ~at:expression.origin callee_ty (TArrow (left_ty, TArrow (right_ty, result_ty)));
      result_ty
  | TypAst.ExprPrefix { operator=Some operator; operand=Some operand } ->
      let callee_ty = lookup_surface_path state env ~level ~at:expression.origin operator in
      let operand_ty = infer_expression state env ~level operand in
      let result_ty = fresh_tyvar state ~level in
      unify state ~at:expression.origin callee_ty (TArrow (operand_ty, result_ty));
      result_ty
  | TypAst.ExprLet { first_binding=Some binding; body=Some body } ->
      let extended_env, _ = infer_let_binding state env ~level ~recursive:false binding in
      infer_expression state extended_env ~level body
  | TypAst.ExprAssert { argument=Some argument } ->
      let inferred = infer_expression state env ~level argument in
      unify state ~at:expression.origin inferred TBool;
      TUnit
  | TypAst.ExprLabeledArg _ ->
      add_diagnostic state (unsupported_syntax expression.origin "labeled argument expression");
      fresh_tyvar state ~level
  | TypAst.ExprOptionalArg _ ->
      add_diagnostic state (unsupported_syntax expression.origin "optional argument expression");
      fresh_tyvar state ~level
  | TypAst.ExprUnsupported summary
  | TypAst.ExprError summary ->
      add_diagnostic state (unsupported_syntax expression.origin summary);
      fresh_tyvar state ~level
  | TypAst.ExprTyped _
  | TypAst.ExprSequence _
  | TypAst.ExprIf _
  | TypAst.ExprInfix _
  | TypAst.ExprPrefix _
  | TypAst.ExprLet _
  | TypAst.ExprAssert _ ->
      add_diagnostic
        state
        (unsupported_syntax expression.origin (Syn.SyntaxKind.to_string expression.origin.kind));
      fresh_tyvar state ~level

and infer_apply = fun state env ~level (expression: TypAst.expr) ->
  let rec collect arguments (current: TypAst.expr) =
    match current.TypAst.view with
    | TypAst.ExprApply { callee=Some callee; argument=Some argument } -> collect
      (argument :: arguments)
      callee
    | TypAst.ExprApply { callee=Some callee; argument=None } -> collect arguments callee
    | _ -> (current, arguments)
  in
  let callee, arguments = collect [] expression in
  let callee_ty = infer_expression state env ~level callee in
  List.fold_left arguments ~init:callee_ty
    ~fn:(fun function_ty argument ->
      let argument_ty = infer_apply_argument state env ~level argument in
      let result_ty = fresh_tyvar state ~level in
      unify state ~at:expression.origin function_ty (TArrow (argument_ty, result_ty));
      result_ty)

and infer_apply_argument = fun state env ~level (argument: TypAst.expr) ->
  match argument.TypAst.view with
  | TypAst.ExprLabeledArg { value=Some value; _ } ->
      add_diagnostic state (unsupported_syntax argument.origin "labeled argument");
      infer_expression state env ~level value
  | TypAst.ExprOptionalArg { value=Some value; _ } ->
      add_diagnostic state (unsupported_syntax argument.origin "optional argument");
      infer_expression state env ~level value
  | TypAst.ExprLabeledArg _
  | TypAst.ExprOptionalArg _ ->
      add_diagnostic state (unsupported_syntax argument.origin "missing argument value");
      fresh_tyvar state ~level
  | _ ->
      infer_expression state env ~level argument

and infer_lambda = fun state env ~level parameters body ->
  match parameters with
  | [] -> infer_expression state env ~level body
  | parameter :: rest ->
      let parameter_ty, parameter_bindings = infer_pattern state env ~level parameter in
      let extended_env = extend_mono env parameter_bindings in
      let result_ty = infer_lambda state extended_env ~level rest body in
      TArrow (parameter_ty, result_ty)

and infer_let_binding = fun state env ~level ~recursive (binding: TypAst.let_binding) ->
  if recursive then
    add_diagnostic state (unsupported_syntax binding.TypAst.origin "recursive let binding");
  let value_ty =
    match binding.body with
    | Some body when List.is_empty binding.parameters -> infer_expression
      state
      env
      ~level:(level + 1)
      body
    | Some body -> infer_lambda state env ~level:(level + 1) binding.parameters body
    | None -> fresh_tyvar state ~level:(level + 1)
  in
  match binding.pattern with
  | Some pattern ->
      let pattern_ty, bindings = infer_pattern state env ~level pattern in
      unify state ~at:binding.origin pattern_ty value_ty;
      let public_bindings = List.map bindings ~fn:public_binding_of_binding in
      let extended_env = extend_generalized env ~level bindings in
      (extended_env, public_bindings)
  | None ->
      add_diagnostic state (unsupported_syntax binding.origin "let binding pattern");
      (env, [])

let infer_let_declaration = fun state env ~level (declaration: TypAst.let_declaration) ->
  List.fold_left declaration.TypAst.bindings ~init:(env, [])
    ~fn:(fun (env, public_bindings) binding ->
      let next_env, item_bindings = infer_let_binding
        state
        env
        ~level
        ~recursive:declaration.recursive
        binding in
      (next_env, List.append public_bindings item_bindings))

let lower_declaration_annotation = fun state ~level origin annotation ->
  match annotation with
  | Some annotation -> lower_core_type state ~level (ref []) annotation
  | None ->
      add_diagnostic state (unsupported_type origin "missing type annotation");
      fresh_tyvar state ~level

let bind_declared_value = fun state env ~level ~origin name annotation ->
  let ty = lower_declaration_annotation state ~level origin annotation in
  let name = SurfacePath.of_name name in
  let binding = make_binding state ~name ~ty in
  let public_binding = public_binding_of_binding binding in
  let extended_env = { binding with ty = generalize level ty } :: env in
  (extended_env, [ public_binding ])

let infer_structure_item = fun state env ~level (item: TypAst.structure_item) ->
  match item.TypAst.view with
  | TypAst.StructureLet declaration ->
      infer_let_declaration state env ~level declaration
  | TypAst.StructureExpr expression ->
      let _ = Option.map expression ~fn:(infer_expression state env ~level) in
      (env, [])
  | TypAst.StructureExternal declaration -> (
      match declaration.name with
      | Some name -> bind_declared_value
        state
        env
        ~level
        ~origin:declaration.origin
        name
        declaration.type_annotation
      | None ->
          add_diagnostic state (unsupported_syntax declaration.origin "external declaration");
          (env, [])
    )
  | TypAst.StructureUnsupported summary
  | TypAst.StructureError summary ->
      add_diagnostic state (unsupported_syntax item.origin summary);
      (env, [])

let check_implementation = fun ~ast ~typing_context items ->
  let state = make_state ~next_binding_stamp:typing_context.Typing_context.next_binding_stamp in
  let env = env_of_typing_context typing_context in
  let _, bindings =
    List.fold_left items ~init:(env, [])
      ~fn:(fun (env, bindings) item ->
        let next_env, item_bindings = infer_structure_item state env ~level:0 item in
        (next_env, List.append bindings item_bindings))
  in
  {
    File.ast;
    diagnostics = List.reverse state.diagnostics;
    bindings;
    typing_context = {
      Typing_context.next_binding_stamp = state.next_binding_stamp;
      values = List.append typing_context.values bindings
    }
  }

let check_signature_item = fun state env ~level (item: TypAst.signature_item) ->
  match item.TypAst.view with
  | TypAst.SignatureValue declaration -> (
      match declaration.name with
      | Some name -> bind_declared_value
        state
        env
        ~level
        ~origin:declaration.origin
        name
        declaration.type_annotation
      | None ->
          add_diagnostic state (unsupported_syntax declaration.origin "value declaration");
          (env, [])
    )
  | TypAst.SignatureExternal declaration -> (
      match declaration.name with
      | Some name -> bind_declared_value
        state
        env
        ~level
        ~origin:declaration.origin
        name
        declaration.type_annotation
      | None ->
          add_diagnostic state (unsupported_syntax declaration.origin "external declaration");
          (env, [])
    )
  | TypAst.SignatureUnsupported summary
  | TypAst.SignatureError summary ->
      add_diagnostic state (unsupported_syntax item.origin summary);
      (env, [])

let check_interface = fun ~ast ~typing_context items ->
  let state = make_state ~next_binding_stamp:typing_context.Typing_context.next_binding_stamp in
  let env = env_of_typing_context typing_context in
  let _, bindings =
    List.fold_left items ~init:(env, [])
      ~fn:(fun (env, bindings) item ->
        let next_env, item_bindings = check_signature_item state env ~level:0 item in
        (next_env, List.append bindings item_bindings))
  in
  {
    File.ast;
    diagnostics = List.reverse state.diagnostics;
    bindings;
    typing_context = {
      Typing_context.next_binding_stamp = state.next_binding_stamp;
      values = List.append typing_context.values bindings
    }
  }

let check_source_file = fun ~typing_context ast ->
  match ast.TypAst.view with
  | Implementation items -> check_implementation ~ast ~typing_context items
  | Interface items -> check_interface ~ast ~typing_context items
  | Empty -> File.empty ~ast ~typing_context

let check_expression = fun expression ->
  let state = make_state ~next_binding_stamp:0 in
  let _ = infer_expression state [] ~level:0 expression in
  List.reverse state.diagnostics

let check_pattern = fun pattern ->
  let state = make_state ~next_binding_stamp:0 in
  let _ = infer_pattern state [] ~level:0 pattern in
  List.reverse state.diagnostics

let check_let_binding = fun binding ->
  let state = make_state ~next_binding_stamp:0 in
  let _ = infer_let_binding state [] ~level:0 ~recursive:false binding in
  List.reverse state.diagnostics

let check_core_type = fun core_type ->
  let state = make_state ~next_binding_stamp:0 in
  let _ = lower_core_type state ~level:0 (ref []) core_type in
  List.reverse state.diagnostics

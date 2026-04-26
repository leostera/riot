module InferEnv = Env

open Std
open Ast

let unify (state: State.t) ~expected ~actual ~on_error =
  match Unifier.unify ~expected ~actual with
  | Ok () -> ()
  | Error err -> Diagnostics.add (State.diagnostics state) (on_error err)

let annotation_diagnostic (annotation: core_type) err =
  match err with
  | Unifier.TypeMismatch { expected; actual } ->
      Diagnostics.Diagnostic.annotation_mismatch
        ~span:annotation.origin.span
        ~annotation_span:annotation.origin.span
        ~expected:(Type.to_string expected)
        ~actual:(Type.to_string actual)
  | Unifier.InfiniteSubstitution { var; type_ } ->
      Diagnostics.Diagnostic.infinite_substitution
        ~span:annotation.origin.span
        ~var:(TypeVar.to_string var.id)
        ~type_:(Type.to_string type_)

module Builtin = struct
  open Model

  let make name =
    let ident = Model.Surface_path.from_name name in
    Type.Constructor { ident; arguments = [] }

  let int = make "int"

  let bool = make "bool"

  let float = make "float"

  let char = make "char"

  let string = make "string"

  let unit = make "unit"
end

let infer_literal _state (lit: literal) =
  let open Builtin in
  match lit with
  | Int -> int
  | Float -> float
  | Char -> char
  | String -> string
  | Bool -> bool

let infer_ident (state: State.t) ident =
  match InferEnv.get_value (State.env state) ~name:ident with
  | Some type_ -> type_
  | None -> State.fresh_var state

let rec infer_expr (state: State.t) (expr: expression) =
  match expr.kind with
  | Literal lit -> infer_literal state lit
  | Ident ident -> infer_ident state ident
  | Tuple parts -> infer_tuple state parts
  | _ -> State.fresh_var state

and infer_tuple (state: State.t) parts =
  let types = List.map ~fn:(infer_expr state) parts in
  Type.Tuple types

let rec type_expression (state: State.t) (expr: expression) =
  let type_ = infer_expr state expr in
  expr.type_ <- Some type_;
  type_

let arrow_label_to_type_label = function
  | NoLabel -> Type.Label.NoLabel
  | Labelled label -> Type.Label.Labelled label
  | Optional label -> Type.Label.Optional label

let rec core_type_to_type (state: State.t) (annotation: core_type) =
  let type_ =
    match annotation.kind with
    | TypeIdent ident -> Type.Constructor { ident; arguments = [] }
    | Apply { constructor; arguments } -> (
        let constructor = core_type_to_type state constructor in
        let arguments = List.map arguments ~fn:(core_type_to_type state) in
        match Unifier.resolve constructor with
        | Type.Constructor { ident; arguments = existing_arguments } ->
            Type.Constructor { ident; arguments = List.append existing_arguments arguments }
        | _ -> State.fresh_var state
      )
    | Arrow { label; parameter; result } ->
        Type.Arrow {
          label = arrow_label_to_type_label label;
          parameter = core_type_to_type state parameter;
          result = core_type_to_type state result;
        }
    | Tuple parts -> Type.Tuple (List.map parts ~fn:(core_type_to_type state))
    | Parenthesized inner
    | ForAll { body = inner; _ } -> core_type_to_type state inner
    | Wildcard
    | Var _
    | PolyVariant _
    | Package _ -> State.fresh_var state
  in
  annotation.type_ <- Some type_;
  type_

let rec bind_pattern (state: State.t) (pattern: pattern) type_ =
  pattern.type_ <- Some type_;
  match pattern.kind with
  | Bind name -> ignore (InferEnv.add_value (State.env state) ~name ~type_)
  | Constraint { pattern; annotation } ->
      let expected = core_type_to_type state annotation in
      unify state ~expected ~actual:type_ ~on_error:(annotation_diagnostic annotation);
      bind_pattern state pattern expected
  | Attribute pattern -> bind_pattern state pattern type_
  | Alias { pattern; alias } ->
      bind_pattern state pattern type_;
      bind_pattern state alias type_
  | _ -> ()

let type_let_binding (state: State.t) (lb: let_binding) =
  let type_ = type_expression state lb.body in
  bind_pattern state lb.pattern type_

let type_let_decl (state: State.t) (ld: let_declaration) =
  List.for_each ld.bindings ~fn:(type_let_binding state)

let type_impl_item (state: State.t) (item: structure_item) =
  match item.kind with
  | Let ld -> type_let_decl state ld
  | _ -> ()

let type_impl (state: State.t) (items: structure_item list) =
  List.for_each items ~fn:(type_impl_item state)

let type_intf (_state: State.t) (_items: signature_item list) = ()

let type_ast (state: State.t) (ast: Ast.t) =
  match ast.kind with
  | Implementation items -> type_impl state items
  | Interface items -> type_intf state items

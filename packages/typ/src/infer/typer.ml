module InferEnv = Env
open Std
open Ast

module WellKnownTypes = struct
  open Model

  let make name =
    let ident = Model.Surface_path.from_name name in
    Type.Constructor { ident; arguments = [] }

  let int = make "int"

  let bool = make "bool"

  let float = make "float"

  let char = make "char"

  let string = make "string"
end

let infer_literal _state (lit: literal) =
  let open WellKnownTypes in
    match lit with
    | Int -> int
    | Float -> float
    | Char -> char
    | String -> string
    | Bool -> bool

let infer_ident (state: State.t) ident =
  match InferEnv.get_value state.env ~name:ident with
  | Some type_ -> type_
  | None -> State.fresh_var state

let rec infer_expr (state: State.t) (expr: expression) =
  match expr.kind with
  | Literal lit -> infer_literal state lit
  | Ident ident -> infer_ident state ident
  | _ -> State.fresh_var state

let rec type_expression (state: State.t) (expr: expression) =
  let type_ = infer_expr state expr in
  expr.type_ <- Some type_;
  type_

let rec bind_pattern (state: State.t) (pattern: pattern) type_ =
  pattern.type_ <- Some type_;
  match pattern.kind with
  | Bind name ->
      ignore (InferEnv.add_value state.env ~name ~type_)
  | Constraint { pattern; _ }
  | Attribute pattern ->
      bind_pattern state pattern type_
  | Alias { pattern; alias } ->
      bind_pattern state pattern type_;
      bind_pattern state alias type_
  | _ ->
      ()

let type_let_binding (state: State.t) (lb: let_binding) =
  let type_ = type_expression state lb.body in
  bind_pattern state lb.pattern type_

let type_let_decl (state: State.t) (ld: let_declaration) = List.for_each
  ld.bindings
  ~fn:(type_let_binding state)

let type_impl_item (state: State.t) (item: structure_item) =
  match item.kind with
  | Let ld -> type_let_decl state ld
  | _ -> ()

let type_impl (state: State.t) (items: structure_item list) = List.for_each
  items
  ~fn:(type_impl_item state)

let type_intf (_state: State.t) (_items: signature_item list) = ()

let type_ast (state: State.t) (ast: Ast.t) =
  match ast.kind with
  | Implementation items -> type_impl state items
  | Interface items -> type_intf state items

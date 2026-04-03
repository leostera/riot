open Std

type origin_kind =
  | Item
  | Expr
  | Pattern

type origin = {
  origin_id: int;
  kind: origin_kind;
  span: Syn.Ceibo.Span.t;
  label: string;
}

type pat_id = int

type expr_id = int

type pattern_desc =
  | PVar of string
  | PWildcard
  | PInt of string
  | PBool of bool
  | PString of string
  | PUnit
  | PTuple of pat_id list
  | PUnsupported of string

type pattern_node = {
  pat_id: pat_id;
  origin_id: int;
  desc: pattern_desc;
}

type match_case = {
  pattern_id: pat_id;
  body_id: expr_id;
}

type expr_desc =
  | EVar of string
  | EInt of string
  | EBool of bool
  | EString of string
  | EUnit
  | ETuple of expr_id list
  | EFun of pat_id list * expr_id
  | EApply of expr_id * expr_id list
  | ELet of binding list * expr_id
  | EIf of expr_id * expr_id * expr_id
  | EMatch of expr_id * match_case list
  | EUnsupported of string
  | EHole of string

and expr_node = {
  expr_id: expr_id;
  origin_id: int;
  desc: expr_desc;
}

and binding = {
  binding_id: int;
  origin_id: int;
  name: string option;
  pattern_id: pat_id;
  value_id: expr_id;
  recursive: bool;
}

type value_item = {
  item_id: int;
  origin_id: int;
  bindings: binding list;
  recursive: bool;
}

type unsupported_item = {
  item_id: int;
  origin_id: int;
  summary: string;
}

type item =
  | Value of value_item
  | Unsupported of unsupported_item

type file = {
  items: item list;
  patterns: pattern_node list;
  expressions: expr_node list;
  origins: origin list;
  diagnostics: Diagnostic.t list;
}

let empty = {
  items = [];
  patterns = [];
  expressions = [];
  origins = [];
  diagnostics = [];
}

let find_origin = fun file origin_id ->
  List.find_opt (fun (origin: origin) -> origin.origin_id = origin_id) file.origins

let find_pattern = fun file pat_id ->
  List.find_opt (fun (node: pattern_node) -> node.pat_id = pat_id) file.patterns

let find_expr = fun file expr_id ->
  List.find_opt (fun (node: expr_node) -> node.expr_id = expr_id) file.expressions

let origin_kind_to_string = function
  | Item -> "item"
  | Expr -> "expr"
  | Pattern -> "pattern"

let render_ids = fun ids ->
  ids
  |> List.map string_of_int
  |> String.concat ", "

let render_pattern_desc = function
  | PVar name -> "var " ^ name
  | PWildcard -> "_"
  | PInt digits -> "int " ^ digits
  | PBool value -> "bool " ^ Bool.to_string value
  | PString value -> "string \"" ^ String.escaped value ^ "\""
  | PUnit -> "unit"
  | PTuple elements -> "tuple [" ^ render_ids elements ^ "]"
  | PUnsupported summary -> "unsupported(" ^ summary ^ ")"

let render_expr_desc = function
  | EVar name -> "var " ^ name
  | EInt digits -> "int " ^ digits
  | EBool value -> "bool " ^ Bool.to_string value
  | EString value -> "string \"" ^ String.escaped value ^ "\""
  | EUnit -> "unit"
  | ETuple elements -> "tuple [" ^ render_ids elements ^ "]"
  | EFun (params, body_id) ->
      "fun [" ^ render_ids params ^ "] -> expr#" ^ Int.to_string body_id
  | EApply (callee_id, args) ->
      "apply expr#" ^ Int.to_string callee_id ^ " [" ^ render_ids args ^ "]"
  | ELet (bindings, body_id) ->
      let binding_ids =
        bindings
        |> List.map (fun (binding: binding) -> binding.binding_id)
        |> render_ids
      in
      "let [" ^ binding_ids ^ "] in expr#" ^ Int.to_string body_id
  | EIf (condition_id, then_id, else_id) ->
      "if expr#"
      ^ Int.to_string condition_id
      ^ " then expr#"
      ^ Int.to_string then_id
      ^ " else expr#"
      ^ Int.to_string else_id
  | EMatch (scrutinee_id, cases) ->
      let cases_text =
        cases
        |> List.map (fun (case: match_case) ->
          "(pat#"
          ^ Int.to_string case.pattern_id
          ^ " -> expr#"
          ^ Int.to_string case.body_id
          ^ ")")
        |> String.concat ", "
      in
      "match expr#" ^ Int.to_string scrutinee_id ^ " with [" ^ cases_text ^ "]"
  | EUnsupported summary -> "unsupported(" ^ summary ^ ")"
  | EHole summary -> "hole(" ^ summary ^ ")"

let render_binding = fun (binding: binding) ->
  let name =
    match binding.name with
    | Some name -> name
    | None -> "_"
  in
  "binding#"
  ^ Int.to_string binding.binding_id
  ^ " "
  ^ name
  ^ " pat#"
  ^ Int.to_string binding.pattern_id
  ^ " expr#"
  ^ Int.to_string binding.value_id
  ^ " recursive="
  ^ Bool.to_string binding.recursive

let render_item = function
  | Value (item: value_item) ->
      let binding_lines =
        item.bindings
        |> List.map render_binding
        |> List.map (fun line -> "    " ^ line)
      in
      String.concat "\n" ([
        "  item#" ^ Int.to_string item.item_id ^ " value recursive=" ^ Bool.to_string item.recursive
      ] @ binding_lines)
  | Unsupported (item: unsupported_item) ->
      "  item#" ^ Int.to_string item.item_id ^ " unsupported " ^ item.summary

let to_string = fun file ->
  let origin_lines =
    file.origins
    |> List.map (fun (origin: origin) ->
      "  origin#"
      ^ Int.to_string origin.origin_id
      ^ " "
      ^ origin_kind_to_string origin.kind
      ^ " "
      ^ origin.label
      ^ " @ "
      ^ Syn.Ceibo.Span.to_string origin.span)
  in
  let pattern_lines =
    file.patterns
    |> List.map (fun (node: pattern_node) ->
      "  pat#"
      ^ Int.to_string node.pat_id
      ^ " origin#"
      ^ Int.to_string node.origin_id
      ^ " "
      ^ render_pattern_desc node.desc)
  in
  let expr_lines =
    file.expressions
    |> List.map (fun (node: expr_node) ->
      "  expr#"
      ^ Int.to_string node.expr_id
      ^ " origin#"
      ^ Int.to_string node.origin_id
      ^ " "
      ^ render_expr_desc node.desc)
  in
  let item_lines = List.map render_item file.items in
  String.concat
    "\n"
    ([
      "origins:";
    ]
    @ origin_lines
    @ [
      "";
      "patterns:";
    ]
    @ pattern_lines
    @ [
      "";
      "expressions:";
    ]
    @ expr_lines
    @ [
      "";
      "items:";
    ]
    @ item_lines
    @ [ "" ])

open Std

type pattern_desc =
  | PVar of string
  | PWildcard
  | PInt of string
  | PBool of bool
  | PString of string
  | PUnit
  | PTuple of PatId.t list
  | PUnsupported of string

type pattern_node = {
  pat_id: PatId.t;
  origin_id: OriginId.t;
  desc: pattern_desc;
}

type match_case = {
  pattern_id: PatId.t;
  body_id: ExprId.t;
}

type expr_desc =
  | EVar of string
  | EInt of string
  | EBool of bool
  | EString of string
  | EUnit
  | ETuple of ExprId.t list
  | EFun of PatId.t list * ExprId.t
  | EApply of ExprId.t * ExprId.t list
  | ELet of BindingId.t list * ExprId.t
  | EIf of ExprId.t * ExprId.t * ExprId.t
  | EMatch of ExprId.t * match_case list
  | EUnsupported of string
  | EHole of string

and expr_node = {
  expr_id: ExprId.t;
  origin_id: OriginId.t;
  desc: expr_desc;
}

and binding = {
  binding_id: BindingId.t;
  origin_id: OriginId.t;
  name: string option;
  pattern_id: PatId.t;
  value_id: ExprId.t;
  recursive: bool;
}

type t = {
  patterns: pattern_node list;
  expressions: expr_node list;
  bindings: binding list;
}

let empty = {
  patterns = [];
  expressions = [];
  bindings = [];
}

let of_lists = fun ~patterns ~expressions ~bindings ->
  { patterns; expressions; bindings }

let patterns = fun arena -> arena.patterns

let expressions = fun arena -> arena.expressions

let bindings = fun arena -> arena.bindings

let find_pattern = fun arena pat_id ->
  List.find_opt (fun (node: pattern_node) -> PatId.equal node.pat_id pat_id) arena.patterns

let find_expr = fun arena expr_id ->
  List.find_opt (fun (node: expr_node) -> ExprId.equal node.expr_id expr_id) arena.expressions

let find_binding = fun arena binding_id ->
  List.find_opt (fun (binding: binding) -> BindingId.equal binding.binding_id binding_id) arena.bindings

let render_ids = fun render ids ->
  ids
  |> List.map render
  |> String.concat ", "

let render_pattern_desc = function
  | PVar name -> "var " ^ name
  | PWildcard -> "_"
  | PInt digits -> "int " ^ digits
  | PBool value -> "bool " ^ Bool.to_string value
  | PString value -> "string \"" ^ String.escaped value ^ "\""
  | PUnit -> "unit"
  | PTuple elements -> "tuple [" ^ render_ids PatId.to_string elements ^ "]"
  | PUnsupported summary -> "unsupported(" ^ summary ^ ")"

let render_expr_desc = function
  | EVar name -> "var " ^ name
  | EInt digits -> "int " ^ digits
  | EBool value -> "bool " ^ Bool.to_string value
  | EString value -> "string \"" ^ String.escaped value ^ "\""
  | EUnit -> "unit"
  | ETuple elements -> "tuple [" ^ render_ids ExprId.to_string elements ^ "]"
  | EFun (parameters, body_id) ->
      "fun [" ^ render_ids PatId.to_string parameters ^ "] -> " ^ ExprId.to_string body_id
  | EApply (callee_id, arguments) ->
      "apply " ^ ExprId.to_string callee_id ^ " [" ^ render_ids ExprId.to_string arguments ^ "]"
  | ELet (binding_ids, body_id) ->
      "let [" ^ render_ids BindingId.to_string binding_ids ^ "] in " ^ ExprId.to_string body_id
  | EIf (condition_id, then_id, else_id) ->
      "if "
      ^ ExprId.to_string condition_id
      ^ " then "
      ^ ExprId.to_string then_id
      ^ " else "
      ^ ExprId.to_string else_id
  | EMatch (scrutinee_id, cases) ->
      let cases_text =
        cases
        |> List.map (fun (case: match_case) ->
          "(" ^ PatId.to_string case.pattern_id ^ " -> " ^ ExprId.to_string case.body_id ^ ")")
        |> String.concat ", "
      in
      "match " ^ ExprId.to_string scrutinee_id ^ " with [" ^ cases_text ^ "]"
  | EUnsupported summary -> "unsupported(" ^ summary ^ ")"
  | EHole summary -> "hole(" ^ summary ^ ")"

let render_binding = fun (binding: binding) ->
  let name =
    match binding.name with
    | Some name -> name
    | None -> "_"
  in
  "  "
  ^ BindingId.to_string binding.binding_id
  ^ " "
  ^ name
  ^ " "
  ^ PatId.to_string binding.pattern_id
  ^ " "
  ^ ExprId.to_string binding.value_id
  ^ " recursive="
  ^ Bool.to_string binding.recursive

let pattern_desc_to_json = function
  | PVar name ->
      Data.Json.Object [ ("tag", Data.Json.String "var"); ("name", Data.Json.String name) ]
  | PWildcard ->
      Data.Json.Object [ ("tag", Data.Json.String "wildcard") ]
  | PInt digits ->
      Data.Json.Object [ ("tag", Data.Json.String "int"); ("digits", Data.Json.String digits) ]
  | PBool value ->
      Data.Json.Object [ ("tag", Data.Json.String "bool"); ("value", Data.Json.Bool value) ]
  | PString value ->
      Data.Json.Object [ ("tag", Data.Json.String "string"); ("value", Data.Json.String value) ]
  | PUnit ->
      Data.Json.Object [ ("tag", Data.Json.String "unit") ]
  | PTuple elements ->
      Data.Json.Object [
        ("tag", Data.Json.String "tuple");
        ("elements", Data.Json.Array (List.map (fun pat_id -> Data.Json.Int (PatId.to_int pat_id)) elements));
      ]
  | PUnsupported summary ->
      Data.Json.Object [ ("tag", Data.Json.String "unsupported"); ("summary", Data.Json.String summary) ]

let match_case_to_json = fun (case: match_case) ->
  Data.Json.Object [
    ("pattern_id", Data.Json.Int (PatId.to_int case.pattern_id));
    ("body_id", Data.Json.Int (ExprId.to_int case.body_id));
  ]

let expr_desc_to_json = function
  | EVar name ->
      Data.Json.Object [ ("tag", Data.Json.String "var"); ("name", Data.Json.String name) ]
  | EInt digits ->
      Data.Json.Object [ ("tag", Data.Json.String "int"); ("digits", Data.Json.String digits) ]
  | EBool value ->
      Data.Json.Object [ ("tag", Data.Json.String "bool"); ("value", Data.Json.Bool value) ]
  | EString value ->
      Data.Json.Object [ ("tag", Data.Json.String "string"); ("value", Data.Json.String value) ]
  | EUnit ->
      Data.Json.Object [ ("tag", Data.Json.String "unit") ]
  | ETuple elements ->
      Data.Json.Object [
        ("tag", Data.Json.String "tuple");
        ("elements", Data.Json.Array (List.map (fun expr_id -> Data.Json.Int (ExprId.to_int expr_id)) elements));
      ]
  | EFun (parameters, body_id) ->
      Data.Json.Object [
        ("tag", Data.Json.String "fun");
        ("parameters", Data.Json.Array (List.map (fun pat_id -> Data.Json.Int (PatId.to_int pat_id)) parameters));
        ("body_id", Data.Json.Int (ExprId.to_int body_id));
      ]
  | EApply (callee_id, arguments) ->
      Data.Json.Object [
        ("tag", Data.Json.String "apply");
        ("callee_id", Data.Json.Int (ExprId.to_int callee_id));
        ("arguments", Data.Json.Array (List.map (fun expr_id -> Data.Json.Int (ExprId.to_int expr_id)) arguments));
      ]
  | ELet (binding_ids, body_id) ->
      Data.Json.Object [
        ("tag", Data.Json.String "let");
        ("binding_ids", Data.Json.Array (List.map (fun binding_id -> Data.Json.Int (BindingId.to_int binding_id)) binding_ids));
        ("body_id", Data.Json.Int (ExprId.to_int body_id));
      ]
  | EIf (condition_id, then_id, else_id) ->
      Data.Json.Object [
        ("tag", Data.Json.String "if");
        ("condition_id", Data.Json.Int (ExprId.to_int condition_id));
        ("then_id", Data.Json.Int (ExprId.to_int then_id));
        ("else_id", Data.Json.Int (ExprId.to_int else_id));
      ]
  | EMatch (scrutinee_id, cases) ->
      Data.Json.Object [
        ("tag", Data.Json.String "match");
        ("scrutinee_id", Data.Json.Int (ExprId.to_int scrutinee_id));
        ("cases", Data.Json.Array (List.map match_case_to_json cases));
      ]
  | EUnsupported summary ->
      Data.Json.Object [ ("tag", Data.Json.String "unsupported"); ("summary", Data.Json.String summary) ]
  | EHole summary ->
      Data.Json.Object [ ("tag", Data.Json.String "hole"); ("summary", Data.Json.String summary) ]

let pattern_node_to_json = fun (node: pattern_node) ->
  Data.Json.Object [
    ("pat_id", Data.Json.Int (PatId.to_int node.pat_id));
    ("origin_id", Data.Json.Int (OriginId.to_int node.origin_id));
    ("desc", pattern_desc_to_json node.desc);
  ]

let expr_node_to_json = fun (node: expr_node) ->
  Data.Json.Object [
    ("expr_id", Data.Json.Int (ExprId.to_int node.expr_id));
    ("origin_id", Data.Json.Int (OriginId.to_int node.origin_id));
    ("desc", expr_desc_to_json node.desc);
  ]

let binding_to_json = fun (binding: binding) ->
  let name_json =
    match binding.name with
    | Some name -> Data.Json.String name
    | None -> Data.Json.Null
  in
  Data.Json.Object [
    ("binding_id", Data.Json.Int (BindingId.to_int binding.binding_id));
    ("origin_id", Data.Json.Int (OriginId.to_int binding.origin_id));
    ("name", name_json);
    ("pattern_id", Data.Json.Int (PatId.to_int binding.pattern_id));
    ("value_id", Data.Json.Int (ExprId.to_int binding.value_id));
    ("recursive", Data.Json.Bool binding.recursive);
  ]

let to_json = fun arena ->
  Data.Json.Object [
    ("patterns", Data.Json.Array (List.map pattern_node_to_json arena.patterns));
    ("bindings", Data.Json.Array (List.map binding_to_json arena.bindings));
    ("expressions", Data.Json.Array (List.map expr_node_to_json arena.expressions));
  ]

let to_string = fun arena ->
  let pattern_lines =
    arena.patterns
    |> List.map (fun (node: pattern_node) ->
      "  "
      ^ PatId.to_string node.pat_id
      ^ " "
      ^ OriginId.to_string node.origin_id
      ^ " "
      ^ render_pattern_desc node.desc)
  in
  let binding_lines =
    arena.bindings
    |> List.map render_binding
  in
  let expr_lines =
    arena.expressions
    |> List.map (fun (node: expr_node) ->
      "  "
      ^ ExprId.to_string node.expr_id
      ^ " "
      ^ OriginId.to_string node.origin_id
      ^ " "
      ^ render_expr_desc node.desc)
  in
  String.concat
    "\n"
    ([
       "patterns:";
     ]
     @ pattern_lines
     @ [
       "";
       "bindings:";
     ]
     @ binding_lines
     @ [
       "";
       "expressions:";
     ]
     @ expr_lines
     @ [ "" ])

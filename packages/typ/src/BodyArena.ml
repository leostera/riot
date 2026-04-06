open Std

type pattern_desc =
  | PVar of string
  | PWildcard
  | PInt of string
  | PFloat of string
  | PBool of bool
  | PString of string
  | PChar of string
  | PUnit
  | PTuple of PatId.t list
  | PConstructor of { constructor: string; arguments: PatId.t list }
  | PRecord of { fields: record_pattern_field list; open_: bool }
  | PList of PatId.t list
  | PAlias of { pattern_id: PatId.t; alias: string }
  | PPolyVariant of { tag: string; payload: PatId.t option }
  | PUnsupported of string

and record_pattern_field = {
  label: string;
  pattern_id: PatId.t;
}

type pattern_node = {
  pat_id: PatId.t;
  origin_id: OriginId.t;
  desc: pattern_desc;
}

type match_case = {
  pattern_id: PatId.t;
  body_id: ExprId.t;
}

type label =
  | Positional
  | Labeled of string
  | Optional of string

type function_parameter = {
  label: label;
  pattern_id: PatId.t;
}

type apply_argument = {
  label: label;
  value_id: ExprId.t;
}

type expr_desc =
  | EVar of string
  | EInt of string
  | EFloat of string
  | EBool of bool
  | EString of string
  | EChar of string
  | EUnit
  | ETuple of ExprId.t list
  | EArray of ExprId.t list
  | ESequence of ExprId.t list
  | EFun of function_parameter list * ExprId.t
  | EApply of ExprId.t * apply_argument list
  | ERecord of { base_id: ExprId.t option; fields: record_expr_field list }
  | EFieldAccess of { receiver_id: ExprId.t; label: string }
  | EIndex of ExprId.t * ExprId.t
  | ELet of BindingId.t list * ExprId.t
  | EIf of ExprId.t * ExprId.t * ExprId.t
  | EMatch of ExprId.t * match_case list
  | ETry of ExprId.t * match_case list
  | EPolyVariant of { tag: string; payload: ExprId.t option }
  | ELocalOpen of { module_path: string; body_id: ExprId.t }
  | EUnsupported of string
  | EHole of string

and record_expr_field = {
  label: string;
  value_id: ExprId.t;
}

and expr_node = {
  expr_id: ExprId.t;
  origin_id: OriginId.t;
  desc: expr_desc;
}

and binding = {
  binding_id: BindingId.t;
  origin_id: OriginId.t;
  scope_path: string list;
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

let empty = { patterns = []; expressions = []; bindings = [] }

let of_lists = fun ~patterns ~expressions ~bindings -> { patterns; expressions; bindings }

let patterns = fun arena -> arena.patterns

let expressions = fun arena -> arena.expressions

let bindings = fun arena -> arena.bindings

let find_pattern = fun arena pat_id ->
  List.find_opt
    (fun (node: pattern_node) ->
      PatId.equal node.pat_id pat_id)
    arena.patterns

let find_expr = fun arena expr_id ->
  List.find_opt
    (fun (node: expr_node) ->
      ExprId.equal node.expr_id expr_id)
    arena.expressions

let find_binding = fun arena binding_id ->
  List.find_opt
    (fun (binding: binding) ->
      BindingId.equal binding.binding_id binding_id)
    arena.bindings

let render_ids = fun render ids -> ids |> List.map render |> String.concat ", "

let render_label = function
  | Positional -> "_"
  | Labeled label -> "~" ^ label
  | Optional label -> "?" ^ label

let render_function_parameter = fun (parameter: function_parameter) ->
  render_label parameter.label ^ ":" ^ PatId.to_string parameter.pattern_id

let render_apply_argument = fun (argument: apply_argument) ->
  render_label argument.label ^ ":" ^ ExprId.to_string argument.value_id

let render_record_pattern_field = fun (field: record_pattern_field) ->
  field.label ^ "=" ^ PatId.to_string field.pattern_id

let render_record_expr_field = fun (field: record_expr_field) ->
  field.label ^ "=" ^ ExprId.to_string field.value_id

let render_pattern_desc = function
  | PVar name ->
      "var " ^ name
  | PWildcard ->
      "_"
  | PInt digits ->
      "int " ^ digits
  | PFloat digits ->
      "float " ^ digits
  | PBool value ->
      "bool " ^ Bool.to_string value
  | PString value ->
      "string \"" ^ String.escaped value ^ "\""
  | PChar value ->
      "char '" ^ String.escaped value ^ "'"
  | PUnit ->
      "unit"
  | PTuple elements ->
      "tuple [" ^ render_ids PatId.to_string elements ^ "]"
  | PConstructor { constructor; arguments } ->
      "constructor " ^ constructor ^ " [" ^ render_ids PatId.to_string arguments ^ "]"
  | PRecord { fields; open_ } ->
      "record { "
      ^ (fields |> List.map render_record_pattern_field |> String.concat ", ")
      ^ " }"
      ^ if open_ then " open" else ""
  | PList elements ->
      "list [" ^ render_ids PatId.to_string elements ^ "]"
  | PAlias { pattern_id; alias } ->
      "alias " ^ alias ^ " = " ^ PatId.to_string pattern_id
  | PPolyVariant { tag; payload } -> (
      match payload with
      | Some pattern_id -> "poly_variant `" ^ tag ^ " " ^ PatId.to_string pattern_id
      | None -> "poly_variant `" ^ tag
    )
  | PUnsupported summary ->
      "unsupported(" ^ summary ^ ")"

let render_expr_desc = function
  | EVar name ->
      "var " ^ name
  | EInt digits ->
      "int " ^ digits
  | EFloat digits ->
      "float " ^ digits
  | EBool value ->
      "bool " ^ Bool.to_string value
  | EString value ->
      "string \"" ^ String.escaped value ^ "\""
  | EChar value ->
      "char '" ^ String.escaped value ^ "'"
  | EUnit ->
      "unit"
  | ETuple elements ->
      "tuple [" ^ render_ids ExprId.to_string elements ^ "]"
  | EArray elements ->
      "array [" ^ render_ids ExprId.to_string elements ^ "]"
  | ESequence elements ->
      "sequence [" ^ render_ids ExprId.to_string elements ^ "]"
  | EFun (parameters, body_id) ->
      "fun ["
      ^ (parameters |> List.map render_function_parameter |> String.concat ", ")
      ^ "] -> "
      ^ ExprId.to_string body_id
  | EApply (callee_id, arguments) ->
      "apply "
      ^ ExprId.to_string callee_id
      ^ " ["
      ^ (arguments |> List.map render_apply_argument |> String.concat ", ")
      ^ "]"
  | ERecord { base_id; fields } ->
      let base =
        match base_id with
        | Some expr_id -> "base=" ^ ExprId.to_string expr_id ^ " "
        | None -> ""
      in
      "record " ^ base ^ "{ " ^ (fields |> List.map render_record_expr_field |> String.concat ", ") ^ " }"
  | EFieldAccess { receiver_id; label } ->
      "field " ^ ExprId.to_string receiver_id ^ "." ^ label
  | EIndex (collection_id, index_id) ->
      "index " ^ ExprId.to_string collection_id ^ " [" ^ ExprId.to_string index_id ^ "]"
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
      let cases_text = cases
      |> List.map
        (fun (case: match_case) ->
          "(" ^ PatId.to_string case.pattern_id ^ " -> " ^ ExprId.to_string case.body_id ^ ")")
      |> String.concat ", " in
      "match " ^ ExprId.to_string scrutinee_id ^ " with [" ^ cases_text ^ "]"
  | ETry (body_id, cases) ->
      let cases_text = cases
      |> List.map
        (fun (case: match_case) ->
          "(" ^ PatId.to_string case.pattern_id ^ " -> " ^ ExprId.to_string case.body_id ^ ")")
      |> String.concat ", " in
      "try " ^ ExprId.to_string body_id ^ " with [" ^ cases_text ^ "]"
  | EPolyVariant { tag; payload } -> (
      match payload with
      | Some expr_id -> "poly_variant `" ^ tag ^ " " ^ ExprId.to_string expr_id
      | None -> "poly_variant `" ^ tag
    )
  | ELocalOpen { module_path; body_id } ->
      "local_open " ^ module_path ^ " (" ^ ExprId.to_string body_id ^ ")"
  | EUnsupported summary ->
      "unsupported(" ^ summary ^ ")"
  | EHole summary ->
      "hole(" ^ summary ^ ")"

let render_binding = fun (binding: binding) ->
  let name =
    match binding.name with
    | Some name -> name
    | None -> "_"
  in
  let qualified_name =
    match binding.scope_path with
    | [] -> name
    | scope_path -> String.concat "." scope_path ^ "." ^ name
  in
  "  "
  ^ BindingId.to_string binding.binding_id
  ^ " "
  ^ qualified_name
  ^ " "
  ^ PatId.to_string binding.pattern_id
  ^ " "
  ^ ExprId.to_string binding.value_id
  ^ " recursive="
  ^ Bool.to_string binding.recursive

let record_pattern_field_to_json = fun (field: record_pattern_field) ->
  Data.Json.Object [
    ("label", Data.Json.String field.label);
    ("pattern_id", Data.Json.Int (PatId.to_int field.pattern_id));
  ]

let record_expr_field_to_json = fun (field: record_expr_field) ->
  Data.Json.Object [
    ("label", Data.Json.String field.label);
    ("value_id", Data.Json.Int (ExprId.to_int field.value_id));
  ]

let pattern_desc_to_json = function
  | PVar name -> Data.Json.Object [
    ("tag", Data.Json.String "var");
    ("name", Data.Json.String name)
  ]
  | PWildcard -> Data.Json.Object [ ("tag", Data.Json.String "wildcard") ]
  | PInt digits -> Data.Json.Object [
    ("tag", Data.Json.String "int");
    ("digits", Data.Json.String digits)
  ]
  | PFloat digits -> Data.Json.Object [
    ("tag", Data.Json.String "float");
    ("digits", Data.Json.String digits)
  ]
  | PConstructor { constructor; arguments } -> Data.Json.Object [
    ("tag", Data.Json.String "constructor");
    ("constructor", Data.Json.String constructor);
    (
      "arguments",
      Data.Json.Array (List.map (fun pat_id -> Data.Json.Int (PatId.to_int pat_id)) arguments)
    );
  ]
  | PRecord { fields; open_ } -> Data.Json.Object [
    ("tag", Data.Json.String "record");
    ("fields", Data.Json.Array (List.map record_pattern_field_to_json fields));
    ("open", Data.Json.Bool open_);
  ]
  | PList elements -> Data.Json.Object [
    ("tag", Data.Json.String "list");
    (
      "elements",
      Data.Json.Array (List.map (fun pat_id -> Data.Json.Int (PatId.to_int pat_id)) elements)
    );
  ]
  | PAlias { pattern_id; alias } -> Data.Json.Object [
    ("tag", Data.Json.String "alias");
    ("pattern_id", Data.Json.Int (PatId.to_int pattern_id));
    ("alias", Data.Json.String alias)
  ]
  | PPolyVariant { tag; payload } ->
      Data.Json.Object [
        ("tag", Data.Json.String "poly_variant");
        ("variant_tag", Data.Json.String tag);
        (
          "payload",
          match payload with
          | Some pattern_id -> Data.Json.Int (PatId.to_int pattern_id)
          | None -> Data.Json.Null
        );
      ]
  | PBool value -> Data.Json.Object [
    ("tag", Data.Json.String "bool");
    ("value", Data.Json.Bool value)
  ]
  | PString value -> Data.Json.Object [
    ("tag", Data.Json.String "string");
    ("value", Data.Json.String value)
  ]
  | PChar value -> Data.Json.Object [
    ("tag", Data.Json.String "char");
    ("value", Data.Json.String value)
  ]
  | PUnit -> Data.Json.Object [ ("tag", Data.Json.String "unit") ]
  | PTuple elements -> Data.Json.Object [
    ("tag", Data.Json.String "tuple");
    (
      "elements",
      Data.Json.Array (List.map (fun pat_id -> Data.Json.Int (PatId.to_int pat_id)) elements)
    );
  ]
  | PUnsupported summary -> Data.Json.Object [
    ("tag", Data.Json.String "unsupported");
    ("summary", Data.Json.String summary)
  ]

let match_case_to_json = fun (case: match_case) ->
  Data.Json.Object [
    ("pattern_id", Data.Json.Int (PatId.to_int case.pattern_id));
    ("body_id", Data.Json.Int (ExprId.to_int case.body_id));
  ]

let label_to_json = function
  | Positional -> Data.Json.Object [ ("tag", Data.Json.String "positional") ]
  | Labeled label -> Data.Json.Object [
    ("tag", Data.Json.String "labeled");
    ("label", Data.Json.String label);
  ]
  | Optional label -> Data.Json.Object [
    ("tag", Data.Json.String "optional");
    ("label", Data.Json.String label);
  ]

let function_parameter_to_json = fun (parameter: function_parameter) ->
  Data.Json.Object [
    ("label", label_to_json parameter.label);
    ("pattern_id", Data.Json.Int (PatId.to_int parameter.pattern_id));
  ]

let apply_argument_to_json = fun (argument: apply_argument) ->
  Data.Json.Object [
    ("label", label_to_json argument.label);
    ("value_id", Data.Json.Int (ExprId.to_int argument.value_id));
  ]

let expr_desc_to_json = function
  | EVar name -> Data.Json.Object [
    ("tag", Data.Json.String "var");
    ("name", Data.Json.String name)
  ]
  | EInt digits -> Data.Json.Object [
    ("tag", Data.Json.String "int");
    ("digits", Data.Json.String digits)
  ]
  | EFloat digits -> Data.Json.Object [
    ("tag", Data.Json.String "float");
    ("digits", Data.Json.String digits)
  ]
  | EBool value -> Data.Json.Object [
    ("tag", Data.Json.String "bool");
    ("value", Data.Json.Bool value)
  ]
  | EString value -> Data.Json.Object [
    ("tag", Data.Json.String "string");
    ("value", Data.Json.String value)
  ]
  | EChar value -> Data.Json.Object [
    ("tag", Data.Json.String "char");
    ("value", Data.Json.String value)
  ]
  | EUnit -> Data.Json.Object [ ("tag", Data.Json.String "unit") ]
  | ETuple elements -> Data.Json.Object [
    ("tag", Data.Json.String "tuple");
    (
      "elements",
      Data.Json.Array (List.map (fun expr_id -> Data.Json.Int (ExprId.to_int expr_id)) elements)
    );
  ]
  | EArray elements -> Data.Json.Object [
    ("tag", Data.Json.String "array");
    (
      "elements",
      Data.Json.Array (List.map (fun expr_id -> Data.Json.Int (ExprId.to_int expr_id)) elements)
    );
  ]
  | ESequence elements -> Data.Json.Object [
    ("tag", Data.Json.String "sequence");
    (
      "elements",
      Data.Json.Array (List.map (fun expr_id -> Data.Json.Int (ExprId.to_int expr_id)) elements)
    );
  ]
  | EFun (parameters, body_id) -> Data.Json.Object [
    ("tag", Data.Json.String "fun");
    ("parameters", Data.Json.Array (List.map function_parameter_to_json parameters));
    ("body_id", Data.Json.Int (ExprId.to_int body_id));
  ]
  | EApply (callee_id, arguments) -> Data.Json.Object [
    ("tag", Data.Json.String "apply");
    ("callee_id", Data.Json.Int (ExprId.to_int callee_id));
    ("arguments", Data.Json.Array (List.map apply_argument_to_json arguments));
  ]
  | ERecord { base_id; fields } ->
      Data.Json.Object [
        ("tag", Data.Json.String "record");
        (
          "base_id",
          match base_id with
          | Some expr_id -> Data.Json.Int (ExprId.to_int expr_id)
          | None -> Data.Json.Null
        );
        ("fields", Data.Json.Array (List.map record_expr_field_to_json fields));
      ]
  | EFieldAccess { receiver_id; label } -> Data.Json.Object [
    ("tag", Data.Json.String "field_access");
    ("receiver_id", Data.Json.Int (ExprId.to_int receiver_id));
    ("label", Data.Json.String label);
  ]
  | EIndex (collection_id, index_id) -> Data.Json.Object [
    ("tag", Data.Json.String "index");
    ("collection_id", Data.Json.Int (ExprId.to_int collection_id));
    ("index_id", Data.Json.Int (ExprId.to_int index_id));
  ]
  | ELet (binding_ids, body_id) -> Data.Json.Object [
    ("tag", Data.Json.String "let");
    (
      "binding_ids",
      Data.Json.Array (List.map (fun binding_id -> Data.Json.Int (BindingId.to_int binding_id)) binding_ids)
    );
    ("body_id", Data.Json.Int (ExprId.to_int body_id));
  ]
  | EIf (condition_id, then_id, else_id) -> Data.Json.Object [
    ("tag", Data.Json.String "if");
    ("condition_id", Data.Json.Int (ExprId.to_int condition_id));
    ("then_id", Data.Json.Int (ExprId.to_int then_id));
    ("else_id", Data.Json.Int (ExprId.to_int else_id));
  ]
  | EMatch (scrutinee_id, cases) -> Data.Json.Object [
    ("tag", Data.Json.String "match");
    ("scrutinee_id", Data.Json.Int (ExprId.to_int scrutinee_id));
    ("cases", Data.Json.Array (List.map match_case_to_json cases));
  ]
  | ETry (body_id, cases) -> Data.Json.Object [
    ("tag", Data.Json.String "try");
    ("body_id", Data.Json.Int (ExprId.to_int body_id));
    ("cases", Data.Json.Array (List.map match_case_to_json cases));
  ]
  | EPolyVariant { tag; payload } ->
      Data.Json.Object [
        ("tag", Data.Json.String "poly_variant");
        ("variant_tag", Data.Json.String tag);
        (
          "payload",
          match payload with
          | Some expr_id -> Data.Json.Int (ExprId.to_int expr_id)
          | None -> Data.Json.Null
        );
      ]
  | ELocalOpen { module_path; body_id } -> Data.Json.Object [
    ("tag", Data.Json.String "local_open");
    ("module_path", Data.Json.String module_path);
    ("body_id", Data.Json.Int (ExprId.to_int body_id));
  ]
  | EUnsupported summary -> Data.Json.Object [
    ("tag", Data.Json.String "unsupported");
    ("summary", Data.Json.String summary)
  ]
  | EHole summary -> Data.Json.Object [
    ("tag", Data.Json.String "hole");
    ("summary", Data.Json.String summary)
  ]

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
    (
      "scope_path",
      Data.Json.Array (List.map (fun segment -> Data.Json.String segment) binding.scope_path)
    );
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
  let pattern_lines = arena.patterns
  |> List.map
    (fun (node: pattern_node) ->
      "  "
      ^ PatId.to_string node.pat_id
      ^ " "
      ^ OriginId.to_string node.origin_id
      ^ " "
      ^ render_pattern_desc node.desc) in
  let binding_lines = arena.bindings |> List.map render_binding in
  let expr_lines = arena.expressions
  |> List.map
    (fun (node: expr_node) ->
      "  "
      ^ ExprId.to_string node.expr_id
      ^ " "
      ^ OriginId.to_string node.origin_id
      ^ " "
      ^ render_expr_desc node.desc) in
  String.concat
    "\n"
    ([ "patterns:"; ]
    @ pattern_lines
    @ [ ""; "bindings:"; ]
    @ binding_lines
    @ [ ""; "expressions:"; ]
    @ expr_lines
    @ [ "" ])

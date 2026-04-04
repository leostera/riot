open Std

type semantic_id =
  | Item of ItemId.t
  | Binding of BindingId.t
  | Expr of ExprId.t
  | Pattern of PatId.t

type kind =
  | ItemKind
  | BindingKind
  | ExprKind
  | PatternKind

type origin = {
  origin_id: OriginId.t;
  source_id: SourceId.t;
  source_revision: int;
  semantic_id: semantic_id;
  label: string;
  syntax_kind: Syn.SyntaxKind.t;
  span: Syn.Ceibo.Span.t;
}

type t = origin list

let empty = []

let of_list = fun origins -> origins

let origins = fun origins -> origins

let kind_of_semantic_id = function
  | Item _ -> ItemKind
  | Binding _ -> BindingKind
  | Expr _ -> ExprKind
  | Pattern _ -> PatternKind

let semantic_id_equal = fun left right ->
  match (left, right) with
  | Item left, Item right -> ItemId.equal left right
  | Binding left, Binding right -> BindingId.equal left right
  | Expr left, Expr right -> ExprId.equal left right
  | Pattern left, Pattern right -> PatId.equal left right
  | _ -> false

let find = fun origins origin_id ->
  List.find_opt (fun (origin: origin) -> OriginId.equal origin.origin_id origin_id) origins

let find_by_semantic_id = fun origins semantic_id ->
  List.find_opt (fun (origin: origin) -> semantic_id_equal origin.semantic_id semantic_id) origins

let find_item = fun origins item_id ->
  find_by_semantic_id origins (Item item_id)

let find_binding = fun origins binding_id ->
  find_by_semantic_id origins (Binding binding_id)

let find_expr = fun origins expr_id ->
  find_by_semantic_id origins (Expr expr_id)

let find_pattern = fun origins pat_id ->
  find_by_semantic_id origins (Pattern pat_id)

let kind_to_string = function
  | ItemKind -> "item"
  | BindingKind -> "binding"
  | ExprKind -> "expr"
  | PatternKind -> "pattern"

let semantic_id_to_string = function
  | Item item_id -> ItemId.to_string item_id
  | Binding binding_id -> BindingId.to_string binding_id
  | Expr expr_id -> ExprId.to_string expr_id
  | Pattern pat_id -> PatId.to_string pat_id

let semantic_id_to_json = function
  | Item item_id ->
      Data.Json.Object [
        ("tag", Data.Json.String "item");
        ("id", Data.Json.Int (ItemId.to_int item_id));
      ]
  | Binding binding_id ->
      Data.Json.Object [
        ("tag", Data.Json.String "binding");
        ("id", Data.Json.Int (BindingId.to_int binding_id));
      ]
  | Expr expr_id ->
      Data.Json.Object [
        ("tag", Data.Json.String "expr");
        ("id", Data.Json.Int (ExprId.to_int expr_id));
      ]
  | Pattern pat_id ->
      Data.Json.Object [
        ("tag", Data.Json.String "pattern");
        ("id", Data.Json.Int (PatId.to_int pat_id));
      ]

let span_to_json = fun (span: Syn.Ceibo.Span.t) ->
  Data.Json.Object [
    ("start", Data.Json.Int span.start);
    ("end", Data.Json.Int span.end_);
  ]

let origin_to_json = fun (origin: origin) ->
  Data.Json.Object [
    ("origin_id", Data.Json.Int (OriginId.to_int origin.origin_id));
    ("source_id", Data.Json.Int (SourceId.to_int origin.source_id));
    ("source_revision", Data.Json.Int origin.source_revision);
    ("kind", Data.Json.String (kind_to_string (kind_of_semantic_id origin.semantic_id)));
    ("semantic_id", semantic_id_to_json origin.semantic_id);
    ("label", Data.Json.String origin.label);
    ("syntax_kind", Data.Json.String (Syn.SyntaxKind.to_string origin.syntax_kind));
    ("span", span_to_json origin.span);
  ]

let to_json = fun origins ->
  Data.Json.Array (List.map origin_to_json origins)

let to_string = fun origins ->
  match origins with
  | [] -> "  none\n"
  | _ ->
      origins
      |> List.map (fun (origin: origin) ->
        "  "
        ^ OriginId.to_string origin.origin_id
        ^ " "
        ^ kind_to_string (kind_of_semantic_id origin.semantic_id)
        ^ " "
        ^ semantic_id_to_string origin.semantic_id
        ^ " "
        ^ origin.label
        ^ " "
        ^ Syn.SyntaxKind.to_string origin.syntax_kind
        ^ " @ "
        ^ Syn.Ceibo.Span.to_string origin.span
        ^ " "
        ^ SourceId.to_string origin.source_id
        ^ " rev="
        ^ Int.to_string origin.source_revision)
      |> String.concat "\n"
      |> fun text -> text ^ "\n"

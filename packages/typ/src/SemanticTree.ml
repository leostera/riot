open Std

type file = {
  item_tree: ItemTree.t;
  body_arena: BodyArena.t;
  origin_map: OriginMap.t;
  diagnostics: Diagnostic.t list;
}

let empty = {
  item_tree = ItemTree.empty;
  body_arena = BodyArena.empty;
  origin_map = OriginMap.empty;
  diagnostics = [];
}

let find_origin = fun file origin_id ->
  OriginMap.find file.origin_map origin_id

let find_item = fun file item_id ->
  ItemTree.find_item file.item_tree item_id

let find_binding = fun file binding_id ->
  BodyArena.find_binding file.body_arena binding_id

let find_pattern = fun file pat_id ->
  BodyArena.find_pattern file.body_arena pat_id

let find_expr = fun file expr_id ->
  BodyArena.find_expr file.body_arena expr_id

let to_string = fun file ->
  String.concat
    ""
    [
      "origin map:\n";
      OriginMap.to_string file.origin_map;
      "item tree:\n";
      ItemTree.to_string file.item_tree;
      "body arena:\n";
      BodyArena.to_string file.body_arena;
    ]

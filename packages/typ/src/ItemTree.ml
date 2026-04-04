open Std

type value_item = {
  item_id: ItemId.t;
  origin_id: OriginId.t;
  binding_ids: BindingId.t list;
  recursive: bool;
}

type unsupported_item = {
  item_id: ItemId.t;
  origin_id: OriginId.t;
  summary: string;
}

type item =
  | Value of value_item
  | Unsupported of unsupported_item

type t = item list

let empty = []

let of_list = fun items -> items

let items = fun items -> items

let find_item = fun items item_id ->
  List.find_opt
    (function
      | Value (item: value_item) -> ItemId.equal item.item_id item_id
      | Unsupported (item: unsupported_item) -> ItemId.equal item.item_id item_id)
    items

let item_to_json = function
  | Value (item: value_item) ->
      Data.Json.Object [
        ("tag", Data.Json.String "value");
        ("item_id", Data.Json.Int (ItemId.to_int item.item_id));
        ("origin_id", Data.Json.Int (OriginId.to_int item.origin_id));
        ("binding_ids", Data.Json.Array (List.map (fun binding_id -> Data.Json.Int (BindingId.to_int binding_id)) item.binding_ids));
        ("recursive", Data.Json.Bool item.recursive);
      ]
  | Unsupported (item: unsupported_item) ->
      Data.Json.Object [
        ("tag", Data.Json.String "unsupported");
        ("item_id", Data.Json.Int (ItemId.to_int item.item_id));
        ("origin_id", Data.Json.Int (OriginId.to_int item.origin_id));
        ("summary", Data.Json.String item.summary);
      ]

let to_json = fun items ->
  Data.Json.Array (List.map item_to_json items)

let to_string = fun items ->
  match items with
  | [] -> "  none\n"
  | _ ->
      items
      |> List.map (function
        | Value (item: value_item) ->
            "  "
            ^ ItemId.to_string item.item_id
            ^ " value "
            ^ OriginId.to_string item.origin_id
            ^ " recursive="
            ^ Bool.to_string item.recursive
            ^ " bindings=["
            ^ (item.binding_ids |> List.map BindingId.to_string |> String.concat ", ")
            ^ "]"
        | Unsupported (item: unsupported_item) ->
            "  "
            ^ ItemId.to_string item.item_id
            ^ " unsupported "
            ^ OriginId.to_string item.origin_id
            ^ " "
            ^ item.summary)
      |> String.concat "\n"
      |> fun text -> text ^ "\n"

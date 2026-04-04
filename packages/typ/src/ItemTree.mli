open Std

(** Body-stable item skeleton for one lowered source. *)
type value_item = {
  (** Stable item identity. *)
  item_id: ItemId.t;
  (** Source origin for this item shell. *)
  origin_id: OriginId.t;
  (** Top-level bindings introduced by this item. *)
  binding_ids: BindingId.t list;
  (** Whether the item's binding group is recursive. *)
  recursive: bool;
}

type unsupported_item = {
  (** Stable item identity. *)
  item_id: ItemId.t;
  (** Source origin for this placeholder item. *)
  origin_id: OriginId.t;
  (** Short recovery summary naming the unsupported syntax family. *)
  summary: string;
}

type item =
  (** Value-bearing top-level item. *)
  | Value of value_item
  (** Placeholder top-level item produced by recovery. *)
  | Unsupported of unsupported_item

(** Ordered top-level item skeleton for one file. *)
type t

(** Empty item tree. *)
val empty: t

(** Build an item tree from prepared items. *)
val of_list: item list -> t

(** Enumerate items in source order. *)
val items: t -> item list

(** Find one item by [ItemId]. *)
val find_item: t -> ItemId.t -> item option

(** Encode the item tree as structured JSON for snapshot tests and tooling. *)
val to_json: t -> Data.Json.t

(** Render the item tree as debug text. *)
val to_string: t -> string

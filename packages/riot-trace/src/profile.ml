open Std

module Ser = Serde.Ser

type call_cost = {
  name: string;
  samples: int;
  total_samples: int;
  self_weight_ns: int;
  total_weight_ns: int;
}

type call_tree_node = {
  name: string;
  self_samples: int;
  total_samples: int;
  self_weight_ns: int;
  total_weight_ns: int;
  children: call_tree_node list;
  hidden_children: int;
}

type t = {
  sample_count: int;
  total_weight_ns: int;
  top_self: call_cost list;
  top_total: call_cost list;
  call_tree: call_tree_node list;
  hidden_call_tree_roots: int;
}

let weight_ms = fun weight_ns -> Float.from_int weight_ns /. 1_000_000.0

let ser_list = fun serializer ->
  Ser.contramap Collections.Vector.from_list (Ser.list serializer)

let call_cost_serializer =
  Ser.record
    (
      Ser.fields [
        Ser.field "name" Ser.string (fun (call: call_cost) -> call.name);
        Ser.field "samples" Ser.int (fun (call: call_cost) -> call.samples);
        Ser.field "total_samples" Ser.int (fun (call: call_cost) -> call.total_samples);
        Ser.field "self_weight_ns" Ser.int (fun (call: call_cost) -> call.self_weight_ns);
        Ser.field
          "self_weight_ms"
          Ser.float
          (fun (call: call_cost) -> weight_ms call.self_weight_ns);
        Ser.field "total_weight_ns" Ser.int (fun (call: call_cost) -> call.total_weight_ns);
        Ser.field
          "total_weight_ms"
          Ser.float
          (fun (call: call_cost) -> weight_ms call.total_weight_ns);
      ]
    )

let rec call_tree_node_serializer =
  {
    Ser.run = (fun backend state node ->
      let serializer =
        Ser.record
          (
            Ser.fields [
              Ser.field "name" Ser.string (fun (node: call_tree_node) -> node.name);
              Ser.field "self_samples" Ser.int (fun (node: call_tree_node) -> node.self_samples);
              Ser.field
                "total_samples"
                Ser.int
                (fun (node: call_tree_node) -> node.total_samples);
              Ser.field
                "self_weight_ns"
                Ser.int
                (fun (node: call_tree_node) -> node.self_weight_ns);
              Ser.field
                "self_weight_ms"
                Ser.float
                (fun (node: call_tree_node) -> weight_ms node.self_weight_ns);
              Ser.field
                "total_weight_ns"
                Ser.int
                (fun (node: call_tree_node) -> node.total_weight_ns);
              Ser.field
                "total_weight_ms"
                Ser.float
                (fun (node: call_tree_node) -> weight_ms node.total_weight_ns);
              Ser.field
                "children"
                (ser_list call_tree_node_serializer)
                (fun (node: call_tree_node) -> node.children);
              Ser.field
                "hidden_children"
                Ser.int
                (fun (node: call_tree_node) -> node.hidden_children);
            ]
          )
      in
      serializer.run backend state node);
  }

let serializer =
  Ser.record
    (
      Ser.fields [
        Ser.field "sample_count" Ser.int (fun (profile: t) -> profile.sample_count);
        Ser.field "total_weight_ns" Ser.int (fun (profile: t) -> profile.total_weight_ns);
        Ser.field
          "total_weight_ms"
          Ser.float
          (fun (profile: t) -> weight_ms profile.total_weight_ns);
        Ser.field "top_self" (ser_list call_cost_serializer) (fun (profile: t) -> profile.top_self);
        Ser.field
          "top_total"
          (ser_list call_cost_serializer)
          (fun (profile: t) -> profile.top_total);
        Ser.field
          "call_tree"
          (ser_list call_tree_node_serializer)
          (fun (profile: t) -> profile.call_tree);
        Ser.field
          "hidden_call_tree_roots"
          Ser.int
          (fun (profile: t) -> profile.hidden_call_tree_roots);
      ]
    )

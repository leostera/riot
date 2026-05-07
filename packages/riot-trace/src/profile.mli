open Std

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

val weight_ms: int -> float

val call_cost_serializer: call_cost Serde.Ser.t

val call_tree_node_serializer: call_tree_node Serde.Ser.t

val serializer: t Serde.Ser.t

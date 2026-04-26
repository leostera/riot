type 'a tree_delta =
  | Leaf_delta
  | Node_delta of 'a tree_delta * 'a * 'a tree_delta
let _ : int tree_delta =
  Node_delta (Leaf_delta, true, Leaf_delta)

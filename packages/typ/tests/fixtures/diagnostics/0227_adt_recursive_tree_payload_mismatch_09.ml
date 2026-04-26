type 'a tree_iota =
  | Leaf_iota
  | Node_iota of 'a tree_iota * 'a * 'a tree_iota
let _ : int tree_iota =
  Node_iota (Leaf_iota, true, Leaf_iota)

type 'a tree_alpha =
  | Leaf_alpha
  | Node_alpha of 'a tree_alpha * 'a * 'a tree_alpha
let _ : int tree_alpha =
  Node_alpha (Leaf_alpha, true, Leaf_alpha)

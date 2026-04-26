type 'a tree_beta =
  | Leaf_beta
  | Node_beta of 'a tree_beta * 'a * 'a tree_beta
let _ : int tree_beta =
  Node_beta (Leaf_beta, true, Leaf_beta)

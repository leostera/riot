type 'a tree_gamma =
  | Leaf_gamma
  | Node_gamma of 'a tree_gamma * 'a * 'a tree_gamma
let _ : int tree_gamma =
  Node_gamma (Leaf_gamma, true, Leaf_gamma)

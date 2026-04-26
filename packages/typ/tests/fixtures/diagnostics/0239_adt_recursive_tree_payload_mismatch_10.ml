type 'a tree_kappa =
  | Leaf_kappa
  | Node_kappa of 'a tree_kappa * 'a * 'a tree_kappa
let _ : int tree_kappa =
  Node_kappa (Leaf_kappa, true, Leaf_kappa)

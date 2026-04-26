type 'a tree_zeta =
  | Leaf_zeta
  | Node_zeta of 'a tree_zeta * 'a * 'a tree_zeta
let _ : int tree_zeta =
  Node_zeta (Leaf_zeta, true, Leaf_zeta)

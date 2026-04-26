type 'a tree_eta =
  | Leaf_eta
  | Node_eta of 'a tree_eta * 'a * 'a tree_eta
let _ : int tree_eta =
  Node_eta (Leaf_eta, true, Leaf_eta)

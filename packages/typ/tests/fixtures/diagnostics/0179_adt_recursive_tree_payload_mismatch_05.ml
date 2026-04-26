type 'a tree_epsilon =
  | Leaf_epsilon
  | Node_epsilon of 'a tree_epsilon * 'a * 'a tree_epsilon
let _ : int tree_epsilon =
  Node_epsilon (Leaf_epsilon, true, Leaf_epsilon)

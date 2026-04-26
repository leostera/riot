type 'a tree_theta =
  | Leaf_theta
  | Node_theta of 'a tree_theta * 'a * 'a tree_theta
let _ : int tree_theta =
  Node_theta (Leaf_theta, true, Leaf_theta)

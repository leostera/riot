type _ pair_box_epsilon =
  | Pair_epsilon : int * bool -> (int * bool) pair_box_epsilon
  | Swap_epsilon : bool * int -> (bool * int) pair_box_epsilon

let _ : (int * bool) pair_box_epsilon = Pair_epsilon (true, 4)

type _ pair_box_alpha =
  | Pair_alpha : int * bool -> (int * bool) pair_box_alpha
  | Swap_alpha : bool * int -> (bool * int) pair_box_alpha

let _ : (int * bool) pair_box_alpha = Pair_alpha (true, 0)

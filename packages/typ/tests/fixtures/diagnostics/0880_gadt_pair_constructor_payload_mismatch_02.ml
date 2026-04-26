type _ pair_box_beta =
  | Pair_beta : int * bool -> (int * bool) pair_box_beta
  | Swap_beta : bool * int -> (bool * int) pair_box_beta

let _ : (int * bool) pair_box_beta = Pair_beta (true, 1)

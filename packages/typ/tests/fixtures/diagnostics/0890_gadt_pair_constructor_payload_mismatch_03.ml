type _ pair_box_gamma =
  | Pair_gamma : int * bool -> (int * bool) pair_box_gamma
  | Swap_gamma : bool * int -> (bool * int) pair_box_gamma

let _ : (int * bool) pair_box_gamma = Pair_gamma (true, 2)

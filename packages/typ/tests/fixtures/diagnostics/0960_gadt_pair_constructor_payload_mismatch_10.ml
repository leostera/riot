type _ pair_box_kappa =
  | Pair_kappa : int * bool -> (int * bool) pair_box_kappa
  | Swap_kappa : bool * int -> (bool * int) pair_box_kappa

let _ : (int * bool) pair_box_kappa = Pair_kappa (true, 9)

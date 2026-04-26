type _ pair_box_zeta =
  | Pair_zeta : int * bool -> (int * bool) pair_box_zeta
  | Swap_zeta : bool * int -> (bool * int) pair_box_zeta

let _ : (int * bool) pair_box_zeta = Pair_zeta (true, 5)

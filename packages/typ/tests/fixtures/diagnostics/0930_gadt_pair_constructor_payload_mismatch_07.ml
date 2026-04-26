type _ pair_box_eta =
  | Pair_eta : int * bool -> (int * bool) pair_box_eta
  | Swap_eta : bool * int -> (bool * int) pair_box_eta

let _ : (int * bool) pair_box_eta = Pair_eta (true, 6)

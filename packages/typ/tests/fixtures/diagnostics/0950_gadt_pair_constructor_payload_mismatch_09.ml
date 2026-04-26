type _ pair_box_iota =
  | Pair_iota : int * bool -> (int * bool) pair_box_iota
  | Swap_iota : bool * int -> (bool * int) pair_box_iota

let _ : (int * bool) pair_box_iota = Pair_iota (true, 8)

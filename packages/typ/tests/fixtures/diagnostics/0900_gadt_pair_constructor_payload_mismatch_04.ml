type _ pair_box_delta =
  | Pair_delta : int * bool -> (int * bool) pair_box_delta
  | Swap_delta : bool * int -> (bool * int) pair_box_delta

let _ : (int * bool) pair_box_delta = Pair_delta (true, 3)

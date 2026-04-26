type _ pair_box_theta =
  | Pair_theta : int * bool -> (int * bool) pair_box_theta
  | Swap_theta : bool * int -> (bool * int) pair_box_theta

let _ : (int * bool) pair_box_theta = Pair_theta (true, 7)

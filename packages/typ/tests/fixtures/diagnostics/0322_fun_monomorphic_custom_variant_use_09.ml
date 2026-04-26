type left_iota = A_iota of int
type right_iota = C_iota of bool
let use_iota f = (f (A_iota 8), f (C_iota true))

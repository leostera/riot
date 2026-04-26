type left_alpha = I_alpha of int
type right_alpha = B_alpha of bool
let use_alpha f = (f (I_alpha 0), f (B_alpha true))

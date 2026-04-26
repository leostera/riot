type left_gamma = IntC_gamma of int
type right_gamma = BoolC_gamma of bool
let use_gamma f = (f (IntC_gamma 2), f (BoolC_gamma true))

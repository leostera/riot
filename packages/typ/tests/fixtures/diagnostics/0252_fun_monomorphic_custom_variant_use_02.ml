type left_beta = Num_beta of int
type right_beta = Flag_beta of bool
let use_beta f = (f (Num_beta 1), f (Flag_beta true))

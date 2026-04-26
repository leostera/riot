type left_epsilon = Hot_epsilon of int
type right_epsilon = Cold_epsilon of bool
let use_epsilon f = (f (Hot_epsilon 4), f (Cold_epsilon true))

type left_theta = On_theta of int
type right_theta = Off_theta of bool
let use_theta f = (f (On_theta 7), f (Off_theta true))

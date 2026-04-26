type left_zeta = Low_zeta of int
type right_zeta = High_zeta of bool
let use_zeta f = (f (Low_zeta 5), f (High_zeta true))

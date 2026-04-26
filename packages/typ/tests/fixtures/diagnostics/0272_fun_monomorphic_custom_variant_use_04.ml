type left_delta = Left_delta of int
type right_delta = Right_delta of bool
let use_delta f = (f (Left_delta 3), f (Right_delta true))

type rec_gamma = IntC of int | BoolC of bool
let read_gamma = function
  | IntC x -> x
  | BoolC y -> y

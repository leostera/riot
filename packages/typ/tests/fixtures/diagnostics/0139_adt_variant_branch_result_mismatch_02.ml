type rec_beta = Num of int | Flag of bool
let read_beta = function
  | Num x -> x
  | Flag y -> y

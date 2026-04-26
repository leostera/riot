type rec_zeta = Low of int | High of bool
let read_zeta = function
  | Low x -> x
  | High y -> y

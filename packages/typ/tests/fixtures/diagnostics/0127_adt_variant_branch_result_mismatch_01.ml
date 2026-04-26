type rec_alpha = I of int | B of bool
let read_alpha = function
  | I x -> x
  | B y -> y

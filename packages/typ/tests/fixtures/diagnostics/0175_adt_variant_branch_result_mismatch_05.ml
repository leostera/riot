type rec_epsilon = Hot of int | Cold of bool
let read_epsilon = function
  | Hot x -> x
  | Cold y -> y

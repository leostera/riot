type rec_theta = On of int | Off of bool
let read_theta = function
  | On x -> x
  | Off y -> y

type rec_delta = Left of int | Right of bool
let read_delta = function
  | Left x -> x
  | Right y -> y

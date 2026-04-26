type rec_iota = A of int | C of bool
let read_iota = function
  | A x -> x
  | C y -> y

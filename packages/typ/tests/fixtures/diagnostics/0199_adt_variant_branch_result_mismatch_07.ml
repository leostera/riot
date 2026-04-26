type rec_eta = Open of int | Closed of bool
let read_eta = function
  | Open x -> x
  | Closed y -> y

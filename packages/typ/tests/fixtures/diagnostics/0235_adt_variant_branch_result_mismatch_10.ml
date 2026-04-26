type rec_kappa = Tag of int | Mark of bool
let read_kappa = function
  | Tag x -> x
  | Mark y -> y

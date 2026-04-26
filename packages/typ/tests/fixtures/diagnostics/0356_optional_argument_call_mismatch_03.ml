let ( + ) (x : int) (y : int) : int = x
let add_gamma ?(x = 2) (y : int) = x + y
let _ = add_gamma ~x:true 3

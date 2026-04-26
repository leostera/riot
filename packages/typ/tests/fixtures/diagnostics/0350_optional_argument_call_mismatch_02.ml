let ( + ) (x : int) (y : int) : int = x
let add_beta ?(x = 1) (y : int) = x + y
let _ = add_beta ~x:true 2

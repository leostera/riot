let ( + ) (x : int) (y : int) : int = x
let add_alpha ?(x = 0) (y : int) = x + y
let _ = add_alpha ~x:true 1

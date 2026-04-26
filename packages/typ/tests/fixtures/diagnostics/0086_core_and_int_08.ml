let ( && ) (x : bool) (y : bool) : bool = x
let _ = match false with true -> true && 0 | false -> false

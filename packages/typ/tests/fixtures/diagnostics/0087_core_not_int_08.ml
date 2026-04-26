let not (x : bool) : bool = x
let _ = match false with true -> not 0 | false -> false

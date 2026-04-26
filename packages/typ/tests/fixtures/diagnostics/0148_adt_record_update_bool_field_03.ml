type rec_gamma = { count : int; flag : bool }
let base_gamma = { count = 2; flag = true }
let _ = { base_gamma with flag = 3 }

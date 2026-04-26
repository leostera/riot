type rec_alpha = { left : int; right : bool }
let base_alpha = { left = 0; right = true }
let _ = { base_alpha with right = 1 }

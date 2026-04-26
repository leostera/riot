type rec_eta = { index : int; seen : bool }
let base_eta = { index = 6; seen = true }
let _ = { base_eta with seen = 7 }

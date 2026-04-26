type rec_iota = { depth : int; done : bool }
let base_iota = { depth = 8; done = true }
let _ = { base_iota with done = 9 }

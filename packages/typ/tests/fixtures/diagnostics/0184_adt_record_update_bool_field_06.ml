type rec_zeta = { age : int; ready : bool }
let base_zeta = { age = 5; ready = false }
let _ = { base_zeta with ready = 6 }

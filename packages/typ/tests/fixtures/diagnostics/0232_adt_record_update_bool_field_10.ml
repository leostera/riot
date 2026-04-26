type rec_kappa = { key : int; ok : bool }
let base_kappa = { key = 9; ok = false }
let _ = { base_kappa with ok = 10 }

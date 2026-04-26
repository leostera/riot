type rec_theta = { score : int; live : bool }
let base_theta = { score = 7; live = false }
let _ = { base_theta with live = 8 }

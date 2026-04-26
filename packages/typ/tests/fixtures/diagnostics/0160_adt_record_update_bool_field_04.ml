type rec_delta = { value : int; mark : bool }
let base_delta = { value = 3; mark = false }
let _ = { base_delta with mark = 4 }

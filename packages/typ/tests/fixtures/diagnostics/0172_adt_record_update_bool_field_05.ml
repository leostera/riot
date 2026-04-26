type rec_epsilon = { size : int; open_ : bool }
let base_epsilon = { size = 4; open_ = true }
let _ = { base_epsilon with open_ = 5 }

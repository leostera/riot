type left_eta = Open_eta of int
type right_eta = Closed_eta of bool
let use_eta f = (f (Open_eta 6), f (Closed_eta true))

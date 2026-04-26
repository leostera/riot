type left_kappa = Tag_kappa of int
type right_kappa = Mark_kappa of bool
let use_kappa f = (f (Tag_kappa 9), f (Mark_kappa true))

type poly_gamma = { run_gamma : 'a. 'a -> 'a }
let bad_gamma : poly_gamma = { run_gamma = fun _ -> 2 }

type poly_beta = { run_beta : 'a. 'a -> 'a }
let bad_beta : poly_beta = { run_beta = fun _ -> 1 }

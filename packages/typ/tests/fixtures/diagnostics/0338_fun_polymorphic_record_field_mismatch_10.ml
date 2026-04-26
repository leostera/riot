type poly_kappa = { run_kappa : 'a. 'a -> 'a }
let bad_kappa : poly_kappa = { run_kappa = fun _ -> 9 }

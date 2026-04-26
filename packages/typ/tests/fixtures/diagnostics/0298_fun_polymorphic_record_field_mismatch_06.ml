type poly_zeta = { run_zeta : 'a. 'a -> 'a }
let bad_zeta : poly_zeta = { run_zeta = fun _ -> 5 }

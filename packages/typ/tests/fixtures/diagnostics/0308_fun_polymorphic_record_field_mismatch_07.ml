type poly_eta = { run_eta : 'a. 'a -> 'a }
let bad_eta : poly_eta = { run_eta = fun _ -> 6 }

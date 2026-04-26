type poly_alpha = { run_alpha : 'a. 'a -> 'a }
let bad_alpha : poly_alpha = { run_alpha = fun _ -> 0 }

type poly_epsilon = { run_epsilon : 'a. 'a -> 'a }
let bad_epsilon : poly_epsilon = { run_epsilon = fun _ -> 4 }

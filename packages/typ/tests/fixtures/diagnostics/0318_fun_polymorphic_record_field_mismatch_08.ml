type poly_theta = { run_theta : 'a. 'a -> 'a }
let bad_theta : poly_theta = { run_theta = fun _ -> 7 }

type poly_delta = { run_delta : 'a. 'a -> 'a }
let bad_delta : poly_delta = { run_delta = fun _ -> 3 }

type poly_iota = { run_iota : 'a. 'a -> 'a }
let bad_iota : poly_iota = { run_iota = fun _ -> 8 }

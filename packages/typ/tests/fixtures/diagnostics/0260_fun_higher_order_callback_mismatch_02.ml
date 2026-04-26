let apply_beta (f : int -> int) = f 1
let _ = apply_beta (fun b -> if b then 1 else 2)

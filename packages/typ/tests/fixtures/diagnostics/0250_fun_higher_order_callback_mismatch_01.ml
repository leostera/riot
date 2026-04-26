let apply_alpha (f : int -> int) = f 0
let _ = apply_alpha (fun b -> if b then 0 else 1)

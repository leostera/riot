let apply_delta (f : int -> int) = f 3
let _ = apply_delta (fun b -> if b then 3 else 4)

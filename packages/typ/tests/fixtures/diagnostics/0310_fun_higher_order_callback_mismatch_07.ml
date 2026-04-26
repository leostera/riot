let apply_eta (f : int -> int) = f 6
let _ = apply_eta (fun b -> if b then 6 else 7)

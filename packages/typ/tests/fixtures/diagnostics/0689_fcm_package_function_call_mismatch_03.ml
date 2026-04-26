module type Fn_gamma = sig
  type t
  val run : t -> t
end

let call_gamma p =
  let module X = (val p : Fn_gamma) in
  X.run true

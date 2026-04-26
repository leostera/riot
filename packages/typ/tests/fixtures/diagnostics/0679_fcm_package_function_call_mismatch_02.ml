module type Fn_beta = sig
  type t
  val run : t -> t
end

let call_beta p =
  let module X = (val p : Fn_beta) in
  X.run true

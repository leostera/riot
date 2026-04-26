module type Fn_kappa = sig
  type t
  val run : t -> t
end

let call_kappa p =
  let module X = (val p : Fn_kappa) in
  X.run true

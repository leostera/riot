module type Fn_zeta = sig
  type t
  val run : t -> t
end

let call_zeta p =
  let module X = (val p : Fn_zeta) in
  X.run true

module type Fn_theta = sig
  type t
  val run : t -> t
end

let call_theta p =
  let module X = (val p : Fn_theta) in
  X.run true

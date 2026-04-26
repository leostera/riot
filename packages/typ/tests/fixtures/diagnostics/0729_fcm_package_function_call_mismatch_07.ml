module type Fn_eta = sig
  type t
  val run : t -> t
end

let call_eta p =
  let module X = (val p : Fn_eta) in
  X.run true

module type Fn_iota = sig
  type t
  val run : t -> t
end

let call_iota p =
  let module X = (val p : Fn_iota) in
  X.run true

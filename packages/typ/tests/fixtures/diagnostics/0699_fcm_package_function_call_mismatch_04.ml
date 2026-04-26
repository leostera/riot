module type Fn_delta = sig
  type t
  val run : t -> t
end

let call_delta p =
  let module X = (val p : Fn_delta) in
  X.run true

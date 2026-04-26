module type Fn_alpha = sig
  type t
  val run : t -> t
end

let call_alpha p =
  let module X = (val p : Fn_alpha) in
  X.run true

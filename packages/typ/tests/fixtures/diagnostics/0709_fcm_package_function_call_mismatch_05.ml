module type Fn_epsilon = sig
  type t
  val run : t -> t
end

let call_epsilon p =
  let module X = (val p : Fn_epsilon) in
  X.run true

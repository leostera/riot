module type Arg_gamma = sig
  type t
  val x : t
end

module Make_gamma (X : Arg_gamma) = struct
  let y : bool = X.x
end

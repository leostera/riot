module type Arg_beta = sig
  type t
  val x : t
end

module Make_beta (X : Arg_beta) = struct
  let y : bool = X.x
end

module type Arg_beta = sig
  type t
  val x : t
end

module Make_beta (X : Arg_beta) = struct
  let y : int = X.x
end

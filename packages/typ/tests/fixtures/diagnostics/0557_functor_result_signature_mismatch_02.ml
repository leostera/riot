module type Arg_beta = sig
  type t
  val x : t
end

module Make_beta (X : Arg_beta) : sig
  val y : bool
end = struct
  let y = X.x
end

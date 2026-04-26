module type Arg_gamma = sig
  type t
  val x : t
end

module Make_gamma (X : Arg_gamma) : sig
  val y : bool
end = struct
  let y = X.x
end

module type Arg_kappa = sig
  type t
  val x : t
end

module Make_kappa (X : Arg_kappa) : sig
  val y : bool
end = struct
  let y = X.x
end

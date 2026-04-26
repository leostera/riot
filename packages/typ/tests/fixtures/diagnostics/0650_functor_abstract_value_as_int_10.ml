module type Arg_kappa = sig
  type t
  val x : t
end

module Make_kappa (X : Arg_kappa) = struct
  let y : int = X.x
end

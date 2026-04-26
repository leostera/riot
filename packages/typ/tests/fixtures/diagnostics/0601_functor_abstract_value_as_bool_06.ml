module type Arg_zeta = sig
  type t
  val x : t
end

module Make_zeta (X : Arg_zeta) = struct
  let y : bool = X.x
end

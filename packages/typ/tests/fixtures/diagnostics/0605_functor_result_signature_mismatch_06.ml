module type Arg_zeta = sig
  type t
  val x : t
end

module Make_zeta (X : Arg_zeta) : sig
  val y : bool
end = struct
  let y = X.x
end

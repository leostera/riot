module type Arg_zeta = sig
  type t
  val id : t -> t
end

module Make_zeta (X : Arg_zeta) = struct
  let y = X.id true
end

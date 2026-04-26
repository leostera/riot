module type Arg_kappa = sig
  type t
  val id : t -> t
end

module Make_kappa (X : Arg_kappa) = struct
  let y = X.id true
end

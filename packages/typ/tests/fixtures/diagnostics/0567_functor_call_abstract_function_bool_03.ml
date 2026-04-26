module type Arg_gamma = sig
  type t
  val id : t -> t
end

module Make_gamma (X : Arg_gamma) = struct
  let y = X.id true
end

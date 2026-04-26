module type Arg_beta = sig
  type t
  val id : t -> t
end

module Make_beta (X : Arg_beta) = struct
  let y = X.id 1
end

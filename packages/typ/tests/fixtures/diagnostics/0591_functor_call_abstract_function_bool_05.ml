module type Arg_epsilon = sig
  type t
  val id : t -> t
end

module Make_epsilon (X : Arg_epsilon) = struct
  let y = X.id true
end

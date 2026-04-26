module type Arg_theta = sig
  type t
  val id : t -> t
end

module Make_theta (X : Arg_theta) = struct
  let y = X.id 7
end

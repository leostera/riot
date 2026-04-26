module type Arg_theta = sig
  type t
  val x : t
end

module Make_theta (X : Arg_theta) = struct
  let y : bool = X.x
end

module type Arg_theta = sig
  type t
  val x : t
end

module Make_theta (X : Arg_theta) : sig
  val y : bool
end = struct
  let y = X.x
end

module type Arg_epsilon = sig
  type t
  val x : t
end

module Make_epsilon (X : Arg_epsilon) = struct
  let y : bool = X.x
end

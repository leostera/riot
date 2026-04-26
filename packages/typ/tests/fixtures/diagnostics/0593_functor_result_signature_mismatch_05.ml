module type Arg_epsilon = sig
  type t
  val x : t
end

module Make_epsilon (X : Arg_epsilon) : sig
  val y : bool
end = struct
  let y = X.x
end

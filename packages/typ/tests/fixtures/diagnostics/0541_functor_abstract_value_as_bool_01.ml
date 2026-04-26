module type Arg_alpha = sig
  type t
  val x : t
end

module Make_alpha (X : Arg_alpha) = struct
  let y : bool = X.x
end

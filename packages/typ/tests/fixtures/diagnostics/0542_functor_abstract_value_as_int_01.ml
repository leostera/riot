module type Arg_alpha = sig
  type t
  val x : t
end

module Make_alpha (X : Arg_alpha) = struct
  let y : int = X.x
end

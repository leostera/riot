module type Arg_alpha = sig
  type t
  val x : t
end

module Make_alpha (X : Arg_alpha) : sig
  val y : bool
end = struct
  let y = X.x
end

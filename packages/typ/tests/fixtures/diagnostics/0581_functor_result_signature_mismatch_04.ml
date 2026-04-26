module type Arg_delta = sig
  type t
  val x : t
end

module Make_delta (X : Arg_delta) : sig
  val y : bool
end = struct
  let y = X.x
end

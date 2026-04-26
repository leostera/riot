module type Arg_delta = sig
  type t
  val x : t
end

module Make_delta (X : Arg_delta) = struct
  let y : int = X.x
end

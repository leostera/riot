module type Arg_iota = sig
  type t
  val x : t
end

module Make_iota (X : Arg_iota) = struct
  let y : bool = X.x
end

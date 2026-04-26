module type Arg_iota = sig
  type t
  val x : t
end

module Make_iota (X : Arg_iota) : sig
  val y : bool
end = struct
  let y = X.x
end

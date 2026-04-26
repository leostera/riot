module type Arg_eta = sig
  type t
  val x : t
end

module Make_eta (X : Arg_eta) : sig
  val y : bool
end = struct
  let y = X.x
end

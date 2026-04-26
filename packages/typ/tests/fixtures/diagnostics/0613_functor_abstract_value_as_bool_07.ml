module type Arg_eta = sig
  type t
  val x : t
end

module Make_eta (X : Arg_eta) = struct
  let y : bool = X.x
end

module type Arg_eta = sig
  type t
  val x : t
end

module Make_eta (X : Arg_eta) = struct
  let y : int = X.x
end

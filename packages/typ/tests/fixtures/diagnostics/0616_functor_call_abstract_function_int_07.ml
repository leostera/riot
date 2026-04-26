module type Arg_eta = sig
  type t
  val id : t -> t
end

module Make_eta (X : Arg_eta) = struct
  let y = X.id 6
end

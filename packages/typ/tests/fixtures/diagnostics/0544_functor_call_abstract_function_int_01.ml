module type Arg_alpha = sig
  type t
  val id : t -> t
end

module Make_alpha (X : Arg_alpha) = struct
  let y = X.id 0
end

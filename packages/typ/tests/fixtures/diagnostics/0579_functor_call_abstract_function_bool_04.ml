module type Arg_delta = sig
  type t
  val id : t -> t
end

module Make_delta (X : Arg_delta) = struct
  let y = X.id true
end

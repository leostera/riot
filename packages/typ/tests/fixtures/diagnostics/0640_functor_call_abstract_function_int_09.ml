module type Arg_iota = sig
  type t
  val id : t -> t
end

module Make_iota (X : Arg_iota) = struct
  let y = X.id 8
end

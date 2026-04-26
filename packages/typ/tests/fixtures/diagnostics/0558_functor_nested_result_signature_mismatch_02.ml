module type Arg_beta = sig
  type t
  val x : t
end

module Make_beta (X : Arg_beta) : sig
  module Inner : sig
    val y : int
  end
end = struct
  module Inner = struct
    let y = X.x
  end
end

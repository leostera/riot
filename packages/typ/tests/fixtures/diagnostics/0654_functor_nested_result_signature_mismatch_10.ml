module type Arg_kappa = sig
  type t
  val x : t
end

module Make_kappa (X : Arg_kappa) : sig
  module Inner : sig
    val y : int
  end
end = struct
  module Inner = struct
    let y = X.x
  end
end

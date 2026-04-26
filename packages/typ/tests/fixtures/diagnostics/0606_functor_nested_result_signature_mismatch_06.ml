module type Arg_zeta = sig
  type t
  val x : t
end

module Make_zeta (X : Arg_zeta) : sig
  module Inner : sig
    val y : int
  end
end = struct
  module Inner = struct
    let y = X.x
  end
end

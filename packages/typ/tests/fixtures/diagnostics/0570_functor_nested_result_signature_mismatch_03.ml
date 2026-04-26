module type Arg_gamma = sig
  type t
  val x : t
end

module Make_gamma (X : Arg_gamma) : sig
  module Inner : sig
    val y : int
  end
end = struct
  module Inner = struct
    let y = X.x
  end
end

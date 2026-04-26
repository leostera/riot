module type Arg_iota = sig
  type t
  val x : t
end

module Make_iota (X : Arg_iota) : sig
  module Inner : sig
    val y : int
  end
end = struct
  module Inner = struct
    let y = X.x
  end
end

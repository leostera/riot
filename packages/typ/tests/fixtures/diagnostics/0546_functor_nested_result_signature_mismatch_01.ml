module type Arg_alpha = sig
  type t
  val x : t
end

module Make_alpha (X : Arg_alpha) : sig
  module Inner : sig
    val y : int
  end
end = struct
  module Inner = struct
    let y = X.x
  end
end

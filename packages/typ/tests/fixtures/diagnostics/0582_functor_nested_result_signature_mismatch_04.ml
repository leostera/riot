module type Arg_delta = sig
  type t
  val x : t
end

module Make_delta (X : Arg_delta) : sig
  module Inner : sig
    val y : int
  end
end = struct
  module Inner = struct
    let y = X.x
  end
end

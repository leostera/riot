module type Arg_epsilon = sig
  type t
  val x : t
end

module Make_epsilon (X : Arg_epsilon) : sig
  module Inner : sig
    val y : int
  end
end = struct
  module Inner = struct
    let y = X.x
  end
end

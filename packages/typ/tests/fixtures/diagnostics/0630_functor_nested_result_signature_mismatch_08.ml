module type Arg_theta = sig
  type t
  val x : t
end

module Make_theta (X : Arg_theta) : sig
  module Inner : sig
    val y : int
  end
end = struct
  module Inner = struct
    let y = X.x
  end
end

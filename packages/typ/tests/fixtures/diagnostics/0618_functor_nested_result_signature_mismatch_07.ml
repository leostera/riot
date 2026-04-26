module type Arg_eta = sig
  type t
  val x : t
end

module Make_eta (X : Arg_eta) : sig
  module Inner : sig
    val y : int
  end
end = struct
  module Inner = struct
    let y = X.x
  end
end

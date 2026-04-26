module rec A_theta : sig
  val x : unit -> int
end = struct
  let x () = B_theta.Inner.y ()
end
and B_theta : sig
  module Inner : sig
    val y : unit -> bool
  end
end = struct
  module Inner = struct
    let y () = A_theta.x ()
  end
end

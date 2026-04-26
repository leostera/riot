module rec A_zeta : sig
  val x : unit -> int
end = struct
  let x () = B_zeta.Inner.y ()
end
and B_zeta : sig
  module Inner : sig
    val y : unit -> bool
  end
end = struct
  module Inner = struct
    let y () = A_zeta.x ()
  end
end

module rec A_gamma : sig
  val x : unit -> int
end = struct
  let x () = B_gamma.Inner.y ()
end
and B_gamma : sig
  module Inner : sig
    val y : unit -> bool
  end
end = struct
  module Inner = struct
    let y () = A_gamma.x ()
  end
end

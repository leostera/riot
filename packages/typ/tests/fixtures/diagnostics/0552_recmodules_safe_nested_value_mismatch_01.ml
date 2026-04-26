module rec A_alpha : sig
  val x : unit -> int
end = struct
  let x () = B_alpha.Inner.y ()
end
and B_alpha : sig
  module Inner : sig
    val y : unit -> bool
  end
end = struct
  module Inner = struct
    let y () = A_alpha.x ()
  end
end

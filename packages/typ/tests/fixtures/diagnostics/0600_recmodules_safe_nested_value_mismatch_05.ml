module rec A_epsilon : sig
  val x : unit -> int
end = struct
  let x () = B_epsilon.Inner.y ()
end
and B_epsilon : sig
  module Inner : sig
    val y : unit -> bool
  end
end = struct
  module Inner = struct
    let y () = A_epsilon.x ()
  end
end

module rec A_eta : sig
  val x : unit -> int
end = struct
  let x () = B_eta.Inner.y ()
end
and B_eta : sig
  module Inner : sig
    val y : unit -> bool
  end
end = struct
  module Inner = struct
    let y () = A_eta.x ()
  end
end

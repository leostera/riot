module rec A_iota : sig
  val x : unit -> int
end = struct
  let x () = B_iota.Inner.y ()
end
and B_iota : sig
  module Inner : sig
    val y : unit -> bool
  end
end = struct
  module Inner = struct
    let y () = A_iota.x ()
  end
end

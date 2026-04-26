module rec A_delta : sig
  val x : unit -> int
end = struct
  let x () = B_delta.Inner.y ()
end
and B_delta : sig
  module Inner : sig
    val y : unit -> bool
  end
end = struct
  module Inner = struct
    let y () = A_delta.x ()
  end
end

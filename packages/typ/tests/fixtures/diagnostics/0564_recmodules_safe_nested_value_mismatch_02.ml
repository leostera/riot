module rec A_beta : sig
  val x : unit -> int
end = struct
  let x () = B_beta.Inner.y ()
end
and B_beta : sig
  module Inner : sig
    val y : unit -> bool
  end
end = struct
  module Inner = struct
    let y () = A_beta.x ()
  end
end

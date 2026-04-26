module rec A_kappa : sig
  val x : unit -> int
end = struct
  let x () = B_kappa.Inner.y ()
end
and B_kappa : sig
  module Inner : sig
    val y : unit -> bool
  end
end = struct
  module Inner = struct
    let y () = A_kappa.x ()
  end
end

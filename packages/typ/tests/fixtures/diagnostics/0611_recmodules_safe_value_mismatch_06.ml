module rec A_zeta : sig
  val x : unit -> int
end = struct
  let x () = B_zeta.y ()
end
and B_zeta : sig
  val y : unit -> bool
end = struct
  let y () = A_zeta.x ()
end

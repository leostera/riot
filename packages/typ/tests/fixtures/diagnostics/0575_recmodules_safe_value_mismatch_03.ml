module rec A_gamma : sig
  val x : unit -> int
end = struct
  let x () = B_gamma.y ()
end
and B_gamma : sig
  val y : unit -> bool
end = struct
  let y () = A_gamma.x ()
end

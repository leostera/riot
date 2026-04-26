module rec A_alpha : sig
  val x : unit -> int
end = struct
  let x () = B_alpha.y ()
end
and B_alpha : sig
  val y : unit -> bool
end = struct
  let y () = A_alpha.x ()
end

module rec A_epsilon : sig
  val x : unit -> int
end = struct
  let x () = B_epsilon.y ()
end
and B_epsilon : sig
  val y : unit -> bool
end = struct
  let y () = A_epsilon.x ()
end

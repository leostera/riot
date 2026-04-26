module rec A_theta : sig
  val x : unit -> int
end = struct
  let x () = B_theta.y ()
end
and B_theta : sig
  val y : unit -> bool
end = struct
  let y () = A_theta.x ()
end

module rec A_beta : sig
  val x : unit -> int
end = struct
  let x () = B_beta.y ()
end
and B_beta : sig
  val y : unit -> bool
end = struct
  let y () = A_beta.x ()
end

module rec A_eta : sig
  val x : unit -> int
end = struct
  let x () = B_eta.y ()
end
and B_eta : sig
  val y : unit -> bool
end = struct
  let y () = A_eta.x ()
end

module rec A_delta : sig
  val x : unit -> int
end = struct
  let x () = B_delta.y ()
end
and B_delta : sig
  val y : unit -> bool
end = struct
  let y () = A_delta.x ()
end

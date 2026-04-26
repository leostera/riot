module rec A_iota : sig
  val x : unit -> int
end = struct
  let x () = B_iota.y ()
end
and B_iota : sig
  val y : unit -> bool
end = struct
  let y () = A_iota.x ()
end

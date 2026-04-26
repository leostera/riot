module rec A_kappa : sig
  val x : unit -> int
end = struct
  let x () = B_kappa.y ()
end
and B_kappa : sig
  val y : unit -> bool
end = struct
  let y () = A_kappa.x ()
end

module R : sig
  type t = { size : bool }
  val x : t
end = struct
  type t = { size : int }
  let x = { size = 4 }
end

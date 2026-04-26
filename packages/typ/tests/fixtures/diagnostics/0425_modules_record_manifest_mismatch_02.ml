module N : sig
  type t = { first : bool }
  val x : t
end = struct
  type t = { first : int }
  let x = { first = 1 }
end

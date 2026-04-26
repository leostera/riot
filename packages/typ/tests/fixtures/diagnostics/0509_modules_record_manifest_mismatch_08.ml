module U : sig
  type t = { score : bool }
  val x : t
end = struct
  type t = { score : int }
  let x = { score = 7 }
end

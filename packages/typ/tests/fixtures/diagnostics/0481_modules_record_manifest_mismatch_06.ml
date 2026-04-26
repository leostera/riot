module S : sig
  type t = { age : bool }
  val x : t
end = struct
  type t = { age : int }
  let x = { age = 5 }
end

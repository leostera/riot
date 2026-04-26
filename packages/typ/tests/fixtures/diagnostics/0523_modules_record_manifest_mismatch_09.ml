module V : sig
  type t = { depth : bool }
  val x : t
end = struct
  type t = { depth : int }
  let x = { depth = 8 }
end

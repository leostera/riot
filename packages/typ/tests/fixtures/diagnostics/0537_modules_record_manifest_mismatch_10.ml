module W : sig
  type t = { key : bool }
  val x : t
end = struct
  type t = { key : int }
  let x = { key = 9 }
end

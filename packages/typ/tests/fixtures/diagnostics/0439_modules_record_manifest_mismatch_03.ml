module P : sig
  type t = { count : bool }
  val x : t
end = struct
  type t = { count : int }
  let x = { count = 2 }
end

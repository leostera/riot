module T : sig
  type t = { index : bool }
  val x : t
end = struct
  type t = { index : int }
  let x = { index = 6 }
end

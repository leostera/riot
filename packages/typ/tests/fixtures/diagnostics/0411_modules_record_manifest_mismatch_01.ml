module M : sig
  type t = { left : bool }
  val x : t
end = struct
  type t = { left : int }
  let x = { left = 0 }
end

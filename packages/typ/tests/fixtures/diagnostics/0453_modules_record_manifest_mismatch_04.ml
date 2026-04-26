module Q : sig
  type t = { value : bool }
  val x : t
end = struct
  type t = { value : int }
  let x = { value = 3 }
end

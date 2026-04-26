module W : sig
  type t = bool
  val x : t
end = struct
  type t = int
  let x = 9
end

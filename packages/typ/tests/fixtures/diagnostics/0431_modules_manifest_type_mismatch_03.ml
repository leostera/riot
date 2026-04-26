module P : sig
  type t = bool
  val x : t
end = struct
  type t = int
  let x = 2
end

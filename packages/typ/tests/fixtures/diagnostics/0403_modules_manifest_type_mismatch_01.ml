module M : sig
  type t = bool
  val x : t
end = struct
  type t = int
  let x = 0
end

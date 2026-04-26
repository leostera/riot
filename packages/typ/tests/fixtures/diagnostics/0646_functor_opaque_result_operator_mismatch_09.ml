let ( + ) (x : int) (y : int) : int = x

module type Arg_iota = sig
  type t
  val x : t
end

module Make_iota (X : Arg_iota) : sig
  type u
  val y : u
end = struct
  type u = X.t
  let y = X.x
end

module Input_iota = struct
  type t = int
  let x = 8
end

module Out_iota = Make_iota (Input_iota)
let _ = Out_iota.y + 9

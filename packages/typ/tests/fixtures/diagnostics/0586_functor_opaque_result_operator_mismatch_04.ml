let ( + ) (x : int) (y : int) : int = x

module type Arg_delta = sig
  type t
  val x : t
end

module Make_delta (X : Arg_delta) : sig
  type u
  val y : u
end = struct
  type u = X.t
  let y = X.x
end

module Input_delta = struct
  type t = int
  let x = 3
end

module Out_delta = Make_delta (Input_delta)
let _ = Out_delta.y + 4

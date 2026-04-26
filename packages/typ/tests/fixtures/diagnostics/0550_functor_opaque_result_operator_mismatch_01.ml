let ( + ) (x : int) (y : int) : int = x

module type Arg_alpha = sig
  type t
  val x : t
end

module Make_alpha (X : Arg_alpha) : sig
  type u
  val y : u
end = struct
  type u = X.t
  let y = X.x
end

module Input_alpha = struct
  type t = int
  let x = 0
end

module Out_alpha = Make_alpha (Input_alpha)
let _ = Out_alpha.y + 1

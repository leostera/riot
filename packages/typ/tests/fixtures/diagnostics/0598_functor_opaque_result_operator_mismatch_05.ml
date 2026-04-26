let ( + ) (x : int) (y : int) : int = x

module type Arg_epsilon = sig
  type t
  val x : t
end

module Make_epsilon (X : Arg_epsilon) : sig
  type u
  val y : u
end = struct
  type u = X.t
  let y = X.x
end

module Input_epsilon = struct
  type t = int
  let x = 4
end

module Out_epsilon = Make_epsilon (Input_epsilon)
let _ = Out_epsilon.y + 5

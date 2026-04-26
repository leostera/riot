module type Arg_theta = sig
  type t
  val x : t
end

module Make_theta (X : Arg_theta) : sig
  type u
  val y : u
end = struct
  type u = X.t
  let y = X.x
end

module Input_theta = struct
  type t = int
  let x = 7
end

module Out_theta = Make_theta (Input_theta)
let _ : int = Out_theta.y

module type Arg_gamma = sig
  type t
  val x : t
end

module Make_gamma (X : Arg_gamma) : sig
  type u
  val y : u
end = struct
  type u = X.t
  let y = X.x
end

module Input_gamma = struct
  type t = int
  let x = 2
end

module Out_gamma = Make_gamma (Input_gamma)
let _ : int = Out_gamma.y

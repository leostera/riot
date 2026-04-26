let ( + ) (x : int) (y : int) : int = x

module type Arg_kappa = sig
  type t
  val x : t
end

module Make_kappa (X : Arg_kappa) : sig
  type u
  val y : u
end = struct
  type u = X.t
  let y = X.x
end

module Input_kappa = struct
  type t = int
  let x = 9
end

module Out_kappa = Make_kappa (Input_kappa)
let _ = Out_kappa.y + 10

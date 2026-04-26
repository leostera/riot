module type Arg_beta = sig
  type t
  val x : t
end

module Make_beta (X : Arg_beta) : sig
  type u
  val y : u
end = struct
  type u = X.t
  let y = X.x
end

module Input_beta = struct
  type t = int
  let x = 1
end

module Out_beta = Make_beta (Input_beta)
let _ : int = Out_beta.y

module type Arg_zeta = sig
  type t
  val x : t
end

module Make_zeta (X : Arg_zeta) : sig
  type u
  val y : u
end = struct
  type u = X.t
  let y = X.x
end

module Input_zeta = struct
  type t = int
  let x = 5
end

module Out_zeta = Make_zeta (Input_zeta)
let _ : int = Out_zeta.y

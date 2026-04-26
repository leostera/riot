module type Arg_eta = sig
  type t
  val x : t
end

module Make_eta (X : Arg_eta) : sig
  type u
  val y : u
end = struct
  type u = X.t
  let y = X.x
end

module Input_eta = struct
  type t = int
  let x = 6
end

module Out_eta = Make_eta (Input_eta)
let _ : int = Out_eta.y

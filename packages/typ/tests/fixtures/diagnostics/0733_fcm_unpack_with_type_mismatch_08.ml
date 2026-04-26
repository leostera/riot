module type Box_theta = sig
  type t
  val value : t
end

module M_theta = struct
  type t = int
  let value = 7
end

let packed_theta = (module M_theta : Box_theta)
let _ =
  let module N_theta = (val packed_theta : Box_theta with type t = bool) in
  N_theta.value

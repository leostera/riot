module type Box_kappa = sig
  type t
  val value : t
end

module M_kappa = struct
  type t = int
  let value = 9
end

let packed_kappa = (module M_kappa : Box_kappa)
let _ =
  let module N_kappa = (val packed_kappa : Box_kappa with type t = bool) in
  N_kappa.value

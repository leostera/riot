module type Box_zeta = sig
  type t
  val value : t
end

module M_zeta = struct
  type t = int
  let value = 5
end

let packed_zeta = (module M_zeta : Box_zeta)
let _ =
  let module N_zeta = (val packed_zeta : Box_zeta with type t = bool) in
  N_zeta.value

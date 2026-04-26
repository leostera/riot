module type Box_gamma = sig
  type t
  val value : t
end

module M_gamma = struct
  type t = int
  let value = 2
end

let packed_gamma = (module M_gamma : Box_gamma)
let _ =
  let module N_gamma = (val packed_gamma : Box_gamma with type t = bool) in
  N_gamma.value

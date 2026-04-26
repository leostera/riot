module type Box_beta = sig
  type t
  val value : t
end

module M_beta = struct
  type t = int
  let value = 1
end

let packed_beta = (module M_beta : Box_beta)
let _ =
  let module N_beta = (val packed_beta : Box_beta with type t = bool) in
  N_beta.value

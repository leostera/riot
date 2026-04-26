module type Box_alpha = sig
  type t
  val value : t
end

module M_alpha = struct
  type t = int
  let value = 0
end

let packed_alpha = (module M_alpha : Box_alpha)
let _ =
  let module N_alpha = (val packed_alpha : Box_alpha with type t = bool) in
  N_alpha.value

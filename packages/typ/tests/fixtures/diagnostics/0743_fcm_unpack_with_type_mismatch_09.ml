module type Box_iota = sig
  type t
  val value : t
end

module M_iota = struct
  type t = int
  let value = 8
end

let packed_iota = (module M_iota : Box_iota)
let _ =
  let module N_iota = (val packed_iota : Box_iota with type t = bool) in
  N_iota.value

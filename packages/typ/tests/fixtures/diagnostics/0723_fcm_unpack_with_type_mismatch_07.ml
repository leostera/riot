module type Box_eta = sig
  type t
  val value : t
end

module M_eta = struct
  type t = int
  let value = 6
end

let packed_eta = (module M_eta : Box_eta)
let _ =
  let module N_eta = (val packed_eta : Box_eta with type t = bool) in
  N_eta.value

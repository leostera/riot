module type Box_delta = sig
  type t
  val value : t
end

module M_delta = struct
  type t = int
  let value = 3
end

let packed_delta = (module M_delta : Box_delta)
let _ =
  let module N_delta = (val packed_delta : Box_delta with type t = bool) in
  N_delta.value

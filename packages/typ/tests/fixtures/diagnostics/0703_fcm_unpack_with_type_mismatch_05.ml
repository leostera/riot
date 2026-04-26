module type Box_epsilon = sig
  type t
  val value : t
end

module M_epsilon = struct
  type t = int
  let value = 4
end

let packed_epsilon = (module M_epsilon : Box_epsilon)
let _ =
  let module N_epsilon = (val packed_epsilon : Box_epsilon with type t = bool) in
  N_epsilon.value

module type Box_zeta = sig
  type t
  val value : t
end

module M_zeta = struct
  type t = int
  let value = 5
end

let _ = (module M_zeta : Box_zeta with type t = bool)

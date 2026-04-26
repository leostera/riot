module type Box_kappa = sig
  type t
  val value : t
end

module M_kappa = struct
  type t = int
  let value = 9
end

let _ = (module M_kappa : Box_kappa with type t = bool)

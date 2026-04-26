module type Box_gamma = sig
  type t
  val value : t
end

module M_gamma = struct
  type t = int
  let value = 2
end

let _ = (module M_gamma : Box_gamma with type t = bool)

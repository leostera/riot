module type Box_alpha = sig
  type t
  val value : t
end

module M_alpha = struct
  type t = int
  let value = 0
end

let _ = (module M_alpha : Box_alpha with type t = bool)

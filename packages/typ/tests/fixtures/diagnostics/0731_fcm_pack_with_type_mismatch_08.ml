module type Box_theta = sig
  type t
  val value : t
end

module M_theta = struct
  type t = int
  let value = 7
end

let _ = (module M_theta : Box_theta with type t = bool)

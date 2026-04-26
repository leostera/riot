module type Box_beta = sig
  type t
  val value : t
end

module M_beta = struct
  type t = int
  let value = 1
end

let _ = (module M_beta : Box_beta with type t = bool)

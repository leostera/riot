module type Box_iota = sig
  type t
  val value : t
end

module M_iota = struct
  type t = int
  let value = 8
end

let _ = (module M_iota : Box_iota with type t = bool)

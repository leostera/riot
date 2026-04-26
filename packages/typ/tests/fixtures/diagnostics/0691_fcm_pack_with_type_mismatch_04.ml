module type Box_delta = sig
  type t
  val value : t
end

module M_delta = struct
  type t = int
  let value = 3
end

let _ = (module M_delta : Box_delta with type t = bool)

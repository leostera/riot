module type Box_eta = sig
  type t
  val value : t
end

module M_eta = struct
  type t = int
  let value = 6
end

let _ = (module M_eta : Box_eta with type t = bool)

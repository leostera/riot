module type Box_epsilon = sig
  type t
  val value : t
end

module M_epsilon = struct
  type t = int
  let value = 4
end

let _ = (module M_epsilon : Box_epsilon with type t = bool)

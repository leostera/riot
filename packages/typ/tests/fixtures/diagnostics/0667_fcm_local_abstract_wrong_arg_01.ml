module type Id_alpha = sig
  type t
  val id : t -> t
end

module M_alpha = struct
  type t = int
  let id x = x
end

let use_alpha (type a) (module X : Id_alpha with type t = a) (y : a) =
  X.id y

let _ = use_alpha (module M_alpha : Id_alpha with type t = int) true

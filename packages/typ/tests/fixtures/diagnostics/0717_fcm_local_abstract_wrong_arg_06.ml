module type Id_zeta = sig
  type t
  val id : t -> t
end

module M_zeta = struct
  type t = int
  let id x = x
end

let use_zeta (type a) (module X : Id_zeta with type t = a) (y : a) =
  X.id y

let _ = use_zeta (module M_zeta : Id_zeta with type t = int) true

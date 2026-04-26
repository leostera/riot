module type Id_zeta = sig
  type t
  val id : t -> t
  val value : t
end

let use_zeta (type a) (module X : Id_zeta with type t = a) : bool =
  X.id X.value

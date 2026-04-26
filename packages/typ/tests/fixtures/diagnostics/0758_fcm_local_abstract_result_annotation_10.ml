module type Id_kappa = sig
  type t
  val id : t -> t
  val value : t
end

let use_kappa (type a) (module X : Id_kappa with type t = a) : bool =
  X.id X.value

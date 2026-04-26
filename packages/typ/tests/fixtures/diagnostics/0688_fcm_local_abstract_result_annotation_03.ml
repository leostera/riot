module type Id_gamma = sig
  type t
  val id : t -> t
  val value : t
end

let use_gamma (type a) (module X : Id_gamma with type t = a) : bool =
  X.id X.value

module type Id_beta = sig
  type t
  val id : t -> t
  val value : t
end

let use_beta (type a) (module X : Id_beta with type t = a) : bool =
  X.id X.value

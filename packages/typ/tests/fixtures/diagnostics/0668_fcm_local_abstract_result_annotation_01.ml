module type Id_alpha = sig
  type t
  val id : t -> t
  val value : t
end

let use_alpha (type a) (module X : Id_alpha with type t = a) : bool =
  X.id X.value

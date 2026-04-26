module type Id_delta = sig
  type t
  val id : t -> t
  val value : t
end

let use_delta (type a) (module X : Id_delta with type t = a) : bool =
  X.id X.value

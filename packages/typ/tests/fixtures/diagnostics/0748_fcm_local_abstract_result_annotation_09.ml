module type Id_iota = sig
  type t
  val id : t -> t
  val value : t
end

let use_iota (type a) (module X : Id_iota with type t = a) : bool =
  X.id X.value

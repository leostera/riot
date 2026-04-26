module type Id_epsilon = sig
  type t
  val id : t -> t
  val value : t
end

let use_epsilon (type a) (module X : Id_epsilon with type t = a) : bool =
  X.id X.value

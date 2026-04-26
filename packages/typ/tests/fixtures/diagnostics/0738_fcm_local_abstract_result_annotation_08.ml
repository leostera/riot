module type Id_theta = sig
  type t
  val id : t -> t
  val value : t
end

let use_theta (type a) (module X : Id_theta with type t = a) : bool =
  X.id X.value

module type Id_eta = sig
  type t
  val id : t -> t
  val value : t
end

let use_eta (type a) (module X : Id_eta with type t = a) : bool =
  X.id X.value

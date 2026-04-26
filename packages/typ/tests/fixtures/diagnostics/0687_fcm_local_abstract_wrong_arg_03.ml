module type Id_gamma = sig
  type t
  val id : t -> t
end

module M_gamma = struct
  type t = int
  let id x = x
end

let use_gamma (type a) (module X : Id_gamma with type t = a) (y : a) =
  X.id y

let _ = use_gamma (module M_gamma : Id_gamma with type t = int) true

module type Id_kappa = sig
  type t
  val id : t -> t
end

module M_kappa = struct
  type t = int
  let id x = x
end

let use_kappa (type a) (module X : Id_kappa with type t = a) (y : a) =
  X.id y

let _ = use_kappa (module M_kappa : Id_kappa with type t = int) true

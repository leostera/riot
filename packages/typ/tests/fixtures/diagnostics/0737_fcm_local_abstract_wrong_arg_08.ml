module type Id_theta = sig
  type t
  val id : t -> t
end

module M_theta = struct
  type t = int
  let id x = x
end

let use_theta (type a) (module X : Id_theta with type t = a) (y : a) =
  X.id y

let _ = use_theta (module M_theta : Id_theta with type t = int) true

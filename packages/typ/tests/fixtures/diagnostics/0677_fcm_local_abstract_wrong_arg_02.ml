module type Id_beta = sig
  type t
  val id : t -> t
end

module M_beta = struct
  type t = int
  let id x = x
end

let use_beta (type a) (module X : Id_beta with type t = a) (y : a) =
  X.id y

let _ = use_beta (module M_beta : Id_beta with type t = int) true

module type Id_iota = sig
  type t
  val id : t -> t
end

module M_iota = struct
  type t = int
  let id x = x
end

let use_iota (type a) (module X : Id_iota with type t = a) (y : a) =
  X.id y

let _ = use_iota (module M_iota : Id_iota with type t = int) true

module type Id_delta = sig
  type t
  val id : t -> t
end

module M_delta = struct
  type t = int
  let id x = x
end

let use_delta (type a) (module X : Id_delta with type t = a) (y : a) =
  X.id y

let _ = use_delta (module M_delta : Id_delta with type t = int) true

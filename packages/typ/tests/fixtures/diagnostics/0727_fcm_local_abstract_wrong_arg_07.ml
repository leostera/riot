module type Id_eta = sig
  type t
  val id : t -> t
end

module M_eta = struct
  type t = int
  let id x = x
end

let use_eta (type a) (module X : Id_eta with type t = a) (y : a) =
  X.id y

let _ = use_eta (module M_eta : Id_eta with type t = int) true

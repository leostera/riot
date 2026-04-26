module type Id_epsilon = sig
  type t
  val id : t -> t
end

module M_epsilon = struct
  type t = int
  let id x = x
end

let use_epsilon (type a) (module X : Id_epsilon with type t = a) (y : a) =
  X.id y

let _ = use_epsilon (module M_epsilon : Id_epsilon with type t = int) true

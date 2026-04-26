module type Box_gamma = sig
  type t
  val value : t
end

let same_gamma p q =
  let module P = (val p : Box_gamma) in
  let module Q = (val q : Box_gamma) in
  (P.value : Q.t)

module type Box_kappa = sig
  type t
  val value : t
end

let same_kappa p q =
  let module P = (val p : Box_kappa) in
  let module Q = (val q : Box_kappa) in
  (P.value : Q.t)

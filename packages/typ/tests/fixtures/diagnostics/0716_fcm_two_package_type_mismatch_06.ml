module type Box_zeta = sig
  type t
  val value : t
end

let same_zeta p q =
  let module P = (val p : Box_zeta) in
  let module Q = (val q : Box_zeta) in
  (P.value : Q.t)

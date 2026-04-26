module type Box_theta = sig
  type t
  val value : t
end

let same_theta p q =
  let module P = (val p : Box_theta) in
  let module Q = (val q : Box_theta) in
  (P.value : Q.t)

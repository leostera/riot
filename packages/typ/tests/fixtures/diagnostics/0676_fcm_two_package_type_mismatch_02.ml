module type Box_beta = sig
  type t
  val value : t
end

let same_beta p q =
  let module P = (val p : Box_beta) in
  let module Q = (val q : Box_beta) in
  (P.value : Q.t)

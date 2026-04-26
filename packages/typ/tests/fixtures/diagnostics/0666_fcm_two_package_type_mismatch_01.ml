module type Box_alpha = sig
  type t
  val value : t
end

let same_alpha p q =
  let module P = (val p : Box_alpha) in
  let module Q = (val q : Box_alpha) in
  (P.value : Q.t)

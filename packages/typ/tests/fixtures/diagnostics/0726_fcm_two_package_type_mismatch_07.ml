module type Box_eta = sig
  type t
  val value : t
end

let same_eta p q =
  let module P = (val p : Box_eta) in
  let module Q = (val q : Box_eta) in
  (P.value : Q.t)

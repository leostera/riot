module type Box_iota = sig
  type t
  val value : t
end

let same_iota p q =
  let module P = (val p : Box_iota) in
  let module Q = (val q : Box_iota) in
  (P.value : Q.t)

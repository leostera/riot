module type Box_delta = sig
  type t
  val value : t
end

let same_delta p q =
  let module P = (val p : Box_delta) in
  let module Q = (val q : Box_delta) in
  (P.value : Q.t)

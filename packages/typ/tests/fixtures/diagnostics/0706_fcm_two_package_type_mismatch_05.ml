module type Box_epsilon = sig
  type t
  val value : t
end

let same_epsilon p q =
  let module P = (val p : Box_epsilon) in
  let module Q = (val q : Box_epsilon) in
  (P.value : Q.t)

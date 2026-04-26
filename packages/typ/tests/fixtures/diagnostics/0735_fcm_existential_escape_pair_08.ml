module type Box_theta = sig
  type t
  val value : t
end

let escape_pair_theta p =
  let module M = (val p : Box_theta) in
  (M.value, M.value)

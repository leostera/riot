module type Box_beta = sig
  type t
  val value : t
end

let escape_pair_beta p =
  let module M = (val p : Box_beta) in
  (M.value, M.value)

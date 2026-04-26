module type Box_gamma = sig
  type t
  val value : t
end

let escape_pair_gamma p =
  let module M = (val p : Box_gamma) in
  (M.value, M.value)

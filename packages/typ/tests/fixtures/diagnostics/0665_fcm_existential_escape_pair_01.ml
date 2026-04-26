module type Box_alpha = sig
  type t
  val value : t
end

let escape_pair_alpha p =
  let module M = (val p : Box_alpha) in
  (M.value, M.value)

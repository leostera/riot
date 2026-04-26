module type Box_alpha = sig
  type t
  val value : t
end

let escape_alpha p =
  let module M = (val p : Box_alpha) in
  M.value

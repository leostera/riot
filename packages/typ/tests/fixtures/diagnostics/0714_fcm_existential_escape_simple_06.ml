module type Box_zeta = sig
  type t
  val value : t
end

let escape_zeta p =
  let module M = (val p : Box_zeta) in
  M.value

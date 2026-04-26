module type Box_kappa = sig
  type t
  val value : t
end

let escape_kappa p =
  let module M = (val p : Box_kappa) in
  M.value

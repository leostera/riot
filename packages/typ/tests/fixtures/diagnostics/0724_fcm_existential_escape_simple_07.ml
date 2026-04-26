module type Box_eta = sig
  type t
  val value : t
end

let escape_eta p =
  let module M = (val p : Box_eta) in
  M.value

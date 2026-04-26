module type Box_theta = sig
  type t
  val value : t
end

let escape_theta p =
  let module M = (val p : Box_theta) in
  M.value

module type Box_delta = sig
  type t
  val value : t
end

let escape_delta p =
  let module M = (val p : Box_delta) in
  M.value

module type Box_gamma = sig
  type t
  val value : t
end

let consume_int_gamma (x : int) = x

let bad_gamma p =
  let module M = (val p : Box_gamma) in
  consume_int_gamma M.value

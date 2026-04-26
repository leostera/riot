module type Box_alpha = sig
  type t
  val value : t
end

let consume_int_alpha (x : int) = x

let bad_alpha p =
  let module M = (val p : Box_alpha) in
  consume_int_alpha M.value

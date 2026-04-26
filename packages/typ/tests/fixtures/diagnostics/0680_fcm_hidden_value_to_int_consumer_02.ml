module type Box_beta = sig
  type t
  val value : t
end

let consume_int_beta (x : int) = x

let bad_beta p =
  let module M = (val p : Box_beta) in
  consume_int_beta M.value

module type Box_delta = sig
  type t
  val value : t
end

let consume_int_delta (x : int) = x

let bad_delta p =
  let module M = (val p : Box_delta) in
  consume_int_delta M.value

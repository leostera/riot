module type Box_iota = sig
  type t
  val value : t
end

let consume_int_iota (x : int) = x

let bad_iota p =
  let module M = (val p : Box_iota) in
  consume_int_iota M.value

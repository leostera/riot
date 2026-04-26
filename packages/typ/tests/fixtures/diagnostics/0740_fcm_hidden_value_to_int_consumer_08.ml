module type Box_theta = sig
  type t
  val value : t
end

let consume_int_theta (x : int) = x

let bad_theta p =
  let module M = (val p : Box_theta) in
  consume_int_theta M.value

module type Box_epsilon = sig
  type t
  val value : t
end

let consume_int_epsilon (x : int) = x

let bad_epsilon p =
  let module M = (val p : Box_epsilon) in
  consume_int_epsilon M.value

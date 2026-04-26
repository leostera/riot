module type Box_eta = sig
  type t
  val value : t
end

let consume_int_eta (x : int) = x

let bad_eta p =
  let module M = (val p : Box_eta) in
  consume_int_eta M.value

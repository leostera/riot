module type Box_kappa = sig
  type t
  val value : t
end

let consume_int_kappa (x : int) = x

let bad_kappa p =
  let module M = (val p : Box_kappa) in
  consume_int_kappa M.value

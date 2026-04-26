module type Box_zeta = sig
  type t
  val value : t
end

let consume_int_zeta (x : int) = x

let bad_zeta p =
  let module M = (val p : Box_zeta) in
  consume_int_zeta M.value

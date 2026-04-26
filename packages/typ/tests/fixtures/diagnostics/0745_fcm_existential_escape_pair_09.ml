module type Box_iota = sig
  type t
  val value : t
end

let escape_pair_iota p =
  let module M = (val p : Box_iota) in
  (M.value, M.value)

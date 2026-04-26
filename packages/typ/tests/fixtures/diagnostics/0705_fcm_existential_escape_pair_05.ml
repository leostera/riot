module type Box_epsilon = sig
  type t
  val value : t
end

let escape_pair_epsilon p =
  let module M = (val p : Box_epsilon) in
  (M.value, M.value)

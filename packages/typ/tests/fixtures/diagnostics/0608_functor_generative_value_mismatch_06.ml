module Make_zeta () = struct
  type t = T
  let value = T
end

module A_zeta = Make_zeta ()
module B_zeta = Make_zeta ()

let _ : B_zeta.t = A_zeta.value

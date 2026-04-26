module Make_iota () = struct
  type t = T
  let value = T
end

module A_iota = Make_iota ()
module B_iota = Make_iota ()

let _ : B_iota.t = A_iota.value

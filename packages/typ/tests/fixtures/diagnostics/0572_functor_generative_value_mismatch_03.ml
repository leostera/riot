module Make_gamma () = struct
  type t = T
  let value = T
end

module A_gamma = Make_gamma ()
module B_gamma = Make_gamma ()

let _ : B_gamma.t = A_gamma.value

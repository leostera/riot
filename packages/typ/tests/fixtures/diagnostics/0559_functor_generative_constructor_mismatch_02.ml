module Make_beta () = struct
  type t = T
end

module A_beta = Make_beta ()
module B_beta = Make_beta ()

let _ : B_beta.t = A_beta.T

module Make_theta () = struct
  type t = T
end

module A_theta = Make_theta ()
module B_theta = Make_theta ()

let _ : B_theta.t = A_theta.T

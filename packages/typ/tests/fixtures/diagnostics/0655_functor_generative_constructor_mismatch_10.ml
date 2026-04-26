module Make_kappa () = struct
  type t = T
end

module A_kappa = Make_kappa ()
module B_kappa = Make_kappa ()

let _ : B_kappa.t = A_kappa.T

module Make_epsilon () = struct
  type t = T
end

module A_epsilon = Make_epsilon ()
module B_epsilon = Make_epsilon ()

let _ : B_epsilon.t = A_epsilon.T

module Make_eta () = struct
  type t = T
  let value = T
end

module A_eta = Make_eta ()
module B_eta = Make_eta ()

let _ : B_eta.t = A_eta.value

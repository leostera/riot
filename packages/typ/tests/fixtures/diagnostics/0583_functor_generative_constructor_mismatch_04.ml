module Make_delta () = struct
  type t = T
end

module A_delta = Make_delta ()
module B_delta = Make_delta ()

let _ : B_delta.t = A_delta.T

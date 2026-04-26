module Make_alpha () = struct
  type t = T
  let value = T
end

module A_alpha = Make_alpha ()
module B_alpha = Make_alpha ()

let _ : B_alpha.t = A_alpha.value

module type S_theta = sig
  type t
  val x : t
end

module Impl_theta = struct
  type t = int
  let x = 7
end

module Hidden_theta : S_theta = Impl_theta
module Alias_theta = Hidden_theta

let _ : int = Alias_theta.x

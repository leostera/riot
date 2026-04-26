module type S_epsilon = sig
  type t
  val x : t
end

module Impl_epsilon = struct
  type t = int
  let x = 4
end

module Hidden_epsilon : S_epsilon = Impl_epsilon
module Alias_epsilon = Hidden_epsilon

let _ : int = Alias_epsilon.x

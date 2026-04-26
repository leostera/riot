module type S_gamma = sig
  type t
  val x : t
end

module Impl_gamma = struct
  type t = int
  let x = 2
end

module Hidden_gamma : S_gamma = Impl_gamma
module Alias_gamma = Hidden_gamma

let _ : int = Alias_gamma.x

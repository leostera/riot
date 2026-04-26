let ( + ) (x : int) (y : int) : int = x

module type S_gamma = sig
  type t
  val x : t
end

module Impl_gamma = struct
  type t = int
  let x = 2
end

module Hidden_gamma : S_gamma = Impl_gamma
let _ = Hidden_gamma.x + 3

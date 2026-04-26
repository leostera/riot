let ( + ) (x : int) (y : int) : int = x

module type S_beta = sig
  type t
  val x : t
end

module Impl_beta = struct
  type t = int
  let x = 1
end

module Hidden_beta : S_beta = Impl_beta
let _ = Hidden_beta.x + 2

let ( + ) (x : int) (y : int) : int = x

module type S_alpha = sig
  type t
  val x : t
end

module Impl_alpha = struct
  type t = int
  let x = 0
end

module Hidden_alpha : S_alpha = Impl_alpha
let _ = Hidden_alpha.x + 1

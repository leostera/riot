let ( + ) (x : int) (y : int) : int = x

module type S_eta = sig
  type t
  val x : t
end

module Impl_eta = struct
  type t = int
  let x = 6
end

module Hidden_eta : S_eta = Impl_eta
let _ = Hidden_eta.x + 7

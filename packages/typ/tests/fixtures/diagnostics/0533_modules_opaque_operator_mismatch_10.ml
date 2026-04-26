let ( + ) (x : int) (y : int) : int = x

module type S_kappa = sig
  type t
  val x : t
end

module Impl_kappa = struct
  type t = int
  let x = 9
end

module Hidden_kappa : S_kappa = Impl_kappa
let _ = Hidden_kappa.x + 10

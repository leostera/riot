let ( + ) (x : int) (y : int) : int = x

module type S_zeta = sig
  type t
  val x : t
end

module Impl_zeta = struct
  type t = int
  let x = 5
end

module Hidden_zeta : S_zeta = Impl_zeta
let _ = Hidden_zeta.x + 6

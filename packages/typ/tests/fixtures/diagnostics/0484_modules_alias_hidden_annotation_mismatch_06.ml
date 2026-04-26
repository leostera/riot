module type S_zeta = sig
  type t
  val x : t
end

module Impl_zeta = struct
  type t = int
  let x = 5
end

module Hidden_zeta : S_zeta = Impl_zeta
module Alias_zeta = Hidden_zeta

let _ : int = Alias_zeta.x

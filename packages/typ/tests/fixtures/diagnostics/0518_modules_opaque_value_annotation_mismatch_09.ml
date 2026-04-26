module type S_iota = sig
  type t
  val x : t
end

module Impl_iota = struct
  type t = int
  let x = 8
end

module Hidden_iota : S_iota = Impl_iota
let _ : int = Hidden_iota.x

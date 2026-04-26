module type S_delta = sig
  type t
  val x : t
end

module Impl_delta = struct
  type t = int
  let x = 3
end

module Hidden_delta : S_delta = Impl_delta
module Alias_delta = Hidden_delta

let _ : int = Alias_delta.x

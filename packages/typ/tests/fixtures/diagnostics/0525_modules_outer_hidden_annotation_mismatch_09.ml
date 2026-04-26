module Outer_iota = struct
  module type S = sig
    type t
    val x : t
  end

  module Impl = struct
    type t = int
    let x = 8
  end

  module Hidden : S = Impl
end

let _ : int = Outer_iota.Hidden.x

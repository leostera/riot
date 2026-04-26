module Outer_gamma = struct
  module type S = sig
    type t
    val x : t
  end

  module Impl = struct
    type t = int
    let x = 2
  end

  module Hidden : S = Impl
end

let _ : int = Outer_gamma.Hidden.x

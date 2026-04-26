module Outer_theta = struct
  module type S = sig
    type t
    val x : t
  end

  module Impl = struct
    type t = int
    let x = 7
  end

  module Hidden : S = Impl
end

let _ : int = Outer_theta.Hidden.x

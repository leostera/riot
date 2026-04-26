module Outer_alpha = struct
  module type S = sig
    type t
    val x : t
  end

  module Impl = struct
    type t = int
    let x = 0
  end

  module Hidden : S = Impl
end

let _ : int = Outer_alpha.Hidden.x

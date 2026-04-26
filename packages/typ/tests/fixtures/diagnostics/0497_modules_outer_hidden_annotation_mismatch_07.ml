module Outer_eta = struct
  module type S = sig
    type t
    val x : t
  end

  module Impl = struct
    type t = int
    let x = 6
  end

  module Hidden : S = Impl
end

let _ : int = Outer_eta.Hidden.x

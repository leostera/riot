module Outer_epsilon = struct
  module type S = sig
    type t
    val x : t
  end

  module Impl = struct
    type t = int
    let x = 4
  end

  module Hidden : S = Impl
end

let _ : int = Outer_epsilon.Hidden.x

module Outer_zeta = struct
  module type S = sig
    type t
    val x : t
  end

  module Impl = struct
    type t = int
    let x = 5
  end

  module Hidden : S = Impl
end

let _ : int = Outer_zeta.Hidden.x

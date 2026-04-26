module Outer_kappa = struct
  module type S = sig
    type t
    val x : t
  end

  module Impl = struct
    type t = int
    let x = 9
  end

  module Hidden : S = Impl
end

let _ : int = Outer_kappa.Hidden.x

module Outer_delta = struct
  module type S = sig
    type t
    val x : t
  end

  module Impl = struct
    type t = int
    let x = 3
  end

  module Hidden : S = Impl
end

let _ : int = Outer_delta.Hidden.x

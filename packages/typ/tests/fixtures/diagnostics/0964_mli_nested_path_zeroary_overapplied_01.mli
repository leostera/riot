module Outer_alpha : sig
  module Inner : sig
    type t
  end
end

val x_alpha : int Outer_alpha.Inner.t

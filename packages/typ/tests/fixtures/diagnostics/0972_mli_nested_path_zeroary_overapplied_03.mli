module Outer_gamma : sig
  module Inner : sig
    type t
  end
end

val x_gamma : int Outer_gamma.Inner.t

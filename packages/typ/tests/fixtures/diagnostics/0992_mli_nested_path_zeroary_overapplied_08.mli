module Outer_theta : sig
  module Inner : sig
    type t
  end
end

val x_theta : int Outer_theta.Inner.t

module Outer_epsilon : sig
  module Inner : sig
    type t
  end
end

val x_epsilon : int Outer_epsilon.Inner.t

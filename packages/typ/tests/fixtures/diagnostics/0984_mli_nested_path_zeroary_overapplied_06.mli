module Outer_zeta : sig
  module Inner : sig
    type t
  end
end

val x_zeta : int Outer_zeta.Inner.t

module Outer_kappa : sig
  module Inner : sig
    type t
  end
end

val x_kappa : int Outer_kappa.Inner.t

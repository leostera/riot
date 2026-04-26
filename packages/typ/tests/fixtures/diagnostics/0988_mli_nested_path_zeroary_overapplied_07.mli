module Outer_eta : sig
  module Inner : sig
    type t
  end
end

val x_eta : int Outer_eta.Inner.t

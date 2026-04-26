module Outer_beta : sig
  module Inner : sig
    type t
  end
end

val x_beta : int Outer_beta.Inner.t

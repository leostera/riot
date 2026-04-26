module Outer_iota : sig
  module Inner : sig
    type t
  end
end

val x_iota : int Outer_iota.Inner.t

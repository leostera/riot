module Outer_delta : sig
  module Inner : sig
    type t
  end
end

val x_delta : int Outer_delta.Inner.t

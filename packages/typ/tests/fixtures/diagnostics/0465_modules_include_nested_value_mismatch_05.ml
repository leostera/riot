module type S_epsilon = sig
  module Inner : sig
    val x : int
  end
end

module R : S_epsilon = struct
  include struct
    module Inner = struct
      let x = true
    end
  end
end

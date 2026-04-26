module type S_delta = sig
  module Inner : sig
    val x : int
  end
end

module Q : S_delta = struct
  include struct
    module Inner = struct
      let x = true
    end
  end
end

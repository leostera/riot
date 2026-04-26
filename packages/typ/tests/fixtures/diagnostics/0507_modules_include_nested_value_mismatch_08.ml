module type S_theta = sig
  module Inner : sig
    val x : int
  end
end

module U : S_theta = struct
  include struct
    module Inner = struct
      let x = true
    end
  end
end

module type S_zeta = sig
  module Inner : sig
    val x : int
  end
end

module S : S_zeta = struct
  include struct
    module Inner = struct
      let x = true
    end
  end
end

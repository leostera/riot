module type S_kappa = sig
  module Inner : sig
    val x : int
  end
end

module W : S_kappa = struct
  include struct
    module Inner = struct
      let x = true
    end
  end
end

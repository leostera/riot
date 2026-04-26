module type S_beta = sig
  module Inner : sig
    val x : int
  end
end

module N : S_beta = struct
  include struct
    module Inner = struct
      let x = true
    end
  end
end

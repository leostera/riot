module type S_gamma = sig
  module Inner : sig
    val x : int
  end
end

module P : S_gamma = struct
  include struct
    module Inner = struct
      let x = true
    end
  end
end

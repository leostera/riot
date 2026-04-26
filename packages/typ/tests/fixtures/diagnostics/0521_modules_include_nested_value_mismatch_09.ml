module type S_iota = sig
  module Inner : sig
    val x : int
  end
end

module V : S_iota = struct
  include struct
    module Inner = struct
      let x = true
    end
  end
end

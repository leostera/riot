module type S_eta = sig
  module Inner : sig
    val x : int
  end
end

module T : S_eta = struct
  include struct
    module Inner = struct
      let x = true
    end
  end
end

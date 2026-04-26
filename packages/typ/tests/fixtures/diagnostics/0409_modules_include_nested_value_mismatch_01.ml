module type S_alpha = sig
  module Inner : sig
    val x : int
  end
end

module M : S_alpha = struct
  include struct
    module Inner = struct
      let x = true
    end
  end
end

module type S_zeta = sig
  val x : int
end

module S : S_zeta = struct
  include struct
    let x = true
  end
end

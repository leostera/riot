module type S_beta = sig
  val x : int
end

module N : S_beta = struct
  include struct
    let x = true
  end
end

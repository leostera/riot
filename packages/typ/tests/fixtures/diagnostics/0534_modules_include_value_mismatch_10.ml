module type S_kappa = sig
  val x : int
end

module W : S_kappa = struct
  include struct
    let x = true
  end
end

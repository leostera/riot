module type S_theta = sig
  val x : int
end

module U : S_theta = struct
  include struct
    let x = true
  end
end

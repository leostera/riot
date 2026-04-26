module type S_delta = sig
  val x : int
end

module Q : S_delta = struct
  include struct
    let x = true
  end
end

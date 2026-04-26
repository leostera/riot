module type S_iota = sig
  val x : int
end

module V : S_iota = struct
  include struct
    let x = true
  end
end

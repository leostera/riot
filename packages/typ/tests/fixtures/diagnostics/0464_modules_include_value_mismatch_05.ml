module type S_epsilon = sig
  val x : int
end

module R : S_epsilon = struct
  include struct
    let x = true
  end
end

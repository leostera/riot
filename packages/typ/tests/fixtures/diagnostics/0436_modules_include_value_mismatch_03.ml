module type S_gamma = sig
  val x : int
end

module P : S_gamma = struct
  include struct
    let x = true
  end
end

module type S_alpha = sig
  val x : int
end

module M : S_alpha = struct
  include struct
    let x = true
  end
end

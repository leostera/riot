module type S_eta = sig
  val x : int
end

module T : S_eta = struct
  include struct
    let x = true
  end
end

module R : sig
  module Inner : sig
    val x : bool
  end
end = struct
  module Inner = struct
    let x = 4
  end
end

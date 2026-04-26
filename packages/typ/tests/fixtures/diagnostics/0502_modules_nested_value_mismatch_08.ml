module U : sig
  module Inner : sig
    val x : bool
  end
end = struct
  module Inner = struct
    let x = 7
  end
end

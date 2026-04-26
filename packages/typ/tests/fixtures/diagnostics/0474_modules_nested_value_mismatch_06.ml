module S : sig
  module Inner : sig
    val x : bool
  end
end = struct
  module Inner = struct
    let x = 5
  end
end

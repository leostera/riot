module Q : sig
  module Inner : sig
    val x : bool
  end
end = struct
  module Inner = struct
    let x = 3
  end
end

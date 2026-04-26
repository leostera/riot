module N : sig
  module Inner : sig
    val x : bool
  end
end = struct
  module Inner = struct
    let x = 1
  end
end

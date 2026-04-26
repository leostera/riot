module T : sig
  module Inner : sig
    val x : bool
  end
end = struct
  module Inner = struct
    let x = 6
  end
end

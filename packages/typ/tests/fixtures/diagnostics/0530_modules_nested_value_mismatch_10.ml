module W : sig
  module Inner : sig
    val x : bool
  end
end = struct
  module Inner = struct
    let x = 9
  end
end

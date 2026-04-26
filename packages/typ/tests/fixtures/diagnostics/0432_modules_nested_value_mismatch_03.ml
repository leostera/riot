module P : sig
  module Inner : sig
    val x : bool
  end
end = struct
  module Inner = struct
    let x = 2
  end
end

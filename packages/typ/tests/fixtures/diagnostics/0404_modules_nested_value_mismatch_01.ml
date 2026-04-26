module M : sig
  module Inner : sig
    val x : bool
  end
end = struct
  module Inner = struct
    let x = 0
  end
end

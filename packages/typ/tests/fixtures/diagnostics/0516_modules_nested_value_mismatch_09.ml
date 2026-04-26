module V : sig
  module Inner : sig
    val x : bool
  end
end = struct
  module Inner = struct
    let x = 8
  end
end

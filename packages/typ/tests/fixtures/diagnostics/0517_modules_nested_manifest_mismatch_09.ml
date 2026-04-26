module V : sig
  module Inner : sig
    type t = bool
    val x : t
  end
end = struct
  module Inner = struct
    type t = int
    let x = 8
  end
end

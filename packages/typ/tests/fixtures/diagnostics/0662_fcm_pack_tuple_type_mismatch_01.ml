module type Cell_alpha = sig
  type t
  val cell : t
end

module M_alpha = struct
  type t = int * int
  let cell = (0, 1)
end

let _ = (module M_alpha : Cell_alpha with type t = bool * bool)

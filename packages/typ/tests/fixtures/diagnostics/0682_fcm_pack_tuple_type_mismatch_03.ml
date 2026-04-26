module type Cell_gamma = sig
  type t
  val cell : t
end

module M_gamma = struct
  type t = int * int
  let cell = (2, 3)
end

let _ = (module M_gamma : Cell_gamma with type t = bool * bool)

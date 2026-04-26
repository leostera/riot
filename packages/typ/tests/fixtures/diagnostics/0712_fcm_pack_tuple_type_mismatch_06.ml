module type Cell_zeta = sig
  type t
  val cell : t
end

module M_zeta = struct
  type t = int * int
  let cell = (5, 6)
end

let _ = (module M_zeta : Cell_zeta with type t = bool * bool)

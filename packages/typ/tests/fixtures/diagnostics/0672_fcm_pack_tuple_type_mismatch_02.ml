module type Cell_beta = sig
  type t
  val cell : t
end

module M_beta = struct
  type t = int * int
  let cell = (1, 2)
end

let _ = (module M_beta : Cell_beta with type t = bool * bool)

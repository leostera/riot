module type Cell_kappa = sig
  type t
  val cell : t
end

module M_kappa = struct
  type t = int * int
  let cell = (9, 10)
end

let _ = (module M_kappa : Cell_kappa with type t = bool * bool)

module type Cell_iota = sig
  type t
  val cell : t
end

module M_iota = struct
  type t = int * int
  let cell = (8, 9)
end

let _ = (module M_iota : Cell_iota with type t = bool * bool)

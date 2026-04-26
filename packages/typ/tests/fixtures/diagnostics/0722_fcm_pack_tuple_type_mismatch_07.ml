module type Cell_eta = sig
  type t
  val cell : t
end

module M_eta = struct
  type t = int * int
  let cell = (6, 7)
end

let _ = (module M_eta : Cell_eta with type t = bool * bool)

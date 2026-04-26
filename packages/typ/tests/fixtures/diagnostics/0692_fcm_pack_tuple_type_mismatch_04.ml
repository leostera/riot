module type Cell_delta = sig
  type t
  val cell : t
end

module M_delta = struct
  type t = int * int
  let cell = (3, 4)
end

let _ = (module M_delta : Cell_delta with type t = bool * bool)

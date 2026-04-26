module type Cell_epsilon = sig
  type t
  val cell : t
end

module M_epsilon = struct
  type t = int * int
  let cell = (4, 5)
end

let _ = (module M_epsilon : Cell_epsilon with type t = bool * bool)

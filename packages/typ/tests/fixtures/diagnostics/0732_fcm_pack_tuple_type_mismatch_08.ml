module type Cell_theta = sig
  type t
  val cell : t
end

module M_theta = struct
  type t = int * int
  let cell = (7, 8)
end

let _ = (module M_theta : Cell_theta with type t = bool * bool)

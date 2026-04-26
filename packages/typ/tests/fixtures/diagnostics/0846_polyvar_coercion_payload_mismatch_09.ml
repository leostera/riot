let bad_iota (x : [ `A of int ]) : [ `A of bool ] =
  (x :> [ `A of bool ])

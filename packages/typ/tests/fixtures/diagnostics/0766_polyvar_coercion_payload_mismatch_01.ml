let bad_alpha (x : [ `A of int ]) : [ `A of bool ] =
  (x :> [ `A of bool ])

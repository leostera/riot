let bad_delta (x : [ `A of int ]) : [ `A of bool ] =
  (x :> [ `A of bool ])

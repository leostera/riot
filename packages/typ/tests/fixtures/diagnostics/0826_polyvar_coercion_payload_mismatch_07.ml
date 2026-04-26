let bad_eta (x : [ `A of int ]) : [ `A of bool ] =
  (x :> [ `A of bool ])

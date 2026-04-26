let _ =
  if true then
    (`A 2 : [ `A of int ])
  else
    (`A true : [ `A of bool ])
